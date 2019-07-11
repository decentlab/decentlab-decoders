
% https://www.decentlab.com/products/co2-temperature-humidity-and-barometric-pressure-sensor-for-lorawan

-module(decentlab_decoder).
-define(PROTOCOL_VERSION, 2).
-export([decode/1, start/0]).

-define(SENSOR_DEFS,
  [
    #{
      length => 2,
      values => [
        #{
          name => <<"Air temperature">>,
          convert => fun(X) -> 175.72 * lists:nth(0 + 1, X) / 65536 - 46.85 end,
          unit => <<"°C"/utf8>>
        },
        #{
          name => <<"Air humidity">>,
          convert => fun(X) -> 125 * lists:nth(1 + 1, X) / 65536 - 6 end,
          unit => <<"%"/utf8>>
        }
      ]
    },
    #{
      length => 2,
      values => [
        #{
          name => <<"Barometer temperature">>,
          convert => fun(X) -> (lists:nth(0 + 1, X) - 5000) / 100 end,
          unit => <<"°C"/utf8>>
        },
        #{
          name => <<"Barometric pressure">>,
          convert => fun(X) -> lists:nth(1 + 1, X) * 2 end,
          unit => <<"Pa"/utf8>>
        }
      ]
    },
    #{
      length => 8,
      values => [
        #{
          name => <<"CO2 concentration">>,
          convert => fun(X) -> lists:nth(0 + 1, X) - 32768 end,
          unit => <<"ppm"/utf8>>
        },
        #{
          name => <<"CO2 concentration LPF">>,
          convert => fun(X) -> lists:nth(1 + 1, X) - 32768 end,
          unit => <<"ppm"/utf8>>
        },
        #{
          name => <<"CO2 sensor temperature">>,
          convert => fun(X) -> (lists:nth(2 + 1, X) - 32768) / 100 end,
          unit => <<"°C"/utf8>>
        },
        #{
          name => <<"Capacitor voltage 1">>,
          convert => fun(X) -> lists:nth(3 + 1, X) / 1000 end,
          unit => <<"V"/utf8>>
        },
        #{
          name => <<"Capacitor voltage 2">>,
          convert => fun(X) -> lists:nth(4 + 1, X) / 1000 end,
          unit => <<"V"/utf8>>
        },
        #{
          name => <<"CO2 sensor status">>,
          convert => fun(X) -> lists:nth(5 + 1, X) end
        },
        #{
          name => <<"Raw IR reading">>,
          convert => fun(X) -> lists:nth(6 + 1, X) end
        },
        #{
          name => <<"Raw IR reading LPF">>,
          convert => fun(X) -> lists:nth(7 + 1, X) end
        }
      ]
    },
    #{
      length => 1,
      values => [
        #{
          name => <<"Battery voltage">>,
          convert => fun(X) -> lists:nth(0 + 1, X) / 1000 end,
          unit => <<"V"/utf8>>
        }
      ]
    }
  ]
).


decode(<<?PROTOCOL_VERSION, DeviceId:16,
         Flags:16/bitstring, Bytes/binary>> = Binary) when is_binary(Binary) ->
  Words = bytes_to_words(Bytes),
  maps:merge(#{<<"Protocol version">> => ?PROTOCOL_VERSION,
               <<"Device ID">> => DeviceId},
             sensor(Words, Flags, ?SENSOR_DEFS));
decode(HexStr) ->
  Binary = hex2bin(HexStr),
  decode(Binary).


start() ->
  io:format("~p~n", [decode("020578000f67bd618d1cedbd1081d981f4895b0bd80bb50000959895390c25")]),
  io:format("~p~n", [decode("020578000b67bd618d1cedbd100c25")]),
  io:format("~p~n", [decode("02057800080c25")]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sensor(_Words, _Flags, []) -> #{};
sensor(Words, <<Flags:15, 0:1>>, [_Cur | Rest]) ->
  sensor(Words, <<0:1, Flags:15>>, Rest);
sensor(Words, <<Flags:15, 1:1>>,
       [#{length := Len, values := ValueDefs} | RestDefs]) ->
  {X, RestWords} = lists:split(Len, Words),
  Values = value(ValueDefs, X),
  RestSensors = sensor(RestWords, <<0:1, Flags:15>>, RestDefs),
  maps:merge(Values, RestSensors).


value([], _X) -> #{};
value([#{convert := Convert,
         name := Name} = ValueDef | RestDefs], X) ->
  Values = value(RestDefs, X),
  Value = #{<<"value">> => Convert(X)},
  Value2 = add_unit(Value, ValueDef),
  maps:put(Name, Value2, Values);
value([_NoConvert | RestDefs], X) -> value(RestDefs, X).


add_unit(Value, #{unit := Unit}) ->
  maps:put(<<"unit">>, Unit, Value);
add_unit(Value, _NoUnit) ->
  Value.


hex2bin(HexStr) when is_binary(HexStr) -> hex2bin(binary_to_list(HexStr));
hex2bin(HexStr) -> hex2bin(HexStr, []).

hex2bin("", Bins) -> list_to_binary(lists:reverse(Bins));
hex2bin(HexStr, Bins) ->
  Bytes = string:slice(HexStr, 0, 2),
  Word = list_to_integer(Bytes, 16),
  hex2bin(string:slice(HexStr, 2), [Word | Bins]).


bytes_to_words(Bytes) -> bytes_to_words(Bytes, []).

bytes_to_words(<<>>, Words) -> lists:reverse(Words);
bytes_to_words(<<Word:16, Rest/binary>>, Words) ->
  bytes_to_words(Rest, [Word | Words]).