
% https://www.decentlab.com/products/indoor-ambiance-monitor-including-co2-tvoc-and-motion-sensor-for-lorawan

-module(decentlab_decoder).
-define(PROTOCOL_VERSION, 2).
-export([decode/1, start/0]).

-define(SENSOR_DEFS,
  [
    #{
      length => 1,
      values => [
        #{
          name => <<"Battery voltage">>,
          convert => fun(X) -> lists:nth(0 + 1, X) / 1000 end,
          unit => <<"V"/utf8>>
        }
      ]
    },
    #{
      length => 2,
      values => [
        #{
          name => <<"Air temperature">>,
          convert => fun(X) -> 175 * lists:nth(0 + 1, X) / 65535 - 45 end,
          unit => <<"°C"/utf8>>
        },
        #{
          name => <<"Air humidity">>,
          convert => fun(X) -> 100 * lists:nth(1 + 1, X) / 65535 end,
          unit => <<"%"/utf8>>
        }
      ]
    },
    #{
      length => 1,
      values => [
        #{
          name => <<"Barometric pressure">>,
          convert => fun(X) -> lists:nth(0 + 1, X) * 2 end,
          unit => <<"Pa"/utf8>>
        }
      ]
    },
    #{
      length => 2,
      values => [
        #{
          name => <<"Ambient light (visible + infrared)">>,
          convert => fun(X) -> lists:nth(0 + 1, X) end
        },
        #{
          name => <<"Ambient light (infrared)">>,
          convert => fun(X) -> lists:nth(1 + 1, X) end
        },
        #{
          name => <<"Illuminance">>,
          convert => fun(X) -> max(max(1.0 * lists:nth(0 + 1, X) - 1.64 * lists:nth(1 + 1, X), 0.59 * lists:nth(0 + 1, X) - 0.86 * lists:nth(1 + 1, X)), 0) * 1.5504 end,
          unit => <<"lx"/utf8>>
        }
      ]
    },
    #{
      length => 3,
      values => [
        #{
          name => <<"CO2 concentration">>,
          convert => fun(X) -> lists:nth(0 + 1, X) - 32768 end,
          unit => <<"ppm"/utf8>>
        },
        #{
          name => <<"CO2 sensor status">>,
          convert => fun(X) -> lists:nth(1 + 1, X) end
        },
        #{
          name => <<"Raw IR reading">>,
          convert => fun(X) -> lists:nth(2 + 1, X) end
        }
      ]
    },
    #{
      length => 1,
      values => [
        #{
          name => <<"Activity counter">>,
          convert => fun(X) -> lists:nth(0 + 1, X) end
        }
      ]
    },
    #{
      length => 1,
      values => [
        #{
          name => <<"Total VOC">>,
          convert => fun(X) -> lists:nth(0 + 1, X) end,
          unit => <<"ppb"/utf8>>
        }
      ]
    }
  ]
).


decode(<<?PROTOCOL_VERSION, DeviceId:16,
         Flags:16/bitstring, Bytes/binary>> = Binary) when is_binary(Binary) ->
  Words = bytes_to_words(Bytes),
  maps:merge(#{<<"Protocol version">> => ?PROTOCOL_VERSION,
               <<"Device Id">> => DeviceId},
             sensor(Words, Flags, ?SENSOR_DEFS));
decode(HexStr) ->
  Binary = hex2bin(HexStr),
  decode(Binary).


start() ->
  io:format("~p~n", [decode("020bbd007f0b926a515d48bc4e0262006981c7000093d4000b0111")]),
  io:format("~p~n", [decode("020bbd006f0b926a515d48bc4e02620069000b0111")]),
  io:format("~p~n", [decode("020bbd00010b92")]).


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