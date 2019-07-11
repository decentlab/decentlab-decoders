
% https://www.decentlab.com/products/eleven-parameter-weather-station-for-lorawan

-module(decentlab_decoder).
-define(PROTOCOL_VERSION, 2).
-export([decode/1, start/0]).

-define(SENSOR_DEFS,
  [
    #{
      length => 17,
      values => [
        #{
          name => <<"Solar radiation">>,
          convert => fun(X) -> lists:nth(0 + 1, X) - 32768 end,
          unit => <<"W⋅m⁻²"/utf8>>
        },
        #{
          name => <<"Precipitation">>,
          convert => fun(X) -> (lists:nth(1 + 1, X) - 32768) / 1000 end,
          unit => <<"mm"/utf8>>
        },
        #{
          name => <<"Lightning strike count">>,
          convert => fun(X) -> lists:nth(2 + 1, X) - 32768 end
        },
        #{
          name => <<"Lightning average distance">>,
          convert => fun(X) -> lists:nth(3 + 1, X) - 32768 end,
          unit => <<"km"/utf8>>
        },
        #{
          name => <<"Wind speed">>,
          convert => fun(X) -> (lists:nth(4 + 1, X) - 32768) / 100 end,
          unit => <<"m⋅s⁻¹"/utf8>>
        },
        #{
          name => <<"Wind direction">>,
          convert => fun(X) -> (lists:nth(5 + 1, X) - 32768) / 10 end,
          unit => <<"°"/utf8>>
        },
        #{
          name => <<"Maximum wind speed">>,
          convert => fun(X) -> (lists:nth(6 + 1, X) - 32768) / 100 end,
          unit => <<"m⋅s⁻¹"/utf8>>
        },
        #{
          name => <<"Air temperature">>,
          convert => fun(X) -> (lists:nth(7 + 1, X) - 32768) / 10 end,
          unit => <<"°C"/utf8>>
        },
        #{
          name => <<"Vapor pressure">>,
          convert => fun(X) -> (lists:nth(8 + 1, X) - 32768) / 100 end,
          unit => <<"kPa"/utf8>>
        },
        #{
          name => <<"Atmospheric pressure">>,
          convert => fun(X) -> (lists:nth(9 + 1, X) - 32768) / 100 end,
          unit => <<"kPa"/utf8>>
        },
        #{
          name => <<"Relative humidity">>,
          convert => fun(X) -> (lists:nth(10 + 1, X) - 32768) / 10 end,
          unit => <<"%"/utf8>>
        },
        #{
          name => <<"Sensor temperature (internal)">>,
          convert => fun(X) -> (lists:nth(11 + 1, X) - 32768) / 10 end,
          unit => <<"°C"/utf8>>
        },
        #{
          name => <<"X orientation angle">>,
          convert => fun(X) -> (lists:nth(12 + 1, X) - 32768) / 10 end,
          unit => <<"°"/utf8>>
        },
        #{
          name => <<"Y orientation angle">>,
          convert => fun(X) -> (lists:nth(13 + 1, X) - 32768) / 10 end,
          unit => <<"°"/utf8>>
        },
        #{
          name => <<"Compass heading">>,
          convert => fun(X) -> lists:nth(14 + 1, X) - 32768 end,
          unit => <<"°"/utf8>>
        },
        #{
          name => <<"North wind speed">>,
          convert => fun(X) -> (lists:nth(15 + 1, X) - 32768) / 100 end,
          unit => <<"m⋅s⁻¹"/utf8>>
        },
        #{
          name => <<"East wind speed">>,
          convert => fun(X) -> (lists:nth(16 + 1, X) - 32768) / 100 end,
          unit => <<"m⋅s⁻¹"/utf8>>
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


decode(<<ProtocolVersion, DeviceId:16,
         Flags:16/bitstring, Bytes/binary>> = Binary) when is_binary(Binary) ->
  Words = bytes_to_words(Bytes),
  maps:merge(#{<<"Protocol version">> => ProtocolVersion,
               <<"Device Id">> => DeviceId},
             sensor(Words, Flags, ?SENSOR_DEFS));
decode(HexStr) ->
  Binary = hex2bin(HexStr),
  decode(Binary).


start() ->
  io:format("~p~n", [decode("02035a0003800a8000800080008009812b8014810880b4a57c820c810980027fe88056800880040bf5")]),
  io:format("~p~n", [decode("02035a00020bf5")]).


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