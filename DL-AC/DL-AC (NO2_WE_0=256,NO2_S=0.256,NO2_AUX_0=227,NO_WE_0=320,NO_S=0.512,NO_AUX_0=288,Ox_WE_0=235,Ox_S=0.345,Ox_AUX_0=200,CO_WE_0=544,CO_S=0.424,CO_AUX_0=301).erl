
% https://www.decentlab.com/products/air-quality-station-no2-no-co-ox-for-lorawan

-module(decentlab_decoder).
-define(PROTOCOL_VERSION, 2).
-export([decode/1, start/0]).

% device-specific parameters
-define(NO2_WE_0, 256).
-define(NO2_S, 0.256).
-define(NO2_AUX_0, 227).
-define(NO_WE_0, 320).
-define(NO_S, 0.512).
-define(NO_AUX_0, 288).
-define(Ox_WE_0, 235).
-define(Ox_S, 0.345).
-define(Ox_AUX_0, 200).
-define(CO_WE_0, 544).
-define(CO_S, 0.424).
-define(CO_AUX_0, 301).

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
          name => <<"CH4: NO2 (we)">>,
          convert => fun(X) -> 3 * (lists:nth(0 + 1, X) / 32768 - 1) * 1000 end,
          unit => <<"mV"/utf8>>
        },
        #{
          name => <<"CH4: NO2 (we-aux)">>,
          convert => fun(X) -> 3 * (lists:nth(1 + 1, X) / 32768 - 1) * 1000 end,
          unit => <<"mV"/utf8>>
        },
        #{
          name => <<"CH4: NO2 concentration (we)">>,
          convert => fun(X) -> (3 * (lists:nth(0 + 1, X) / 32768 - 1) * 1000 - ?NO2_WE_0) / ?NO2_S end,
          unit => <<"ppb"/utf8>>
        },
        #{
          name => <<"CH4: NO2 concentration (we-aux)">>,
          convert => fun(X) -> (3 * (lists:nth(1 + 1, X) / 32768 - 1) * 1000 - ?NO2_WE_0 + ?NO2_AUX_0) / ?NO2_S end,
          unit => <<"ppb"/utf8>>
        }
      ]
    },
    #{
      length => 2,
      values => [
        #{
          name => <<"CH5: NO (we)">>,
          convert => fun(X) -> 3 * (lists:nth(0 + 1, X) / 32768 - 1) * 1000 end,
          unit => <<"mV"/utf8>>
        },
        #{
          name => <<"CH5: NO (we-aux)">>,
          convert => fun(X) -> 3 * (lists:nth(1 + 1, X) / 32768 - 1) * 1000 end,
          unit => <<"mV"/utf8>>
        },
        #{
          name => <<"CH5: NO concentration (we)">>,
          convert => fun(X) -> (3 * (lists:nth(0 + 1, X) / 32768 - 1) * 1000 - ?NO_WE_0) / ?NO_S end,
          unit => <<"ppb"/utf8>>
        },
        #{
          name => <<"CH5: NO concentration (we-aux)">>,
          convert => fun(X) -> (3 * (lists:nth(1 + 1, X) / 32768 - 1) * 1000 - ?NO_WE_0 + ?NO_AUX_0) / ?NO_S end,
          unit => <<"ppb"/utf8>>
        }
      ]
    },
    #{
      length => 2,
      values => [
        #{
          name => <<"CH6: Ox (we)">>,
          convert => fun(X) -> 3 * (lists:nth(0 + 1, X) / 32768 - 1) * 1000 end,
          unit => <<"mV"/utf8>>
        },
        #{
          name => <<"CH6: Ox (we-aux)">>,
          convert => fun(X) -> 3 * (lists:nth(1 + 1, X) / 32768 - 1) * 1000 end,
          unit => <<"mV"/utf8>>
        },
        #{
          name => <<"CH6: Ox concentration (we)">>,
          convert => fun(X) -> (3 * (lists:nth(0 + 1, X) / 32768 - 1) * 1000 - ?Ox_WE_0) / ?Ox_S end,
          unit => <<"ppb"/utf8>>
        },
        #{
          name => <<"CH6: Ox concentration (we-aux)">>,
          convert => fun(X) -> (3 * (lists:nth(1 + 1, X) / 32768 - 1) * 1000 - ?Ox_WE_0 + ?Ox_AUX_0) / ?Ox_S end,
          unit => <<"ppb"/utf8>>
        }
      ]
    },
    #{
      length => 2,
      values => [
        #{
          name => <<"CH7: CO (we)">>,
          convert => fun(X) -> 3 * (lists:nth(0 + 1, X) / 32768 - 1) * 1000 end,
          unit => <<"mV"/utf8>>
        },
        #{
          name => <<"CH7: CO (we-aux)">>,
          convert => fun(X) -> 3 * (lists:nth(1 + 1, X) / 32768 - 1) * 1000 end,
          unit => <<"mV"/utf8>>
        },
        #{
          name => <<"CH7: CO concentration (we)">>,
          convert => fun(X) -> (3 * (lists:nth(0 + 1, X) / 32768 - 1) * 1000 - ?CO_WE_0) / ?CO_S end,
          unit => <<"ppb"/utf8>>
        },
        #{
          name => <<"CH7: CO concentration (we-aux)">>,
          convert => fun(X) -> (3 * (lists:nth(1 + 1, X) / 32768 - 1) * 1000 - ?CO_WE_0 + ?CO_AUX_0) / ?CO_S end,
          unit => <<"ppb"/utf8>>
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
               <<"Device Id">> => DeviceId},
             sensor(Words, Flags, ?SENSOR_DEFS));
decode(HexStr) ->
  Binary = hex2bin(HexStr),
  decode(Binary).


start() ->
  io:format("~p~n", [decode("020fa0003f66b49b8c8966803c8cf580238a68804c903783f4158a")]),
  io:format("~p~n", [decode("020fa00020158a")]).


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