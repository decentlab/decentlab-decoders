
% https://www.decentlab.com/products/sapflow-sensor-for-lorawan

-module(decentlab_decoder).
-define(PROTOCOL_VERSION, 2).
-export([decode/1, start/0]).

-define(SENSOR_DEFS,
  [
    #{
      length => 16,
      values => [
        #{
          name => <<"Sap flow">>,
          convert => fun(X) -> (lists:nth(0 + 1, X) * 16 - 50000) / 1000 end,
          unit => <<"l⋅h⁻¹"/utf8>>
        },
        #{
          name => <<"Heat velocity (outer)">>,
          convert => fun(X) -> (lists:nth(1 + 1, X) * 16 - 50000) / 1000 end,
          unit => <<"cm⋅h⁻¹"/utf8>>
        },
        #{
          name => <<"Heat velocity (inner)">>,
          convert => fun(X) -> (lists:nth(2 + 1, X) * 16 - 50000) / 1000 end,
          unit => <<"cm⋅h⁻¹"/utf8>>
        },
        #{
          name => <<"Alpha (outer)">>,
          convert => fun(X) -> (lists:nth(3 + 1, X) * 32 - 1000000) / 100000 end
        },
        #{
          name => <<"Alpha (inner)">>,
          convert => fun(X) -> (lists:nth(4 + 1, X) * 32 - 1000000) / 100000 end
        },
        #{
          name => <<"Beta (outer)">>,
          convert => fun(X) -> (lists:nth(5 + 1, X) * 32 - 1000000) / 100000 end
        },
        #{
          name => <<"Beta (inner)">>,
          convert => fun(X) -> (lists:nth(6 + 1, X) * 32 - 1000000) / 100000 end
        },
        #{
          name => <<"Tmax (outer)">>,
          convert => fun(X) -> (lists:nth(7 + 1, X) * 2) / 1000 end,
          unit => <<"s"/utf8>>
        },
        #{
          name => <<"Tmax (inner)">>,
          convert => fun(X) -> (lists:nth(8 + 1, X) * 2) / 1000 end,
          unit => <<"s"/utf8>>
        },
        #{
          name => <<"Temperature (outer)">>,
          convert => fun(X) -> (lists:nth(9 + 1, X) - 32768) / 100 end,
          unit => <<"°C"/utf8>>
        },
        #{
          name => <<"Max voltage">>,
          convert => fun(X) -> (lists:nth(10 + 1, X) - 32768) / 1000 end,
          unit => <<"V"/utf8>>
        },
        #{
          name => <<"Min voltage">>,
          convert => fun(X) -> (lists:nth(11 + 1, X) - 32768) / 1000 end,
          unit => <<"V"/utf8>>
        },
        #{
          name => <<"Diagnostic">>,
          convert => fun(X) -> lists:nth(12 + 1, X) + lists:nth(13 + 1, X) * 65536 end
        },
        #{
          name => <<"Upstream Tmax (outer)">>,
          convert => fun(X) -> (lists:nth(14 + 1, X) * 2) / 1000 end,
          unit => <<"s"/utf8>>
        },
        #{
          name => <<"Upstream Tmax (inner)">>,
          convert => fun(X) -> (lists:nth(15 + 1, X) * 2) / 1000 end,
          unit => <<"s"/utf8>>
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
  io:format("~p~n", [decode("023d0100030c290bab0c3e79707a1d78437991490845997e4cacdeaa6e00000000457e415a0b59")]),
  io:format("~p~n", [decode("023d0100020b59")]).


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

where(true, IfTrue, _IfFalse) -> IfTrue;
where(false, _IfTrue, IfFalse) -> IfFalse.