
% https://www.decentlab.com/products/particulate-matter-temperature-humidity-and-barometric-pressure-sensor-for-lorawan

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
      length => 10,
      values => [
        #{
          name => <<"PM1.0 mass concentration">>,
          convert => fun(X) -> lists:nth(0 + 1, X) / 10 end,
          unit => <<"µg⋅m⁻³"/utf8>>
        },
        #{
          name => <<"PM2.5 mass concentration">>,
          convert => fun(X) -> lists:nth(1 + 1, X) / 10 end,
          unit => <<"µg⋅m⁻³"/utf8>>
        },
        #{
          name => <<"PM4 mass concentration">>,
          convert => fun(X) -> lists:nth(2 + 1, X) / 10 end,
          unit => <<"µg⋅m⁻³"/utf8>>
        },
        #{
          name => <<"PM10 mass concentration">>,
          convert => fun(X) -> lists:nth(3 + 1, X) / 10 end,
          unit => <<"µg⋅m⁻³"/utf8>>
        },
        #{
          name => <<"Typical particle size">>,
          convert => fun(X) -> lists:nth(4 + 1, X) end,
          unit => <<"nm"/utf8>>
        },
        #{
          name => <<"PM0.5 number concentration">>,
          convert => fun(X) -> lists:nth(5 + 1, X) / 10 end,
          unit => <<"1⋅cm⁻³"/utf8>>
        },
        #{
          name => <<"PM1.0 number concentration">>,
          convert => fun(X) -> lists:nth(6 + 1, X) / 10 end,
          unit => <<"1⋅cm⁻³"/utf8>>
        },
        #{
          name => <<"PM2.5 number concentration">>,
          convert => fun(X) -> lists:nth(7 + 1, X) / 10 end,
          unit => <<"1⋅cm⁻³"/utf8>>
        },
        #{
          name => <<"PM4 number concentration">>,
          convert => fun(X) -> lists:nth(8 + 1, X) / 10 end,
          unit => <<"1⋅cm⁻³"/utf8>>
        },
        #{
          name => <<"PM10 number concentration">>,
          convert => fun(X) -> lists:nth(9 + 1, X) / 10 end,
          unit => <<"1⋅cm⁻³"/utf8>>
        }
      ]
    },
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
      length => 1,
      values => [
        #{
          name => <<"Barometric pressure">>,
          convert => fun(X) -> lists:nth(0 + 1, X) * 2 end,
          unit => <<"Pa"/utf8>>
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
  io:format("~p~n", [decode("021b50000f0c25002500270027002701f50107012c012d012d012d67bd618dbd10")]),
  io:format("~p~n", [decode("021b50000d0c2567bd618dbd10")]),
  io:format("~p~n", [decode("021b5000010c25")]).


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