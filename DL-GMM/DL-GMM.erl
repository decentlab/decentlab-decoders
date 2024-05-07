
% https://www.decentlab.com/products/greenhouse-multi-monitor-for-lorawan

-module(decentlab_decoder).
-define(PROTOCOL_VERSION, 2).
-export([decode/1, start/0]).

-define(SENSOR_DEFS,
  [
    #{
      length => 7,
      values => [
        #{
          name => <<"Photosynthetically active radiation">>,
          convert => fun(X) -> (lists:nth(0 + 1, X) - 32768) / 10 end,
          unit => <<"µmol⋅m⁻²⋅s⁻¹"/utf8>>
        },
        #{
          name => <<"Air temperature">>,
          convert => fun(X) -> (lists:nth(1 + 1, X) - 32768) / 100 end,
          unit => <<"°C"/utf8>>
        },
        #{
          name => <<"Air humidity">>,
          convert => fun(X) -> (lists:nth(2 + 1, X) - 32768) / 10 end,
          unit => <<"%"/utf8>>
        },
        #{
          name => <<"CO2 concentration">>,
          convert => fun(X) -> (lists:nth(3 + 1, X) - 32768) / 1 end,
          unit => <<"ppm"/utf8>>
        },
        #{
          name => <<"Atmospheric pressure">>,
          convert => fun(X) -> (lists:nth(4 + 1, X) - 32768) / 100 end,
          unit => <<"kPa"/utf8>>
        },
        #{
          name => <<"Vapor pressure deficit">>,
          convert => fun(X) -> (lists:nth(5 + 1, X) - 32768) / 100 end,
          unit => <<"kPa"/utf8>>
        },
        #{
          name => <<"Dew point">>,
          convert => fun(X) -> (lists:nth(6 + 1, X) - 32768) / 100 end,
          unit => <<"°C"/utf8>>
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
  io:format("~p~n", [decode("02532b00038726892081148297a57380cf81700bbc")]),
  io:format("~p~n", [decode("02528500020bbc")]).


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