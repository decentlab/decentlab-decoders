defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 16,
        values: [
          %{
            :name => "CH0: Temperature",
            :convert => fn x -> (Enum.at(x, 0) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH1: Temperature",
            :convert => fn x -> (Enum.at(x, 1) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH2: Temperature",
            :convert => fn x -> (Enum.at(x, 2) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH3: Temperature",
            :convert => fn x -> (Enum.at(x, 3) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH4: Temperature",
            :convert => fn x -> (Enum.at(x, 4) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH5: Temperature",
            :convert => fn x -> (Enum.at(x, 5) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH6: Temperature",
            :convert => fn x -> (Enum.at(x, 6) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH7: Temperature",
            :convert => fn x -> (Enum.at(x, 7) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH8: Temperature",
            :convert => fn x -> (Enum.at(x, 8) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH9: Temperature",
            :convert => fn x -> (Enum.at(x, 9) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH10: Temperature",
            :convert => fn x -> (Enum.at(x, 10) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH11: Temperature",
            :convert => fn x -> (Enum.at(x, 11) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH12: Temperature",
            :convert => fn x -> (Enum.at(x, 12) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH13: Temperature",
            :convert => fn x -> (Enum.at(x, 13) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH14: Temperature",
            :convert => fn x -> (Enum.at(x, 14) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "CH15: Temperature",
            :convert => fn x -> (Enum.at(x, 15) - 32768) / 100 end,
            :unit => "°C"
          }
        ]
      },
      %{
        length: 1,
        values: [
          %{
            :name => "Battery voltage",
            :convert => fn x -> Enum.at(x, 0) / 1000 end,
            :unit => "V"
          }
        ]
      }
    ]
  end


  def decode(msg, :hex) do
    {:ok, bytes} = Base.decode16(msg, case: :mixed)
    decode(bytes)
  end

  def decode(msg) when is_binary(msg), do: decode_binary(msg)

  def decode(msg), do: to_string(msg) |> decode

  defp decode_binary(<<@protocol_version, device_id::size(16), flags::binary-size(2), bytes::binary>>) do
    bytes
    |> bytes_to_words()
    |> sensor(flags, sensor_defs())
    |> Map.put("Device ID", device_id)
    |> Map.put("Protocol version", @protocol_version)
  end


  defp bytes_to_words(<<>>), do: []

  defp bytes_to_words(<<word::size(16), rest::binary>>), do: [word | bytes_to_words(rest)]


  defp sensor(words, <<flags::size(15), 1::size(1)>>, [%{length: len, values: value_defs} | rest]) do
    {x, rest_words} = Enum.split(words, len)
    value(value_defs, x)
    |> Map.merge(sensor(rest_words, <<0::size(1), flags::size(15)>>, rest))
  end

  defp sensor(words, <<flags::size(15), 0::size(1)>>, [_cur | rest]) do
    sensor(words, <<0::size(1), flags::size(15)>>, rest)
  end

  defp sensor([], _flags, []), do: %{}


  defp value([], _x), do: %{}

  defp value([%{convert: nil} | rest], x), do: value(rest, x)

  defp value([%{name: name, unit: unit, convert: convert} | rest], x) do
    value(rest, x)
    |> Map.put(name, %{"unit" => unit, "value" => convert.(x)})
  end

end

IO.inspect(DecentlabDecoder.decode("023e3e00038abc8a928aa08a848ab38a898ac38aad8ab78a928aa1000000000000000000000afc", :hex))
IO.inspect(DecentlabDecoder.decode("023e3e00020afc", :hex))
