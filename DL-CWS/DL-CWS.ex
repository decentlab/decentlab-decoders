
# https://www.decentlab.com/support

defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 6,
        values: [
          %{
            :name => "Surface temperature",
            :convert => fn x -> (Enum.at(x, 0) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Air temperature",
            :convert => fn x -> (Enum.at(x, 1) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Air humidity",
            :convert => fn x -> (Enum.at(x, 2) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Dew point",
            :convert => fn x -> (Enum.at(x, 3) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Angle",
            :convert => fn x -> (Enum.at(x, 4) - 32768) end,
            :unit => "°"
          },
          %{
            :name => "Sensor temperature",
            :convert => fn x -> (Enum.at(x, 5) - 32768) / 100 end,
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

IO.inspect(DecentlabDecoder.decode("02463900038a778a95977a874c80478a5e0b74", :hex))
IO.inspect(DecentlabDecoder.decode("02463900020b74", :hex))
