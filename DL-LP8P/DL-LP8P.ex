
# https://www.decentlab.com/products/co2-temperature-humidity-and-barometric-pressure-sensor-for-lorawan

defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 2,
        values: [
          %{
            :name => "Air temperature",
            :convert => fn x -> 175.72 * Enum.at(x, 0) / 65536 - 46.85 end,
            :unit => "°C"
          },
          %{
            :name => "Air humidity",
            :convert => fn x -> 125 * Enum.at(x, 1) / 65536 - 6 end,
            :unit => "%"
          }
        ]
      },
      %{
        length: 2,
        values: [
          %{
            :name => "Barometer temperature",
            :convert => fn x -> (Enum.at(x, 0) - 5000) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Barometric pressure",
            :convert => fn x -> Enum.at(x, 1) * 2 end,
            :unit => "Pa"
          }
        ]
      },
      %{
        length: 8,
        values: [
          %{
            :name => "CO2 concentration",
            :convert => fn x -> Enum.at(x, 0) - 32768 end,
            :unit => "ppm"
          },
          %{
            :name => "CO2 concentration LPF",
            :convert => fn x -> Enum.at(x, 1) - 32768 end,
            :unit => "ppm"
          },
          %{
            :name => "CO2 sensor temperature",
            :convert => fn x -> (Enum.at(x, 2) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Capacitor voltage 1",
            :convert => fn x -> Enum.at(x, 3) / 1000 end,
            :unit => "V"
          },
          %{
            :name => "Capacitor voltage 2",
            :convert => fn x -> Enum.at(x, 4) / 1000 end,
            :unit => "V"
          },
          %{
            :name => "CO2 sensor status",
            :convert => fn x -> Enum.at(x, 5) end,
            :unit => nil
          },
          %{
            :name => "Raw IR reading",
            :convert => fn x -> Enum.at(x, 6) end,
            :unit => nil
          },
          %{
            :name => "Raw IR reading LPF",
            :convert => fn x -> Enum.at(x, 7) end,
            :unit => nil
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

  defp where(true, if_true, _if_false), do: if_true
  defp where(false, _if_true, if_false), do: if_false

end

IO.inspect(DecentlabDecoder.decode("020578000f67bd618d1cedbd1081d981f4895b0bd80bb50000959895390c25", :hex))
IO.inspect(DecentlabDecoder.decode("020578000b67bd618d1cedbd100c25", :hex))
IO.inspect(DecentlabDecoder.decode("02057800080c25", :hex))
