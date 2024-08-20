
# https://www.decentlab.com/products/particulate-matter-temperature-humidity-and-barometric-pressure-sensor-for-lorawan

defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 1,
        values: [
          %{
            :name => "Battery voltage",
            :convert => fn x -> Enum.at(x, 0) / 1000 end,
            :unit => "V"
          }
        ]
      },
      %{
        length: 10,
        values: [
          %{
            :name => "PM1.0 mass concentration",
            :convert => fn x -> Enum.at(x, 0) / 10 end,
            :unit => "µg⋅m⁻³"
          },
          %{
            :name => "PM2.5 mass concentration",
            :convert => fn x -> Enum.at(x, 1) / 10 end,
            :unit => "µg⋅m⁻³"
          },
          %{
            :name => "PM4 mass concentration",
            :convert => fn x -> Enum.at(x, 2) / 10 end,
            :unit => "µg⋅m⁻³"
          },
          %{
            :name => "PM10 mass concentration",
            :convert => fn x -> Enum.at(x, 3) / 10 end,
            :unit => "µg⋅m⁻³"
          },
          %{
            :name => "Typical particle size",
            :convert => fn x -> Enum.at(x, 4) end,
            :unit => "nm"
          },
          %{
            :name => "PM0.5 number concentration",
            :convert => fn x -> Enum.at(x, 5) / 10 end,
            :unit => "1⋅cm⁻³"
          },
          %{
            :name => "PM1.0 number concentration",
            :convert => fn x -> Enum.at(x, 6) / 10 end,
            :unit => "1⋅cm⁻³"
          },
          %{
            :name => "PM2.5 number concentration",
            :convert => fn x -> Enum.at(x, 7) / 10 end,
            :unit => "1⋅cm⁻³"
          },
          %{
            :name => "PM4 number concentration",
            :convert => fn x -> Enum.at(x, 8) / 10 end,
            :unit => "1⋅cm⁻³"
          },
          %{
            :name => "PM10 number concentration",
            :convert => fn x -> Enum.at(x, 9) / 10 end,
            :unit => "1⋅cm⁻³"
          }
        ]
      },
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
        length: 1,
        values: [
          %{
            :name => "Barometric pressure",
            :convert => fn x -> Enum.at(x, 0) * 2 end,
            :unit => "Pa"
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

IO.inspect(DecentlabDecoder.decode("021b50000f0c25002500270027002701f50107012c012d012d012d67bd618dbd10", :hex))
IO.inspect(DecentlabDecoder.decode("021b50000d0c2567bd618dbd10", :hex))
IO.inspect(DecentlabDecoder.decode("021b5000010c25", :hex))
