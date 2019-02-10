
# https://www.decentlab.com/support

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
        length: 2,
        values: [
          %{
            :name => "Air temperature",
            :convert => fn x -> 175 * Enum.at(x, 0) / 65535 - 45 end,
            :unit => "°C"
          },
          %{
            :name => "Air humidity",
            :convert => fn x -> 100 * Enum.at(x, 1) / 65535 end,
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
      },
      %{
        length: 2,
        values: [
          %{
            :name => "Ambient light (visible + infrared)",
            :convert => fn x -> Enum.at(x, 0) end,
            :unit => nil
          },
          %{
            :name => "Ambient light (infrared)",
            :convert => fn x -> Enum.at(x, 1) end,
            :unit => nil
          },
          %{
            :name => "Illuminance",
            :convert => fn x -> max(max(1.0 * Enum.at(x, 0) - 1.64 * Enum.at(x, 1), 0.59 * Enum.at(x, 0) - 0.86 * Enum.at(x, 1)), 0) * 1.5504 end,
            :unit => "lx"
          }
        ]
      },
      %{
        length: 3,
        values: [
          %{
            :name => "CO2 concentration",
            :convert => fn x -> Enum.at(x, 0) - 32768 end,
            :unit => "ppm"
          },
          %{
            :name => "CO2 sensor status",
            :convert => fn x -> Enum.at(x, 1) end,
            :unit => nil
          },
          %{
            :name => "Raw IR reading",
            :convert => fn x -> Enum.at(x, 2) end,
            :unit => nil
          }
        ]
      },
      %{
        length: 1,
        values: [
          %{
            :name => "Activity counter",
            :convert => fn x -> Enum.at(x, 0) end,
            :unit => nil
          }
        ]
      },
      %{
        length: 1,
        values: [
          %{
            :name => "Total VOC",
            :convert => fn x -> Enum.at(x, 0) end,
            :unit => "ppb"
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

IO.inspect(DecentlabDecoder.decode("020bbd007f0b926a515d48bc4e0262006981c7000093d4000b0111", :hex))
IO.inspect(DecentlabDecoder.decode("020bbd006f0b926a515d48bc4e02620069000b0111", :hex))
IO.inspect(DecentlabDecoder.decode("020bbd00010b92", :hex))
