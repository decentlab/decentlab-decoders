
# https://www.decentlab.com/products/eleven-parameter-weather-station-for-lorawan

defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 17,
        values: [
          %{
            :name => "Solar radiation",
            :convert => fn x -> (Enum.at(x, 0) - 32768) / 10 end,
            :unit => "W⋅m⁻²"
          },
          %{
            :name => "Precipitation",
            :convert => fn x -> Enum.at(x, 1) / 1000 end,
            :unit => "mm"
          },
          %{
            :name => "Lightning strike count",
            :convert => fn x -> Enum.at(x, 2) - 32768 end,
            :unit => "None"
          },
          %{
            :name => "Lightning average distance",
            :convert => fn x -> Enum.at(x, 3) - 32768 end,
            :unit => "km"
          },
          %{
            :name => "Wind speed",
            :convert => fn x -> (Enum.at(x, 4) - 32768) / 100 end,
            :unit => "m⋅s⁻¹"
          },
          %{
            :name => "Wind direction",
            :convert => fn x -> (Enum.at(x, 5) - 32768) / 10 end,
            :unit => "°"
          },
          %{
            :name => "Maximum wind speed",
            :convert => fn x -> (Enum.at(x, 6) - 32768) / 100 end,
            :unit => "m⋅s⁻¹"
          },
          %{
            :name => "Air temperature",
            :convert => fn x -> (Enum.at(x, 7) - 32768) / 10 end,
            :unit => "°C"
          },
          %{
            :name => "Vapor pressure",
            :convert => fn x -> (Enum.at(x, 8) - 32768) / 100 end,
            :unit => "kPa"
          },
          %{
            :name => "Barometric pressure",
            :convert => fn x -> (Enum.at(x, 9) - 32768) / 100 end,
            :unit => "kPa"
          },
          %{
            :name => "Relative humidity",
            :convert => fn x -> (Enum.at(x, 10) - 32768) / 10 end,
            :unit => "%"
          },
          %{
            :name => "Internal temperature",
            :convert => fn x -> (Enum.at(x, 11) - 32768) / 10 end,
            :unit => "°C"
          },
          %{
            :name => "Tilt angle, X orientation",
            :convert => fn x -> (Enum.at(x, 12) - 32768) / 10 end,
            :unit => "°"
          },
          %{
            :name => "Tilt angle, Y orientation",
            :convert => fn x -> (Enum.at(x, 13) - 32768) / 10 end,
            :unit => "°"
          },
          %{
            :name => "Precipitation electrical conductivity",
            :convert => fn x -> Enum.at(x, 14) - 32768 end,
            :unit => "µS⋅cm⁻¹"
          },
          %{
            :name => "Cumulative precipitation",
            :convert => fn x -> (Enum.at(x, 15) + Enum.at(x, 16) * 65536) / 1000 end,
            :unit => "mm"
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

IO.inspect(DecentlabDecoder.decode("025ef80003805c000080008000803484b3803680e78086a60181d680ed81c9809f8000117000010adc", :hex))
IO.inspect(DecentlabDecoder.decode("025ef800020adc", :hex))
