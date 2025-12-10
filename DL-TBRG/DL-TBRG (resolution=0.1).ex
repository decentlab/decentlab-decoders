
# https://www.decentlab.com/products/tipping-bucket-rain-gauge-for-lorawan

defmodule DecentlabDecoder do
  @protocol_version 2

  # device-specific parameters
  @resolution 0.1

  defp sensor_defs do
    [
      %{
        length: 4,
        values: [
          %{
            :name => "Precipitation",
            :convert => fn x -> Enum.at(x, 0) * @resolution end,
            :unit => "mm"
          },
          %{
            :name => "Precipitation interval",
            :convert => fn x -> Enum.at(x, 1) end,
            :unit => "s"
          },
          %{
            :name => "Cumulative precipitation",
            :convert => fn x -> (Enum.at(x, 2) + Enum.at(x, 3) * 65536) * @resolution end,
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

end

IO.inspect(DecentlabDecoder.decode("0202f8000300040258409a00000c54", :hex))
IO.inspect(DecentlabDecoder.decode("0202f800020c54", :hex))
