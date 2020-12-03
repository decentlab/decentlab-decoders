
# https://decentlab.squarespace.com/products/laser-distance-level-sensor-for-lorawan

defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 11,
        values: [
          %{
            :name => "Distance: average",
            :convert => fn x -> Enum.at(x, 0) end,
            :unit => "mm"
          },
          %{
            :name => "Distance: minimum",
            :convert => fn x -> Enum.at(x, 1) end,
            :unit => "mm"
          },
          %{
            :name => "Distance: maximum",
            :convert => fn x -> Enum.at(x, 2) end,
            :unit => "mm"
          },
          %{
            :name => "Distance: median",
            :convert => fn x -> Enum.at(x, 3) end,
            :unit => "mm"
          },
          %{
            :name => "Distance: 10th percentile",
            :convert => fn x -> Enum.at(x, 4) end,
            :unit => "mm"
          },
          %{
            :name => "Distance: 25th percentile",
            :convert => fn x -> Enum.at(x, 5) end,
            :unit => "mm"
          },
          %{
            :name => "Distance: 75th percentile",
            :convert => fn x -> Enum.at(x, 6) end,
            :unit => "mm"
          },
          %{
            :name => "Distance: 90th percentile",
            :convert => fn x -> Enum.at(x, 7) end,
            :unit => "mm"
          },
          %{
            :name => "Distance: most frequent value",
            :convert => fn x -> Enum.at(x, 8) end,
            :unit => "mm"
          },
          %{
            :name => "Number of samples",
            :convert => fn x -> Enum.at(x, 9) end,
            :unit => nil
          },
          %{
            :name => "Total acquisition time",
            :convert => fn x -> Enum.at(x, 10) / 1.024 end,
            :unit => "ms"
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

IO.inspect(DecentlabDecoder.decode("0211c90003119b117611bc119e118a119411a811a81194006401990abd", :hex))
IO.inspect(DecentlabDecoder.decode("0211c900020abd", :hex))
