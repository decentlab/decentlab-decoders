
# https://www.decentlab.com/products/pressure-/-liquid-level-temperature-and-electrical-conductivity-sensor-for-lorawan

defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 4,
        values: [
          %{
            :name => "Water depth",
            :convert => fn x -> Enum.at(x, 0) - 32768 end,
            :unit => "mm"
          },
          %{
            :name => "Temperature",
            :convert => fn x -> (Enum.at(x, 1) - 32768) / 10 end,
            :unit => "°C"
          },
          %{
            :name => "Electrical conductivity",
            :convert => fn x -> Enum.at(x, 2) * 2 end,
            :unit => "µS⋅cm⁻¹"
          },
          %{
            :name => "Freezing flag",
            :convert => fn x -> Enum.at(x, 3) end,
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

IO.inspect(DecentlabDecoder.decode("0207d9000390888081006400000c60", :hex))
IO.inspect(DecentlabDecoder.decode("0207d900020c60", :hex))
