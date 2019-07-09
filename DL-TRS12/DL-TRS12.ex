
# https://www.decentlab.com/support

defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 3,
        values: [
          %{
            :name => "Dielectric permittivity",
            :convert => fn x -> :math.pow(0.000000002887 * :math.pow(Enum.at(x, 0)/10, 3) - 0.0000208 * :math.pow(Enum.at(x, 0)/10, 2) + 0.05276 * (Enum.at(x, 0)/10) - 43.39, 2) end,
            :unit => "None"
          },
          %{
            :name => "Volumetric water content",
            :convert => fn x -> Enum.at(x, 0)/10 * 0.0003879 - 0.6956 end,
            :unit => "m³⋅m⁻³"
          },
          %{
            :name => "Soil temperature",
            :convert => fn x -> (Enum.at(x, 1) - 32768) / 10 end,
            :unit => "°C"
          },
          %{
            :name => "Electrical conductivity",
            :convert => fn x -> Enum.at(x, 2) end,
            :unit => "µS⋅cm⁻¹"
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

IO.inspect(DecentlabDecoder.decode("0210d3000346be813d00000c80", :hex))
IO.inspect(DecentlabDecoder.decode("0210d300020c80", :hex))
