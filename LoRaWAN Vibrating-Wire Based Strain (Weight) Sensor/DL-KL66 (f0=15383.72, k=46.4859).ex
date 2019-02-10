
# https://www.decentlab.com/support

defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 3,
        values: [
          %{
            :name => "Counter reading",
            :convert => fn x -> Enum.at(x, 0) end,
            :unit => nil
          },
          %{
            :name => "Measurement interval",
            :convert => fn x -> Enum.at(x, 1) / 32768 end,
            :unit => nil
          },
          %{
            :name => "Frequency",
            :convert => fn x -> Enum.at(x, 0) / Enum.at(x, 1) * 32768 end,
            :unit => "Hz"
          },
          %{
            :name => "Weight",
            :convert => fn x -> (:math.pow(Enum.at(x, 0) / Enum.at(x, 1) * 32768, 2) - :math.pow((15383.72), 2)) * (46.4859) / 1000000 end,
            :unit => "g"
          },
          %{
            :name => "Elongation",
            :convert => fn x -> (:math.pow(Enum.at(x, 0) / Enum.at(x, 1) * 32768, 2) - :math.pow((15383.72), 2)) * (46.4859) / 1000000 * (-1.5) / 1000 * 9.8067 end,
            :unit => "µm"
          },
          %{
            :name => "Strain",
            :convert => fn x -> (:math.pow(Enum.at(x, 0) / Enum.at(x, 1) * 32768, 2) - :math.pow((15383.72), 2)) * (46.4859) / 1000000 * (-1.5) / 1000 * 9.8067 / 0.066 end,
            :unit => "µm⋅m⁻¹"
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

IO.inspect(DecentlabDecoder.decode("0203d400033bf67fff3bf60c60", :hex))
IO.inspect(DecentlabDecoder.decode("0203d400020c60", :hex))
