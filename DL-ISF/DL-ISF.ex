
# https://www.decentlab.com/products/sapflow-sensor-for-lorawan

defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 16,
        values: [
          %{
            :name => "Sap flow",
            :convert => fn x -> (Enum.at(x, 0) * 16 - 50000) / 1000 end,
            :unit => "l⋅h⁻¹"
          },
          %{
            :name => "Heat velocity (outer)",
            :convert => fn x -> (Enum.at(x, 1) * 16 - 50000) / 1000 end,
            :unit => "cm⋅h⁻¹"
          },
          %{
            :name => "Heat velocity (inner)",
            :convert => fn x -> (Enum.at(x, 2) * 16 - 50000) / 1000 end,
            :unit => "cm⋅h⁻¹"
          },
          %{
            :name => "Alpha (outer)",
            :convert => fn x -> (Enum.at(x, 3) * 32 - 1000000) / 100000 end,
            :unit => nil
          },
          %{
            :name => "Alpha (inner)",
            :convert => fn x -> (Enum.at(x, 4) * 32 - 1000000) / 100000 end,
            :unit => nil
          },
          %{
            :name => "Beta (outer)",
            :convert => fn x -> (Enum.at(x, 5) * 32 - 1000000) / 100000 end,
            :unit => nil
          },
          %{
            :name => "Beta (inner)",
            :convert => fn x -> (Enum.at(x, 6) * 32 - 1000000) / 100000 end,
            :unit => nil
          },
          %{
            :name => "Tmax (outer)",
            :convert => fn x -> (Enum.at(x, 7) * 2) / 1000 end,
            :unit => "s"
          },
          %{
            :name => "Tmax (inner)",
            :convert => fn x -> (Enum.at(x, 8) * 2) / 1000 end,
            :unit => "s"
          },
          %{
            :name => "Temperature (outer)",
            :convert => fn x -> (Enum.at(x, 9) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Max voltage",
            :convert => fn x -> (Enum.at(x, 10) - 32768) / 1000 end,
            :unit => "V"
          },
          %{
            :name => "Min voltage",
            :convert => fn x -> (Enum.at(x, 11) - 32768) / 1000 end,
            :unit => "V"
          },
          %{
            :name => "Diagnostic",
            :convert => fn x -> Enum.at(x, 12) + Enum.at(x, 13) * 65536 end,
            :unit => nil
          },
          %{
            :name => "Upstream Tmax (outer)",
            :convert => fn x -> (Enum.at(x, 14) * 2) / 1000 end,
            :unit => "s"
          },
          %{
            :name => "Upstream Tmax (inner)",
            :convert => fn x -> (Enum.at(x, 15) * 2) / 1000 end,
            :unit => "s"
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

IO.inspect(DecentlabDecoder.decode("023d0100030c290bab0c3e79707a1d78437991490845997e4cacdeaa6e00000000457e415a0b59", :hex))
IO.inspect(DecentlabDecoder.decode("023d0100020b59", :hex))
