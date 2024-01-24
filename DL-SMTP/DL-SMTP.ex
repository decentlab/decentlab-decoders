
# https://www.decentlab.com/products/soil-moisture-and-temperature-profile-for-lorawan

defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 16,
        values: [
          %{
            :name => "Soil moisture at depth 0",
            :convert => fn x -> (Enum.at(x, 0) - 2500) / 500 end,
            :unit => nil
          },
          %{
            :name => "Soil temperature at depth 0",
            :convert => fn x -> (Enum.at(x, 1) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Soil moisture at depth 1",
            :convert => fn x -> (Enum.at(x, 2) - 2500) / 500 end,
            :unit => nil
          },
          %{
            :name => "Soil temperature at depth 1",
            :convert => fn x -> (Enum.at(x, 3) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Soil moisture at depth 2",
            :convert => fn x -> (Enum.at(x, 4) - 2500) / 500 end,
            :unit => nil
          },
          %{
            :name => "Soil temperature at depth 2",
            :convert => fn x -> (Enum.at(x, 5) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Soil moisture at depth 3",
            :convert => fn x -> (Enum.at(x, 6) - 2500) / 500 end,
            :unit => nil
          },
          %{
            :name => "Soil temperature at depth 3",
            :convert => fn x -> (Enum.at(x, 7) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Soil moisture at depth 4",
            :convert => fn x -> (Enum.at(x, 8) - 2500) / 500 end,
            :unit => nil
          },
          %{
            :name => "Soil temperature at depth 4",
            :convert => fn x -> (Enum.at(x, 9) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Soil moisture at depth 5",
            :convert => fn x -> (Enum.at(x, 10) - 2500) / 500 end,
            :unit => nil
          },
          %{
            :name => "Soil temperature at depth 5",
            :convert => fn x -> (Enum.at(x, 11) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Soil moisture at depth 6",
            :convert => fn x -> (Enum.at(x, 12) - 2500) / 500 end,
            :unit => nil
          },
          %{
            :name => "Soil temperature at depth 6",
            :convert => fn x -> (Enum.at(x, 13) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Soil moisture at depth 7",
            :convert => fn x -> (Enum.at(x, 14) - 2500) / 500 end,
            :unit => nil
          },
          %{
            :name => "Soil temperature at depth 7",
            :convert => fn x -> (Enum.at(x, 15) - 32768) / 100 end,
            :unit => "°C"
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

IO.inspect(DecentlabDecoder.decode("020b50000309018a8c09438a9809278a920b3c8aa50c9c8a8c11e08aa500000000000000000b3b", :hex))
IO.inspect(DecentlabDecoder.decode("020b5000020b3b", :hex))
