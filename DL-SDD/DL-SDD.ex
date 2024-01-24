defmodule DecentlabDecoder do
  @protocol_version 2

  defp sensor_defs do
    [
      %{
        length: 18,
        values: [
          %{
            :name => "Moisture at level 0",
            :convert => fn x -> (Enum.at(x, 0) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Moisture at level 1",
            :convert => fn x -> (Enum.at(x, 1) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Moisture at level 2",
            :convert => fn x -> (Enum.at(x, 2) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Moisture at level 3",
            :convert => fn x -> (Enum.at(x, 3) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Moisture at level 4",
            :convert => fn x -> (Enum.at(x, 4) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Moisture at level 5",
            :convert => fn x -> (Enum.at(x, 5) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Temperature at level 0",
            :convert => fn x -> (Enum.at(x, 6) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Temperature at level 1",
            :convert => fn x -> (Enum.at(x, 7) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Temperature at level 2",
            :convert => fn x -> (Enum.at(x, 8) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Temperature at level 3",
            :convert => fn x -> (Enum.at(x, 9) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Temperature at level 4",
            :convert => fn x -> (Enum.at(x, 10) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Temperature at level 5",
            :convert => fn x -> (Enum.at(x, 11) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Salinity at level 0",
            :convert => fn x -> Enum.at(x, 12) - 100 end,
            :unit => nil
          },
          %{
            :name => "Salinity at level 1",
            :convert => fn x -> Enum.at(x, 13) - 100 end,
            :unit => nil
          },
          %{
            :name => "Salinity at level 2",
            :convert => fn x -> Enum.at(x, 14) - 100 end,
            :unit => nil
          },
          %{
            :name => "Salinity at level 3",
            :convert => fn x -> Enum.at(x, 15) - 100 end,
            :unit => nil
          },
          %{
            :name => "Salinity at level 4",
            :convert => fn x -> Enum.at(x, 16) - 100 end,
            :unit => nil
          },
          %{
            :name => "Salinity at level 5",
            :convert => fn x -> Enum.at(x, 17) - 100 end,
            :unit => nil
          }
        ]
      },
      %{
        length: 18,
        values: [
          %{
            :name => "Moisture at level 6",
            :convert => fn x -> (Enum.at(x, 0) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Moisture at level 7",
            :convert => fn x -> (Enum.at(x, 1) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Moisture at level 8",
            :convert => fn x -> (Enum.at(x, 2) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Moisture at level 9",
            :convert => fn x -> (Enum.at(x, 3) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Moisture at level 10",
            :convert => fn x -> (Enum.at(x, 4) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Moisture at level 11",
            :convert => fn x -> (Enum.at(x, 5) - 32768) / 100 end,
            :unit => "%"
          },
          %{
            :name => "Temperature at level 6",
            :convert => fn x -> (Enum.at(x, 6) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Temperature at level 7",
            :convert => fn x -> (Enum.at(x, 7) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Temperature at level 8",
            :convert => fn x -> (Enum.at(x, 8) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Temperature at level 9",
            :convert => fn x -> (Enum.at(x, 9) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Temperature at level 10",
            :convert => fn x -> (Enum.at(x, 10) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Temperature at level 11",
            :convert => fn x -> (Enum.at(x, 11) - 32768) / 100 end,
            :unit => "°C"
          },
          %{
            :name => "Salinity at level 6",
            :convert => fn x -> Enum.at(x, 12) - 100 end,
            :unit => nil
          },
          %{
            :name => "Salinity at level 7",
            :convert => fn x -> Enum.at(x, 13) - 100 end,
            :unit => nil
          },
          %{
            :name => "Salinity at level 8",
            :convert => fn x -> Enum.at(x, 14) - 100 end,
            :unit => nil
          },
          %{
            :name => "Salinity at level 9",
            :convert => fn x -> Enum.at(x, 15) - 100 end,
            :unit => nil
          },
          %{
            :name => "Salinity at level 10",
            :convert => fn x -> Enum.at(x, 16) - 100 end,
            :unit => nil
          },
          %{
            :name => "Salinity at level 11",
            :convert => fn x -> Enum.at(x, 17) - 100 end,
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

IO.inspect(DecentlabDecoder.decode("0243e300058000800080008000800080008741877b8749876c876c876600000000000000000000014a09e3", :hex))
IO.inspect(DecentlabDecoder.decode("0243e3000409e3", :hex))
