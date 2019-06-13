
# https://www.decentlab.com/products/air-quality-station-no2-no-co-ox-for-lorawan

defmodule DecentlabDecoder do
  @protocol_version 2

  # Device specific parameters
  @no2_we_0 256
  @no2_s 0.256
  @no2_aux_0 227
  @no_we_0 320
  @no_s 0.512
  @no_aux_0 288
  @ox_we_0 235
  @ox_s 0.345
  @ox_aux_0 200
  @co_we_0 544
  @co_s 0.424
  @co_aux_0 301

  defp sensor_defs do
    [
      %{
        length: 2,
        values: [
          %{
            :name => "Air temperature",
            :convert => fn x -> 175.72 * Enum.at(x, 0) / 65536 - 46.85 end,
            :unit => "°C"
          },
          %{
            :name => "Air humidity",
            :convert => fn x -> 125 * Enum.at(x, 1) / 65536 - 6 end,
            :unit => "%"
          }
        ]
      },
      %{
        length: 2,
        values: [
          %{
            :name => "CH4: NO2 (we)",
            :convert => fn x -> 3 * (Enum.at(x, 0) / 32768 - 1) * 1000 end,
            :unit => "mV"
          },
          %{
            :name => "CH4: NO2 (we-aux)",
            :convert => fn x -> 3 * (Enum.at(x, 1) / 32768 - 1) * 1000 end,
            :unit => "mV"
          },
          %{
            :name => "CH4: NO2 concentration (we)",
            :convert => fn x -> (3 * (Enum.at(x, 0) / 32768 - 1) * 1000 - @no2_we_0) / @no2_s end,
            :unit => "ppb"
          },
          %{
            :name => "CH4: NO2 concentration (we-aux)",
            :convert => fn x -> (3 * (Enum.at(x, 1) / 32768 - 1) * 1000 - @no2_we_0 + @no2_aux_0) / @no2_s end,
            :unit => "ppb"
          }
        ]
      },
      %{
        length: 2,
        values: [
          %{
            :name => "CH5: NO (we)",
            :convert => fn x -> 3 * (Enum.at(x, 0) / 32768 - 1) * 1000 end,
            :unit => "mV"
          },
          %{
            :name => "CH5: NO (we-aux)",
            :convert => fn x -> 3 * (Enum.at(x, 1) / 32768 - 1) * 1000 end,
            :unit => "mV"
          },
          %{
            :name => "CH5: NO concentration (we)",
            :convert => fn x -> (3 * (Enum.at(x, 0) / 32768 - 1) * 1000 - @no_we_0) / @no_s end,
            :unit => "ppb"
          },
          %{
            :name => "CH5: NO concentration (we-aux)",
            :convert => fn x -> (3 * (Enum.at(x, 1) / 32768 - 1) * 1000 - @no_we_0 + @no_aux_0) / @no_s end,
            :unit => "ppb"
          }
        ]
      },
      %{
        length: 2,
        values: [
          %{
            :name => "CH6: Ox (we)",
            :convert => fn x -> 3 * (Enum.at(x, 0) / 32768 - 1) * 1000 end,
            :unit => "mV"
          },
          %{
            :name => "CH6: Ox (we-aux)",
            :convert => fn x -> 3 * (Enum.at(x, 1) / 32768 - 1) * 1000 end,
            :unit => "mV"
          },
          %{
            :name => "CH6: Ox concentration (we)",
            :convert => fn x -> (3 * (Enum.at(x, 0) / 32768 - 1) * 1000 - @ox_we_0) / @ox_s end,
            :unit => "ppb"
          },
          %{
            :name => "CH6: Ox concentration (we-aux)",
            :convert => fn x -> (3 * (Enum.at(x, 1) / 32768 - 1) * 1000 - @ox_we_0 + @ox_aux_0) / @ox_s end,
            :unit => "ppb"
          }
        ]
      },
      %{
        length: 2,
        values: [
          %{
            :name => "CH7: CO (we)",
            :convert => fn x -> 3 * (Enum.at(x, 0) / 32768 - 1) * 1000 end,
            :unit => "mV"
          },
          %{
            :name => "CH7: CO (we-aux)",
            :convert => fn x -> 3 * (Enum.at(x, 1) / 32768 - 1) * 1000 end,
            :unit => "mV"
          },
          %{
            :name => "CH7: CO concentration (we)",
            :convert => fn x -> (3 * (Enum.at(x, 0) / 32768 - 1) * 1000 - @co_we_0) / @co_s end,
            :unit => "ppb"
          },
          %{
            :name => "CH7: CO concentration (we-aux)",
            :convert => fn x -> (3 * (Enum.at(x, 1) / 32768 - 1) * 1000 - @co_we_0 + @co_aux_0) / @co_s end,
            :unit => "ppb"
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

IO.inspect(DecentlabDecoder.decode("020fa0003f66b49b8c8966803c8cf580238a68804c903783f4158a", :hex))
IO.inspect(DecentlabDecoder.decode("020fa00020158a", :hex))
