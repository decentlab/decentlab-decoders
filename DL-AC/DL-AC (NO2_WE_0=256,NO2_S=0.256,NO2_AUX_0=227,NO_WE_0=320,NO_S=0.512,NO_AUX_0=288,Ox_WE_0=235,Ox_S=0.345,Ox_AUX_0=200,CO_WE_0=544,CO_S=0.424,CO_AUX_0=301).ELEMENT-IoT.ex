
# https://www.decentlab.com/products/air-quality-station-no2-no-co-ox-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour

  # device-specific parameters
  defp no2_we_0(), do: 256
  defp no2_s(), do: 0.256
  defp no2_aux_0(), do: 227
  defp no_we_0(), do: 320
  defp no_s(), do: 0.512
  defp no_aux_0(), do: 288
  defp ox_we_0(), do: 235
  defp ox_s(), do: 0.345
  defp ox_aux_0(), do: 200
  defp co_we_0(), do: 544
  defp co_s(), do: 0.424
  defp co_aux_0(), do: 301
  
  ## test payloads
  # 020fa0003f66b49b8c8966803c8cf580238a68804c903783f4158a
  # 020fa00020158a
  
  def fields do
    [
      %{field: "air_temperature", display: "Air temperature", unit: "°C"},
      %{field: "air_humidity", display: "Air humidity", unit: "%"},
      %{field: "ch4_no2_we", display: "CH4: NO2 (we)", unit: "mV"},
      %{field: "ch4_no2_we_aux", display: "CH4: NO2 (we-aux)", unit: "mV"},
      %{field: "ch4_no2_concentration_we", display: "CH4: NO2 concentration (we)", unit: "ppb"},
      %{field: "ch4_no2_concentration_we_aux", display: "CH4: NO2 concentration (we-aux)", unit: "ppb"},
      %{field: "ch5_no_we", display: "CH5: NO (we)", unit: "mV"},
      %{field: "ch5_no_we_aux", display: "CH5: NO (we-aux)", unit: "mV"},
      %{field: "ch5_no_concentration_we", display: "CH5: NO concentration (we)", unit: "ppb"},
      %{field: "ch5_no_concentration_we_aux", display: "CH5: NO concentration (we-aux)", unit: "ppb"},
      %{field: "ch6_ox_we", display: "CH6: Ox (we)", unit: "mV"},
      %{field: "ch6_ox_we_aux", display: "CH6: Ox (we-aux)", unit: "mV"},
      %{field: "ch6_ox_concentration_we", display: "CH6: Ox concentration (we)", unit: "ppb"},
      %{field: "ch6_ox_concentration_we_aux", display: "CH6: Ox concentration (we-aux)", unit: "ppb"},
      %{field: "ch7_co_we", display: "CH7: CO (we)", unit: "mV"},
      %{field: "ch7_co_we_aux", display: "CH7: CO (we-aux)", unit: "mV"},
      %{field: "ch7_co_concentration_we", display: "CH7: CO concentration (we)", unit: "ppb"},
      %{field: "ch7_co_concentration_we_aux", display: "CH7: CO concentration (we-aux)", unit: "ppb"},
      %{field: "battery_voltage", display: "Battery voltage", unit: "V"}
    ]
  end

  def parse(<<2, device_id::size(16), flags::binary-size(2), words::binary>>, _meta) do
    {_remaining, result} =
      {words, %{:device_id => device_id, :protocol_version => 2}}
      |> sensor0(flags)
      |> sensor1(flags)
      |> sensor2(flags)
      |> sensor3(flags)
      |> sensor4(flags)
      |> sensor5(flags)

    result
  end

  
  defp sensor0({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :air_temperature => 175.72 * x0 / 65536 - 46.85,
                 :air_humidity => 125 * x1 / 65536 - 6
               })}
  end
  defp sensor0(result, _flags), do: result
  
  defp sensor1({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(14), 1::size(1), _::size(1)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :ch4_no2_we => 3 * (x0 / 32768 - 1) * 1000,
                 :ch4_no2_we_aux => 3 * (x1 / 32768 - 1) * 1000,
                 :ch4_no2_concentration_we => (3 * (x0 / 32768 - 1) * 1000 - no2_we_0()) / no2_s(),
                 :ch4_no2_concentration_we_aux => (3 * (x1 / 32768 - 1) * 1000 - no2_we_0() + no2_aux_0()) / no2_s()
               })}
  end
  defp sensor1(result, _flags), do: result
  
  defp sensor2({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(13), 1::size(1), _::size(2)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :ch5_no_we => 3 * (x0 / 32768 - 1) * 1000,
                 :ch5_no_we_aux => 3 * (x1 / 32768 - 1) * 1000,
                 :ch5_no_concentration_we => (3 * (x0 / 32768 - 1) * 1000 - no_we_0()) / no_s(),
                 :ch5_no_concentration_we_aux => (3 * (x1 / 32768 - 1) * 1000 - no_we_0() + no_aux_0()) / no_s()
               })}
  end
  defp sensor2(result, _flags), do: result
  
  defp sensor3({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(12), 1::size(1), _::size(3)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :ch6_ox_we => 3 * (x0 / 32768 - 1) * 1000,
                 :ch6_ox_we_aux => 3 * (x1 / 32768 - 1) * 1000,
                 :ch6_ox_concentration_we => (3 * (x0 / 32768 - 1) * 1000 - ox_we_0()) / ox_s(),
                 :ch6_ox_concentration_we_aux => (3 * (x1 / 32768 - 1) * 1000 - ox_we_0() + ox_aux_0()) / ox_s()
               })}
  end
  defp sensor3(result, _flags), do: result
  
  defp sensor4({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(11), 1::size(1), _::size(4)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :ch7_co_we => 3 * (x0 / 32768 - 1) * 1000,
                 :ch7_co_we_aux => 3 * (x1 / 32768 - 1) * 1000,
                 :ch7_co_concentration_we => (3 * (x0 / 32768 - 1) * 1000 - co_we_0()) / co_s(),
                 :ch7_co_concentration_we_aux => (3 * (x1 / 32768 - 1) * 1000 - co_we_0() + co_aux_0()) / co_s()
               })}
  end
  defp sensor4(result, _flags), do: result
  
  defp sensor5({<<x0::size(16), remaining::binary>>, result},
               <<_::size(10), 1::size(1), _::size(5)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :battery_voltage => x0 / 1000
               })}
  end
  defp sensor5(result, _flags), do: result
  
end