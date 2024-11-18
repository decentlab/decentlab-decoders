
# https://www.decentlab.com/products/indoor-ambiance-monitor-including-co2-tvoc-and-motion-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 020bbd007f0b926a515d48bc4e0262006981c7000093d4000b0111
  # 020bbd006f0b926a515d48bc4e02620069000b0111
  # 020bbd00010b92
  
  def fields do
    [
      %{field: "battery_voltage", display: "Battery voltage", unit: "V"},
      %{field: "air_temperature", display: "Air temperature", unit: "°C"},
      %{field: "air_humidity", display: "Air humidity", unit: "%"},
      %{field: "barometric_pressure", display: "Barometric pressure", unit: "Pa"},
      %{field: "ambient_light_visible_infrared", display: "Ambient light (visible + infrared)", unit: ""},
      %{field: "ambient_light_infrared", display: "Ambient light (infrared)", unit: ""},
      %{field: "illuminance", display: "Illuminance", unit: "lx"},
      %{field: "co2_concentration", display: "CO2 concentration", unit: "ppm"},
      %{field: "co2_sensor_status", display: "CO2 sensor status", unit: ""},
      %{field: "raw_ir_reading", display: "Raw IR reading", unit: ""},
      %{field: "activity_counter", display: "Activity counter", unit: ""},
      %{field: "total_voc", display: "Total VOC", unit: "ppb"}
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
      |> sensor6(flags)

    result
  end

  def parse(_payload, _meta), do: nil

  defp where(true, if_true, _if_false), do: if_true
  defp where(false, _if_true, if_false), do: if_false

  
  defp sensor0({<<x0::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :battery_voltage => x0 / 1000
               })}
  end
  defp sensor0(result, _flags), do: result
  
  defp sensor1({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(14), 1::size(1), _::size(1)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :air_temperature => 175 * x0 / 65535 - 45,
                 :air_humidity => 100 * x1 / 65535
               })}
  end
  defp sensor1(result, _flags), do: result
  
  defp sensor2({<<x0::size(16), remaining::binary>>, result},
               <<_::size(13), 1::size(1), _::size(2)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :barometric_pressure => x0 * 2
               })}
  end
  defp sensor2(result, _flags), do: result
  
  defp sensor3({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(12), 1::size(1), _::size(3)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :ambient_light_visible_infrared => x0,
                 :ambient_light_infrared => x1,
                 :illuminance => max(max(1.0 * x0 - 1.64 * x1, 0.59 * x0 - 0.86 * x1), 0) * 1.5504
               })}
  end
  defp sensor3(result, _flags), do: result
  
  defp sensor4({<<x0::size(16), x1::size(16), x2::size(16), remaining::binary>>, result},
               <<_::size(11), 1::size(1), _::size(4)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :co2_concentration => x0 - 32768,
                 :co2_sensor_status => x1,
                 :raw_ir_reading => x2
               })}
  end
  defp sensor4(result, _flags), do: result
  
  defp sensor5({<<x0::size(16), remaining::binary>>, result},
               <<_::size(10), 1::size(1), _::size(5)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :activity_counter => x0
               })}
  end
  defp sensor5(result, _flags), do: result
  
  defp sensor6({<<x0::size(16), remaining::binary>>, result},
               <<_::size(9), 1::size(1), _::size(6)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :total_voc => x0
               })}
  end
  defp sensor6(result, _flags), do: result
  
end