
# https://www.decentlab.com/support

defmodule Parser do
  use Platform.Parsing.Behaviour

  # Device specific parameters
  
  ## Test payloads
  # 020578000f67bd618d1cedbd1081d981f4895b0bd80bb50000959895390c25
  # 020578000b67bd618d1cedbd100c25
  # 02057800080c25
  
  def fields do
    [
      %{field: "air_temperature", display: "Air temperature", unit: "°C"},
      %{field: "air_humidity", display: "Air humidity", unit: "%"},
      %{field: "barometer_temperature", display: "Barometer temperature", unit: "°C"},
      %{field: "barometric_pressure", display: "Barometric pressure", unit: "Pa"},
      %{field: "co2_concentration", display: "CO2 concentration", unit: "ppm"},
      %{field: "co2_concentration_lpf", display: "CO2 concentration LPF", unit: "ppm"},
      %{field: "co2_sensor_temperature", display: "CO2 sensor temperature", unit: "°C"},
      %{field: "capacitor_voltage_1", display: "Capacitor voltage 1", unit: "V"},
      %{field: "capacitor_voltage_2", display: "Capacitor voltage 2", unit: "V"},
      %{field: "co2_sensor_status", display: "CO2 sensor status", unit: ""},
      %{field: "raw_ir_reading", display: "Raw IR reading", unit: ""},
      %{field: "raw_ir_reading_lpf", display: "Raw IR reading LPF", unit: ""},
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
                 :barometer_temperature => (x0 - 5000) / 100,
                 :barometric_pressure => x1 * 2
               })}
  end
  defp sensor1(result, _flags), do: result
  
  defp sensor2({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), x6::size(16), x7::size(16), remaining::binary>>, result},
               <<_::size(13), 1::size(1), _::size(2)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :co2_concentration => x0 - 32768,
                 :co2_concentration_lpf => x1 - 32768,
                 :co2_sensor_temperature => (x2 - 32768) / 100,
                 :capacitor_voltage_1 => x3 / 1000,
                 :capacitor_voltage_2 => x4 / 1000,
                 :co2_sensor_status => x5,
                 :raw_ir_reading => x6,
                 :raw_ir_reading_lpf => x7
               })}
  end
  defp sensor2(result, _flags), do: result
  
  defp sensor3({<<x0::size(16), remaining::binary>>, result},
               <<_::size(12), 1::size(1), _::size(3)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :battery_voltage => x0 / 1000
               })}
  end
  defp sensor3(result, _flags), do: result
  
end