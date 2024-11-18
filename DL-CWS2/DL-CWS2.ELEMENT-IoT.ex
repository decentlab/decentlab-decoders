
# https://www.decentlab.com/products/high-precision-winter-road-maintenance-sensor-with-radiation-shield-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 0258c000074676fa0a81c9813fa6d88137802581300b91
  # 0258c000040b91
  
  def fields do
    [
      %{field: "air_temperature_radiation_shield", display: "Air temperature (radiation shield)", unit: "°C"},
      %{field: "air_humidity_radiation_shield", display: "Air humidity (radiation shield)", unit: "%"},
      %{field: "surface_temperature", display: "Surface temperature", unit: "°C"},
      %{field: "air_temperature", display: "Air temperature", unit: "°C"},
      %{field: "air_humidity", display: "Air humidity", unit: "%"},
      %{field: "dew_point", display: "Dew point", unit: "°C"},
      %{field: "angle", display: "Angle", unit: "°"},
      %{field: "sensor_temperature", display: "Sensor temperature", unit: "°C"},
      %{field: "battery_voltage", display: "Battery voltage", unit: "V"}
    ]
  end

  def parse(<<2, device_id::size(16), flags::binary-size(2), words::binary>>, _meta) do
    {_remaining, result} =
      {words, %{:device_id => device_id, :protocol_version => 2}}
      |> sensor0(flags)
      |> sensor1(flags)
      |> sensor2(flags)

    result
  end

  def parse(_payload, _meta), do: nil

  defp where(true, if_true, _if_false), do: if_true
  defp where(false, _if_true, if_false), do: if_false

  
  defp sensor0({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :air_temperature_radiation_shield => 175 * x0 / 65535 - 45,
                 :air_humidity_radiation_shield => 100 * x1 / 65535
               })}
  end
  defp sensor0(result, _flags), do: result
  
  defp sensor1({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), remaining::binary>>, result},
               <<_::size(14), 1::size(1), _::size(1)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :surface_temperature => (x0 - 32768) / 100,
                 :air_temperature => (x1 - 32768) / 100,
                 :air_humidity => (x2 - 32768) / 100,
                 :dew_point => (x3 - 32768) / 100,
                 :angle => (x4 - 32768),
                 :sensor_temperature => (x5 - 32768) / 100
               })}
  end
  defp sensor1(result, _flags), do: result
  
  defp sensor2({<<x0::size(16), remaining::binary>>, result},
               <<_::size(13), 1::size(1), _::size(2)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :battery_voltage => x0 / 1000
               })}
  end
  defp sensor2(result, _flags), do: result
  
end