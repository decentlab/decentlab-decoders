
# https://www.decentlab.com/products/high-precision-winter-road-maintenance-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 02463900038a778a95977a874c80478a5e0b74
  # 02463900020b74
  
  def fields do
    [
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

    result
  end

  def parse(_payload, _meta), do: nil

  defp where(true, if_true, _if_false), do: if_true
  defp where(false, _if_true, if_false), do: if_false

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
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
  defp sensor0(result, _flags), do: result
  
  defp sensor1({<<x0::size(16), remaining::binary>>, result},
               <<_::size(14), 1::size(1), _::size(1)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :battery_voltage => x0 / 1000
               })}
  end
  defp sensor1(result, _flags), do: result
  
end