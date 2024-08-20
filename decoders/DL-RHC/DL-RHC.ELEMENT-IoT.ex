
# https://www.decentlab.com/products/high-precision-air-temperature-and-humidity-sensor-with-radiation-shield-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 0202e00003a9fd01341ca285f30c60
  
  def fields do
    [
      %{field: "sensor_id", display: "Sensor ID", unit: ""},
      %{field: "air_humidity", display: "Air humidity", unit: "%"},
      %{field: "air_temperature", display: "Air temperature", unit: "°C"},
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

  defp where(true, if_true, _if_false), do: if_true
  defp where(false, _if_true, if_false), do: if_false

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :sensor_id => x0 + x1 * 65536,
                 :air_humidity => x2 / 100,
                 :air_temperature => (x3 - 32768) / 100
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