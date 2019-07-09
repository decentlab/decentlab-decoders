
# https://www.decentlab.com/support

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 0202df000393710c60
  # 0202df00020c60
  
  def fields do
    [
      %{field: "raw_sensor_reading", display: "Raw sensor reading", unit: "mV"},
      %{field: "volumetric_water_content", display: "Volumetric water content", unit: "m³⋅m⁻³"},
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

  
  defp sensor0({<<x0::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :raw_sensor_reading => 3 * (x0 - 32768) / 32768 * 1000,
                 :volumetric_water_content => 2.97*:math.pow(10, -9) * :math.pow(3000*(x0-32768)/32768, 3) - 7.37*:math.pow(10, -6) * :math.pow(3000*(x0-32768)/32768, 2) + 6.69*:math.pow(10, -3) * (3000*(x0-32768)/32768) - 1.92
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