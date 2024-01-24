
# https://www.decentlab.com/products/high-precision-pressure-/-liquid-level-temperature-and-electrical-conductivity-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour

  # device-specific parameters
  defp kp(), do: 8192
  defp kec(), do: 1024
  
  ## test payloads
  # 020a17000380079786978180060c2b
  # 020a1700020c2b
  
  def fields do
    [
      %{field: "pressure", display: "Pressure", unit: "bar"},
      %{field: "temperature_electronics", display: "Temperature (electronics)", unit: "°C"},
      %{field: "temperature_pt1000", display: "Temperature (PT1000)", unit: "°C"},
      %{field: "electrical_conductivity", display: "Electrical conductivity", unit: "mS⋅cm⁻¹"},
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
                 :pressure => (x0 - 32768) / kp(),
                 :temperature_electronics => (x1 - 32768) / 256,
                 :temperature_pt1000 => (x2 - 32768) / 256,
                 :electrical_conductivity => (x3 - 32768) / kec()
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