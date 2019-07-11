
# https://www.decentlab.com/products/pressure-/-liquid-level-temperature-and-electrical-conductivity-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 0207d9000390888081006400000c60
  # 0207d900020c60
  
  def fields do
    [
      %{field: "water_depth", display: "Water depth", unit: "mm"},
      %{field: "temperature", display: "Temperature", unit: "°C"},
      %{field: "electrical_conductivity", display: "Electrical conductivity", unit: "µS⋅cm⁻¹"},
      %{field: "freezing_flag", display: "Freezing flag", unit: ""},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :water_depth => x0 - 32768,
                 :temperature => (x1 - 32768) / 10,
                 :electrical_conductivity => x2,
                 :freezing_flag => x3
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