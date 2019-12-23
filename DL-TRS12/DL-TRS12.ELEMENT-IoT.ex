
# https://www.decentlab.com/products/soil-moisture-temperature-and-electrical-conductivity-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 0210d3000346be813d00000c80
  # 0210d300020c80
  
  def fields do
    [
      %{field: "dielectric_permittivity", display: "Dielectric permittivity", unit: ""},
      %{field: "volumetric_water_content", display: "Volumetric water content", unit: "m³⋅m⁻³"},
      %{field: "soil_temperature", display: "Soil temperature", unit: "°C"},
      %{field: "electrical_conductivity", display: "Electrical conductivity", unit: "µS⋅cm⁻¹"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :dielectric_permittivity => :math.pow(0.000000002887 * :math.pow(x0/10, 3) - 0.0000208 * :math.pow(x0/10, 2) + 0.05276 * (x0/10) - 43.39, 2),
                 :volumetric_water_content => x0/10 * 0.0003879 - 0.6956,
                 :soil_temperature => (x1 - 32768) / 10,
                 :electrical_conductivity => x2
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