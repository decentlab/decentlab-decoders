
# https://www.decentlab.com/support

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 02031c00030037027100000c60
  # 02031c00020c60
  
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

  defp where(true, if_true, _if_false), do: if_true
  defp where(false, _if_true, if_false), do: if_false

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :dielectric_permittivity => x0 / 50,
                 :volumetric_water_content => 0.0000043 * :math.pow(x0/50, 3) - 0.00055 * :math.pow(x0/50, 2) + 0.0292 * (x0/50) - 0.053,
                 :soil_temperature => (x1 - 400) / 10,
                 :electrical_conductivity => x2 * 10
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