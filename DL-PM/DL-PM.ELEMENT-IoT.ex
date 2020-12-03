
# https://decentlab.squarespace.com/products/particulate-matter-temperature-humidity-and-barometric-pressure-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 021b50000f0c25002500270027002701f50107012c012d012d012d67bd618dbd10
  # 021b50000d0c2567bd618dbd10
  # 021b5000010c25
  
  def fields do
    [
      %{field: "battery_voltage", display: "Battery voltage", unit: "V"},
      %{field: "pm1_0_mass_concentration", display: "PM1.0 mass concentration", unit: "µg⋅m⁻³"},
      %{field: "pm2_5_mass_concentration", display: "PM2.5 mass concentration", unit: "µg⋅m⁻³"},
      %{field: "pm4_mass_concentration", display: "PM4 mass concentration", unit: "µg⋅m⁻³"},
      %{field: "pm10_mass_concentration", display: "PM10 mass concentration", unit: "µg⋅m⁻³"},
      %{field: "typical_particle_size", display: "Typical particle size", unit: "nm"},
      %{field: "pm0_5_number_concentration", display: "PM0.5 number concentration", unit: ""},
      %{field: "pm1_0_number_concentration", display: "PM1.0 number concentration", unit: ""},
      %{field: "pm2_5_number_concentration", display: "PM2.5 number concentration", unit: ""},
      %{field: "pm4_number_concentration", display: "PM4 number concentration", unit: ""},
      %{field: "pm10_number_concentration", display: "PM10 number concentration", unit: ""},
      %{field: "air_temperature", display: "Air temperature", unit: "°C"},
      %{field: "air_humidity", display: "Air humidity", unit: "%"},
      %{field: "barometric_pressure", display: "Barometric pressure", unit: "Pa"}
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

  
  defp sensor0({<<x0::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :battery_voltage => x0 / 1000
               })}
  end
  defp sensor0(result, _flags), do: result
  
  defp sensor1({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), x6::size(16), x7::size(16), x8::size(16), x9::size(16), remaining::binary>>, result},
               <<_::size(14), 1::size(1), _::size(1)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :pm1_0_mass_concentration => x0 / 10,
                 :pm2_5_mass_concentration => x1 / 10,
                 :pm4_mass_concentration => x2 / 10,
                 :pm10_mass_concentration => x3 / 10,
                 :typical_particle_size => x4,
                 :pm0_5_number_concentration => x5 / 10,
                 :pm1_0_number_concentration => x6 / 10,
                 :pm2_5_number_concentration => x7 / 10,
                 :pm4_number_concentration => x8 / 10,
                 :pm10_number_concentration => x9 / 10
               })}
  end
  defp sensor1(result, _flags), do: result
  
  defp sensor2({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(13), 1::size(1), _::size(2)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :air_temperature => 175.72 * x0 / 65536 - 46.85,
                 :air_humidity => 125 * x1 / 65536 - 6
               })}
  end
  defp sensor2(result, _flags), do: result
  
  defp sensor3({<<x0::size(16), remaining::binary>>, result},
               <<_::size(12), 1::size(1), _::size(3)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :barometric_pressure => x0 * 2
               })}
  end
  defp sensor3(result, _flags), do: result
  
end