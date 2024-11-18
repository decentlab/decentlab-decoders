
# https://www.decentlab.com/products/greenhouse-multi-monitor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 02532b00038726892081148297a57380cf81700bbc
  # 02528500020bbc
  
  def fields do
    [
      %{field: "photosynthetically_active_radiation", display: "Photosynthetically active radiation", unit: "µmol⋅m⁻²⋅s⁻¹"},
      %{field: "air_temperature", display: "Air temperature", unit: "°C"},
      %{field: "air_humidity", display: "Air humidity", unit: "%"},
      %{field: "co2_concentration", display: "CO2 concentration", unit: "ppm"},
      %{field: "atmospheric_pressure", display: "Atmospheric pressure", unit: "kPa"},
      %{field: "vapor_pressure_deficit", display: "Vapor pressure deficit", unit: "kPa"},
      %{field: "dew_point", display: "Dew point", unit: "°C"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), x6::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :photosynthetically_active_radiation => (x0 - 32768) / 10,
                 :air_temperature => (x1 - 32768) / 100,
                 :air_humidity => (x2 - 32768) / 10,
                 :co2_concentration => (x3 - 32768) / 1,
                 :atmospheric_pressure => (x4 - 32768) / 100,
                 :vapor_pressure_deficit => (x5 - 32768) / 100,
                 :dew_point => (x6 - 32768) / 100
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