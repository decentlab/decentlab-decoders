
# https://www.decentlab.com/products/eleven-parameter-weather-station-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 02035a0003800a8000800080008009812b8014810880b4a57c820c810980027fe88056800880040bf5
  # 02035a00020bf5
  
  def fields do
    [
      %{field: "solar_radiation", display: "Solar radiation", unit: "W⋅m⁻²"},
      %{field: "precipitation", display: "Precipitation", unit: "mm"},
      %{field: "lightning_strike_count", display: "Lightning strike count", unit: ""},
      %{field: "lightning_average_distance", display: "Lightning average distance", unit: "km"},
      %{field: "wind_speed", display: "Wind speed", unit: "m⋅s⁻¹"},
      %{field: "wind_direction", display: "Wind direction", unit: "°"},
      %{field: "maximum_wind_speed", display: "Maximum wind speed", unit: "m⋅s⁻¹"},
      %{field: "air_temperature", display: "Air temperature", unit: "°C"},
      %{field: "vapor_pressure", display: "Vapor pressure", unit: "kPa"},
      %{field: "atmospheric_pressure", display: "Atmospheric pressure", unit: "kPa"},
      %{field: "relative_humidity", display: "Relative humidity", unit: "%"},
      %{field: "sensor_temperature_internal", display: "Sensor temperature (internal)", unit: "°C"},
      %{field: "x_orientation_angle", display: "X orientation angle", unit: "°"},
      %{field: "y_orientation_angle", display: "Y orientation angle", unit: "°"},
      %{field: "compass_heading", display: "Compass heading", unit: "°"},
      %{field: "north_wind_speed", display: "North wind speed", unit: "m⋅s⁻¹"},
      %{field: "east_wind_speed", display: "East wind speed", unit: "m⋅s⁻¹"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), x6::size(16), x7::size(16), x8::size(16), x9::size(16), x10::size(16), x11::size(16), x12::size(16), x13::size(16), x14::size(16), x15::size(16), x16::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :solar_radiation => x0 - 32768,
                 :precipitation => (x1 - 32768) / 1000,
                 :lightning_strike_count => x2 - 32768,
                 :lightning_average_distance => x3 - 32768,
                 :wind_speed => (x4 - 32768) / 100,
                 :wind_direction => (x5 - 32768) / 10,
                 :maximum_wind_speed => (x6 - 32768) / 100,
                 :air_temperature => (x7 - 32768) / 10,
                 :vapor_pressure => (x8 - 32768) / 100,
                 :atmospheric_pressure => (x9 - 32768) / 100,
                 :relative_humidity => (x10 - 32768) / 10,
                 :sensor_temperature_internal => (x11 - 32768) / 10,
                 :x_orientation_angle => (x12 - 32768) / 10,
                 :y_orientation_angle => (x13 - 32768) / 10,
                 :compass_heading => x14 - 32768,
                 :north_wind_speed => (x15 - 32768) / 100,
                 :east_wind_speed => (x16 - 32768) / 100
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