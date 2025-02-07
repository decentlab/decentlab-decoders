
# https://www.decentlab.com/products/eleven-parameter-weather-station-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 025ef80003805c000080008000803484b3803680e78086a60181d680ed81c9809f8000117000010adc
  # 025ef800020adc
  
  def fields do
    [
      %{field: "solar_radiation", display: "Solar radiation", unit: "W⋅m⁻²"},
      %{field: "precipitation", display: "Precipitation", unit: "mm"},
      %{field: "lightning_strike_count", display: "Lightning strike count", unit: "None"},
      %{field: "lightning_average_distance", display: "Lightning average distance", unit: "km"},
      %{field: "wind_speed", display: "Wind speed", unit: "m⋅s⁻¹"},
      %{field: "wind_direction", display: "Wind direction", unit: "°"},
      %{field: "maximum_wind_speed", display: "Maximum wind speed", unit: "m⋅s⁻¹"},
      %{field: "air_temperature", display: "Air temperature", unit: "°C"},
      %{field: "vapor_pressure", display: "Vapor pressure", unit: "kPa"},
      %{field: "barometric_pressure", display: "Barometric pressure", unit: "kPa"},
      %{field: "relative_humidity", display: "Relative humidity", unit: "%"},
      %{field: "internal_temperature", display: "Internal temperature", unit: "°C"},
      %{field: "tilt_angle_x_orientation", display: "Tilt angle, X orientation", unit: "°"},
      %{field: "tilt_angle_y_orientation", display: "Tilt angle, Y orientation", unit: "°"},
      %{field: "precipitation_electrical_conductivity", display: "Precipitation electrical conductivity", unit: "µS⋅cm⁻¹"},
      %{field: "cumulative_precipitation", display: "Cumulative precipitation", unit: "mm"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), x6::size(16), x7::size(16), x8::size(16), x9::size(16), x10::size(16), x11::size(16), x12::size(16), x13::size(16), x14::size(16), x15::size(16), x16::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :solar_radiation => (x0 - 32768) / 10,
                 :precipitation => x1 / 1000,
                 :lightning_strike_count => x2 - 32768,
                 :lightning_average_distance => x3 - 32768,
                 :wind_speed => (x4 - 32768) / 100,
                 :wind_direction => (x5 - 32768) / 10,
                 :maximum_wind_speed => (x6 - 32768) / 100,
                 :air_temperature => (x7 - 32768) / 10,
                 :vapor_pressure => (x8 - 32768) / 100,
                 :barometric_pressure => (x9 - 32768) / 100,
                 :relative_humidity => (x10 - 32768) / 10,
                 :internal_temperature => (x11 - 32768) / 10,
                 :tilt_angle_x_orientation => (x12 - 32768) / 10,
                 :tilt_angle_y_orientation => (x13 - 32768) / 10,
                 :precipitation_electrical_conductivity => x14 - 32768,
                 :cumulative_precipitation => (x15 + x16 * 65536) / 1000
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