
# https://www.decentlab.com/products/wind-speed-wind-direction-and-temperature-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour

  # Device specific parameters
  
  ## Test payloads
  # 0208c900038009812b8014810880027fe8800880040bf5
  # 0208c900020bf5
  
  def fields do
    [
      %{field: "wind_speed", display: "Wind speed", unit: "m⋅s⁻¹"},
      %{field: "wind_direction", display: "Wind direction", unit: "°"},
      %{field: "maximum_wind_speed", display: "Maximum wind speed", unit: "m⋅s⁻¹"},
      %{field: "air_temperature", display: "Air temperature", unit: "°C"},
      %{field: "x_orientation_angle", display: "X orientation angle", unit: "°"},
      %{field: "y_orientation_angle", display: "Y orientation angle", unit: "°"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), x6::size(16), x7::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :wind_speed => (x0 - 32768) / 100,
                 :wind_direction => (x1 - 32768) / 10,
                 :maximum_wind_speed => (x2 - 32768) / 100,
                 :air_temperature => (x3 - 32768) / 10,
                 :x_orientation_angle => (x4 - 32768) / 10,
                 :y_orientation_angle => (x5 - 32768) / 10,
                 :north_wind_speed => (x6 - 32768) / 100,
                 :east_wind_speed => (x7 - 32768) / 100
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