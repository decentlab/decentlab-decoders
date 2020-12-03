
# https://decentlab.squarespace.com/products/optical-dissolved-oxygen-and-temperature-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 02186c000300008862a618836583650c60
  # 02186c00020c60
  
  def fields do
    [
      %{field: "status", display: "Status", unit: ""},
      %{field: "temperature", display: "Temperature", unit: "°C"},
      %{field: "oxygen_saturation", display: "Oxygen saturation", unit: "%"},
      %{field: "oxygen_concentration", display: "Oxygen concentration", unit: "mg⋅L⁻¹"},
      %{field: "oxygen_concentration_alt", display: "Oxygen concentration (alt)", unit: "ppm"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :status => x0,
                 :temperature => (x1 - 32768) / 100,
                 :oxygen_saturation => (x2 - 32768) / 100,
                 :oxygen_concentration => (x3 - 32768) / 100,
                 :oxygen_concentration_alt => (x4 - 32768) / 100
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