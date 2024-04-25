
# https://www.decentlab.com/large-range-optical-turbidity-and-temperature-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 024E030003000088e6210800f223650af5
  # 024E0300020af5
  
  def fields do
    [
      %{field: "status", display: "Status", unit: ""},
      %{field: "temperature", display: "Temperature", unit: "°C"},
      %{field: "sludge_blanket", display: "Sludge blanket", unit: "%"},
      %{field: "suspended_solid", display: "Suspended solid", unit: "g⋅L⁻¹"},
      %{field: "turbidity", display: "Turbidity", unit: "FAU"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :status => x0,
                 :temperature => (x1 - 32768) / 100,
                 :sludge_blanket => x2 / 100,
                 :suspended_solid => x3 / 100,
                 :turbidity => x4 / 10
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