
# https://www.decentlab.com/products/optical-turbidity-and-temperature-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 022332000300008885013e013e02330b10
  # 02233200020b10
  
  def fields do
    [
      %{field: "status", display: "Status", unit: ""},
      %{field: "temperature", display: "Temperature", unit: "°C"},
      %{field: "turbidity_in_ntu", display: "Turbidity in NTU", unit: "NTU"},
      %{field: "turbidity_in_fnu", display: "Turbidity in FNU", unit: "FNU"},
      %{field: "turbidity_in_mg/l", display: "Turbidity in mg/L", unit: "mg⋅L⁻¹"},
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
                 :turbidity_in_ntu => x2 / 10,
                 :turbidity_in_fnu => x3 / 10,
                 :turbidity_in_mg/l => x4 / 10
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