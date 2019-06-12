
# https://www.decentlab.com/products/analog-or-digital-sensor-device-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour

  # Device specific parameters
  defp r(), do: 10.0
  
  ## Test payloads
  # 0208b200038bb80c60
  # 0208b200020c60
  
  def fields do
    [
      %{field: "current", display: "Current", unit: "mA"},
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

  
  defp sensor0({<<x0::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :current => 3 * (x0 - 32768) / 32768 / 2 / r() * 1000
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