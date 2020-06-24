
# https://www.decentlab.com/products/analog-or-digital-sensor-device-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour

  # device-specific parameters
  defp r(), do: 2000
  
  ## test payloads
  # 0207df000317de008d0c60
  # 0207df00020c60
  
  def fields do
    [
      %{field: "temperature", display: "Temperature", unit: ""},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :temperature => -244.83 + 2.3419 * ((((x0 + x1*65536) / 8388608 - 1) / 2) * r() / (1 - (((x0 + x1*65536) / 8388608 - 1) / 2))) + 0.0010664 * :math.pow((((x0 + x1*65536) / 8388608 - 1) / 2) * r() / (1 - (((x0 + x1*65536) / 8388608 - 1) / 2)), 2)
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