
# https://www.decentlab.com/products/analog-or-digital-sensor-device-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 0243710007000100000af9
  # 02437100040af9
  
  def fields do
    [
      %{field: "ch0_input", display: "CH0: Input", unit: ""},
      %{field: "ch1_input", display: "CH1: Input", unit: ""},
      %{field: "battery_voltage", display: "Battery voltage", unit: "V"}
    ]
  end

  def parse(<<2, device_id::size(16), flags::binary-size(2), words::binary>>, _meta) do
    {_remaining, result} =
      {words, %{:device_id => device_id, :protocol_version => 2}}
      |> sensor0(flags)
      |> sensor1(flags)
      |> sensor2(flags)

    result
  end

  
  defp sensor0({<<x0::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :ch0_input => x0
               })}
  end
  defp sensor0(result, _flags), do: result
  
  defp sensor1({<<x0::size(16), remaining::binary>>, result},
               <<_::size(14), 1::size(1), _::size(1)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :ch1_input => x0
               })}
  end
  defp sensor1(result, _flags), do: result
  
  defp sensor2({<<x0::size(16), remaining::binary>>, result},
               <<_::size(13), 1::size(1), _::size(2)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :battery_voltage => x0 / 1000
               })}
  end
  defp sensor2(result, _flags), do: result
  
end