
# https://www.decentlab.com/products/analog-or-digital-sensor-device-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 02198f0007000402580bf0000100000258dece00000c33
  # 02198f00040c33
  
  def fields do
    [
      %{field: "ch0_pulse_count", display: "CH0: Pulse count", unit: ""},
      %{field: "ch0_pulse_interval", display: "CH0: Pulse interval", unit: "s"},
      %{field: "ch0_cumulative_pulse_count", display: "CH0: Cumulative pulse count", unit: ""},
      %{field: "ch1_pulse_count", display: "CH1: Pulse count", unit: ""},
      %{field: "ch1_pulse_interval", display: "CH1: Pulse interval", unit: "s"},
      %{field: "ch1_cumulative_pulse_count", display: "CH1: Cumulative pulse count", unit: ""},
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

  def parse(_payload, _meta), do: nil

  defp where(true, if_true, _if_false), do: if_true
  defp where(false, _if_true, if_false), do: if_false

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :ch0_pulse_count => x0,
                 :ch0_pulse_interval => x1,
                 :ch0_cumulative_pulse_count => (x2 + x3 * 65536)
               })}
  end
  defp sensor0(result, _flags), do: result
  
  defp sensor1({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), remaining::binary>>, result},
               <<_::size(14), 1::size(1), _::size(1)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :ch1_pulse_count => x0,
                 :ch1_pulse_interval => x1,
                 :ch1_cumulative_pulse_count => (x2 + x3 * 65536)
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