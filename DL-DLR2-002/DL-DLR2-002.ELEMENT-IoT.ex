defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 02199e000300000258000000000c9b
  
  def fields do
    [
      %{field: "pulse_count", display: "Pulse count", unit: ""},
      %{field: "pulse_interval", display: "Pulse interval", unit: "s"},
      %{field: "accumulated_pulse_count", display: "Accumulated pulse count", unit: ""},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :pulse_count => x0,
                 :pulse_interval => x1,
                 :accumulated_pulse_count => (x2 + x3 * 65536)
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