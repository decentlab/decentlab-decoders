
# https://www.decentlab.com/products/dendrometer-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 0211110003409a00863039003e0c54
  
  def fields do
    [
      %{field: "dendrometer_a_position", display: "Dendrometer A Position", unit: "µm"},
      %{field: "dendrometer_b_position", display: "Dendrometer B Position", unit: "µm"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :dendrometer_a_position => ((x0 + x1*65536) / 8388608 - 1) * 20000,
                 :dendrometer_b_position => ((x0 + x1*65536) / 8388608 - (x2 + x3*65536) / 8388608) * 20000
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