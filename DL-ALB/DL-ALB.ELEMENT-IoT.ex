
# https://www.decentlab.com/albedometer-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 0252850003809980220bd3
  # 02528500020bd3
  
  def fields do
    [
      %{field: "incoming_radiation", display: "Incoming radiation", unit: "W⋅m⁻²"},
      %{field: "reflected_radiation", display: "Reflected radiation", unit: "W⋅m⁻²"},
      %{field: "albedo", display: "albedo", unit: ""},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :incoming_radiation => (x0 - 32768) / 10,
                 :reflected_radiation => (x1 - 32768) / 10,
                 :albedo => where((((x0 - 32768) > 0) and ((x1 - 32768) > 0)), (x1 - 32768) / (x0 - 32768), 0)
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