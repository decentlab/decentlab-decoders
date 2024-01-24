
# https://www.decentlab.com/products/pressure-/-liquid-level-and-temperature-sensor-with-g1/4-pipe-thread-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour

  # device-specific parameters
  defp pmin(), do: -1.0
  defp pmax(), do: 10.0
  
  ## test payloads
  # 02016700034e8060170c7f
  # 02016700020c7f
  
  def fields do
    [
      %{field: "pressure", display: "Pressure", unit: "bar"},
      %{field: "temperature", display: "Temperature", unit: "°C"},
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
                 :pressure => (x0 - 16384) / 32768 * (pmax() - pmin()) + pmin(),
                 :temperature => (x1 - 384) * 0.003125 - 50
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