
# https://www.decentlab.com/products/weighing-scale-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour

  # device-specific parameters
  defp f02(), do: 232263168
  defp s(), do: 0.000302459
  defp m0(), do: 1370
  
  ## test payloads
  # 0203d400033bf67fff3bf60c60
  # 0203d400020c60
  
  def fields do
    [
      %{field: "frequency", display: "Frequency", unit: "Hz"},
      %{field: "weight", display: "Weight", unit: "g"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :frequency => x0 / x1 * 32768,
                 :weight => (:math.pow(x0 / x1 * 32768, 2) - f02()) * s() + m0()
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