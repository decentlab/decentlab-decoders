
# https://www.decentlab.com/products/analog-or-digital-sensor-device-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour

  # device-specific parameters
  defp r(), do: 2000
  
  ## test payloads
  # 024c620003573400ad0ae1
  # 024c6200020ae1
  
  def fields do
    [
      %{field: "thermistor_resistance", display: "Thermistor resistance", unit: "Ω"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :thermistor_resistance => ((x0 + x1*65536) / 8388608 - 1) * 2000 / (1 - ((x0 + x1*65536) / 8388608 - 1)),
                 :temperature => -245.18 + 0.23469 * (((x0 + x1*65536) / 8388608 - 1) * 2000 / (1 - ((x0 + x1*65536) / 8388608 - 1))) + 0.0000104876 * :math.pow(((x0 + x1*65536) / 8388608 - 1) * 2000 / (1 - ((x0 + x1*65536) / 8388608 - 1)), 2)
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