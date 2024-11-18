
# https://www.decentlab.com/products/black-globe-temperature-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 0230c50003a40c00810c60
  # 0230c500020c60
  
  def fields do
    [
      %{field: "voltage_ratio", display: "Voltage ratio", unit: ""},
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

  def parse(_payload, _meta), do: nil

  defp where(true, if_true, _if_false), do: if_true
  defp where(false, _if_true, if_false), do: if_false

  
  defp sensor0({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :voltage_ratio => ((x0 + x1*65536) / 8388608 - 1) / 2,
                 :thermistor_resistance => 1000 / (((x0 + x1*65536) / 8388608 - 1) / 2) - 41000,
                 :temperature => (1 / (0.0008271111 + 0.000208802 * :math.log(1000 / (((x0 + x1*65536) / 8388608 - 1) / 2) - 41000) + 0.000000080592 * :math.pow(:math.log(1000 / (((x0 + x1*65536) / 8388608 - 1) / 2) - 41000), 3) )) - 273.15
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