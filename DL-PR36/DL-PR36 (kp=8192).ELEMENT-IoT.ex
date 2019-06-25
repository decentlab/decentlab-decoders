
# https://www.decentlab.com/support

defmodule Parser do
  use Platform.Parsing.Behaviour

  # device-specific parameters
  defp kp(), do: 8192
  
  ## test payloads
  # 02032b0003806797810c2b
  # 02032b00020c2b
  
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

  
  defp sensor0({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :pressure => (x0 - 32768) / kp(),
                 :temperature => (x1 - 32768) / 256
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