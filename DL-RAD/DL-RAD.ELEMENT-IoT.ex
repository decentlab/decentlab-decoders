
# https://www.decentlab.com/products/radar-distance-level-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 02586b0003074c88b68a8c00000b93
  # 02586b00020b93
  
  def fields do
    [
      %{field: "distance", display: "Distance", unit: "mm"},
      %{field: "temperature", display: "Temperature", unit: "°C"},
      %{field: "reliability", display: "Reliability", unit: "dB"},
      %{field: "status", display: "Status", unit: ""},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :distance => x0,
                 :temperature => (x1 - 32768) / 100,
                 :reliability => (x2 - 32768) / 100,
                 :status => x3
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