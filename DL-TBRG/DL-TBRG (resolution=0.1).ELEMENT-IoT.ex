
# https://www.decentlab.com/support

defmodule Parser do
  use Platform.Parsing.Behaviour

  # device-specific parameters
  defp resolution(), do: 0.1
  
  ## test payloads
  # 0202f8000300040258409a00000c54
  # 0202f800020c54
  
  def fields do
    [
      %{field: "precipitation_in_interval", display: "Precipitation in interval", unit: "mm"},
      %{field: "interval", display: "Interval", unit: "s"},
      %{field: "total_accumulated_precipitation", display: "Total accumulated precipitation", unit: "mm"},
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
                 :precipitation_in_interval => x0 * resolution(),
                 :interval => x1,
                 :total_accumulated_precipitation => (x2 + x3 * 65536) * resolution()
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