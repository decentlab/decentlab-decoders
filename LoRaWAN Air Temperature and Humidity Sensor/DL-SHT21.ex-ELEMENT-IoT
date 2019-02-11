
# https://www.decentlab.com/support

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## Test payloads
  # 02030e000364a079b10c60
  # 02030e00020c60
  
  def fields do
    [
      %{field: "Air temperature", display: "Air temperature", unit: "°C"},
      %{field: "Air humidity", display: "Air humidity", unit: "%"},
      %{field: "Battery voltage", display: "Battery voltage", unit: "V"}
    ]
  end

  def parse(<<2, device_id::size(16), flags::binary-size(2), words::binary>>, _meta) do
    {_remaining, result} =
      {words, %{"Device ID" => device_id, "Protocol version" => 2}}
      |> sensor0(flags)
      |> sensor1(flags)

    result
  end
  
  defp sensor0({<<x0::size(16), x1::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 "Air temperature" => 175.72 * x0 / 65536 - 46.85,
                 "Air humidity" => 125 * x1 / 65536 - 6
               })}
  end
  defp sensor0(result, _flags), do: result
  
  defp sensor1({<<x0::size(16), remaining::binary>>, result},
               <<_::size(14), 1::size(1), _::size(1)>>) do
    {remaining,
     Map.merge(result,
               %{
                 "Battery voltage" => x0 / 1000
               })}
  end
  defp sensor1(result, _flags), do: result
  
end