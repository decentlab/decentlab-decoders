
# https://www.decentlab.com/support

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## Test payloads
  # 0208c900038009812b8014810880027fe8800880040bf5
  # 0208c900020bf5
  
  def fields do
    [
      %{field: "Wind speed", display: "Wind speed", unit: "m⋅s⁻¹"},
      %{field: "Wind direction", display: "Wind direction", unit: "°"},
      %{field: "Maximum wind speed", display: "Maximum wind speed", unit: "m⋅s⁻¹"},
      %{field: "Air temperature", display: "Air temperature", unit: "°C"},
      %{field: "X orientation angle", display: "X orientation angle", unit: "°"},
      %{field: "Y orientation angle", display: "Y orientation angle", unit: "°"},
      %{field: "North wind speed", display: "North wind speed", unit: "m⋅s⁻¹"},
      %{field: "East wind speed", display: "East wind speed", unit: "m⋅s⁻¹"},
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
  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), x6::size(16), x7::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 "Wind speed" => (x0 - 32768) / 100,
                 "Wind direction" => (x1 - 32768) / 10,
                 "Maximum wind speed" => (x2 - 32768) / 100,
                 "Air temperature" => (x3 - 32768) / 10,
                 "X orientation angle" => (x4 - 32768) / 10,
                 "Y orientation angle" => (x5 - 32768) / 10,
                 "North wind speed" => (x6 - 32768) / 100,
                 "East wind speed" => (x7 - 32768) / 100
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