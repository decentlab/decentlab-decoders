
# https://www.decentlab.com/support

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## Test payloads
  # 020a17000380079786978180060c2b
  # 020a1700020c2b
  
  def fields do
    [
      %{field: "Pressure", display: "Pressure", unit: "bar"},
      %{field: "Temperature (electronics)", display: "Temperature (electronics)", unit: "°C"},
      %{field: "Temperature (PT1000)", display: "Temperature (PT1000)", unit: "°C"},
      %{field: "Electrical conductivity", display: "Electrical conductivity", unit: "mS⋅cm⁻¹"},
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
  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 "Pressure" => (x0 - 32768) / 8192,
                 "Temperature (electronics)" => (x1 - 32768) / 256,
                 "Temperature (PT1000)" => (x2 - 32768) / 256,
                 "Electrical conductivity" => (x3 - 32768) / 1024
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