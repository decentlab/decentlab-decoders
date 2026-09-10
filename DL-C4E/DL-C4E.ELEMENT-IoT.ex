
# https://www.decentlab.com/products/conductivity-salinity-and-temperature-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 026934000300008A444F78806A0776064B0C0D
  # 02693400020C0D
  
  def fields do
    [
      %{field: "status", display: "Status", unit: "None"},
      %{field: "temperature", display: "Temperature", unit: "°C"},
      %{field: "conductivity", display: "Conductivity", unit: "mS⋅cm⁻¹"},
      %{field: "salinity", display: "Salinity", unit: "g⋅kg⁻¹"},
      %{field: "tds", display: "TDS", unit: "ppm"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :status => x0,
                 :temperature => (x1 - 32768) / 100,
                 :conductivity => (x2 + x3 * 65536 - 2147483648) / 1000000,
                 :salinity => x4 / 500,
                 :tds => x5 / 0.4
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