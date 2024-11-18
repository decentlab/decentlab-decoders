
# https://www.decentlab.com/products/ph-orp-and-temperature-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 0252b800030000884282c77f637ff60c5c
  # 0252b800020c5c
  
  def fields do
    [
      %{field: "status", display: "Status", unit: ""},
      %{field: "temperature", display: "Temperature", unit: "°C"},
      %{field: "ph", display: "pH", unit: ""},
      %{field: "redox", display: "Redox", unit: "mV"},
      %{field: "ph_mv", display: "pH-mV", unit: "mV"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :status => x0,
                 :temperature => (x1 - 32768) / 100,
                 :ph => (x2 - 32768) / 100,
                 :redox => (x3 - 32768) / 10,
                 :ph_mv => (x4 - 32768) / 10
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