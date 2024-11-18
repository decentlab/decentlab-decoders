defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 023b760003800c0c39
  # 023b7600020c39
  
  def fields do
    [
      %{field: "uv_millivolt", display: "UV millivolt", unit: "mV"},
      %{field: "uv_index", display: "UV index", unit: ""},
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

  
  defp sensor0({<<x0::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :uv_millivolt => 3 * (x0 / 32768 - 1) * 1000,
                 :uv_index => :math.floor(3 * (x0 / 32768 - 1) * 1000 / 150)
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