defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 023e3e00038abc8a928aa08a848ab38a898ac38aad8ab78a928aa1000000000000000000000afc
  # 023e3e00020afc
  
  def fields do
    [
      %{field: "temperature_at_level_0", display: "Temperature at level 0", unit: "°C"},
      %{field: "temperature_at_level_1", display: "Temperature at level 1", unit: "°C"},
      %{field: "temperature_at_level_2", display: "Temperature at level 2", unit: "°C"},
      %{field: "temperature_at_level_3", display: "Temperature at level 3", unit: "°C"},
      %{field: "temperature_at_level_4", display: "Temperature at level 4", unit: "°C"},
      %{field: "temperature_at_level_5", display: "Temperature at level 5", unit: "°C"},
      %{field: "temperature_at_level_6", display: "Temperature at level 6", unit: "°C"},
      %{field: "temperature_at_level_7", display: "Temperature at level 7", unit: "°C"},
      %{field: "temperature_at_level_8", display: "Temperature at level 8", unit: "°C"},
      %{field: "temperature_at_level_9", display: "Temperature at level 9", unit: "°C"},
      %{field: "temperature_at_level_10", display: "Temperature at level 10", unit: "°C"},
      %{field: "temperature_at_level_11", display: "Temperature at level 11", unit: "°C"},
      %{field: "temperature_at_level_12", display: "Temperature at level 12", unit: "°C"},
      %{field: "temperature_at_level_13", display: "Temperature at level 13", unit: "°C"},
      %{field: "temperature_at_level_14", display: "Temperature at level 14", unit: "°C"},
      %{field: "temperature_at_level_15", display: "Temperature at level 15", unit: "°C"},
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

  defp where(true, if_true, _if_false), do: if_true
  defp where(false, _if_true, if_false), do: if_false

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), x6::size(16), x7::size(16), x8::size(16), x9::size(16), x10::size(16), x11::size(16), x12::size(16), x13::size(16), x14::size(16), x15::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :temperature_at_level_0 => (x0 - 32768) / 100,
                 :temperature_at_level_1 => (x1 - 32768) / 100,
                 :temperature_at_level_2 => (x2 - 32768) / 100,
                 :temperature_at_level_3 => (x3 - 32768) / 100,
                 :temperature_at_level_4 => (x4 - 32768) / 100,
                 :temperature_at_level_5 => (x5 - 32768) / 100,
                 :temperature_at_level_6 => (x6 - 32768) / 100,
                 :temperature_at_level_7 => (x7 - 32768) / 100,
                 :temperature_at_level_8 => (x8 - 32768) / 100,
                 :temperature_at_level_9 => (x9 - 32768) / 100,
                 :temperature_at_level_10 => (x10 - 32768) / 100,
                 :temperature_at_level_11 => (x11 - 32768) / 100,
                 :temperature_at_level_12 => (x12 - 32768) / 100,
                 :temperature_at_level_13 => (x13 - 32768) / 100,
                 :temperature_at_level_14 => (x14 - 32768) / 100,
                 :temperature_at_level_15 => (x15 - 32768) / 100
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