defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 023e3e00038abc8a928aa08a848ab38a898ac38aad8ab78a928aa1000000000000000000000afc
  # 023e3e00020afc
  
  def fields do
    [
      %{field: "ch0_temperature", display: "CH0: Temperature", unit: "°C"},
      %{field: "ch1_temperature", display: "CH1: Temperature", unit: "°C"},
      %{field: "ch2_temperature", display: "CH2: Temperature", unit: "°C"},
      %{field: "ch3_temperature", display: "CH3: Temperature", unit: "°C"},
      %{field: "ch4_temperature", display: "CH4: Temperature", unit: "°C"},
      %{field: "ch5_temperature", display: "CH5: Temperature", unit: "°C"},
      %{field: "ch6_temperature", display: "CH6: Temperature", unit: "°C"},
      %{field: "ch7_temperature", display: "CH7: Temperature", unit: "°C"},
      %{field: "ch8_temperature", display: "CH8: Temperature", unit: "°C"},
      %{field: "ch9_temperature", display: "CH9: Temperature", unit: "°C"},
      %{field: "ch10_temperature", display: "CH10: Temperature", unit: "°C"},
      %{field: "ch11_temperature", display: "CH11: Temperature", unit: "°C"},
      %{field: "ch12_temperature", display: "CH12: Temperature", unit: "°C"},
      %{field: "ch13_temperature", display: "CH13: Temperature", unit: "°C"},
      %{field: "ch14_temperature", display: "CH14: Temperature", unit: "°C"},
      %{field: "ch15_temperature", display: "CH15: Temperature", unit: "°C"},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), x6::size(16), x7::size(16), x8::size(16), x9::size(16), x10::size(16), x11::size(16), x12::size(16), x13::size(16), x14::size(16), x15::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :ch0_temperature => (x0 - 32768) / 100,
                 :ch1_temperature => (x1 - 32768) / 100,
                 :ch2_temperature => (x2 - 32768) / 100,
                 :ch3_temperature => (x3 - 32768) / 100,
                 :ch4_temperature => (x4 - 32768) / 100,
                 :ch5_temperature => (x5 - 32768) / 100,
                 :ch6_temperature => (x6 - 32768) / 100,
                 :ch7_temperature => (x7 - 32768) / 100,
                 :ch8_temperature => (x8 - 32768) / 100,
                 :ch9_temperature => (x9 - 32768) / 100,
                 :ch10_temperature => (x10 - 32768) / 100,
                 :ch11_temperature => (x11 - 32768) / 100,
                 :ch12_temperature => (x12 - 32768) / 100,
                 :ch13_temperature => (x13 - 32768) / 100,
                 :ch14_temperature => (x14 - 32768) / 100,
                 :ch15_temperature => (x15 - 32768) / 100
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