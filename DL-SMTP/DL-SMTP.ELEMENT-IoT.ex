
# https://www.decentlab.com/products/soil-moisture-and-temperature-profile-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 020b50000309018a8c09438a9809278a920b3c8aa50c9c8a8c11e08aa500000000000000000b3b
  # 020b5000020b3b
  
  def fields do
    [
      %{field: "soil_moisture_at_depth_0", display: "Soil moisture at depth 0", unit: "None"},
      %{field: "soil_temperature_at_depth_0", display: "Soil temperature at depth 0", unit: "°C"},
      %{field: "soil_moisture_at_depth_1", display: "Soil moisture at depth 1", unit: "None"},
      %{field: "soil_temperature_at_depth_1", display: "Soil temperature at depth 1", unit: "°C"},
      %{field: "soil_moisture_at_depth_2", display: "Soil moisture at depth 2", unit: "None"},
      %{field: "soil_temperature_at_depth_2", display: "Soil temperature at depth 2", unit: "°C"},
      %{field: "soil_moisture_at_depth_3", display: "Soil moisture at depth 3", unit: "None"},
      %{field: "soil_temperature_at_depth_3", display: "Soil temperature at depth 3", unit: "°C"},
      %{field: "soil_moisture_at_depth_4", display: "Soil moisture at depth 4", unit: "None"},
      %{field: "soil_temperature_at_depth_4", display: "Soil temperature at depth 4", unit: "°C"},
      %{field: "soil_moisture_at_depth_5", display: "Soil moisture at depth 5", unit: "None"},
      %{field: "soil_temperature_at_depth_5", display: "Soil temperature at depth 5", unit: "°C"},
      %{field: "soil_moisture_at_depth_6", display: "Soil moisture at depth 6", unit: "None"},
      %{field: "soil_temperature_at_depth_6", display: "Soil temperature at depth 6", unit: "°C"},
      %{field: "soil_moisture_at_depth_7", display: "Soil moisture at depth 7", unit: "None"},
      %{field: "soil_temperature_at_depth_7", display: "Soil temperature at depth 7", unit: "°C"},
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
                 :soil_moisture_at_depth_0 => (x0 - 2500) / 500,
                 :soil_temperature_at_depth_0 => (x1 - 32768) / 100,
                 :soil_moisture_at_depth_1 => (x2 - 2500) / 500,
                 :soil_temperature_at_depth_1 => (x3 - 32768) / 100,
                 :soil_moisture_at_depth_2 => (x4 - 2500) / 500,
                 :soil_temperature_at_depth_2 => (x5 - 32768) / 100,
                 :soil_moisture_at_depth_3 => (x6 - 2500) / 500,
                 :soil_temperature_at_depth_3 => (x7 - 32768) / 100,
                 :soil_moisture_at_depth_4 => (x8 - 2500) / 500,
                 :soil_temperature_at_depth_4 => (x9 - 32768) / 100,
                 :soil_moisture_at_depth_5 => (x10 - 2500) / 500,
                 :soil_temperature_at_depth_5 => (x11 - 32768) / 100,
                 :soil_moisture_at_depth_6 => (x12 - 2500) / 500,
                 :soil_temperature_at_depth_6 => (x13 - 32768) / 100,
                 :soil_moisture_at_depth_7 => (x14 - 2500) / 500,
                 :soil_temperature_at_depth_7 => (x15 - 32768) / 100
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