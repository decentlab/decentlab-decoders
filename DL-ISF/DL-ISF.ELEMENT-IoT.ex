
# https://www.decentlab.com/products/sapflow-sensor-for-lorawan

defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 023d0100030c290bab0c3e79707a1d78437991490845997e4cacdeaa6e00000000457e415a0b59
  # 023d0100020b59
  
  def fields do
    [
      %{field: "sap_flow", display: "Sap flow", unit: "l⋅h⁻¹"},
      %{field: "heat_velocity_outer", display: "Heat velocity (outer)", unit: "cm⋅h⁻¹"},
      %{field: "heat_velocity_inner", display: "Heat velocity (inner)", unit: "cm⋅h⁻¹"},
      %{field: "alpha_outer", display: "Alpha (outer)", unit: ""},
      %{field: "alpha_inner", display: "Alpha (inner)", unit: ""},
      %{field: "beta_outer", display: "Beta (outer)", unit: ""},
      %{field: "beta_inner", display: "Beta (inner)", unit: ""},
      %{field: "tmax_outer", display: "Tmax (outer)", unit: "s"},
      %{field: "tmax_inner", display: "Tmax (inner)", unit: "s"},
      %{field: "temperature_outer", display: "Temperature (outer)", unit: "°C"},
      %{field: "max_voltage", display: "Max voltage", unit: "V"},
      %{field: "min_voltage", display: "Min voltage", unit: "V"},
      %{field: "diagnostic", display: "Diagnostic", unit: ""},
      %{field: "upstream_tmax_outer", display: "Upstream Tmax (outer)", unit: "s"},
      %{field: "upstream_tmax_inner", display: "Upstream Tmax (inner)", unit: "s"},
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
                 :sap_flow => (x0 * 16 - 50000) / 1000,
                 :heat_velocity_outer => (x1 * 16 - 50000) / 1000,
                 :heat_velocity_inner => (x2 * 16 - 50000) / 1000,
                 :alpha_outer => (x3 * 32 - 1000000) / 100000,
                 :alpha_inner => (x4 * 32 - 1000000) / 100000,
                 :beta_outer => (x5 * 32 - 1000000) / 100000,
                 :beta_inner => (x6 * 32 - 1000000) / 100000,
                 :tmax_outer => (x7 * 2) / 1000,
                 :tmax_inner => (x8 * 2) / 1000,
                 :temperature_outer => (x9 - 32768) / 100,
                 :max_voltage => (x10 - 32768) / 1000,
                 :min_voltage => (x11 - 32768) / 1000,
                 :diagnostic => x12 + x13 * 65536,
                 :upstream_tmax_outer => (x14 * 2) / 1000,
                 :upstream_tmax_inner => (x15 * 2) / 1000
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