defmodule Parser do
  use Platform.Parsing.Behaviour
  
  ## test payloads
  # 023ec50003067a06360686067c0636067c068606860686000a0053000a0be6
  # 023ec500020be6
  
  def fields do
    [
      %{field: "distance_average", display: "Distance: average", unit: "mm"},
      %{field: "distance_minimum", display: "Distance: minimum", unit: "mm"},
      %{field: "distance_maximum", display: "Distance: maximum", unit: "mm"},
      %{field: "distance_median", display: "Distance: median", unit: "mm"},
      %{field: "distance_10th_percentile", display: "Distance: 10th percentile", unit: "mm"},
      %{field: "distance_25th_percentile", display: "Distance: 25th percentile", unit: "mm"},
      %{field: "distance_75th_percentile", display: "Distance: 75th percentile", unit: "mm"},
      %{field: "distance_90th_percentile", display: "Distance: 90th percentile", unit: "mm"},
      %{field: "distance_most_frequent_value", display: "Distance: most frequent value", unit: "mm"},
      %{field: "number_of_valid_samples", display: "Number of valid samples", unit: ""},
      %{field: "total_acquisition_time", display: "Total acquisition time", unit: "ms"},
      %{field: "number_of_total_samples", display: "Number of total samples", unit: ""},
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

  
  defp sensor0({<<x0::size(16), x1::size(16), x2::size(16), x3::size(16), x4::size(16), x5::size(16), x6::size(16), x7::size(16), x8::size(16), x9::size(16), x10::size(16), x11::size(16), remaining::binary>>, result},
               <<_::size(15), 1::size(1), _::size(0)>>) do
    {remaining,
     Map.merge(result,
               %{
                 :distance_average => x0,
                 :distance_minimum => x1,
                 :distance_maximum => x2,
                 :distance_median => x3,
                 :distance_10th_percentile => x4,
                 :distance_25th_percentile => x5,
                 :distance_75th_percentile => x6,
                 :distance_90th_percentile => x7,
                 :distance_most_frequent_value => x8,
                 :number_of_valid_samples => x9,
                 :total_acquisition_time => x10 / 1.024,
                 :number_of_total_samples => x11
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
