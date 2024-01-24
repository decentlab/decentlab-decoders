
-- https://www.decentlab.com/products/laser-distance-level-sensor-for-lorawan

local PROTOCOL_VERSION = 2

-- sensor definitions
local SENSORS = {
  {["length"] = 12,
   ["values"] = {
     {["name"] = "distance_average",
      ["display_name"] = "Distance: average",
      ["convert"] = function (x) return x[0 + 1] end,
      ["unit"] = "mm"},
     {["name"] = "distance_minimum",
      ["display_name"] = "Distance: minimum",
      ["convert"] = function (x) return x[1 + 1] end,
      ["unit"] = "mm"},
     {["name"] = "distance_maximum",
      ["display_name"] = "Distance: maximum",
      ["convert"] = function (x) return x[2 + 1] end,
      ["unit"] = "mm"},
     {["name"] = "distance_median",
      ["display_name"] = "Distance: median",
      ["convert"] = function (x) return x[3 + 1] end,
      ["unit"] = "mm"},
     {["name"] = "distance_10th_percentile",
      ["display_name"] = "Distance: 10th percentile",
      ["convert"] = function (x) return x[4 + 1] end,
      ["unit"] = "mm"},
     {["name"] = "distance_25th_percentile",
      ["display_name"] = "Distance: 25th percentile",
      ["convert"] = function (x) return x[5 + 1] end,
      ["unit"] = "mm"},
     {["name"] = "distance_75th_percentile",
      ["display_name"] = "Distance: 75th percentile",
      ["convert"] = function (x) return x[6 + 1] end,
      ["unit"] = "mm"},
     {["name"] = "distance_90th_percentile",
      ["display_name"] = "Distance: 90th percentile",
      ["convert"] = function (x) return x[7 + 1] end,
      ["unit"] = "mm"},
     {["name"] = "distance_most_frequent_value",
      ["display_name"] = "Distance: most frequent value",
      ["convert"] = function (x) return x[8 + 1] end,
      ["unit"] = "mm"},
     {["name"] = "number_of_valid_samples",
      ["display_name"] = "Number of valid samples",
      ["convert"] = function (x) return x[9 + 1] end},
     {["name"] = "total_acquisition_time",
      ["display_name"] = "Total acquisition time",
      ["convert"] = function (x) return x[10 + 1] / 1.024 end,
      ["unit"] = "ms"},
     {["name"] = "number_of_total_samples",
      ["display_name"] = "Number of total samples",
      ["convert"] = function (x) return x[11 + 1] end}
   }},
  {["length"] = 1,
   ["values"] = {
     {["name"] = "battery_voltage",
      ["display_name"] = "Battery voltage",
      ["convert"] = function (x) return x[0 + 1] / 1000 end,
      ["unit"] = "V"}
   }}
}

-- helper functions
local function fromhex(s)
  local arr = {}
  local k = 1
  for i = 1, #s, 2 do
    arr[k] = tonumber(s:sub(i, i + 1), 16)
    k = k + 1
  end
  return arr
end

function where(condition, if_true, if_false)
  if condition then return if_true else return if_false end
end

local function toint(b1, b2)
  return b1 * 256 + b2
end


-- decoding function
local function decentlab_decode(msg)
  local bytes = msg
  if type(msg) == "string" then
    bytes = fromhex(msg)
  end

  local version = bytes[1]
  if version ~= PROTOCOL_VERSION then
    error("protocol version " .. version .. " doesn't match v2")
  end

  local device_id = toint(bytes[2], bytes[3])
  local flags = toint(bytes[4], bytes[5])
  local result = {["device_id"] = device_id, ["protocol_version"] = version}
  local k = 6
  -- decode sensors
  for _, sensor in ipairs(SENSORS) do
    if flags % 2 == 1 then
      local x = {}
      for j = 1, sensor["length"] do
        x[#x + 1] = toint(bytes[k], bytes[k + 1])
        k = k + 2
      end

      -- decode sensor values
      for _, value in ipairs(sensor["values"]) do
        if value["convert"] then
          result[value["name"]] = {
            ["value"] = value["convert"](x),
            ["display_name"] = value["display_name"],
            ["unit"] = value["unit"]
          }
        end -- if sensor value used
      end -- for each sensor value
    end -- if sensor values present
    flags = math.floor(flags / 2)
  end -- for each sensor

  return result
end


-- test
local payloads = {
  "023ec50003067a06360686067c0636067c068606860686000a0053000a0be6",
  "023ec500020be6",
}

local function main()
  for _, pl in ipairs(payloads) do
    local decoded = decentlab_decode(pl)
    for k, v in pairs(decoded) do
      print(k .. ": " .. (type(v) == "table" and (v["value"] .. " " .. (v["unit"]  or "")) or v))
    end
    print()
  end
end

main()
