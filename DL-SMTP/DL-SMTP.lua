
-- https://www.decentlab.com/products/soil-moisture-and-temperature-profile-for-lorawan

local PROTOCOL_VERSION = 2

-- sensor definitions
local SENSORS = {
  {["length"] = 16,
   ["values"] = {
     {["name"] = "soil_moisture_at_depth_0",
      ["display_name"] = "Soil moisture at depth 0",
      ["convert"] = function (x) return (x[0 + 1] - 2500) / 500 end},
     {["name"] = "soil_temperature_at_depth_0",
      ["display_name"] = "Soil temperature at depth 0",
      ["convert"] = function (x) return (x[1 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "soil_moisture_at_depth_1",
      ["display_name"] = "Soil moisture at depth 1",
      ["convert"] = function (x) return (x[2 + 1] - 2500) / 500 end},
     {["name"] = "soil_temperature_at_depth_1",
      ["display_name"] = "Soil temperature at depth 1",
      ["convert"] = function (x) return (x[3 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "soil_moisture_at_depth_2",
      ["display_name"] = "Soil moisture at depth 2",
      ["convert"] = function (x) return (x[4 + 1] - 2500) / 500 end},
     {["name"] = "soil_temperature_at_depth_2",
      ["display_name"] = "Soil temperature at depth 2",
      ["convert"] = function (x) return (x[5 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "soil_moisture_at_depth_3",
      ["display_name"] = "Soil moisture at depth 3",
      ["convert"] = function (x) return (x[6 + 1] - 2500) / 500 end},
     {["name"] = "soil_temperature_at_depth_3",
      ["display_name"] = "Soil temperature at depth 3",
      ["convert"] = function (x) return (x[7 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "soil_moisture_at_depth_4",
      ["display_name"] = "Soil moisture at depth 4",
      ["convert"] = function (x) return (x[8 + 1] - 2500) / 500 end},
     {["name"] = "soil_temperature_at_depth_4",
      ["display_name"] = "Soil temperature at depth 4",
      ["convert"] = function (x) return (x[9 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "soil_moisture_at_depth_5",
      ["display_name"] = "Soil moisture at depth 5",
      ["convert"] = function (x) return (x[10 + 1] - 2500) / 500 end},
     {["name"] = "soil_temperature_at_depth_5",
      ["display_name"] = "Soil temperature at depth 5",
      ["convert"] = function (x) return (x[11 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "soil_moisture_at_depth_6",
      ["display_name"] = "Soil moisture at depth 6",
      ["convert"] = function (x) return (x[12 + 1] - 2500) / 500 end},
     {["name"] = "soil_temperature_at_depth_6",
      ["display_name"] = "Soil temperature at depth 6",
      ["convert"] = function (x) return (x[13 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "soil_moisture_at_depth_7",
      ["display_name"] = "Soil moisture at depth 7",
      ["convert"] = function (x) return (x[14 + 1] - 2500) / 500 end},
     {["name"] = "soil_temperature_at_depth_7",
      ["display_name"] = "Soil temperature at depth 7",
      ["convert"] = function (x) return (x[15 + 1] - 32768) / 100 end,
      ["unit"] = "°C"}
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
  "020b50000309018a8c09438a9809278a920b3c8aa50c9c8a8c11e08aa500000000000000000b3b",
  "020b5000020b3b",
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
