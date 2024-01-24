
-- https://www.decentlab.com/products/indoor-ambiance-monitor-including-co2-tvoc-and-motion-sensor-for-lorawan

local PROTOCOL_VERSION = 2

-- sensor definitions
local SENSORS = {
  {["length"] = 1,
   ["values"] = {
     {["name"] = "battery_voltage",
      ["display_name"] = "Battery voltage",
      ["convert"] = function (x) return x[0 + 1] / 1000 end,
      ["unit"] = "V"}
   }},
  {["length"] = 2,
   ["values"] = {
     {["name"] = "air_temperature",
      ["display_name"] = "Air temperature",
      ["convert"] = function (x) return 175 * x[0 + 1] / 65535 - 45 end,
      ["unit"] = "°C"},
     {["name"] = "air_humidity",
      ["display_name"] = "Air humidity",
      ["convert"] = function (x) return 100 * x[1 + 1] / 65535 end,
      ["unit"] = "%"}
   }},
  {["length"] = 1,
   ["values"] = {
     {["name"] = "barometric_pressure",
      ["display_name"] = "Barometric pressure",
      ["convert"] = function (x) return x[0 + 1] * 2 end,
      ["unit"] = "Pa"}
   }},
  {["length"] = 2,
   ["values"] = {
     {["name"] = "ambient_light_visible_infrared",
      ["display_name"] = "Ambient light (visible + infrared)",
      ["convert"] = function (x) return x[0 + 1] end},
     {["name"] = "ambient_light_infrared",
      ["display_name"] = "Ambient light (infrared)",
      ["convert"] = function (x) return x[1 + 1] end},
     {["name"] = "illuminance",
      ["display_name"] = "Illuminance",
      ["convert"] = function (x) return math.max(math.max(1.0 * x[0 + 1] - 1.64 * x[1 + 1], 0.59 * x[0 + 1] - 0.86 * x[1 + 1]), 0) * 1.5504 end,
      ["unit"] = "lx"}
   }},
  {["length"] = 3,
   ["values"] = {
     {["name"] = "co2_concentration",
      ["display_name"] = "CO2 concentration",
      ["convert"] = function (x) return x[0 + 1] - 32768 end,
      ["unit"] = "ppm"},
     {["name"] = "co2_sensor_status",
      ["display_name"] = "CO2 sensor status",
      ["convert"] = function (x) return x[1 + 1] end},
     {["name"] = "raw_ir_reading",
      ["display_name"] = "Raw IR reading",
      ["convert"] = function (x) return x[2 + 1] end}
   }},
  {["length"] = 1,
   ["values"] = {
     {["name"] = "activity_counter",
      ["display_name"] = "Activity counter",
      ["convert"] = function (x) return x[0 + 1] end}
   }},
  {["length"] = 1,
   ["values"] = {
     {["name"] = "total_voc",
      ["display_name"] = "Total VOC",
      ["convert"] = function (x) return x[0 + 1] end,
      ["unit"] = "ppb"}
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
  "020bbd007f0b926a515d48bc4e0262006981c7000093d4000b0111",
  "020bbd006f0b926a515d48bc4e02620069000b0111",
  "020bbd00010b92",
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
