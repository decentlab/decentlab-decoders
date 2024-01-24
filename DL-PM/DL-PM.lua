
-- https://www.decentlab.com/products/particulate-matter-temperature-humidity-and-barometric-pressure-sensor-for-lorawan

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
  {["length"] = 10,
   ["values"] = {
     {["name"] = "pm1_0_mass_concentration",
      ["display_name"] = "PM1.0 mass concentration",
      ["convert"] = function (x) return x[0 + 1] / 10 end,
      ["unit"] = "µg⋅m⁻³"},
     {["name"] = "pm2_5_mass_concentration",
      ["display_name"] = "PM2.5 mass concentration",
      ["convert"] = function (x) return x[1 + 1] / 10 end,
      ["unit"] = "µg⋅m⁻³"},
     {["name"] = "pm4_mass_concentration",
      ["display_name"] = "PM4 mass concentration",
      ["convert"] = function (x) return x[2 + 1] / 10 end,
      ["unit"] = "µg⋅m⁻³"},
     {["name"] = "pm10_mass_concentration",
      ["display_name"] = "PM10 mass concentration",
      ["convert"] = function (x) return x[3 + 1] / 10 end,
      ["unit"] = "µg⋅m⁻³"},
     {["name"] = "typical_particle_size",
      ["display_name"] = "Typical particle size",
      ["convert"] = function (x) return x[4 + 1] end,
      ["unit"] = "nm"},
     {["name"] = "pm0_5_number_concentration",
      ["display_name"] = "PM0.5 number concentration",
      ["convert"] = function (x) return x[5 + 1] / 10 end,
      ["unit"] = "1⋅cm⁻³"},
     {["name"] = "pm1_0_number_concentration",
      ["display_name"] = "PM1.0 number concentration",
      ["convert"] = function (x) return x[6 + 1] / 10 end,
      ["unit"] = "1⋅cm⁻³"},
     {["name"] = "pm2_5_number_concentration",
      ["display_name"] = "PM2.5 number concentration",
      ["convert"] = function (x) return x[7 + 1] / 10 end,
      ["unit"] = "1⋅cm⁻³"},
     {["name"] = "pm4_number_concentration",
      ["display_name"] = "PM4 number concentration",
      ["convert"] = function (x) return x[8 + 1] / 10 end,
      ["unit"] = "1⋅cm⁻³"},
     {["name"] = "pm10_number_concentration",
      ["display_name"] = "PM10 number concentration",
      ["convert"] = function (x) return x[9 + 1] / 10 end,
      ["unit"] = "1⋅cm⁻³"}
   }},
  {["length"] = 2,
   ["values"] = {
     {["name"] = "air_temperature",
      ["display_name"] = "Air temperature",
      ["convert"] = function (x) return 175.72 * x[0 + 1] / 65536 - 46.85 end,
      ["unit"] = "°C"},
     {["name"] = "air_humidity",
      ["display_name"] = "Air humidity",
      ["convert"] = function (x) return 125 * x[1 + 1] / 65536 - 6 end,
      ["unit"] = "%"}
   }},
  {["length"] = 1,
   ["values"] = {
     {["name"] = "barometric_pressure",
      ["display_name"] = "Barometric pressure",
      ["convert"] = function (x) return x[0 + 1] * 2 end,
      ["unit"] = "Pa"}
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
  "021b50000f0c25002500270027002701f50107012c012d012d012d67bd618dbd10",
  "021b50000d0c2567bd618dbd10",
  "021b5000010c25",
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
