
-- https://www.decentlab.com/products/greenhouse-multi-monitor-for-lorawan

local PROTOCOL_VERSION = 2

-- sensor definitions
local SENSORS = {
  {["length"] = 7,
   ["values"] = {
     {["name"] = "photosynthetically_active_radiation",
      ["display_name"] = "Photosynthetically active radiation",
      ["convert"] = function (x) return (x[0 + 1] - 32768) / 10 end,
      ["unit"] = "µmol⋅m⁻²⋅s⁻¹"},
     {["name"] = "air_temperature",
      ["display_name"] = "Air temperature",
      ["convert"] = function (x) return (x[1 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "air_humidity",
      ["display_name"] = "Air humidity",
      ["convert"] = function (x) return (x[2 + 1] - 32768) / 10 end,
      ["unit"] = "%"},
     {["name"] = "co2_concentration",
      ["display_name"] = "CO2 concentration",
      ["convert"] = function (x) return (x[3 + 1] - 32768) / 1 end,
      ["unit"] = "ppm"},
     {["name"] = "atmospheric_pressure",
      ["display_name"] = "Atmospheric pressure",
      ["convert"] = function (x) return (x[4 + 1] - 32768) / 100 end,
      ["unit"] = "kPa"},
     {["name"] = "vapor_pressure_deficit",
      ["display_name"] = "Vapor pressure deficit",
      ["convert"] = function (x) return (x[5 + 1] - 32768) / 100 end,
      ["unit"] = "kPa"},
     {["name"] = "dew_point",
      ["display_name"] = "Dew point",
      ["convert"] = function (x) return (x[6 + 1] - 32768) / 100 end,
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
  "02532b00038726892081148297a57380cf81700bbc",
  "02528500020bbc",
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
