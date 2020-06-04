
-- https://www.decentlab.com/products/air-quality-station-no2-no-co-ox-for-lorawan

local PROTOCOL_VERSION = 2

-- device-specific parameters
local PARAMETERS = {
  ["NO2_WE_0"] = 256,
  ["NO2_S"] = 0.256,
  ["NO2_AUX_0"] = 227,
  ["NO_WE_0"] = 320,
  ["NO_S"] = 0.512,
  ["NO_AUX_0"] = 288,
  ["Ox_WE_0"] = 235,
  ["Ox_S"] = 0.345,
  ["Ox_AUX_0"] = 200,
  ["CO_WE_0"] = 544,
  ["CO_S"] = 0.424,
  ["CO_AUX_0"] = 301
}

-- sensor definitions
local SENSORS = {
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
  {["length"] = 2,
   ["values"] = {
     {["name"] = "ch4_no2_we",
      ["display_name"] = "CH4: NO2 (we)",
      ["convert"] = function (x) return 3 * (x[0 + 1] / 32768 - 1) * 1000 end,
      ["unit"] = "mV"},
     {["name"] = "ch4_no2_we_aux",
      ["display_name"] = "CH4: NO2 (we-aux)",
      ["convert"] = function (x) return 3 * (x[1 + 1] / 32768 - 1) * 1000 end,
      ["unit"] = "mV"},
     {["name"] = "ch4_no2_concentration_we",
      ["display_name"] = "CH4: NO2 concentration (we)",
      ["convert"] = function (x) return (3 * (x[0 + 1] / 32768 - 1) * 1000 - PARAMETERS["NO2_WE_0"]) / PARAMETERS["NO2_S"] end,
      ["unit"] = "ppb"},
     {["name"] = "ch4_no2_concentration_we_aux",
      ["display_name"] = "CH4: NO2 concentration (we-aux)",
      ["convert"] = function (x) return (3 * (x[1 + 1] / 32768 - 1) * 1000 - PARAMETERS["NO2_WE_0"] + PARAMETERS["NO2_AUX_0"]) / PARAMETERS["NO2_S"] end,
      ["unit"] = "ppb"}
   }},
  {["length"] = 2,
   ["values"] = {
     {["name"] = "ch5_no_we",
      ["display_name"] = "CH5: NO (we)",
      ["convert"] = function (x) return 3 * (x[0 + 1] / 32768 - 1) * 1000 end,
      ["unit"] = "mV"},
     {["name"] = "ch5_no_we_aux",
      ["display_name"] = "CH5: NO (we-aux)",
      ["convert"] = function (x) return 3 * (x[1 + 1] / 32768 - 1) * 1000 end,
      ["unit"] = "mV"},
     {["name"] = "ch5_no_concentration_we",
      ["display_name"] = "CH5: NO concentration (we)",
      ["convert"] = function (x) return (3 * (x[0 + 1] / 32768 - 1) * 1000 - PARAMETERS["NO_WE_0"]) / PARAMETERS["NO_S"] end,
      ["unit"] = "ppb"},
     {["name"] = "ch5_no_concentration_we_aux",
      ["display_name"] = "CH5: NO concentration (we-aux)",
      ["convert"] = function (x) return (3 * (x[1 + 1] / 32768 - 1) * 1000 - PARAMETERS["NO_WE_0"] + PARAMETERS["NO_AUX_0"]) / PARAMETERS["NO_S"] end,
      ["unit"] = "ppb"}
   }},
  {["length"] = 2,
   ["values"] = {
     {["name"] = "ch6_ox_we",
      ["display_name"] = "CH6: Ox (we)",
      ["convert"] = function (x) return 3 * (x[0 + 1] / 32768 - 1) * 1000 end,
      ["unit"] = "mV"},
     {["name"] = "ch6_ox_we_aux",
      ["display_name"] = "CH6: Ox (we-aux)",
      ["convert"] = function (x) return 3 * (x[1 + 1] / 32768 - 1) * 1000 end,
      ["unit"] = "mV"},
     {["name"] = "ch6_ox_concentration_we",
      ["display_name"] = "CH6: Ox concentration (we)",
      ["convert"] = function (x) return (3 * (x[0 + 1] / 32768 - 1) * 1000 - PARAMETERS["Ox_WE_0"]) / PARAMETERS["Ox_S"] end,
      ["unit"] = "ppb"},
     {["name"] = "ch6_ox_concentration_we_aux",
      ["display_name"] = "CH6: Ox concentration (we-aux)",
      ["convert"] = function (x) return (3 * (x[1 + 1] / 32768 - 1) * 1000 - PARAMETERS["Ox_WE_0"] + PARAMETERS["Ox_AUX_0"]) / PARAMETERS["Ox_S"] end,
      ["unit"] = "ppb"}
   }},
  {["length"] = 2,
   ["values"] = {
     {["name"] = "ch7_co_we",
      ["display_name"] = "CH7: CO (we)",
      ["convert"] = function (x) return 3 * (x[0 + 1] / 32768 - 1) * 1000 end,
      ["unit"] = "mV"},
     {["name"] = "ch7_co_we_aux",
      ["display_name"] = "CH7: CO (we-aux)",
      ["convert"] = function (x) return 3 * (x[1 + 1] / 32768 - 1) * 1000 end,
      ["unit"] = "mV"},
     {["name"] = "ch7_co_concentration_we",
      ["display_name"] = "CH7: CO concentration (we)",
      ["convert"] = function (x) return (3 * (x[0 + 1] / 32768 - 1) * 1000 - PARAMETERS["CO_WE_0"]) / PARAMETERS["CO_S"] end,
      ["unit"] = "ppb"},
     {["name"] = "ch7_co_concentration_we_aux",
      ["display_name"] = "CH7: CO concentration (we-aux)",
      ["convert"] = function (x) return (3 * (x[1 + 1] / 32768 - 1) * 1000 - PARAMETERS["CO_WE_0"] + PARAMETERS["CO_AUX_0"]) / PARAMETERS["CO_S"] end,
      ["unit"] = "ppb"}
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
  "020fa0003f66b49b8c8966803c8cf580238a68804c903783f4158a",
  "020fa00020158a",
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
