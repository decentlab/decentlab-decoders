local PROTOCOL_VERSION = 2

-- sensor definitions
local SENSORS = {
  {["length"] = 16,
   ["values"] = {
     {["name"] = "ch0_temperature",
      ["display_name"] = "CH0: Temperature",
      ["convert"] = function (x) return (x[0 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch1_temperature",
      ["display_name"] = "CH1: Temperature",
      ["convert"] = function (x) return (x[1 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch2_temperature",
      ["display_name"] = "CH2: Temperature",
      ["convert"] = function (x) return (x[2 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch3_temperature",
      ["display_name"] = "CH3: Temperature",
      ["convert"] = function (x) return (x[3 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch4_temperature",
      ["display_name"] = "CH4: Temperature",
      ["convert"] = function (x) return (x[4 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch5_temperature",
      ["display_name"] = "CH5: Temperature",
      ["convert"] = function (x) return (x[5 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch6_temperature",
      ["display_name"] = "CH6: Temperature",
      ["convert"] = function (x) return (x[6 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch7_temperature",
      ["display_name"] = "CH7: Temperature",
      ["convert"] = function (x) return (x[7 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch8_temperature",
      ["display_name"] = "CH8: Temperature",
      ["convert"] = function (x) return (x[8 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch9_temperature",
      ["display_name"] = "CH9: Temperature",
      ["convert"] = function (x) return (x[9 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch10_temperature",
      ["display_name"] = "CH10: Temperature",
      ["convert"] = function (x) return (x[10 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch11_temperature",
      ["display_name"] = "CH11: Temperature",
      ["convert"] = function (x) return (x[11 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch12_temperature",
      ["display_name"] = "CH12: Temperature",
      ["convert"] = function (x) return (x[12 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch13_temperature",
      ["display_name"] = "CH13: Temperature",
      ["convert"] = function (x) return (x[13 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch14_temperature",
      ["display_name"] = "CH14: Temperature",
      ["convert"] = function (x) return (x[14 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "ch15_temperature",
      ["display_name"] = "CH15: Temperature",
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
  "023e3e00038abc8a928aa08a848ab38a898ac38aad8ab78a928aa1000000000000000000000afc",
  "023e3e00020afc",
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
