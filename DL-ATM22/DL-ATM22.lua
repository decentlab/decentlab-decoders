
-- https://www.decentlab.com/products/wind-speed-wind-direction-and-temperature-sensor-for-lorawan

local PROTOCOL_VERSION = 2
-- Device-specific parameters
local PARAMETERS = {
}
-- sensor definitions
local SENSORS = {
  {["length"] = 8,
   ["values"] = {
     {["name"] = "Wind speed",
      ["convert"] = function (x) return (x[0 + 1] - 32768) / 100 end,
      ["unit"] = "m⋅s⁻¹"},
     {["name"] = "Wind direction",
      ["convert"] = function (x) return (x[1 + 1] - 32768) / 10 end,
      ["unit"] = "°"},
     {["name"] = "Maximum wind speed",
      ["convert"] = function (x) return (x[2 + 1] - 32768) / 100 end,
      ["unit"] = "m⋅s⁻¹"},
     {["name"] = "Air temperature",
      ["convert"] = function (x) return (x[3 + 1] - 32768) / 10 end,
      ["unit"] = "°C"},
     {["name"] = "X orientation angle",
      ["convert"] = function (x) return (x[4 + 1] - 32768) / 10 end,
      ["unit"] = "°"},
     {["name"] = "Y orientation angle",
      ["convert"] = function (x) return (x[5 + 1] - 32768) / 10 end,
      ["unit"] = "°"},
     {["name"] = "North wind speed",
      ["convert"] = function (x) return (x[6 + 1] - 32768) / 100 end,
      ["unit"] = "m⋅s⁻¹"},
     {["name"] = "East wind speed",
      ["convert"] = function (x) return (x[7 + 1] - 32768) / 100 end,
      ["unit"] = "m⋅s⁻¹"}
   }},
  {["length"] = 1,
   ["values"] = {
     {["name"] = "Battery voltage",
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
  local result = {["Device ID"] = device_id, ["Protocol version"] = version}
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
  "0208c900038009812b8014810880027fe8800880040bf5",
  "0208c900020bf5",
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
