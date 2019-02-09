
-- sensor definitions
local SENSORS = {
  {["length"] = 3,
   ["values"] = {
     {["name"] = "Counter reading",
      ["convert"] = function (x) return x[0 + 1] end},
     {["name"] = "Measurement interval",
      ["convert"] = function (x) return x[1 + 1] / 32768 end},
     {["name"] = "Frequency",
      ["convert"] = function (x) return x[0 + 1] / x[1 + 1] * 32768 end,
      ["unit"] = "Hz"},
     {["name"] = "Weight",
      ["convert"] = function (x) return (math.pow(x[0 + 1] / x[1 + 1] * 32768, 2) - math.pow((15383.72), 2)) * (46.4859) / 1000000 end,
      ["unit"] = "g"},
     {["name"] = "Elongation",
      ["convert"] = function (x) return (math.pow(x[0 + 1] / x[1 + 1] * 32768, 2) - math.pow((15383.72), 2)) * (46.4859) / 1000000 * (-1.5) / 1000 * 9.8067 end,
      ["unit"] = "µm"},
     {["name"] = "Strain",
      ["convert"] = function (x) return (math.pow(x[0 + 1] / x[1 + 1] * 32768, 2) - math.pow((15383.72), 2)) * (46.4859) / 1000000 * (-1.5) / 1000 * 9.8067 / 0.066 end,
      ["unit"] = "µm⋅m⁻¹"}
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
  if version ~= 2 then
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
  "0203d400033bf67fff3bf60c60",
  "0203d400020c60",
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
