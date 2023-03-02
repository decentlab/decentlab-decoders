
-- https://www.decentlab.com/products/sapflow-sensor-for-lorawan

local PROTOCOL_VERSION = 2

-- sensor definitions
local SENSORS = {
  {["length"] = 16,
   ["values"] = {
     {["name"] = "sap_flow",
      ["display_name"] = "Sap flow",
      ["convert"] = function (x) return (x[0 + 1] * 16 - 50000) / 1000 end,
      ["unit"] = "l⋅h⁻¹"},
     {["name"] = "heat_velocity_outer",
      ["display_name"] = "Heat velocity (outer)",
      ["convert"] = function (x) return (x[1 + 1] * 16 - 50000) / 1000 end,
      ["unit"] = "cm⋅h⁻¹"},
     {["name"] = "heat_velocity_inner",
      ["display_name"] = "Heat velocity (inner)",
      ["convert"] = function (x) return (x[2 + 1] * 16 - 50000) / 1000 end,
      ["unit"] = "cm⋅h⁻¹"},
     {["name"] = "alpha_outer",
      ["display_name"] = "Alpha (outer)",
      ["convert"] = function (x) return (x[3 + 1] * 32 - 1000000) / 100000 end},
     {["name"] = "alpha_inner",
      ["display_name"] = "Alpha (inner)",
      ["convert"] = function (x) return (x[4 + 1] * 32 - 1000000) / 100000 end},
     {["name"] = "beta_outer",
      ["display_name"] = "Beta (outer)",
      ["convert"] = function (x) return (x[5 + 1] * 32 - 1000000) / 100000 end},
     {["name"] = "beta_inner",
      ["display_name"] = "Beta (inner)",
      ["convert"] = function (x) return (x[6 + 1] * 32 - 1000000) / 100000 end},
     {["name"] = "tmax_outer",
      ["display_name"] = "Tmax (outer)",
      ["convert"] = function (x) return (x[7 + 1] * 2) / 1000 end,
      ["unit"] = "s"},
     {["name"] = "tmax_inner",
      ["display_name"] = "Tmax (inner)",
      ["convert"] = function (x) return (x[8 + 1] * 2) / 1000 end,
      ["unit"] = "s"},
     {["name"] = "temperature_outer",
      ["display_name"] = "Temperature (outer)",
      ["convert"] = function (x) return (x[9 + 1] - 32768) / 100 end,
      ["unit"] = "°C"},
     {["name"] = "max_voltage",
      ["display_name"] = "Max voltage",
      ["convert"] = function (x) return (x[10 + 1] - 32768) / 1000 end,
      ["unit"] = "V"},
     {["name"] = "min_voltage",
      ["display_name"] = "Min voltage",
      ["convert"] = function (x) return (x[11 + 1] - 32768) / 1000 end,
      ["unit"] = "V"},
     {["name"] = "diagnostic",
      ["display_name"] = "Diagnostic",
      ["convert"] = function (x) return x[12 + 1] + x[13 + 1] * 65536 end},
     {["name"] = "upstream_tmax_outer",
      ["display_name"] = "Upstream Tmax (outer)",
      ["convert"] = function (x) return (x[14 + 1] * 2) / 1000 end,
      ["unit"] = "s"},
     {["name"] = "upstream_tmax_inner",
      ["display_name"] = "Upstream Tmax (inner)",
      ["convert"] = function (x) return (x[15 + 1] * 2) / 1000 end,
      ["unit"] = "s"}
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
  "023d0100030c290bab0c3e79707a1d78437991490845997e4cacdeaa6e00000000457e415a0b59",
  "023d0100020b59",
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
