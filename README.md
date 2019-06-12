# Payload decoders for Decentlab sensor devices

These samples are provided to boost your integration of Decentlab sensor devices.

Supported languages are *JavaScript*, *C#*, *Lua*, *Elixir*, *ELEMENT IoT Elixir* and *Python*.

Pull requests fixing issues, improving the quality or supporting new language/environments are welcome.

Please browse the devices in the corresponding directories:

Device | Name
---|---
[DL-LP8P](DL-LP8P) | [CO2, Temperature, Humidity and Barometric Pressure Sensor for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/products/co2-temperature-humidity-and-barometric-pressure-sensor-for-lorawan)
[DL-PR26](DL-PR26) | [Pressure / Liquid Level and Temperature Sensor for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/products/pressure-/-liquid-level-and-temperature-sensor-for-lorawan)
[DL-PR21](DL-PR21) | [Pressure / Liquid Level and Temperature Sensor with Pipe Thread for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/products/pressure-/-liquid-level-and-temperature-sensor-with-g1/4-pipe-thread-for-lorawan)
[DL-PR36](DL-PR36) | [High-Precision Pressure / Liquid Level and Temperature Sensor for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/support)
[DL-PR36CTD](DL-PR36CTD) | [High-Precision Pressure / Liquid Level, Temperature and Electrical Conductivity Sensor for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/products/high-precision-pressure-/-liquid-level-temperature-and-electrical-conductivity-sensor-for-lorawan)
[DL-IAM](DL-IAM) | [Indoor Ambiance Monitor for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/products/indoor-ambiance-monitor-including-co2-tvoc-and-motion-sensor-for-lorawan)
[DL-SHT21](DL-SHT21) | [Air Temperature and Humidity Sensor with Radiation Shield for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/support)
[DL-KL66](DL-KL66) | [Strain / Weight Sensor for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/products/strain-/-weight-sensor-for-lorawan)
[DL-ATM22](DL-ATM22) | [Wind Speed, Wind Direction and Temperature Sensor for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/products/wind-speed-wind-direction-and-temperature-sensor-for-lorawan)
[DL-MBX](DL-MBX) | [Ultrasonic Distance / Level Sensor for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/products/ultrasonic-distance-/-level-sensor-for-lorawan)
[DL-DLR2-004](DL-DLR2-004) | [Analog 4 … 20mA sensor for LoRaWAN<sup>TM</sup>](https://www.decentlab.com/products/analog-or-digital-sensor-device-for-lorawan)

# Integration guide for some platforms

## The Things Network

Go to your application on *TTN Console* and select `Payload Formats`. Take the *JavaScript* implementation and paste into the `decoder` window by overwriting its content. Remove the `main()` function and its call.
```js
function main() {
    ...
}

main();
```

Append the following lines.
```js
function Decoder(bytes, port) {
  return decentlab_decoder.decode(bytes);
}
```

Copy example payload message from the datasheet, paste into `Payload`, and click `Test`. Make sure the output values match against the datasheet example and  save the decoder.

## ELEMENT IoT

Go to `Automation`, `Parser`, and create a new parser. Take *ELEMENT-IoT Elixir* implementation and paste into the `Code` window by overwriting its content. Test the provided payloads against the datasheet and save.

## ResIOT

Go to `Nodes/Devices` and select the target device. Select `Manual Lua Scene` from the `Payload parsing scene mode` list.

Take the *Lua* implementation and paste into the `Lua Code` editor. Remove the `main()` function and its call.
```lua
local function main()
  ...
end

main()
```

Replace the spaces in the field names with `_`. For example:
```lua
  ...
   ["values"] = {
     {["name"] = "Battery voltage",
      ["convert"] = function (x) return x[0 + 1] / 1000 end,
      ["unit"] = "V"}
  ...
```
becomes
```lua
  ...
   ["values"] = {
     {["name"] = "Battery_voltage",
      ["convert"] = function (x) return x[0 + 1] / 1000 end,
      ["unit"] = "V"}
  ...
```

Append the following lines.
```lua
-- get payload
if resiot_startfrom() == "Manual" then
    payload_hex = payloads[1]
    port = "99"
    deveui = ""
    appeui = ""
else
    payload_hex = resiot_comm_getparam("payload")
    port = resiot_comm_getparam("port")
    deveui = resiot_payload_getparam("deveui")
    appeui = resiot_payload_getparam("appeui")
end

-- decode
local decoded = decentlab_decode(payload_hex)

-- set decoded fields
for k, v in pairs(decoded) do
  if type(v) == "table" then
    if resiot_startfrom() == "Manual" then
      resiot_debug(k .. ": " .. v["value"] .. " " .. (v["unit"] or ""))
    else
      resiot_setnodevalue(appeui, deveui, k, v["value"])
    end
  else
    if resiot_startfrom() == "Manual" then
      resiot_debug(k .. ": " .. v)
    else
      resiot_setnodevalue(appeui, deveui, k, v)
    end
  end
end
```

Test by clicking `Run` and make sure the output values match against the datasheet. Configure the fields in `Node fields` accordingly and press `Save` icon.