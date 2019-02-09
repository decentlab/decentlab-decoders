# Payload decoders for Decentlab sensor devices

These samples are provided to boost your integration of Decentlab sensor devices.

Supported languages are *JavaScript*, *C#*, and *Python*.

We would be more than happy to merge pull requests fixing issues, improving the quality or even supporting new language/environments.

Please browse the devices in the corresponding directories.

# Integration guide for some platforms

## The Things Network

Go to your application on the TTN Console and select `Payload Formats` and . Take the *JavaScript* implementation and paste into the `decoder` window by overwriting its content. Remove the `main()` function and its call.
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

Test and save the decoder. That's all.

## ELEMENT IoT
TODO

## ResIOT

Go to `Nodes/Devices` and select the target device. Select `Manual Lua Scene` from the `Payload parsing scene mode` list.

Take the *Lua* implementation and paste into the `Lua Code` editor. Remove the `main()` function and its call.
```lua
local function main()
  ...
end

main()
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

Test by clicking `Run` and make sure the values match against the datasheet. Configure the fields in `Node fields` accordingly and press `Save` icon. That's all.
