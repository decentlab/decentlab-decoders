# Payload decoders for Decentlab sensor devices

These samples are provided to boost your integration of Decentlab sensor devices.

Supported languages are *JavaScript*, *C#*, and *Python*.

We would be more than happy to merge pull requests fixing issues, improving the quality or even supporting new language/environments.

Please browse the devices in the corresponding directories.

# Integration guide for some platforms

## The Things Network

Take the JavaScript implementation and paste into the `Payload Formats`'s `decoder` window in the TTN Console by overwriting its content. Remove the `main()` function and its call.
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
TODO