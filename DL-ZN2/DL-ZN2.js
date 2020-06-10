
/* https://www.decentlab.com/products/dendrometer-for-lorawan */

var decentlab_decoder = {
  PROTOCOL_VERSION: 2,
  SENSORS: [
    {length: 4,
     values: [{name: 'dendrometer_a_position',
               displayName: 'Dendrometer A Position',
               convert: function (x) { return ((x[0] + x[1]*65536) / 8388608 - 1) * 20000; },
               unit: 'µm'},
              {name: 'dendrometer_b_position',
               displayName: 'Dendrometer B Position',
               convert: function (x) { return ((x[0] + x[1]*65536) / 8388608 - (x[2] + x[3]*65536) / 8388608) * 20000; },
               unit: 'µm'}]},
    {length: 1,
     values: [{name: 'battery_voltage',
               displayName: 'Battery voltage',
               convert: function (x) { return x[0] / 1000; },
               unit: 'V'}]}
  ],

  read_int: function (bytes, pos) {
    return (bytes[pos] << 8) + bytes[pos + 1];
  },

  decode: function (msg) {
    var bytes = msg;
    var i, j;
    if (typeof msg === 'string') {
      bytes = [];
      for (i = 0; i < msg.length; i += 2) {
        bytes.push(parseInt(msg.substring(i, i + 2), 16));
      }
    }

    var version = bytes[0];
    if (version != this.PROTOCOL_VERSION) {
      return {error: "protocol version " + version + " doesn't match v2"};
    }

    var deviceId = this.read_int(bytes, 1);
    var flags = this.read_int(bytes, 3);
    var result = {'protocol_version': version, 'device_id': deviceId};
    // decode payload
    var pos = 5;
    for (i = 0; i < this.SENSORS.length; i++, flags >>= 1) {
      if ((flags & 1) !== 1)
        continue;

      var sensor = this.SENSORS[i];
      var x = [];
      // convert data to 16-bit integer array
      for (j = 0; j < sensor.length; j++) {
        x.push(this.read_int(bytes, pos));
        pos += 2;
      }

      // decode sensor values
      for (j = 0; j < sensor.values.length; j++) {
        var value = sensor.values[j];
        if ('convert' in value) {
          result[value.name] = {displayName: value.displayName,
                                value: value.convert(x),
                                unit: value.unit};
        }
      }
    }
    return result;
  }
};

function main() {
  console.log(decentlab_decoder.decode("0211110003409a00863039003e0c54"));
}

main();