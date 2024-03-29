
/* https://www.decentlab.com/products/optical-turbidity-and-temperature-sensor-for-lorawan */

var decentlab_decoder = {
  PROTOCOL_VERSION: 2,
  SENSORS: [
    {length: 5,
     values: [{name: 'status',
               displayName: 'Status',
               convert: function (x) { return x[0]; }},
              {name: 'temperature',
               displayName: 'Temperature',
               convert: function (x) { return (x[1] - 32768) / 100; },
               unit: '°C'},
              {name: 'turbidity_in_ntu',
               displayName: 'Turbidity in NTU',
               convert: function (x) { return x[2] / 10; },
               unit: 'NTU'},
              {name: 'turbidity_in_fnu',
               displayName: 'Turbidity in FNU',
               convert: function (x) { return x[3] / 10; },
               unit: 'FNU'},
              {name: 'turbidity_in_mg_l',
               displayName: 'Turbidity in mg/L',
               convert: function (x) { return x[4] / 10; },
               unit: 'mg⋅L⁻¹'}]},
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
                                value: value.convert.bind(this)(x)};
          if ('unit' in value)
            result[value.name]['unit'] = value.unit;
        }
      }
    }
    return result;
  }
};

function main() {
  console.log(decentlab_decoder.decode("022332000300008885013e013e02330b10"));
  console.log(decentlab_decoder.decode("02233200020b10"));
}

main();
