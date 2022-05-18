var decentlab_decoder = {
  PROTOCOL_VERSION: 2,
  SENSORS: [
    {length: 16,
     values: [{name: 'ch0_temperature',
               displayName: 'CH0: Temperature',
               convert: function (x) { return (x[0] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch1_temperature',
               displayName: 'CH1: Temperature',
               convert: function (x) { return (x[1] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch2_temperature',
               displayName: 'CH2: Temperature',
               convert: function (x) { return (x[2] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch3_temperature',
               displayName: 'CH3: Temperature',
               convert: function (x) { return (x[3] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch4_temperature',
               displayName: 'CH4: Temperature',
               convert: function (x) { return (x[4] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch5_temperature',
               displayName: 'CH5: Temperature',
               convert: function (x) { return (x[5] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch6_temperature',
               displayName: 'CH6: Temperature',
               convert: function (x) { return (x[6] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch7_temperature',
               displayName: 'CH7: Temperature',
               convert: function (x) { return (x[7] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch8_temperature',
               displayName: 'CH8: Temperature',
               convert: function (x) { return (x[8] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch9_temperature',
               displayName: 'CH9: Temperature',
               convert: function (x) { return (x[9] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch10_temperature',
               displayName: 'CH10: Temperature',
               convert: function (x) { return (x[10] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch11_temperature',
               displayName: 'CH11: Temperature',
               convert: function (x) { return (x[11] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch12_temperature',
               displayName: 'CH12: Temperature',
               convert: function (x) { return (x[12] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch13_temperature',
               displayName: 'CH13: Temperature',
               convert: function (x) { return (x[13] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch14_temperature',
               displayName: 'CH14: Temperature',
               convert: function (x) { return (x[14] - 32768) / 100; },
               unit: '°C'},
              {name: 'ch15_temperature',
               displayName: 'CH15: Temperature',
               convert: function (x) { return (x[15] - 32768) / 100; },
               unit: '°C'}]},
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
  console.log(decentlab_decoder.decode("023e3e00038abc8a928aa08a848ab38a898ac38aad8ab78a928aa1000000000000000000000afc"));
  console.log(decentlab_decoder.decode("023e3e00020afc"));
}

main();
