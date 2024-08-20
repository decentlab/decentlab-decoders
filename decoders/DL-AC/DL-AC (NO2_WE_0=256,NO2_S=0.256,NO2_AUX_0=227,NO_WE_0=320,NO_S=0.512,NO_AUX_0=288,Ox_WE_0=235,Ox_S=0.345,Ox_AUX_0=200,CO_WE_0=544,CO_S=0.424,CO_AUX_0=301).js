
/* https://www.decentlab.com/products/air-quality-station-no2-no-co-ox-for-lorawan */

var decentlab_decoder = {
  PROTOCOL_VERSION: 2,
  /* device-specific parameters */
  PARAMETERS: {
    NO2_WE_0: 256,
    NO2_S: 0.256,
    NO2_AUX_0: 227,
    NO_WE_0: 320,
    NO_S: 0.512,
    NO_AUX_0: 288,
    Ox_WE_0: 235,
    Ox_S: 0.345,
    Ox_AUX_0: 200,
    CO_WE_0: 544,
    CO_S: 0.424,
    CO_AUX_0: 301
  },
  SENSORS: [
    {length: 2,
     values: [{name: 'air_temperature',
               displayName: 'Air temperature',
               convert: function (x) { return 175.72 * x[0] / 65536 - 46.85; },
               unit: '°C'},
              {name: 'air_humidity',
               displayName: 'Air humidity',
               convert: function (x) { return 125 * x[1] / 65536 - 6; },
               unit: '%'}]},
    {length: 2,
     values: [{name: 'ch4_no2_we',
               displayName: 'CH4: NO2 (we)',
               convert: function (x) { return 3 * (x[0] / 32768 - 1) * 1000; },
               unit: 'mV'},
              {name: 'ch4_no2_we_aux',
               displayName: 'CH4: NO2 (we-aux)',
               convert: function (x) { return 3 * (x[1] / 32768 - 1) * 1000; },
               unit: 'mV'},
              {name: 'ch4_no2_concentration_we',
               displayName: 'CH4: NO2 concentration (we)',
               convert: function (x) { return (3 * (x[0] / 32768 - 1) * 1000 - this.PARAMETERS.NO2_WE_0) / this.PARAMETERS.NO2_S; },
               unit: 'ppb'},
              {name: 'ch4_no2_concentration_we_aux',
               displayName: 'CH4: NO2 concentration (we-aux)',
               convert: function (x) { return (3 * (x[1] / 32768 - 1) * 1000 - this.PARAMETERS.NO2_WE_0 + this.PARAMETERS.NO2_AUX_0) / this.PARAMETERS.NO2_S; },
               unit: 'ppb'}]},
    {length: 2,
     values: [{name: 'ch5_no_we',
               displayName: 'CH5: NO (we)',
               convert: function (x) { return 3 * (x[0] / 32768 - 1) * 1000; },
               unit: 'mV'},
              {name: 'ch5_no_we_aux',
               displayName: 'CH5: NO (we-aux)',
               convert: function (x) { return 3 * (x[1] / 32768 - 1) * 1000; },
               unit: 'mV'},
              {name: 'ch5_no_concentration_we',
               displayName: 'CH5: NO concentration (we)',
               convert: function (x) { return (3 * (x[0] / 32768 - 1) * 1000 - this.PARAMETERS.NO_WE_0) / this.PARAMETERS.NO_S; },
               unit: 'ppb'},
              {name: 'ch5_no_concentration_we_aux',
               displayName: 'CH5: NO concentration (we-aux)',
               convert: function (x) { return (3 * (x[1] / 32768 - 1) * 1000 - this.PARAMETERS.NO_WE_0 + this.PARAMETERS.NO_AUX_0) / this.PARAMETERS.NO_S; },
               unit: 'ppb'}]},
    {length: 2,
     values: [{name: 'ch6_ox_we',
               displayName: 'CH6: Ox (we)',
               convert: function (x) { return 3 * (x[0] / 32768 - 1) * 1000; },
               unit: 'mV'},
              {name: 'ch6_ox_we_aux',
               displayName: 'CH6: Ox (we-aux)',
               convert: function (x) { return 3 * (x[1] / 32768 - 1) * 1000; },
               unit: 'mV'},
              {name: 'ch6_ox_concentration_we',
               displayName: 'CH6: Ox concentration (we)',
               convert: function (x) { return (3 * (x[0] / 32768 - 1) * 1000 - this.PARAMETERS.Ox_WE_0) / this.PARAMETERS.Ox_S; },
               unit: 'ppb'},
              {name: 'ch6_ox_concentration_we_aux',
               displayName: 'CH6: Ox concentration (we-aux)',
               convert: function (x) { return (3 * (x[1] / 32768 - 1) * 1000 - this.PARAMETERS.Ox_WE_0 + this.PARAMETERS.Ox_AUX_0) / this.PARAMETERS.Ox_S; },
               unit: 'ppb'}]},
    {length: 2,
     values: [{name: 'ch7_co_we',
               displayName: 'CH7: CO (we)',
               convert: function (x) { return 3 * (x[0] / 32768 - 1) * 1000; },
               unit: 'mV'},
              {name: 'ch7_co_we_aux',
               displayName: 'CH7: CO (we-aux)',
               convert: function (x) { return 3 * (x[1] / 32768 - 1) * 1000; },
               unit: 'mV'},
              {name: 'ch7_co_concentration_we',
               displayName: 'CH7: CO concentration (we)',
               convert: function (x) { return (3 * (x[0] / 32768 - 1) * 1000 - this.PARAMETERS.CO_WE_0) / this.PARAMETERS.CO_S; },
               unit: 'ppb'},
              {name: 'ch7_co_concentration_we_aux',
               displayName: 'CH7: CO concentration (we-aux)',
               convert: function (x) { return (3 * (x[1] / 32768 - 1) * 1000 - this.PARAMETERS.CO_WE_0 + this.PARAMETERS.CO_AUX_0) / this.PARAMETERS.CO_S; },
               unit: 'ppb'}]},
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
  console.log(decentlab_decoder.decode("020fa0003f66b49b8c8966803c8cf580238a68804c903783f4158a"));
  console.log(decentlab_decoder.decode("020fa00020158a"));
}

main();
