# -*- coding: utf-8 -*-

# https://www.decentlab.com/products/air-quality-station-no2-no-co-ox-for-lorawan

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import struct
from math import log, floor
from base64 import binascii

PROTOCOL_VERSION = 2

# device-specific parameters
PARAMETERS = {
  'NO2_WE_0': 256,
  'NO2_S': 0.256,
  'NO2_AUX_0': 227,
  'NO_WE_0': 320,
  'NO_S': 0.512,
  'NO_AUX_0': 288,
  'Ox_WE_0': 235,
  'Ox_S': 0.345,
  'Ox_AUX_0': 200,
  'CO_WE_0': 544,
  'CO_S': 0.424,
  'CO_AUX_0': 301
}

SENSORS = [
    {'length': 2,
     'values': [{'name': 'Air temperature',
                 'convert': lambda x: 175.72 * x[0] / 65536 - 46.85,
                 'unit': '°C'},
                {'name': 'Air humidity',
                 'convert': lambda x: 125 * x[1] / 65536 - 6,
                 'unit': '%'}]},
    {'length': 2,
     'values': [{'name': 'CH4: NO2 (we)',
                 'convert': lambda x: 3 * (x[0] / 32768 - 1) * 1000,
                 'unit': 'mV'},
                {'name': 'CH4: NO2 (we-aux)',
                 'convert': lambda x: 3 * (x[1] / 32768 - 1) * 1000,
                 'unit': 'mV'},
                {'name': 'CH4: NO2 concentration (we)',
                 'convert': lambda x: (3 * (x[0] / 32768 - 1) * 1000 - PARAMETERS['NO2_WE_0']) / PARAMETERS['NO2_S'],
                 'unit': 'ppb'},
                {'name': 'CH4: NO2 concentration (we-aux)',
                 'convert': lambda x: (3 * (x[1] / 32768 - 1) * 1000 - PARAMETERS['NO2_WE_0'] + PARAMETERS['NO2_AUX_0']) / PARAMETERS['NO2_S'],
                 'unit': 'ppb'}]},
    {'length': 2,
     'values': [{'name': 'CH5: NO (we)',
                 'convert': lambda x: 3 * (x[0] / 32768 - 1) * 1000,
                 'unit': 'mV'},
                {'name': 'CH5: NO (we-aux)',
                 'convert': lambda x: 3 * (x[1] / 32768 - 1) * 1000,
                 'unit': 'mV'},
                {'name': 'CH5: NO concentration (we)',
                 'convert': lambda x: (3 * (x[0] / 32768 - 1) * 1000 - PARAMETERS['NO_WE_0']) / PARAMETERS['NO_S'],
                 'unit': 'ppb'},
                {'name': 'CH5: NO concentration (we-aux)',
                 'convert': lambda x: (3 * (x[1] / 32768 - 1) * 1000 - PARAMETERS['NO_WE_0'] + PARAMETERS['NO_AUX_0']) / PARAMETERS['NO_S'],
                 'unit': 'ppb'}]},
    {'length': 2,
     'values': [{'name': 'CH6: Ox (we)',
                 'convert': lambda x: 3 * (x[0] / 32768 - 1) * 1000,
                 'unit': 'mV'},
                {'name': 'CH6: Ox (we-aux)',
                 'convert': lambda x: 3 * (x[1] / 32768 - 1) * 1000,
                 'unit': 'mV'},
                {'name': 'CH6: Ox concentration (we)',
                 'convert': lambda x: (3 * (x[0] / 32768 - 1) * 1000 - PARAMETERS['Ox_WE_0']) / PARAMETERS['Ox_S'],
                 'unit': 'ppb'},
                {'name': 'CH6: Ox concentration (we-aux)',
                 'convert': lambda x: (3 * (x[1] / 32768 - 1) * 1000 - PARAMETERS['Ox_WE_0'] + PARAMETERS['Ox_AUX_0']) / PARAMETERS['Ox_S'],
                 'unit': 'ppb'}]},
    {'length': 2,
     'values': [{'name': 'CH7: CO (we)',
                 'convert': lambda x: 3 * (x[0] / 32768 - 1) * 1000,
                 'unit': 'mV'},
                {'name': 'CH7: CO (we-aux)',
                 'convert': lambda x: 3 * (x[1] / 32768 - 1) * 1000,
                 'unit': 'mV'},
                {'name': 'CH7: CO concentration (we)',
                 'convert': lambda x: (3 * (x[0] / 32768 - 1) * 1000 - PARAMETERS['CO_WE_0']) / PARAMETERS['CO_S'],
                 'unit': 'ppb'},
                {'name': 'CH7: CO concentration (we-aux)',
                 'convert': lambda x: (3 * (x[1] / 32768 - 1) * 1000 - PARAMETERS['CO_WE_0'] + PARAMETERS['CO_AUX_0']) / PARAMETERS['CO_S'],
                 'unit': 'ppb'}]},
    {'length': 1,
     'values': [{'name': 'Battery voltage',
                 'convert': lambda x: x[0] / 1000,
                 'unit': 'V'}]}
]


def decode(msg, hex=False):
    """msg: payload as one of hex string, list, or bytearray"""
    bytes_ = bytearray(binascii.a2b_hex(msg)
                       if hex
                       else msg)
    version = bytes_[0]
    if version != PROTOCOL_VERSION:
        raise ValueError("protocol version {} doesn't match v2".format(version))

    devid = struct.unpack('>H', bytes_[1:3])[0]
    bin_flags = bin(struct.unpack('>H', bytes_[3:5])[0])
    flags = bin_flags[2:].zfill(struct.calcsize('>H') * 8)[::-1]

    words = [struct.unpack('>H', bytes_[i:i + 2])[0]
             for i
             in range(5, len(bytes_), 2)]

    cur = 0
    result = {'Device ID': devid, 'Protocol version': version}
    for flag, sensor in zip(flags, SENSORS):
        if flag != '1':
            continue

        x = words[cur:cur + sensor["length"]]
        cur += sensor["length"]
        for value in sensor['values']:
            if 'convert' not in value:
                continue

            result[value['name']] = {'value': value['convert'](x),
                                     'unit': value.get('unit', None)}

    return result


if __name__ == '__main__':

    import pprint
    payloads = [
        b'020fa0003f66b49b8c8966803c8cf580238a68804c903783f4158a',
        b'020fa00020158a',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl, hex=True))
        print("")
