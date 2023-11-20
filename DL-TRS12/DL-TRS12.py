# -*- coding: utf-8 -*-

# https://www.decentlab.com/products/soil-moisture-temperature-and-electrical-conductivity-sensor-for-lorawan

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import struct
from math import log, floor
from base64 import binascii

PROTOCOL_VERSION = 2

SENSORS = [
    {'length': 3,
     'values': [{'name': 'Dielectric permittivity',
                 'convert': lambda x: pow(0.000000002887 * pow(x[0]/10, 3) - 0.0000208 * pow(x[0]/10, 2) + 0.05276 * (x[0]/10) - 43.39, 2)},
                {'name': 'Volumetric water content',
                 'convert': lambda x: x[0]/10 * 0.0003879 - 0.6956,
                 'unit': 'm³⋅m⁻³'},
                {'name': 'Soil temperature',
                 'convert': lambda x: (x[1] - 32768) / 10,
                 'unit': '°C'},
                {'name': 'Electrical conductivity',
                 'convert': lambda x: x[2],
                 'unit': 'µS⋅cm⁻¹'}]},
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
        b'0210d3000346be813d00000c80',
        b'0210d300020c80',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl, hex=True))
        print("")
