# -*- coding: utf-8 -*-

# https://www.decentlab.com/products/optical-dissolved-oxygen-and-temperature-sensor-for-lorawan

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import struct
from base64 import binascii

PROTOCOL_VERSION = 2

SENSORS = [
    {'length': 5,
     'values': [{'name': 'Status',
                 'convert': lambda x: x[0]},
                {'name': 'Temperature',
                 'convert': lambda x: (x[1] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Oxygen saturation',
                 'convert': lambda x: (x[2] - 32768) / 100,
                 'unit': '%'},
                {'name': 'Oxygen concentration',
                 'convert': lambda x: (x[3] - 32768) / 100,
                 'unit': 'mg⋅L⁻¹'},
                {'name': 'Oxygen concentration (alt)',
                 'convert': lambda x: (x[4] - 32768) / 100,
                 'unit': 'ppm'}]},
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
        b'02186c000300008862a618836583650c60',
        b'02186c00020c60',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl, hex=True))
        print("")
