# -*- coding: utf-8 -*-

# https://www.decentlab.com/products/soil-moisture-and-temperature-profile-for-lorawan

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import struct
from base64 import binascii

PROTOCOL_VERSION = 2

SENSORS = [
    {'length': 16,
     'values': [{'name': 'Soil moisture at depth 0',
                 'convert': lambda x: (x[0] - 2500) / 500},
                {'name': 'Soil temperature at depth 0',
                 'convert': lambda x: (x[1] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Soil moisture at depth 1',
                 'convert': lambda x: (x[2] - 2500) / 500},
                {'name': 'Soil temperature at depth 1',
                 'convert': lambda x: (x[3] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Soil moisture at depth 2',
                 'convert': lambda x: (x[4] - 2500) / 500},
                {'name': 'Soil temperature at depth 2',
                 'convert': lambda x: (x[5] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Soil moisture at depth 3',
                 'convert': lambda x: (x[6] - 2500) / 500},
                {'name': 'Soil temperature at depth 3',
                 'convert': lambda x: (x[7] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Soil moisture at depth 4',
                 'convert': lambda x: (x[8] - 2500) / 500},
                {'name': 'Soil temperature at depth 4',
                 'convert': lambda x: (x[9] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Soil moisture at depth 5',
                 'convert': lambda x: (x[10] - 2500) / 500},
                {'name': 'Soil temperature at depth 5',
                 'convert': lambda x: (x[11] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Soil moisture at depth 6',
                 'convert': lambda x: (x[12] - 2500) / 500},
                {'name': 'Soil temperature at depth 6',
                 'convert': lambda x: (x[13] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Soil moisture at depth 7',
                 'convert': lambda x: (x[14] - 2500) / 500},
                {'name': 'Soil temperature at depth 7',
                 'convert': lambda x: (x[15] - 32768) / 100,
                 'unit': '°C'}]},
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
        b'020b50000309018a8c09438a9809278a920b3c8aa50c9c8a8c11e08aa500000000000000000b3b',
        b'020b5000020b3b',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl, hex=True))
        print("")
