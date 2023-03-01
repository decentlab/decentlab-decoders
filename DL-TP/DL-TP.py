# -*- coding: utf-8 -*-
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import struct
from math import log, floor
from base64 import binascii

PROTOCOL_VERSION = 2

SENSORS = [
    {'length': 16,
     'values': [{'name': 'Temperature at level 0',
                 'convert': lambda x: (x[0] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 1',
                 'convert': lambda x: (x[1] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 2',
                 'convert': lambda x: (x[2] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 3',
                 'convert': lambda x: (x[3] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 4',
                 'convert': lambda x: (x[4] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 5',
                 'convert': lambda x: (x[5] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 6',
                 'convert': lambda x: (x[6] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 7',
                 'convert': lambda x: (x[7] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 8',
                 'convert': lambda x: (x[8] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 9',
                 'convert': lambda x: (x[9] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 10',
                 'convert': lambda x: (x[10] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 11',
                 'convert': lambda x: (x[11] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 12',
                 'convert': lambda x: (x[12] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 13',
                 'convert': lambda x: (x[13] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 14',
                 'convert': lambda x: (x[14] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Temperature at level 15',
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
        b'023e3e00038abc8a928aa08a848ab38a898ac38aad8ab78a928aa1000000000000000000000afc',
        b'023e3e00020afc',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl, hex=True))
        print("")
