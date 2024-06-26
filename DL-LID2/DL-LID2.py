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
    {'length': 12,
     'values': [{'name': 'Distance: average',
                 'convert': lambda x: x[0],
                 'unit': 'mm'},
                {'name': 'Distance: minimum',
                 'convert': lambda x: x[1],
                 'unit': 'mm'},
                {'name': 'Distance: maximum',
                 'convert': lambda x: x[2],
                 'unit': 'mm'},
                {'name': 'Distance: median',
                 'convert': lambda x: x[3],
                 'unit': 'mm'},
                {'name': 'Distance: 10th percentile',
                 'convert': lambda x: x[4],
                 'unit': 'mm'},
                {'name': 'Distance: 25th percentile',
                 'convert': lambda x: x[5],
                 'unit': 'mm'},
                {'name': 'Distance: 75th percentile',
                 'convert': lambda x: x[6],
                 'unit': 'mm'},
                {'name': 'Distance: 90th percentile',
                 'convert': lambda x: x[7],
                 'unit': 'mm'},
                {'name': 'Distance: most frequent value',
                 'convert': lambda x: x[8],
                 'unit': 'mm'},
                {'name': 'Number of valid samples',
                 'convert': lambda x: x[9]},
                {'name': 'Total acquisition time',
                 'convert': lambda x: x[10] / 1.024,
                 'unit': 'ms'},
                {'name': 'Number of total samples',
                 'convert': lambda x: x[11]}]},
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
        b'023ec50003067a06360686067c0636067c068606860686000a0053000a0be6',
        b'023ec500020be6',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl, hex=True))
        print("")
