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
    {'length': 2,
     'values': [{'name': 'Voltage ratio',
                 'convert': lambda x: ((x[0] + x[1]*65536) / 8388608 - 1) / 2},
                {'name': 'Thermistor resistance',
                 'convert': lambda x: 1000 / (((x[0] + x[1]*65536) / 8388608 - 1) / 2) - 41000,
                 'unit': 'Ω'},
                {'name': 'Temperature',
                 'convert': lambda x: (1 / (0.0008271111 + 0.000208802 * log(1000 / (((x[0] + x[1]*65536) / 8388608 - 1) / 2) - 41000) + 0.000000080592 * pow(log(1000 / (((x[0] + x[1]*65536) / 8388608 - 1) / 2) - 41000), 3) )) - 273.15,
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
        b'0230c50003a40c00810c60',
        b'0230c500020c60',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl, hex=True))
        print("")
