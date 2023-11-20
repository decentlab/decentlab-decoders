# -*- coding: utf-8 -*-

# https://www.decentlab.com/products/wind-speed-wind-direction-and-temperature-sensor-for-lorawan

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import struct
from math import log, floor
from base64 import binascii

PROTOCOL_VERSION = 2

SENSORS = [
    {'length': 8,
     'values': [{'name': 'Wind speed',
                 'convert': lambda x: (x[0] - 32768) / 100,
                 'unit': 'm⋅s⁻¹'},
                {'name': 'Wind direction',
                 'convert': lambda x: (x[1] - 32768) / 10,
                 'unit': '°'},
                {'name': 'Maximum wind speed',
                 'convert': lambda x: (x[2] - 32768) / 100,
                 'unit': 'm⋅s⁻¹'},
                {'name': 'Air temperature',
                 'convert': lambda x: (x[3] - 32768) / 10,
                 'unit': '°C'},
                {'name': 'X orientation angle',
                 'convert': lambda x: (x[4] - 32768) / 10,
                 'unit': '°'},
                {'name': 'Y orientation angle',
                 'convert': lambda x: (x[5] - 32768) / 10,
                 'unit': '°'},
                {'name': 'North wind speed',
                 'convert': lambda x: (x[6] - 32768) / 100,
                 'unit': 'm⋅s⁻¹'},
                {'name': 'East wind speed',
                 'convert': lambda x: (x[7] - 32768) / 100,
                 'unit': 'm⋅s⁻¹'}]},
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
        b'0208c900038009812b8014810880027fe8800880040bf5',
        b'0208c900020bf5',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl, hex=True))
        print("")
