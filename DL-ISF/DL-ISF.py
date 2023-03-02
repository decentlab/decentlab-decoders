# -*- coding: utf-8 -*-

# https://www.decentlab.com/products/sapflow-sensor-for-lorawan

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
     'values': [{'name': 'Sap flow',
                 'convert': lambda x: (x[0] * 16 - 50000) / 1000,
                 'unit': 'l⋅h⁻¹'},
                {'name': 'Heat velocity (outer)',
                 'convert': lambda x: (x[1] * 16 - 50000) / 1000,
                 'unit': 'cm⋅h⁻¹'},
                {'name': 'Heat velocity (inner)',
                 'convert': lambda x: (x[2] * 16 - 50000) / 1000,
                 'unit': 'cm⋅h⁻¹'},
                {'name': 'Alpha (outer)',
                 'convert': lambda x: (x[3] * 32 - 1000000) / 100000},
                {'name': 'Alpha (inner)',
                 'convert': lambda x: (x[4] * 32 - 1000000) / 100000},
                {'name': 'Beta (outer)',
                 'convert': lambda x: (x[5] * 32 - 1000000) / 100000},
                {'name': 'Beta (inner)',
                 'convert': lambda x: (x[6] * 32 - 1000000) / 100000},
                {'name': 'Tmax (outer)',
                 'convert': lambda x: (x[7] * 2) / 1000,
                 'unit': 's'},
                {'name': 'Tmax (inner)',
                 'convert': lambda x: (x[8] * 2) / 1000,
                 'unit': 's'},
                {'name': 'Temperature (outer)',
                 'convert': lambda x: (x[9] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Max voltage',
                 'convert': lambda x: (x[10] - 32768) / 1000,
                 'unit': 'V'},
                {'name': 'Min voltage',
                 'convert': lambda x: (x[11] - 32768) / 1000,
                 'unit': 'V'},
                {'name': 'Diagnostic',
                 'convert': lambda x: x[12] + x[13] * 65536},
                {'name': 'Upstream Tmax (outer)',
                 'convert': lambda x: (x[14] * 2) / 1000,
                 'unit': 's'},
                {'name': 'Upstream Tmax (inner)',
                 'convert': lambda x: (x[15] * 2) / 1000,
                 'unit': 's'}]},
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
        b'023d0100030c290bab0c3e79707a1d78437991490845997e4cacdeaa6e00000000457e415a0b59',
        b'023d0100020b59',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl, hex=True))
        print("")
