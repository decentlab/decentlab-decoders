# -*- coding: utf-8 -*-

# https://www.decentlab.com/products/eleven-parameter-weather-station-for-lorawan

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import struct
from math import log, floor
from base64 import binascii

PROTOCOL_VERSION = 2

SENSORS = [
    {'length': 17,
     'values': [{'name': 'Solar radiation',
                 'convert': lambda x: (x[0] - 32768) / 10,
                 'unit': 'W⋅m⁻²'},
                {'name': 'Precipitation',
                 'convert': lambda x: x[1] / 1000,
                 'unit': 'mm'},
                {'name': 'Lightning strike count',
                 'convert': lambda x: x[2] - 32768,
                 'unit': 'None'},
                {'name': 'Lightning average distance',
                 'convert': lambda x: x[3] - 32768,
                 'unit': 'km'},
                {'name': 'Wind speed',
                 'convert': lambda x: (x[4] - 32768) / 100,
                 'unit': 'm⋅s⁻¹'},
                {'name': 'Wind direction',
                 'convert': lambda x: (x[5] - 32768) / 10,
                 'unit': '°'},
                {'name': 'Maximum wind speed',
                 'convert': lambda x: (x[6] - 32768) / 100,
                 'unit': 'm⋅s⁻¹'},
                {'name': 'Air temperature',
                 'convert': lambda x: (x[7] - 32768) / 10,
                 'unit': '°C'},
                {'name': 'Vapor pressure',
                 'convert': lambda x: (x[8] - 32768) / 100,
                 'unit': 'kPa'},
                {'name': 'Barometric pressure',
                 'convert': lambda x: (x[9] - 32768) / 100,
                 'unit': 'kPa'},
                {'name': 'Relative humidity',
                 'convert': lambda x: (x[10] - 32768) / 10,
                 'unit': '%'},
                {'name': 'Internal temperature',
                 'convert': lambda x: (x[11] - 32768) / 10,
                 'unit': '°C'},
                {'name': 'Tilt angle, X orientation',
                 'convert': lambda x: (x[12] - 32768) / 10,
                 'unit': '°'},
                {'name': 'Tilt angle, Y orientation',
                 'convert': lambda x: (x[13] - 32768) / 10,
                 'unit': '°'},
                {'name': 'Precipitation electrical conductivity',
                 'convert': lambda x: x[14] - 32768,
                 'unit': 'µS⋅cm⁻¹'},
                {'name': 'Cumulative precipitation',
                 'convert': lambda x: (x[15] + x[16] * 65536) / 1000,
                 'unit': 'mm'}]},
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
        b'025ef80003805c000080008000803484b3803680e78086a60181d680ed81c9809f8000117000010adc',
        b'025ef800020adc',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl, hex=True))
        print("")
