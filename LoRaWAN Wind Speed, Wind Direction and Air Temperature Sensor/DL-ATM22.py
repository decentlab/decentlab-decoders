#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# https://www.decentlab.com/support


import logging
import os
import struct
from base64 import binascii
import json


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


def decode(msg):
    """msg: payload as one of hex string, list, or bytearray"""
    bytes_ = bytearray(binascii.a2b_hex(msg)
                       if isinstance(msg, str)
                        else msg)

    if bytes_[0] != 2:
        raise ValueError("protocol version {} doesn't match v2".format(bytes_[0]))

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
    payloads = [
        '0208c900038009812b8014810880027fe8800880040bf5',
        '0208c900020bf5',
    ]
    for pl in payloads:
        print(json.dumps(decode(pl), indent=True) + "\n")
