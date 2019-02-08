#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import logging
import os
import struct
from base64 import binascii
import json


SENSORS = [
    {'length': 2,
     'values': [{'name': 'Distance',
                 'convert': lambda x: x[0],
                 'unit': 'mm'},
                {'name': 'Number of valid samples',
                 'convert': lambda x: x[1]}]},
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
    result = {}
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
        '02012f000304d200010bb1',
        '02012f00020bb1',
    ]
    for pl in payloads:
        print(json.dumps(decode(pl), indent=True) + "\n")
