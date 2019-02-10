#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# https://www.decentlab.com/support


import logging
import os
import struct
from base64 import binascii
import json


SENSORS = [
    {'length': 2,
     'values': [{'name': 'Pressure',
                 'convert': lambda x: (x[0] - 16384) / 32768 * ((10.0) - (-1.0)) + (-1.0),
                 'unit': 'bar'},
                {'name': 'Temperature',
                 'convert': lambda x: (x[1] - 384) * 0.003125 - 50,
                 'unit': '°C'}]},
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

    import pprint
    payloads = [
        '02016700034e8060170c7f',
        '02016700020c7f',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl))
        print("")
