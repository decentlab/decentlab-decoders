# -*- coding: utf-8 -*-

# https://www.decentlab.com/products/indoor-ambiance-monitor-including-co2-tvoc-and-motion-sensor-for-lorawan

import struct
from base64 import binascii

PROTOCOL_VERSION = 2

SENSORS = [
    {'length': 1,
     'values': [{'name': 'Battery voltage',
                 'convert': lambda x: x[0] / 1000,
                 'unit': 'V'}]},
    {'length': 2,
     'values': [{'name': 'Air temperature',
                 'convert': lambda x: 175 * x[0] / 65535 - 45,
                 'unit': '°C'},
                {'name': 'Air humidity',
                 'convert': lambda x: 100 * x[1] / 65535,
                 'unit': '%'}]},
    {'length': 1,
     'values': [{'name': 'Barometric pressure',
                 'convert': lambda x: x[0] * 2,
                 'unit': 'Pa'}]},
    {'length': 2,
     'values': [{'name': 'Ambient light (visible + infrared)',
                 'convert': lambda x: x[0]},
                {'name': 'Ambient light (infrared)',
                 'convert': lambda x: x[1]},
                {'name': 'Illuminance',
                 'convert': lambda x: max(max(1.0 * x[0] - 1.64 * x[1], 0.59 * x[0] - 0.86 * x[1]), 0) * 1.5504,
                 'unit': 'lx'}]},
    {'length': 3,
     'values': [{'name': 'CO2 concentration',
                 'convert': lambda x: x[0] - 32768,
                 'unit': 'ppm'},
                {'name': 'CO2 sensor status',
                 'convert': lambda x: x[1]},
                {'name': 'Raw IR reading',
                 'convert': lambda x: x[2]}]},
    {'length': 1,
     'values': [{'name': 'Activity counter',
                 'convert': lambda x: x[0]}]},
    {'length': 1,
     'values': [{'name': 'Total VOC',
                 'convert': lambda x: x[0],
                 'unit': 'ppb'}]}
]


def decode(msg):
    """msg: payload as one of hex string, list, or bytearray"""
    bytes_ = bytearray(binascii.a2b_hex(msg)
                       if isinstance(msg, str)
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
        '020bbd007f0b926a515d48bc4e0262006981c7000093d4000b0111',
        '020bbd006f0b926a515d48bc4e02620069000b0111',
        '020bbd00010b92',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl))
        print("")
