# -*- coding: utf-8 -*-

# https://www.decentlab.com/products/co2-temperature-humidity-and-barometric-pressure-sensor-for-lorawan

import struct
from base64 import binascii

PROTOCOL_VERSION = 2

SENSORS = [
    {'length': 2,
     'values': [{'name': 'Air temperature',
                 'convert': lambda x: 175.72 * x[0] / 65536 - 46.85,
                 'unit': '°C'},
                {'name': 'Air humidity',
                 'convert': lambda x: 125 * x[1] / 65536 - 6,
                 'unit': '%'}]},
    {'length': 2,
     'values': [{'name': 'Barometer temperature',
                 'convert': lambda x: (x[0] - 5000) / 100,
                 'unit': '°C'},
                {'name': 'Barometric pressure',
                 'convert': lambda x: x[1] * 2,
                 'unit': 'Pa'}]},
    {'length': 8,
     'values': [{'name': 'CO2 concentration',
                 'convert': lambda x: x[0] - 32768,
                 'unit': 'ppm'},
                {'name': 'CO2 concentration LPF',
                 'convert': lambda x: x[1] - 32768,
                 'unit': 'ppm'},
                {'name': 'CO2 sensor temperature',
                 'convert': lambda x: (x[2] - 32768) / 100,
                 'unit': '°C'},
                {'name': 'Capacitor voltage 1',
                 'convert': lambda x: x[3] / 1000,
                 'unit': 'V'},
                {'name': 'Capacitor voltage 2',
                 'convert': lambda x: x[4] / 1000,
                 'unit': 'V'},
                {'name': 'CO2 sensor status',
                 'convert': lambda x: x[5]},
                {'name': 'Raw IR reading',
                 'convert': lambda x: x[6]},
                {'name': 'Raw IR reading LPF',
                 'convert': lambda x: x[7]}]},
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
        '020578000f67bd618d1cedbd1081d981f4895b0bd80bb50000959895390c25',
        '020578000b67bd618d1cedbd100c25',
        '02057800080c25',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl))
        print("")
