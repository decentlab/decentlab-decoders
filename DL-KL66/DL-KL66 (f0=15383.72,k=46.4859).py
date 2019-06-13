# -*- coding: utf-8 -*-

# https://www.decentlab.com/products/strain-/-weight-sensor-for-lorawan

import struct
from base64 import binascii

PROTOCOL_VERSION = 2
# Device-specific parameters
PARAMETERS = {
  'f0': 15383.72,
  'k': 46.4859
}
SENSORS = [
    {'length': 3,
     'values': [{'name': 'Counter reading',
                 'convert': lambda x: x[0]},
                {'name': 'Measurement interval',
                 'convert': lambda x: x[1] / 32768},
                {'name': 'Frequency',
                 'convert': lambda x: x[0] / x[1] * 32768,
                 'unit': 'Hz'},
                {'name': 'Weight',
                 'convert': lambda x: (pow(x[0] / x[1] * 32768, 2) - pow(PARAMETERS['f0'], 2)) * PARAMETERS['k'] / 1000000,
                 'unit': 'g'},
                {'name': 'Elongation',
                 'convert': lambda x: (pow(x[0] / x[1] * 32768, 2) - pow(PARAMETERS['f0'], 2)) * PARAMETERS['k'] / 1000000 * (-1.5) / 1000 * 9.8067,
                 'unit': 'µm'},
                {'name': 'Strain',
                 'convert': lambda x: (pow(x[0] / x[1] * 32768, 2) - pow(PARAMETERS['f0'], 2)) * PARAMETERS['k'] / 1000000 * (-1.5) / 1000 * 9.8067 / 0.066,
                 'unit': 'µm⋅m⁻¹'}]},
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
        '0203d400033bf67fff3bf60c60',
        '0203d400020c60',
    ]
    for pl in payloads:
        pprint.pprint(decode(pl))
        print("")
