<?php
/* https://www.decentlab.com/products/analog-or-digital-sensor-device-for-lorawan */

abstract class DecentlabDecoder
{
    const PROTOCOL_VERSION = 2;

    protected $sensors;

    public function decode($payload = '')
    {
        $bytes = hex2bin($payload);
        $parts = [];

        $parts['version'] = ord($bytes[0]);
        if ($parts['version'] != self::PROTOCOL_VERSION) {
            $parts['error'] = sprintf("protocol version %u doesn't match v2", $parts['version']);
            return $parts;
        }

        $parts['device_id'] = unpack('n', substr($bytes, 1))[1];
        $flags = unpack('n', substr($bytes, 3))[1];

        /* decode payload */
        $k = 5;
        foreach ($this->sensors as $sensor) {
            if (($flags & 1) == 1) {
                $x = [];
                /* convert data to 16-bit integer array */
                for ($j = 0; $j < $sensor['length']; $j++) {
                    array_push($x, unpack('n', substr($bytes, $k))[1]);
                    $k += 2;
                }

                /* decode sensor values */
                foreach ($sensor['values'] as $value) {
                    if ($value['convert'] != NULL) {
                        $parts[$value['name'] . '_value'] = $value['convert']($x);
                        if ($value['unit'] != NULL) {
                            $parts[$value['name'] . '_unit'] = $value['unit'];
                        }
                    }
                }
            }
            $flags >>= 1;
        }

        return $parts;
    }
}

class DL_DLR2_008_Decoder extends DecentlabDecoder {
    /* device-specific parameters */
    public function __construct($R)
    {
        $this->sensors = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'temperature',
                        'convert' => function ($x) use ($R) { return -244.83 + 2.3419 * (((($x[0] + $x[1]*65536) / 8388608 - 1) / 2) * $R / (1 - ((($x[0] + $x[1]*65536) / 8388608 - 1) / 2))) + 0.0010664 * pow(((($x[0] + $x[1]*65536) / 8388608 - 1) / 2) * $R / (1 - ((($x[0] + $x[1]*65536) / 8388608 - 1) / 2)), 2); },
                        'unit' => '°C',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'battery_voltage',
                        'convert' => function ($x) use ($R) { return $x[0] / 1000; },
                        'unit' => 'V',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_DLR2_008_Decoder(2000);
$payloads = [
    '0207df000317de008d0c60',
    '0207df00020c60',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
