<?php
/* https://www.decentlab.com/products/tipping-bucket-rain-gauge-for-lorawan */

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

class DL_TBRG_Decoder extends DecentlabDecoder {
    /* device-specific parameters */
    public function __construct($resolution)
    {
        $this->sensors = [
            [
                'length' => 4,
                'values' => [
                    [
                        'name' => 'precipitation',
                        'convert' => function ($x) use ($resolution) { return $x[0] * $resolution; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'precipitation_interval',
                        'convert' => function ($x) use ($resolution) { return $x[1]; },
                        'unit' => 's',
                    ],
                    [
                        'name' => 'cumulative_precipitation',
                        'convert' => function ($x) use ($resolution) { return ($x[2] + $x[3] * 65536) * $resolution; },
                        'unit' => 'mm',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'battery_voltage',
                        'convert' => function ($x) use ($resolution) { return $x[0] / 1000; },
                        'unit' => 'V',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_TBRG_Decoder(0.1);
$payloads = [
    '0202f8000300040258409a00000c54',
    '0202f800020c54',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
