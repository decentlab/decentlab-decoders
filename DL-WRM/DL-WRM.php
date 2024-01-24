<?php
/* https://www.decentlab.com/products/winter-road-maintenance-sensor-for-lorawan */

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

class DL_WRM_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'air_temperature',
                        'convert' => function ($x) { return 175 * $x[0] / 65535 - 45; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'air_humidity',
                        'convert' => function ($x) { return 100 * $x[1] / 65535; },
                        'unit' => '%',
                    ],
                ],
            ],
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'surface_temperature',
                        'convert' => function ($x) { return ($x[0] - 1000) / 10; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'head_temperature',
                        'convert' => function ($x) { return ($x[1] - 1000) / 10; },
                        'unit' => '°C',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'battery_voltage',
                        'convert' => function ($x) { return $x[0] / 1000; },
                        'unit' => 'V',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_WRM_Decoder();
$payloads = [
    '021a10000764a079b104f904c40c60',
    '021a1000040c60',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
