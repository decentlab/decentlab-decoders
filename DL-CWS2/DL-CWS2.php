<?php
/* https://www.decentlab.com/products/high-precision-winter-road-maintenance-sensor-with-radiation-shield-for-lorawan */

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

class DL_CWS2_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'air_temperature_radiation_shield',
                        'convert' => function ($x) { return 175 * $x[0] / 65535 - 45; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'air_humidity_radiation_shield',
                        'convert' => function ($x) { return 100 * $x[1] / 65535; },
                        'unit' => '%',
                    ],
                ],
            ],
            [
                'length' => 6,
                'values' => [
                    [
                        'name' => 'surface_temperature',
                        'convert' => function ($x) { return ($x[0] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'air_temperature',
                        'convert' => function ($x) { return ($x[1] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'air_humidity',
                        'convert' => function ($x) { return ($x[2] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'dew_point',
                        'convert' => function ($x) { return ($x[3] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'angle',
                        'convert' => function ($x) { return ($x[4] - 32768); },
                        'unit' => '°',
                    ],
                    [
                        'name' => 'sensor_temperature',
                        'convert' => function ($x) { return ($x[5] - 32768) / 100; },
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


$decoder = new DL_CWS2_Decoder();
$payloads = [
    '0258c000074676fa0a81c9813fa6d88137802581300b91',
    '0258c000040b91',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
