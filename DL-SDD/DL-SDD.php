<?php
/* https://www.decentlab.com/soil-moisture-temperature-and-salinity-profile */

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

class DL_SDD_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 18,
                'values' => [
                    [
                        'name' => 'moisture_at_level_0',
                        'convert' => function ($x) { return ($x[0] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'moisture_at_level_1',
                        'convert' => function ($x) { return ($x[1] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'moisture_at_level_2',
                        'convert' => function ($x) { return ($x[2] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'moisture_at_level_3',
                        'convert' => function ($x) { return ($x[3] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'moisture_at_level_4',
                        'convert' => function ($x) { return ($x[4] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'moisture_at_level_5',
                        'convert' => function ($x) { return ($x[5] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'temperature_at_level_0',
                        'convert' => function ($x) { return ($x[6] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_1',
                        'convert' => function ($x) { return ($x[7] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_2',
                        'convert' => function ($x) { return ($x[8] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_3',
                        'convert' => function ($x) { return ($x[9] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_4',
                        'convert' => function ($x) { return ($x[10] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_5',
                        'convert' => function ($x) { return ($x[11] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'salinity_at_level_0',
                        'convert' => function ($x) { return $x[12] - 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'salinity_at_level_1',
                        'convert' => function ($x) { return $x[13] - 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'salinity_at_level_2',
                        'convert' => function ($x) { return $x[14] - 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'salinity_at_level_3',
                        'convert' => function ($x) { return $x[15] - 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'salinity_at_level_4',
                        'convert' => function ($x) { return $x[16] - 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'salinity_at_level_5',
                        'convert' => function ($x) { return $x[17] - 100; },
                        'unit' => NULL,
                    ],
                ],
            ],
            [
                'length' => 18,
                'values' => [
                    [
                        'name' => 'moisture_at_level_6',
                        'convert' => function ($x) { return ($x[0] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'moisture_at_level_7',
                        'convert' => function ($x) { return ($x[1] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'moisture_at_level_8',
                        'convert' => function ($x) { return ($x[2] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'moisture_at_level_9',
                        'convert' => function ($x) { return ($x[3] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'moisture_at_level_10',
                        'convert' => function ($x) { return ($x[4] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'moisture_at_level_11',
                        'convert' => function ($x) { return ($x[5] - 32768) / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'temperature_at_level_6',
                        'convert' => function ($x) { return ($x[6] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_7',
                        'convert' => function ($x) { return ($x[7] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_8',
                        'convert' => function ($x) { return ($x[8] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_9',
                        'convert' => function ($x) { return ($x[9] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_10',
                        'convert' => function ($x) { return ($x[10] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_11',
                        'convert' => function ($x) { return ($x[11] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'salinity_at_level_6',
                        'convert' => function ($x) { return $x[12] - 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'salinity_at_level_7',
                        'convert' => function ($x) { return $x[13] - 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'salinity_at_level_8',
                        'convert' => function ($x) { return $x[14] - 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'salinity_at_level_9',
                        'convert' => function ($x) { return $x[15] - 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'salinity_at_level_10',
                        'convert' => function ($x) { return $x[16] - 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'salinity_at_level_11',
                        'convert' => function ($x) { return $x[17] - 100; },
                        'unit' => NULL,
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


$decoder = new DL_SDD_Decoder();
$payloads = [
    '0243e300058000800080008000800080008741877b8749876c876c876600000000000000000000014a09e3',
    '0243e3000409e3',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
