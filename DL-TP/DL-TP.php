<?php
/* https://www.decentlab.com/products/temperature-profile-for-lorawan */

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

class DL_TP_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 16,
                'values' => [
                    [
                        'name' => 'temperature_at_level_0',
                        'convert' => function ($x) { return ($x[0] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_1',
                        'convert' => function ($x) { return ($x[1] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_2',
                        'convert' => function ($x) { return ($x[2] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_3',
                        'convert' => function ($x) { return ($x[3] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_4',
                        'convert' => function ($x) { return ($x[4] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_5',
                        'convert' => function ($x) { return ($x[5] - 32768) / 100; },
                        'unit' => '°C',
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
                        'name' => 'temperature_at_level_12',
                        'convert' => function ($x) { return ($x[12] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_13',
                        'convert' => function ($x) { return ($x[13] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_14',
                        'convert' => function ($x) { return ($x[14] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'temperature_at_level_15',
                        'convert' => function ($x) { return ($x[15] - 32768) / 100; },
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


$decoder = new DL_TP_Decoder();
$payloads = [
    '023e3e00038abc8a928aa08a848ab38a898ac38aad8ab78a928aa1000000000000000000000afc',
    '023e3e00020afc',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
