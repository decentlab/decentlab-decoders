<?php
/* https://www.decentlab.com/products/soil-moisture-and-temperature-profile-for-lorawan */

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

class DL_SMTP_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 16,
                'values' => [
                    [
                        'name' => 'soil_moisture_at_depth_0',
                        'convert' => function ($x) { return ($x[0] - 2500) / 500; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'soil_temperature_at_depth_0',
                        'convert' => function ($x) { return ($x[1] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'soil_moisture_at_depth_1',
                        'convert' => function ($x) { return ($x[2] - 2500) / 500; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'soil_temperature_at_depth_1',
                        'convert' => function ($x) { return ($x[3] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'soil_moisture_at_depth_2',
                        'convert' => function ($x) { return ($x[4] - 2500) / 500; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'soil_temperature_at_depth_2',
                        'convert' => function ($x) { return ($x[5] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'soil_moisture_at_depth_3',
                        'convert' => function ($x) { return ($x[6] - 2500) / 500; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'soil_temperature_at_depth_3',
                        'convert' => function ($x) { return ($x[7] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'soil_moisture_at_depth_4',
                        'convert' => function ($x) { return ($x[8] - 2500) / 500; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'soil_temperature_at_depth_4',
                        'convert' => function ($x) { return ($x[9] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'soil_moisture_at_depth_5',
                        'convert' => function ($x) { return ($x[10] - 2500) / 500; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'soil_temperature_at_depth_5',
                        'convert' => function ($x) { return ($x[11] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'soil_moisture_at_depth_6',
                        'convert' => function ($x) { return ($x[12] - 2500) / 500; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'soil_temperature_at_depth_6',
                        'convert' => function ($x) { return ($x[13] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'soil_moisture_at_depth_7',
                        'convert' => function ($x) { return ($x[14] - 2500) / 500; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'soil_temperature_at_depth_7',
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


$decoder = new DL_SMTP_Decoder();
$payloads = [
    '020b50000309018a8c09438a9809278a920b3c8aa50c9c8a8c11e08aa500000000000000000b3b',
    '020b5000020b3b',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
