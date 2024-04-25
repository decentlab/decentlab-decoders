<?php
/* https://www.decentlab.com/greenhouse-multi-monitor-for-lorawan */

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

class DL_GMM_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 7,
                'values' => [
                    [
                        'name' => 'photosynthetically_active_radiation',
                        'convert' => function ($x) { return ($x[0] - 32768) / 10; },
                        'unit' => 'µmol⋅m⁻²⋅s⁻¹',
                    ],
                    [
                        'name' => 'air_temperature',
                        'convert' => function ($x) { return ($x[1] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'air_humidity',
                        'convert' => function ($x) { return ($x[2] - 32768) / 10; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'co2_concentration',
                        'convert' => function ($x) { return ($x[3] - 32768) / 1; },
                        'unit' => 'ppm',
                    ],
                    [
                        'name' => 'atmospheric_pressure',
                        'convert' => function ($x) { return ($x[4] - 32768) / 100; },
                        'unit' => 'kPa',
                    ],
                    [
                        'name' => 'vapor_pressure_deficit',
                        'convert' => function ($x) { return ($x[5] - 32768) / 100; },
                        'unit' => 'kPa',
                    ],
                    [
                        'name' => 'dew_point',
                        'convert' => function ($x) { return ($x[6] - 32768) / 100; },
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


$decoder = new DL_GMM_Decoder();
$payloads = [
    '02532b00038726892081148297a57380cf81700bbc',
    '02528500020bbc',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
