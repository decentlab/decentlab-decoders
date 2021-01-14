<?php
/* https://www.decentlab.com/products/particulate-matter-temperature-humidity-and-barometric-pressure-sensor-for-lorawan */

abstract class DecentlabDecoder
{
    const PROTOCOL_VERSION = 2;

    protected $sensors;

    public function decode($payload = '')
    {
        $this->bytes = hex2bin($payload);
        $this->parts = [];

        $this->parts['version'] = ord($this->bytes[0]);
        if ($this->parts['version'] != self::PROTOCOL_VERSION) {
            $this->parts['error'] = sprintf("protocol version %u doesn't match v2", $this->parts['version']);
            return $this->parts;
        }

        $this->parts['device_id'] = unpack('n', substr($this->bytes, 1))[1];
        $flags = unpack('n', substr($this->bytes, 3))[1];

        /* decode payload */
        $k = 5;
        foreach ($this->sensors as $sensor) {
            if (($flags & 1) == 1) {
                $x = [];
                /* convert data to 16-bit integer array */
                for ($j = 0; $j < $sensor['length']; $j++) {
                    array_push($x, unpack('n', substr($this->bytes, $k))[1]);
                    $k += 2;
                }

                /* decode sensor values */
                foreach ($sensor['values'] as $value) {
                    if ($value['convert'] != NULL) {
                        $this->parts[$value['name'] . '_value'] = $value['convert']($x);
                        if ($value['unit'] != NULL) {
                            $this->parts[$value['name'] . '_unit'] = $value['unit'];
                        }
                    }
                }
            }
            $flags >>= 1;
        }

        return $this->parts;
    }
}

class DL_PM_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
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
            [
                'length' => 10,
                'values' => [
                    [
                        'name' => 'pm1_0_mass_concentration',
                        'convert' => function ($x) { return $x[0] / 10; },
                        'unit' => 'µg⋅m⁻³',
                    ],
                    [
                        'name' => 'pm2_5_mass_concentration',
                        'convert' => function ($x) { return $x[1] / 10; },
                        'unit' => 'µg⋅m⁻³',
                    ],
                    [
                        'name' => 'pm4_mass_concentration',
                        'convert' => function ($x) { return $x[2] / 10; },
                        'unit' => 'µg⋅m⁻³',
                    ],
                    [
                        'name' => 'pm10_mass_concentration',
                        'convert' => function ($x) { return $x[3] / 10; },
                        'unit' => 'µg⋅m⁻³',
                    ],
                    [
                        'name' => 'typical_particle_size',
                        'convert' => function ($x) { return $x[4]; },
                        'unit' => 'nm',
                    ],
                    [
                        'name' => 'pm0_5_number_concentration',
                        'convert' => function ($x) { return $x[5] / 10; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'pm1_0_number_concentration',
                        'convert' => function ($x) { return $x[6] / 10; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'pm2_5_number_concentration',
                        'convert' => function ($x) { return $x[7] / 10; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'pm4_number_concentration',
                        'convert' => function ($x) { return $x[8] / 10; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'pm10_number_concentration',
                        'convert' => function ($x) { return $x[9] / 10; },
                        'unit' => NULL,
                    ],
                ],
            ],
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'air_temperature',
                        'convert' => function ($x) { return 175.72 * $x[0] / 65536 - 46.85; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'air_humidity',
                        'convert' => function ($x) { return 125 * $x[1] / 65536 - 6; },
                        'unit' => '%',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'barometric_pressure',
                        'convert' => function ($x) { return $x[0] * 2; },
                        'unit' => 'Pa',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_PM_Decoder();
$payloads = [
    '021b50000f0c25002500270027002701f50107012c012d012d012d67bd618dbd10',
    '021b50000d0c2567bd618dbd10',
    '021b5000010c25',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
