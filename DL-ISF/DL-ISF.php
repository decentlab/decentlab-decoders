<?php
/* https://www.decentlab.com/products/sapflow-sensor-for-lorawan */

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

class DL_ISF_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 16,
                'values' => [
                    [
                        'name' => 'sap_flow',
                        'convert' => function ($x) { return ($x[0] * 16 - 50000) / 1000; },
                        'unit' => 'l⋅h⁻¹',
                    ],
                    [
                        'name' => 'heat_velocity_outer',
                        'convert' => function ($x) { return ($x[1] * 16 - 50000) / 1000; },
                        'unit' => 'cm⋅h⁻¹',
                    ],
                    [
                        'name' => 'heat_velocity_inner',
                        'convert' => function ($x) { return ($x[2] * 16 - 50000) / 1000; },
                        'unit' => 'cm⋅h⁻¹',
                    ],
                    [
                        'name' => 'alpha_outer',
                        'convert' => function ($x) { return ($x[3] * 32 - 1000000) / 100000; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'alpha_inner',
                        'convert' => function ($x) { return ($x[4] * 32 - 1000000) / 100000; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'beta_outer',
                        'convert' => function ($x) { return ($x[5] * 32 - 1000000) / 100000; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'beta_inner',
                        'convert' => function ($x) { return ($x[6] * 32 - 1000000) / 100000; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'tmax_outer',
                        'convert' => function ($x) { return ($x[7] * 2) / 1000; },
                        'unit' => 's',
                    ],
                    [
                        'name' => 'tmax_inner',
                        'convert' => function ($x) { return ($x[8] * 2) / 1000; },
                        'unit' => 's',
                    ],
                    [
                        'name' => 'temperature_outer',
                        'convert' => function ($x) { return ($x[9] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'max_voltage',
                        'convert' => function ($x) { return ($x[10] - 32768) / 1000; },
                        'unit' => 'V',
                    ],
                    [
                        'name' => 'min_voltage',
                        'convert' => function ($x) { return ($x[11] - 32768) / 1000; },
                        'unit' => 'V',
                    ],
                    [
                        'name' => 'diagnostic',
                        'convert' => function ($x) { return $x[12] + $x[13] * 65536; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'upstream_tmax_outer',
                        'convert' => function ($x) { return ($x[14] * 2) / 1000; },
                        'unit' => 's',
                    ],
                    [
                        'name' => 'upstream_tmax_inner',
                        'convert' => function ($x) { return ($x[15] * 2) / 1000; },
                        'unit' => 's',
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


$decoder = new DL_ISF_Decoder();
$payloads = [
    '023d0100030c290bab0c3e79707a1d78437991490845997e4cacdeaa6e00000000457e415a0b59',
    '023d0100020b59',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
