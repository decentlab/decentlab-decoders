<?php
/* https://www.decentlab.com/products/air-quality-station-no2-no-co-ox-for-lorawan */

abstract class DecentlabDecoder
{
    const PROTOCOL_VERSION = 2;

    private static $SENSORS;

    public function decode($payload = '')
    {
        $this->bytes = hex2bin($payload);
        $this->parts = [];

        $this->parts['version'] = ord($this->bytes[0]);
        if ($this->parts['version'] != self::PROTOCOL_VERSION) {
            $this->parts['error'] = sprintf('protocol version %u doesn\'t match v2', $this->parts[version]);
            return;
        }

        $this->parts['device_id'] = unpack('n', $this->bytes, 1)[1];
        $flags = unpack('n', $this->bytes, 3)[1];

        // decode payload
        $k = 5;
        foreach ($this->SENSORS as $sensor) {
            if (($flags & 1) == 1) {
                $x = [];
                // convert data to 16-bit integer array
                for ($j = 0; $j < $sensor['length']; $j++) {
                    array_push($x, unpack('n', $this->bytes, $k)[1]);
                    $k += 2;
                }

                // decode sensor values
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
    }
}

class DL_AC_Decoder extends DecentlabDecoder {
    /* device-specific parameters */
    public function __construct($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0)
    {
        $this->SENSORS = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'air_temperature',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return 175.72 * $x[0] / 65536 - 46.85; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'air_humidity',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return 125 * $x[1] / 65536 - 6; },
                        'unit' => '%',
                    ],
                ],
            ],
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'ch4_no2_we',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return 3 * ($x[0] / 32768 - 1) * 1000; },
                        'unit' => 'mV',
                    ],
                    [
                        'name' => 'ch4_no2_we_aux',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return 3 * ($x[1] / 32768 - 1) * 1000; },
                        'unit' => 'mV',
                    ],
                    [
                        'name' => 'ch4_no2_concentration_we',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return (3 * ($x[0] / 32768 - 1) * 1000 - $NO2_WE_0) / $NO2_S; },
                        'unit' => 'ppb',
                    ],
                    [
                        'name' => 'ch4_no2_concentration_we_aux',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return (3 * ($x[1] / 32768 - 1) * 1000 - $NO2_WE_0 + $NO2_AUX_0) / $NO2_S; },
                        'unit' => 'ppb',
                    ],
                ],
            ],
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'ch5_no_we',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return 3 * ($x[0] / 32768 - 1) * 1000; },
                        'unit' => 'mV',
                    ],
                    [
                        'name' => 'ch5_no_we_aux',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return 3 * ($x[1] / 32768 - 1) * 1000; },
                        'unit' => 'mV',
                    ],
                    [
                        'name' => 'ch5_no_concentration_we',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return (3 * ($x[0] / 32768 - 1) * 1000 - $NO_WE_0) / $NO_S; },
                        'unit' => 'ppb',
                    ],
                    [
                        'name' => 'ch5_no_concentration_we_aux',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return (3 * ($x[1] / 32768 - 1) * 1000 - $NO_WE_0 + $NO_AUX_0) / $NO_S; },
                        'unit' => 'ppb',
                    ],
                ],
            ],
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'ch6_ox_we',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return 3 * ($x[0] / 32768 - 1) * 1000; },
                        'unit' => 'mV',
                    ],
                    [
                        'name' => 'ch6_ox_we_aux',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return 3 * ($x[1] / 32768 - 1) * 1000; },
                        'unit' => 'mV',
                    ],
                    [
                        'name' => 'ch6_ox_concentration_we',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return (3 * ($x[0] / 32768 - 1) * 1000 - $Ox_WE_0) / $Ox_S; },
                        'unit' => 'ppb',
                    ],
                    [
                        'name' => 'ch6_ox_concentration_we_aux',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return (3 * ($x[1] / 32768 - 1) * 1000 - $Ox_WE_0 + $Ox_AUX_0) / $Ox_S; },
                        'unit' => 'ppb',
                    ],
                ],
            ],
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'ch7_co_we',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return 3 * ($x[0] / 32768 - 1) * 1000; },
                        'unit' => 'mV',
                    ],
                    [
                        'name' => 'ch7_co_we_aux',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return 3 * ($x[1] / 32768 - 1) * 1000; },
                        'unit' => 'mV',
                    ],
                    [
                        'name' => 'ch7_co_concentration_we',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return (3 * ($x[0] / 32768 - 1) * 1000 - $CO_WE_0) / $CO_S; },
                        'unit' => 'ppb',
                    ],
                    [
                        'name' => 'ch7_co_concentration_we_aux',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return (3 * ($x[1] / 32768 - 1) * 1000 - $CO_WE_0 + $CO_AUX_0) / $CO_S; },
                        'unit' => 'ppb',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'battery_voltage',
                        'convert' => function ($x) use ($NO2_WE_0, $NO2_S, $NO2_AUX_0, $NO_WE_0, $NO_S, $NO_AUX_0, $Ox_WE_0, $Ox_S, $Ox_AUX_0, $CO_WE_0, $CO_S, $CO_AUX_0) { return $x[0] / 1000; },
                        'unit' => 'V',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_AC_Decoder(256, 0.256, 227, 320, 0.512, 288, 235, 0.345, 200, 544, 0.424, 301);
$payloads = [
    '020fa0003f66b49b8c8966803c8cf580238a68804c903783f4158a',
    '020fa00020158a',
];

foreach($payloads as $payload) {
    $decoder->decode($payload);
    var_dump($decoder->parts);
}
