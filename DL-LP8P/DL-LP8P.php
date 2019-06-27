<?php
/* https://www.decentlab.com/products/co2-temperature-humidity-and-barometric-pressure-sensor-for-lorawan */

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
            return;
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

class DL_LP8P_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
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
                'length' => 2,
                'values' => [
                    [
                        'name' => 'barometer_temperature',
                        'convert' => function ($x) { return ($x[0] - 5000) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'barometric_pressure',
                        'convert' => function ($x) { return $x[1] * 2; },
                        'unit' => 'Pa',
                    ],
                ],
            ],
            [
                'length' => 8,
                'values' => [
                    [
                        'name' => 'co2_concentration',
                        'convert' => function ($x) { return $x[0] - 32768; },
                        'unit' => 'ppm',
                    ],
                    [
                        'name' => 'co2_concentration_lpf',
                        'convert' => function ($x) { return $x[1] - 32768; },
                        'unit' => 'ppm',
                    ],
                    [
                        'name' => 'co2_sensor_temperature',
                        'convert' => function ($x) { return ($x[2] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'capacitor_voltage_1',
                        'convert' => function ($x) { return $x[3] / 1000; },
                        'unit' => 'V',
                    ],
                    [
                        'name' => 'capacitor_voltage_2',
                        'convert' => function ($x) { return $x[4] / 1000; },
                        'unit' => 'V',
                    ],
                    [
                        'name' => 'co2_sensor_status',
                        'convert' => function ($x) { return $x[5]; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'raw_ir_reading',
                        'convert' => function ($x) { return $x[6]; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'raw_ir_reading_lpf',
                        'convert' => function ($x) { return $x[7]; },
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


$decoder = new DL_LP8P_Decoder();
$payloads = [
    '020578000f67bd618d1cedbd1081d981f4895b0bd80bb50000959895390c25',
    '020578000b67bd618d1cedbd100c25',
    '02057800080c25',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
