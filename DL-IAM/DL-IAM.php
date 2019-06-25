<?php
/* https://www.decentlab.com/products/indoor-ambiance-monitor-including-co2-tvoc-and-motion-sensor-for-lorawan */

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

class DL_IAM_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->SENSORS = [
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
                'length' => 1,
                'values' => [
                    [
                        'name' => 'barometric_pressure',
                        'convert' => function ($x) { return $x[0] * 2; },
                        'unit' => 'Pa',
                    ],
                ],
            ],
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'ambient_light_visible_infrared',
                        'convert' => function ($x) { return $x[0]; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'ambient_light_infrared',
                        'convert' => function ($x) { return $x[1]; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'illuminance',
                        'convert' => function ($x) { return max(max(1.0 * $x[0] - 1.64 * $x[1], 0.59 * $x[0] - 0.86 * $x[1]), 0) * 1.5504; },
                        'unit' => 'lx',
                    ],
                ],
            ],
            [
                'length' => 3,
                'values' => [
                    [
                        'name' => 'co2_concentration',
                        'convert' => function ($x) { return $x[0] - 32768; },
                        'unit' => 'ppm',
                    ],
                    [
                        'name' => 'co2_sensor_status',
                        'convert' => function ($x) { return $x[1]; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'raw_ir_reading',
                        'convert' => function ($x) { return $x[2]; },
                        'unit' => NULL,
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'activity_counter',
                        'convert' => function ($x) { return $x[0]; },
                        'unit' => NULL,
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'total_voc',
                        'convert' => function ($x) { return $x[0]; },
                        'unit' => 'ppb',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_IAM_Decoder();
$payloads = [
    '020bbd007f0b926a515d48bc4e0262006981c7000093d4000b0111',
    '020bbd006f0b926a515d48bc4e02620069000b0111',
    '020bbd00010b92',
];

foreach($payloads as $payload) {
    $decoder->decode($payload);
    var_dump($decoder->parts);
}
