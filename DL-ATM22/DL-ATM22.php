<?php
/* https://www.decentlab.com/products/wind-speed-wind-direction-and-temperature-sensor-for-lorawan */

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

class DL_ATM22_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 8,
                'values' => [
                    [
                        'name' => 'wind_speed',
                        'convert' => function ($x) { return ($x[0] - 32768) / 100; },
                        'unit' => 'm⋅s⁻¹',
                    ],
                    [
                        'name' => 'wind_direction',
                        'convert' => function ($x) { return ($x[1] - 32768) / 10; },
                        'unit' => '°',
                    ],
                    [
                        'name' => 'maximum_wind_speed',
                        'convert' => function ($x) { return ($x[2] - 32768) / 100; },
                        'unit' => 'm⋅s⁻¹',
                    ],
                    [
                        'name' => 'air_temperature',
                        'convert' => function ($x) { return ($x[3] - 32768) / 10; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'x_orientation_angle',
                        'convert' => function ($x) { return ($x[4] - 32768) / 10; },
                        'unit' => '°',
                    ],
                    [
                        'name' => 'y_orientation_angle',
                        'convert' => function ($x) { return ($x[5] - 32768) / 10; },
                        'unit' => '°',
                    ],
                    [
                        'name' => 'north_wind_speed',
                        'convert' => function ($x) { return ($x[6] - 32768) / 100; },
                        'unit' => 'm⋅s⁻¹',
                    ],
                    [
                        'name' => 'east_wind_speed',
                        'convert' => function ($x) { return ($x[7] - 32768) / 100; },
                        'unit' => 'm⋅s⁻¹',
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


$decoder = new DL_ATM22_Decoder();
$payloads = [
    '0208c900038009812b8014810880027fe8800880040bf5',
    '0208c900020bf5',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
