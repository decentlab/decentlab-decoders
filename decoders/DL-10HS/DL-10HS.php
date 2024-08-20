<?php
/* https://www.decentlab.com/support */

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

class DL_10HS_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'raw_sensor_reading',
                        'convert' => function ($x) { return 3 * ($x[0] - 32768) / 32768 * 1000; },
                        'unit' => 'mV',
                    ],
                    [
                        'name' => 'volumetric_water_content',
                        'convert' => function ($x) { return 2.97*pow(10, -9) * pow(3000*($x[0]-32768)/32768, 3) - 7.37*pow(10, -6) * pow(3000*($x[0]-32768)/32768, 2) + 6.69*pow(10, -3) * (3000*($x[0]-32768)/32768) - 1.92; },
                        'unit' => 'm³⋅m⁻³',
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


$decoder = new DL_10HS_Decoder();
$payloads = [
    '0202df000393710c60',
    '0202df00020c60',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
