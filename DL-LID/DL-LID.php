<?php
/* https://www.decentlab.com/products/laser-distance-level-sensor-for-lorawan */

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

class DL_LID_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 11,
                'values' => [
                    [
                        'name' => 'distance_average',
                        'convert' => function ($x) { return $x[0]; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'distance_minimum',
                        'convert' => function ($x) { return $x[1]; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'distance_maximum',
                        'convert' => function ($x) { return $x[2]; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'distance_median',
                        'convert' => function ($x) { return $x[3]; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'distance_10th_percentile',
                        'convert' => function ($x) { return $x[4]; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'distance_25th_percentile',
                        'convert' => function ($x) { return $x[5]; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'distance_75th_percentile',
                        'convert' => function ($x) { return $x[6]; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'distance_90th_percentile',
                        'convert' => function ($x) { return $x[7]; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'distance_most_frequent_value',
                        'convert' => function ($x) { return $x[8]; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'number_of_samples',
                        'convert' => function ($x) { return $x[9]; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'total_acquisition_time',
                        'convert' => function ($x) { return $x[10] / 1.024; },
                        'unit' => 'ms',
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


$decoder = new DL_LID_Decoder();
$payloads = [
    '0211c90003119b117611bc119e118a119411a811a81194006401990abd',
    '0211c900020abd',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
