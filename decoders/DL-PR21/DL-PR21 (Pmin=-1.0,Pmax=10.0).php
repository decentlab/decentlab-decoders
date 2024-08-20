<?php
/* https://www.decentlab.com/products/pressure-/-liquid-level-and-temperature-sensor-with-g1/4-pipe-thread-for-lorawan */

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

class DL_PR21_Decoder extends DecentlabDecoder {
    /* device-specific parameters */
    public function __construct($Pmin, $Pmax)
    {
        $this->sensors = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'pressure',
                        'convert' => function ($x) use ($Pmin, $Pmax) { return ($x[0] - 16384) / 32768 * ($Pmax - $Pmin) + $Pmin; },
                        'unit' => 'bar',
                    ],
                    [
                        'name' => 'temperature',
                        'convert' => function ($x) use ($Pmin, $Pmax) { return ($x[1] - 384) * 0.003125 - 50; },
                        'unit' => '°C',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'battery_voltage',
                        'convert' => function ($x) use ($Pmin, $Pmax) { return $x[0] / 1000; },
                        'unit' => 'V',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_PR21_Decoder(-1.0, 10.0);
$payloads = [
    '02016700034e8060170c7f',
    '02016700020c7f',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
