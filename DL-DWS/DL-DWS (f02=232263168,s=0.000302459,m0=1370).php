<?php
/* https://www.decentlab.com/products/weighing-scale-for-lorawan */

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

class DL_DWS_Decoder extends DecentlabDecoder {
    /* device-specific parameters */
    public function __construct($f02, $s, $m0)
    {
        $this->sensors = [
            [
                'length' => 3,
                'values' => [
                    [
                        'name' => 'frequency',
                        'convert' => function ($x) use ($f02, $s, $m0) { return $x[0] / $x[1] * 32768; },
                        'unit' => 'Hz',
                    ],
                    [
                        'name' => 'weight',
                        'convert' => function ($x) use ($f02, $s, $m0) { return (pow($x[0] / $x[1] * 32768, 2) - $f02) * $s + $m0; },
                        'unit' => 'g',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'battery_voltage',
                        'convert' => function ($x) use ($f02, $s, $m0) { return $x[0] / 1000; },
                        'unit' => 'V',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_DWS_Decoder(232263168, 0.000302459, 1370);
$payloads = [
    '0203d400033bf67fff3bf60c60',
    '0203d400020c60',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
