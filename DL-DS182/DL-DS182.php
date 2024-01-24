<?php

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

class DL_DS182_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'ch0_temperature',
                        'convert' => function ($x) { return ($x[0] - 32768) / 16; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'ch0_id',
                        'convert' => function ($x) { return $x[1]; },
                        'unit' => NULL,
                    ],
                ],
            ],
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'ch1_temperature',
                        'convert' => function ($x) { return ($x[0] - 32768) / 16; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'ch1_id',
                        'convert' => function ($x) { return $x[1]; },
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


$decoder = new DL_DS182_Decoder();
$payloads = [
    '023ca4000781775166817359990c60',
    '023ca400040c60',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
