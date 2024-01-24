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

class DL_MES5_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 5,
                'values' => [
                    [
                        'name' => 'status',
                        'convert' => function ($x) { return $x[0]; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'temperature',
                        'convert' => function ($x) { return ($x[1] - 32768) / 100; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'sludge_blanket',
                        'convert' => function ($x) { return $x[2] / 100; },
                        'unit' => '%',
                    ],
                    [
                        'name' => 'suspended_solid',
                        'convert' => function ($x) { return $x[3] / 100; },
                        'unit' => 'g⋅L⁻¹',
                    ],
                    [
                        'name' => 'turbidity',
                        'convert' => function ($x) { return $x[4] / 10; },
                        'unit' => 'FAU',
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


$decoder = new DL_MES5_Decoder();
$payloads = [
    '024E030003000088e6210800f223650af5',
    '024E0300020af5',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
