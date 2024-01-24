<?php
/* https://www.decentlab.com/products/high-precision-pressure-/-liquid-level-and-temperature-sensor-for-lorawan */

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

class DL_PR36_Decoder extends DecentlabDecoder {
    /* device-specific parameters */
    public function __construct($kp)
    {
        $this->sensors = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'pressure',
                        'convert' => function ($x) use ($kp) { return ($x[0] - 32768) / $kp; },
                        'unit' => 'bar',
                    ],
                    [
                        'name' => 'temperature',
                        'convert' => function ($x) use ($kp) { return ($x[1] - 32768) / 256; },
                        'unit' => '°C',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'battery_voltage',
                        'convert' => function ($x) use ($kp) { return $x[0] / 1000; },
                        'unit' => 'V',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_PR36_Decoder(8192);
$payloads = [
    '02032b0003806797810c2b',
    '02032b00020c2b',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
