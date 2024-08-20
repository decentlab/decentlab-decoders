<?php
/* https://www.decentlab.com/products/analog-or-digital-sensor-device-for-lorawan */

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

class DL_DLR2_004_Decoder extends DecentlabDecoder {
    /* device-specific parameters */
    public function __construct($R)
    {
        $this->sensors = [
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'current',
                        'convert' => function ($x) use ($R) { return 3 * ($x[0] - 32768) / 32768 / 2 / $R * 1000; },
                        'unit' => 'mA',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'battery_voltage',
                        'convert' => function ($x) use ($R) { return $x[0] / 1000; },
                        'unit' => 'V',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_DLR2_004_Decoder(10.0);
$payloads = [
    '0208b200038bb80c60',
    '0208b200020c60',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
