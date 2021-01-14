<?php
/* https://www.decentlab.com/products/weighing-scale-for-lorawan */

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
