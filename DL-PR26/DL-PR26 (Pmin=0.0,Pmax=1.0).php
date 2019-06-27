<?php
/* https://www.decentlab.com/products/pressure-/-liquid-level-and-temperature-sensor-for-lorawan */

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

class DL_PR26_Decoder extends DecentlabDecoder {
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


$decoder = new DL_PR26_Decoder(0.0, 1.0);
$payloads = [
    '02016700033e8060170c7f',
    '02016700020c7f',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
