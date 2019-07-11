<?php
/* https://www.decentlab.com/products/pressure-/-liquid-level-temperature-and-electrical-conductivity-sensor-for-lorawan */

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

class DL_CTD10_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 4,
                'values' => [
                    [
                        'name' => 'water_depth',
                        'convert' => function ($x) { return $x[0] - 32768; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'temperature',
                        'convert' => function ($x) { return ($x[1] - 32768) / 10; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'electrical_conductivity',
                        'convert' => function ($x) { return $x[2]; },
                        'unit' => 'µS⋅cm⁻¹',
                    ],
                    [
                        'name' => 'freezing_flag',
                        'convert' => function ($x) { return $x[3]; },
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


$decoder = new DL_CTD10_Decoder();
$payloads = [
    '0207d9000390888081006400000c60',
    '0207d900020c60',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
