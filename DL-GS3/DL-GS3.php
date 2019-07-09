<?php
/* https://www.decentlab.com/support */

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

class DL_GS3_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 3,
                'values' => [
                    [
                        'name' => 'dielectric_permittivity',
                        'convert' => function ($x) { return $x[0] / 100; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'volumetric_water_content',
                        'convert' => function ($x) { return 0.00000589 * pow($x[0]/100, 3) - 0.000762 * pow($x[0]/100, 2) + 0.0367 * ($x[0]/100) - 0.0753; },
                        'unit' => 'm³⋅m⁻³',
                    ],
                    [
                        'name' => 'soil_temperature',
                        'convert' => function ($x) { return ($x[1] - 32768) / 10; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'electrical_conductivity',
                        'convert' => function ($x) { return $x[2]; },
                        'unit' => 'µS⋅cm⁻¹',
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


$decoder = new DL_GS3_Decoder();
$payloads = [
    '0203630003009980e100010c60',
    '02036300020c60',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
