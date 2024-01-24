<?php
/* https://www.decentlab.com/products/soil-moisture-temperature-and-electrical-conductivity-sensor-for-lorawan */

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

class DL_TRS11_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'dielectric_permittivity',
                        'convert' => function ($x) { return pow(0.000000002887 * pow($x[0]/10, 3) - 0.0000208 * pow($x[0]/10, 2) + 0.05276 * ($x[0]/10) - 43.39, 2); },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'volumetric_water_content',
                        'convert' => function ($x) { return $x[0]/10 * 0.0003879 - 0.6956; },
                        'unit' => 'm³⋅m⁻³',
                    ],
                    [
                        'name' => 'soil_temperature',
                        'convert' => function ($x) { return ($x[1] - 32768) / 10; },
                        'unit' => '°C',
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


$decoder = new DL_TRS11_Decoder();
$payloads = [
    '0210d50003463f810b0c79',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
