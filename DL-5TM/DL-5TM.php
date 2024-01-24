<?php
/* https://www.decentlab.com/products/legacy-soil-moisture-and-temperature-sensor-for-lorawan */

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

class DL_5TM_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'dielectric_permittivity',
                        'convert' => function ($x) { return $x[0] / 50; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'volumetric_water_content',
                        'convert' => function ($x) { return 0.0000043 * pow($x[0]/50, 3) - 0.00055 * pow($x[0]/50, 2) + 0.0292 * ($x[0]/50) - 0.053; },
                        'unit' => 'm³⋅m⁻³',
                    ],
                    [
                        'name' => 'soil_temperature',
                        'convert' => function ($x) { return ($x[1] - 400) / 10; },
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


$decoder = new DL_5TM_Decoder();
$payloads = [
    '02023b0003003702710c60',
    '02023b00020c60',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
