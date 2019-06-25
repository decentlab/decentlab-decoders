<?php
/* https://www.decentlab.com/products/strain-/-weight-sensor-for-lorawan */

abstract class DecentlabDecoder
{
    const PROTOCOL_VERSION = 2;

    private static $SENSORS;

    public function decode($payload = '')
    {
        $this->bytes = hex2bin($payload);
        $this->parts = [];

        $this->parts['version'] = ord($this->bytes[0]);
        if ($this->parts['version'] != self::PROTOCOL_VERSION) {
            $this->parts['error'] = sprintf('protocol version %u doesn\'t match v2', $this->parts[version]);
            return;
        }

        $this->parts['device_id'] = unpack('n', $this->bytes, 1)[1];
        $flags = unpack('n', $this->bytes, 3)[1];

        // decode payload
        $k = 5;
        foreach ($this->SENSORS as $sensor) {
            if (($flags & 1) == 1) {
                $x = [];
                // convert data to 16-bit integer array
                for ($j = 0; $j < $sensor['length']; $j++) {
                    array_push($x, unpack('n', $this->bytes, $k)[1]);
                    $k += 2;
                }

                // decode sensor values
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
    }
}

class DL_KL66_Decoder extends DecentlabDecoder {
    /* device-specific parameters */
    public function __construct($f0, $k)
    {
        $this->SENSORS = [
            [
                'length' => 3,
                'values' => [
                    [
                        'name' => 'counter_reading',
                        'convert' => function ($x) use ($f0, $k) { return $x[0]; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'measurement_interval',
                        'convert' => function ($x) use ($f0, $k) { return $x[1] / 32768; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'frequency',
                        'convert' => function ($x) use ($f0, $k) { return $x[0] / $x[1] * 32768; },
                        'unit' => 'Hz',
                    ],
                    [
                        'name' => 'weight',
                        'convert' => function ($x) use ($f0, $k) { return (pow($x[0] / $x[1] * 32768, 2) - pow($f0, 2)) * $k / 1000000; },
                        'unit' => 'g',
                    ],
                    [
                        'name' => 'elongation',
                        'convert' => function ($x) use ($f0, $k) { return (pow($x[0] / $x[1] * 32768, 2) - pow($f0, 2)) * $k / 1000000 * (-1.5) / 1000 * 9.8067; },
                        'unit' => 'µm',
                    ],
                    [
                        'name' => 'strain',
                        'convert' => function ($x) use ($f0, $k) { return (pow($x[0] / $x[1] * 32768, 2) - pow($f0, 2)) * $k / 1000000 * (-1.5) / 1000 * 9.8067 / 0.066; },
                        'unit' => 'µm⋅m⁻¹',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'battery_voltage',
                        'convert' => function ($x) use ($f0, $k) { return $x[0] / 1000; },
                        'unit' => 'V',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_KL66_Decoder(15383.72, 46.4859);
$payloads = [
    '0203d400033bf67fff3bf60c60',
    '0203d400020c60',
];

foreach($payloads as $payload) {
    $decoder->decode($payload);
    var_dump($decoder->parts);
}
