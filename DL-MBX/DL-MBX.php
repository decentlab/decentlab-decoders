<?php
/* https://www.decentlab.com/products/ultrasonic-distance-/-level-sensor-for-lorawan */

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

class DL_MBX_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->SENSORS = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'distance',
                        'convert' => function ($x) { return $x[0]; },
                        'unit' => 'mm',
                    ],
                    [
                        'name' => 'number_of_valid_samples',
                        'convert' => function ($x) { return $x[1]; },
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


$decoder = new DL_MBX_Decoder();
$payloads = [
    '02012f000304d200010bb1',
    '02012f00020bb1',
];

foreach($payloads as $payload) {
    $decoder->decode($payload);
    var_dump($decoder->parts);
}
