<?php
/* https://www.decentlab.com/support */

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

class DL_SHT21_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->SENSORS = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'air_temperature',
                        'convert' => function ($x) { return 175.72 * $x[0] / 65536 - 46.85; },
                        'unit' => '°C',
                    ],
                    [
                        'name' => 'air_humidity',
                        'convert' => function ($x) { return 125 * $x[1] / 65536 - 6; },
                        'unit' => '%',
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


$decoder = new DL_SHT21_Decoder();
$payloads = [
    '02030e000364a079b10c60',
    '02030e00020c60',
];

foreach($payloads as $payload) {
    $decoder->decode($payload);
    var_dump($decoder->parts);
}
