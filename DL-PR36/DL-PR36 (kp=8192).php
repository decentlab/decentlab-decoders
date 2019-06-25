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

class DL_PR36_Decoder extends DecentlabDecoder {
    /* device-specific parameters */
    public function __construct($kp)
    {
        $this->SENSORS = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'pressure',
                        'convert' => function ($x) use ($kp) { return ($x[0] - 32768) / $kp; },
                        'unit' => 'bar',
                    ],
                    [
                        'name' => 'temperature',
                        'convert' => function ($x) use ($kp) { return ($x[1] - 32768) / 256; },
                        'unit' => '°C',
                    ],
                ],
            ],
            [
                'length' => 1,
                'values' => [
                    [
                        'name' => 'battery_voltage',
                        'convert' => function ($x) use ($kp) { return $x[0] / 1000; },
                        'unit' => 'V',
                    ],
                ],
            ],
        ];
    }
}


$decoder = new DL_PR36_Decoder(8192);
$payloads = [
    '02032b0003806797810c2b',
    '02032b00020c2b',
];

foreach($payloads as $payload) {
    $decoder->decode($payload);
    var_dump($decoder->parts);
}
