<?php

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

class DL_BLG_Decoder extends DecentlabDecoder {
    
    public function __construct()
    {
        $this->sensors = [
            [
                'length' => 2,
                'values' => [
                    [
                        'name' => 'voltage_ratio',
                        'convert' => function ($x) { return (($x[0] + $x[1]*65536) / 8388608 - 1) / 2; },
                        'unit' => NULL,
                    ],
                    [
                        'name' => 'thermistor_resistance',
                        'convert' => function ($x) { return 1000 / ((($x[0] + $x[1]*65536) / 8388608 - 1) / 2) - 41000; },
                        'unit' => 'Ω',
                    ],
                    [
                        'name' => 'temperature',
                        'convert' => function ($x) { return (1 / (0.0008271111 + 0.000208802 * log(1000 / ((($x[0] + $x[1]*65536) / 8388608 - 1) / 2) - 41000) + 0.000000080592 * pow(log(1000 / ((($x[0] + $x[1]*65536) / 8388608 - 1) / 2) - 41000), 3) )) - 273.15; },
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


$decoder = new DL_BLG_Decoder();
$payloads = [
    '0230c50003a40c00810c60',
    '0230c500020c60',
];

foreach($payloads as $payload) {
    var_dump($decoder->decode($payload));
}
