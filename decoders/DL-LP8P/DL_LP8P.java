
/* https://www.decentlab.com/products/co2-temperature-humidity-and-barometric-pressure-sensor-for-lorawan */

import java.io.InputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;

interface Conversion {
    double execute(double x[]);
}

class Sensor {
    public int length;
    public SensorValue values[];
    Sensor(int length, SensorValue values[]) {
        this.length = length;
        this.values = values;
    }
}

class SensorValue {
    public String name;
    public String unit;
    public Conversion convert;
    SensorValue(String name, String unit, Conversion convert) {
        this.name = name;
        this.unit = unit;
        this.convert = convert;
    }
}

class DecodedValue {
    public double value;
    public String unit;
    DecodedValue(double value, String unit) {
        this.value = value;
        this.unit = unit;
    }

    public String toString() {
        return this.value + (this.unit != null ? " [" + this.unit + "]" : "") ;
    }
}

class DecentlabDecoder {
    public static final int PROTOCOL_VERSION = 2;

    private static int readInt(InputStream is) throws IOException {
        return ((is.read() & 0xff) << 8) + (is.read() & 0xff);
    }

    public static Map<String, DecodedValue> decode(Sensor[] SENSORS, byte[] msg) throws IOException {
        return decode(SENSORS, new ByteArrayInputStream(msg));
    }

    public static Map<String, DecodedValue> decode(Sensor[] SENSORS, String msg) throws IOException {
        byte[] buf = new byte[msg.length() / 2];
        for (int i = 0, j = 0; i < msg.length(); i += 2, j++) {
            buf[j] = (byte) Integer.parseInt(msg.substring(i, i + 2), 16);
        }
        return decode(SENSORS, new ByteArrayInputStream(buf));
    }

    public static Map<String, DecodedValue> decode(Sensor[] SENSORS, InputStream msg) throws IOException {
        int version = msg.read();
        if (version != PROTOCOL_VERSION) {
            throw new IOException("protocol version " + version + " doesn't match v2");
        }

        Map<String, DecodedValue> result = new HashMap<String, DecodedValue>();
        result.put("Protocol version", new DecodedValue(version, null));

        int deviceId = readInt(msg);
        result.put("Device ID", new DecodedValue(deviceId, null));

        int flags = readInt(msg);
        for (Sensor sensor : SENSORS) {
            if ((flags & 1) == 1) {
                double[] x = new double[sensor.length];
                for (int i = 0; i < sensor.length; i++) {
                    x[i] = readInt(msg);
                }
                for (SensorValue val : sensor.values) {
                    if (val.convert != null) {
                        result.put(val.name, new DecodedValue(val.convert.execute(x), val.unit));
                    }
                }
            }
            flags >>= 1;
        }

        return result;
    }
}

class DL_LP8P_Definition {
    public static final Sensor SENSORS[] = new Sensor[] {
        new Sensor(2, new SensorValue[] {
            new SensorValue("Air temperature", "°C", new Conversion() {
                public double execute(double x[]) { return 175.72 * x[0] / 65536 - 46.85; }
            }),
            new SensorValue("Air humidity", "%", new Conversion() {
                public double execute(double x[]) { return 125 * x[1] / 65536 - 6; }
            })
        }),
        new Sensor(2, new SensorValue[] {
            new SensorValue("Barometer temperature", "°C", new Conversion() {
                public double execute(double x[]) { return (x[0] - 5000) / 100; }
            }),
            new SensorValue("Barometric pressure", "Pa", new Conversion() {
                public double execute(double x[]) { return x[1] * 2; }
            })
        }),
        new Sensor(8, new SensorValue[] {
            new SensorValue("CO2 concentration", "ppm", new Conversion() {
                public double execute(double x[]) { return x[0] - 32768; }
            }),
            new SensorValue("CO2 concentration LPF", "ppm", new Conversion() {
                public double execute(double x[]) { return x[1] - 32768; }
            }),
            new SensorValue("CO2 sensor temperature", "°C", new Conversion() {
                public double execute(double x[]) { return (x[2] - 32768) / 100; }
            }),
            new SensorValue("Capacitor voltage 1", "V", new Conversion() {
                public double execute(double x[]) { return x[3] / 1000; }
            }),
            new SensorValue("Capacitor voltage 2", "V", new Conversion() {
                public double execute(double x[]) { return x[4] / 1000; }
            }),
            new SensorValue("CO2 sensor status", null, new Conversion() {
                public double execute(double x[]) { return x[5]; }
            }),
            new SensorValue("Raw IR reading", null, new Conversion() {
                public double execute(double x[]) { return x[6]; }
            }),
            new SensorValue("Raw IR reading LPF", null, new Conversion() {
                public double execute(double x[]) { return x[7]; }
            })
        }),
        new Sensor(1, new SensorValue[] {
            new SensorValue("Battery voltage", "V", new Conversion() {
                public double execute(double x[]) { return x[0] / 1000; }
            })
        })
    };
}

public class DL_LP8P {
    public static void main(String[] args) {
        String[] payloads = new String[] {
            "020578000f67bd618d1cedbd1081d981f4895b0bd80bb50000959895390c25",
            "020578000b67bd618d1cedbd100c25",
            "02057800080c25"
        };

        for (String pl : payloads) {
            try {
                Map<String, DecodedValue> decoded = DecentlabDecoder.decode(DL_LP8P_Definition.SENSORS, pl);
                for (Map.Entry<String, DecodedValue> v : decoded.entrySet()) {
                    System.out.println(v.getKey() + ": " + v.getValue());
                }
            }
            catch(IOException e) {
                e.printStackTrace();
            }
            System.out.println();
        }
    }
}

