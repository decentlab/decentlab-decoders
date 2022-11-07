
/* https://www.decentlab.com/products/indoor-ambiance-monitor-including-co2-tvoc-and-motion-sensor-for-lorawan */

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

class DL_IAM_Definition {
    public static final Sensor SENSORS[] = new Sensor[] {
        new Sensor(1, new SensorValue[] {
            new SensorValue("Battery voltage", "V", new Conversion() {
                public double execute(double x[]) { return x[0] / 1000; }
            })
        }),
        new Sensor(2, new SensorValue[] {
            new SensorValue("Air temperature", "°C", new Conversion() {
                public double execute(double x[]) { return 175 * x[0] / 65535 - 45; }
            }),
            new SensorValue("Air humidity", "%", new Conversion() {
                public double execute(double x[]) { return 100 * x[1] / 65535; }
            })
        }),
        new Sensor(1, new SensorValue[] {
            new SensorValue("Barometric pressure", "Pa", new Conversion() {
                public double execute(double x[]) { return x[0] * 2; }
            })
        }),
        new Sensor(2, new SensorValue[] {
            new SensorValue("Ambient light (visible + infrared)", null, new Conversion() {
                public double execute(double x[]) { return x[0]; }
            }),
            new SensorValue("Ambient light (infrared)", null, new Conversion() {
                public double execute(double x[]) { return x[1]; }
            }),
            new SensorValue("Illuminance", "lx", new Conversion() {
                public double execute(double x[]) { return Math.max(Math.max(1.0 * x[0] - 1.64 * x[1], 0.59 * x[0] - 0.86 * x[1]), 0) * 1.5504; }
            })
        }),
        new Sensor(3, new SensorValue[] {
            new SensorValue("CO2 concentration", "ppm", new Conversion() {
                public double execute(double x[]) { return x[0] - 32768; }
            }),
            new SensorValue("CO2 sensor status", null, new Conversion() {
                public double execute(double x[]) { return x[1]; }
            }),
            new SensorValue("Raw IR reading", null, new Conversion() {
                public double execute(double x[]) { return x[2]; }
            })
        }),
        new Sensor(1, new SensorValue[] {
            new SensorValue("Activity counter", null, new Conversion() {
                public double execute(double x[]) { return x[0]; }
            })
        }),
        new Sensor(1, new SensorValue[] {
            new SensorValue("Total VOC", "ppb", new Conversion() {
                public double execute(double x[]) { return x[0]; }
            })
        })
    };
}

public class DL_IAM {
    public static void main(String[] args) {
        String[] payloads = new String[] {
            "020bbd007f0b926a515d48bc4e0262006981c7000093d4000b0111",
            "020bbd006f0b926a515d48bc4e02620069000b0111",
            "020bbd00010b92"
        };

        for (String pl : payloads) {
            try {
                Map<String, DecodedValue> decoded = DecentlabDecoder.decode(DL_IAM_Definition.SENSORS, pl);
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

