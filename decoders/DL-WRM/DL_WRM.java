
/* https://www.decentlab.com/products/winter-road-maintenance-sensor-for-lorawan */

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

class DL_WRM_Definition {
    public static final Sensor SENSORS[] = new Sensor[] {
        new Sensor(2, new SensorValue[] {
            new SensorValue("Air temperature", "°C", new Conversion() {
                public double execute(double x[]) { return 175 * x[0] / 65535 - 45; }
            }),
            new SensorValue("Air humidity", "%", new Conversion() {
                public double execute(double x[]) { return 100 * x[1] / 65535; }
            })
        }),
        new Sensor(2, new SensorValue[] {
            new SensorValue("Surface temperature", "°C", new Conversion() {
                public double execute(double x[]) { return (x[0] - 1000) / 10; }
            }),
            new SensorValue("Head temperature", "°C", new Conversion() {
                public double execute(double x[]) { return (x[1] - 1000) / 10; }
            })
        }),
        new Sensor(1, new SensorValue[] {
            new SensorValue("Battery voltage", "V", new Conversion() {
                public double execute(double x[]) { return x[0] / 1000; }
            })
        })
    };
}

public class DL_WRM {
    public static void main(String[] args) {
        String[] payloads = new String[] {
            "021a10000764a079b104f904c40c60",
            "021a1000040c60"
        };

        for (String pl : payloads) {
            try {
                Map<String, DecodedValue> decoded = DecentlabDecoder.decode(DL_WRM_Definition.SENSORS, pl);
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

