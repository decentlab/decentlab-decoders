
/* https://www.decentlab.com/products/ph-orp-and-temperature-sensor-for-lorawan */

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

class DL_PHEHT_Definition {
    public static final Sensor SENSORS[] = new Sensor[] {
        new Sensor(5, new SensorValue[] {
            new SensorValue("Status", null, new Conversion() {
                public double execute(double x[]) { return x[0]; }
            }),
            new SensorValue("Temperature", "°C", new Conversion() {
                public double execute(double x[]) { return (x[1] - 32768) / 100; }
            }),
            new SensorValue("pH", null, new Conversion() {
                public double execute(double x[]) { return (x[2] - 32768) / 100; }
            }),
            new SensorValue("Redox", "mV", new Conversion() {
                public double execute(double x[]) { return (x[3] - 32768) / 10; }
            }),
            new SensorValue("pH-mV", "mV", new Conversion() {
                public double execute(double x[]) { return (x[4] - 32768) / 10; }
            })
        }),
        new Sensor(1, new SensorValue[] {
            new SensorValue("Battery voltage", "V", new Conversion() {
                public double execute(double x[]) { return x[0] / 1000; }
            })
        })
    };
}

public class DL_PHEHT {
    public static void main(String[] args) {
        String[] payloads = new String[] {
            "0252b800030000884282c77f637ff60c5c",
            "0252b800020c5c"
        };

        for (String pl : payloads) {
            try {
                Map<String, DecodedValue> decoded = DecentlabDecoder.decode(DL_PHEHT_Definition.SENSORS, pl);
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

