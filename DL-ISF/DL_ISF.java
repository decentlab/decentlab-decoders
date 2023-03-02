
/* https://www.decentlab.com/products/sapflow-sensor-for-lorawan */

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

class DL_ISF_Definition {
    public static final Sensor SENSORS[] = new Sensor[] {
        new Sensor(16, new SensorValue[] {
            new SensorValue("Sap flow", "l⋅h⁻¹", new Conversion() {
                public double execute(double x[]) { return (x[0] * 16 - 50000) / 1000; }
            }),
            new SensorValue("Heat velocity (outer)", "cm⋅h⁻¹", new Conversion() {
                public double execute(double x[]) { return (x[1] * 16 - 50000) / 1000; }
            }),
            new SensorValue("Heat velocity (inner)", "cm⋅h⁻¹", new Conversion() {
                public double execute(double x[]) { return (x[2] * 16 - 50000) / 1000; }
            }),
            new SensorValue("Alpha (outer)", null, new Conversion() {
                public double execute(double x[]) { return (x[3] * 32 - 1000000) / 100000; }
            }),
            new SensorValue("Alpha (inner)", null, new Conversion() {
                public double execute(double x[]) { return (x[4] * 32 - 1000000) / 100000; }
            }),
            new SensorValue("Beta (outer)", null, new Conversion() {
                public double execute(double x[]) { return (x[5] * 32 - 1000000) / 100000; }
            }),
            new SensorValue("Beta (inner)", null, new Conversion() {
                public double execute(double x[]) { return (x[6] * 32 - 1000000) / 100000; }
            }),
            new SensorValue("Tmax (outer)", "s", new Conversion() {
                public double execute(double x[]) { return (x[7] * 2) / 1000; }
            }),
            new SensorValue("Tmax (inner)", "s", new Conversion() {
                public double execute(double x[]) { return (x[8] * 2) / 1000; }
            }),
            new SensorValue("Temperature (outer)", "°C", new Conversion() {
                public double execute(double x[]) { return (x[9] - 32768) / 100; }
            }),
            new SensorValue("Max voltage", "V", new Conversion() {
                public double execute(double x[]) { return (x[10] - 32768) / 1000; }
            }),
            new SensorValue("Min voltage", "V", new Conversion() {
                public double execute(double x[]) { return (x[11] - 32768) / 1000; }
            }),
            new SensorValue("Diagnostic", null, new Conversion() {
                public double execute(double x[]) { return x[12] + x[13] * 65536; }
            }),
            new SensorValue("Upstream Tmax (outer)", "s", new Conversion() {
                public double execute(double x[]) { return (x[14] * 2) / 1000; }
            }),
            new SensorValue("Upstream Tmax (inner)", "s", new Conversion() {
                public double execute(double x[]) { return (x[15] * 2) / 1000; }
            })
        }),
        new Sensor(1, new SensorValue[] {
            new SensorValue("Battery voltage", "V", new Conversion() {
                public double execute(double x[]) { return x[0] / 1000; }
            })
        })
    };
}

public class DL_ISF {
    public static void main(String[] args) {
        String[] payloads = new String[] {
            "023d0100030c290bab0c3e79707a1d78437991490845997e4cacdeaa6e00000000457e415a0b59",
            "023d0100020b59"
        };

        for (String pl : payloads) {
            try {
                Map<String, DecodedValue> decoded = DecentlabDecoder.decode(DL_ISF_Definition.SENSORS, pl);
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

