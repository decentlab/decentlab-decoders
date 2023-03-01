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

class DL_TP_Definition {
    public static final Sensor SENSORS[] = new Sensor[] {
        new Sensor(16, new SensorValue[] {
            new SensorValue("Temperature at level 0", "°C", new Conversion() {
                public double execute(double x[]) { return (x[0] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 1", "°C", new Conversion() {
                public double execute(double x[]) { return (x[1] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 2", "°C", new Conversion() {
                public double execute(double x[]) { return (x[2] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 3", "°C", new Conversion() {
                public double execute(double x[]) { return (x[3] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 4", "°C", new Conversion() {
                public double execute(double x[]) { return (x[4] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 5", "°C", new Conversion() {
                public double execute(double x[]) { return (x[5] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 6", "°C", new Conversion() {
                public double execute(double x[]) { return (x[6] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 7", "°C", new Conversion() {
                public double execute(double x[]) { return (x[7] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 8", "°C", new Conversion() {
                public double execute(double x[]) { return (x[8] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 9", "°C", new Conversion() {
                public double execute(double x[]) { return (x[9] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 10", "°C", new Conversion() {
                public double execute(double x[]) { return (x[10] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 11", "°C", new Conversion() {
                public double execute(double x[]) { return (x[11] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 12", "°C", new Conversion() {
                public double execute(double x[]) { return (x[12] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 13", "°C", new Conversion() {
                public double execute(double x[]) { return (x[13] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 14", "°C", new Conversion() {
                public double execute(double x[]) { return (x[14] - 32768) / 100; }
            }),
            new SensorValue("Temperature at level 15", "°C", new Conversion() {
                public double execute(double x[]) { return (x[15] - 32768) / 100; }
            })
        }),
        new Sensor(1, new SensorValue[] {
            new SensorValue("Battery voltage", "V", new Conversion() {
                public double execute(double x[]) { return x[0] / 1000; }
            })
        })
    };
}

public class DL_TP {
    public static void main(String[] args) {
        String[] payloads = new String[] {
            "023e3e00038abc8a928aa08a848ab38a898ac38aad8ab78a928aa1000000000000000000000afc",
            "023e3e00020afc"
        };

        for (String pl : payloads) {
            try {
                Map<String, DecodedValue> decoded = DecentlabDecoder.decode(DL_TP_Definition.SENSORS, pl);
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

