
/* https://www.decentlab.com/products/air-quality-station-no2-no-co-ox-for-lorawan */

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

class DL_AC_Definition {

    /* device-specific parameters */
    public static final double NO2_WE_0 = 256;
    public static final double NO2_S = 0.256;
    public static final double NO2_AUX_0 = 227;
    public static final double NO_WE_0 = 320;
    public static final double NO_S = 0.512;
    public static final double NO_AUX_0 = 288;
    public static final double Ox_WE_0 = 235;
    public static final double Ox_S = 0.345;
    public static final double Ox_AUX_0 = 200;
    public static final double CO_WE_0 = 544;
    public static final double CO_S = 0.424;
    public static final double CO_AUX_0 = 301;
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
            new SensorValue("CH4: NO2 (we)", "mV", new Conversion() {
                public double execute(double x[]) { return 3 * (x[0] / 32768 - 1) * 1000; }
            }),
            new SensorValue("CH4: NO2 (we-aux)", "mV", new Conversion() {
                public double execute(double x[]) { return 3 * (x[1] / 32768 - 1) * 1000; }
            }),
            new SensorValue("CH4: NO2 concentration (we)", "ppb", new Conversion() {
                public double execute(double x[]) { return (3 * (x[0] / 32768 - 1) * 1000 - NO2_WE_0) / NO2_S; }
            }),
            new SensorValue("CH4: NO2 concentration (we-aux)", "ppb", new Conversion() {
                public double execute(double x[]) { return (3 * (x[1] / 32768 - 1) * 1000 - NO2_WE_0 + NO2_AUX_0) / NO2_S; }
            })
        }),
        new Sensor(2, new SensorValue[] {
            new SensorValue("CH5: NO (we)", "mV", new Conversion() {
                public double execute(double x[]) { return 3 * (x[0] / 32768 - 1) * 1000; }
            }),
            new SensorValue("CH5: NO (we-aux)", "mV", new Conversion() {
                public double execute(double x[]) { return 3 * (x[1] / 32768 - 1) * 1000; }
            }),
            new SensorValue("CH5: NO concentration (we)", "ppb", new Conversion() {
                public double execute(double x[]) { return (3 * (x[0] / 32768 - 1) * 1000 - NO_WE_0) / NO_S; }
            }),
            new SensorValue("CH5: NO concentration (we-aux)", "ppb", new Conversion() {
                public double execute(double x[]) { return (3 * (x[1] / 32768 - 1) * 1000 - NO_WE_0 + NO_AUX_0) / NO_S; }
            })
        }),
        new Sensor(2, new SensorValue[] {
            new SensorValue("CH6: Ox (we)", "mV", new Conversion() {
                public double execute(double x[]) { return 3 * (x[0] / 32768 - 1) * 1000; }
            }),
            new SensorValue("CH6: Ox (we-aux)", "mV", new Conversion() {
                public double execute(double x[]) { return 3 * (x[1] / 32768 - 1) * 1000; }
            }),
            new SensorValue("CH6: Ox concentration (we)", "ppb", new Conversion() {
                public double execute(double x[]) { return (3 * (x[0] / 32768 - 1) * 1000 - Ox_WE_0) / Ox_S; }
            }),
            new SensorValue("CH6: Ox concentration (we-aux)", "ppb", new Conversion() {
                public double execute(double x[]) { return (3 * (x[1] / 32768 - 1) * 1000 - Ox_WE_0 + Ox_AUX_0) / Ox_S; }
            })
        }),
        new Sensor(2, new SensorValue[] {
            new SensorValue("CH7: CO (we)", "mV", new Conversion() {
                public double execute(double x[]) { return 3 * (x[0] / 32768 - 1) * 1000; }
            }),
            new SensorValue("CH7: CO (we-aux)", "mV", new Conversion() {
                public double execute(double x[]) { return 3 * (x[1] / 32768 - 1) * 1000; }
            }),
            new SensorValue("CH7: CO concentration (we)", "ppb", new Conversion() {
                public double execute(double x[]) { return (3 * (x[0] / 32768 - 1) * 1000 - CO_WE_0) / CO_S; }
            }),
            new SensorValue("CH7: CO concentration (we-aux)", "ppb", new Conversion() {
                public double execute(double x[]) { return (3 * (x[1] / 32768 - 1) * 1000 - CO_WE_0 + CO_AUX_0) / CO_S; }
            })
        }),
        new Sensor(1, new SensorValue[] {
            new SensorValue("Battery voltage", "V", new Conversion() {
                public double execute(double x[]) { return x[0] / 1000; }
            })
        })
    };
}

public class DL_AC {
    public static void main(String[] args) {
        String[] payloads = new String[] {
            "020fa0003f66b49b8c8966803c8cf580238a68804c903783f4158a",
            "020fa00020158a"
        };

        for (String pl : payloads) {
            try {
                Map<String, DecodedValue> decoded = DecentlabDecoder.decode(DL_AC_Definition.SENSORS, pl);
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

