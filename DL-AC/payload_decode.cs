
/* https://www.decentlab.com/products/air-quality-station-no2-no-co-ox-for-lorawan */

using System;
using System.IO;
using System.Collections.Generic;

public class DecentlabDecoder
{
  private delegate double conversion(double[] x);
  public const int PROTOCOL_VERSION = 2;
  /* Device-specific parameters */
  public const double NO2_WE_0 = 256;
  public const double NO2_S = 0.256;
  public const double NO2_AUX_0 = 227;
  public const double NO_WE_0 = 320;
  public const double NO_S = 0.512;
  public const double NO_AUX_0 = 288;
  public const double Ox_WE_0 = 235;
  public const double Ox_S = 0.345;
  public const double Ox_AUX_0 = 200;
  public const double CO_WE_0 = 544;
  public const double CO_S = 0.424;
  public const double CO_AUX_0 = 301;

  private class Sensor
  {
    internal int length { get; set; }
    internal List<SensorValue> values { get; set; }
    internal Sensor(int length, List<SensorValue> values)
    {
      this.length = length;
      this.values = values;
    }
  }

  private class SensorValue
  {
    internal string name { get; set; }
    internal string unit { get; set; }
    internal conversion convert;
    internal SensorValue(string name, string unit, conversion convert)
    {
      this.name = name;
      this.unit = unit;
      this.convert = convert;
    }
  }

  private static readonly List<Sensor> SENSORS = new List<Sensor>() {
    new Sensor(2, new List<SensorValue>() {
      new SensorValue("Air temperature", "°C", x => 175.72 * x[0] / 65536 - 46.85),
      new SensorValue("Air humidity", "%", x => 125 * x[1] / 65536 - 6)
    }),
    new Sensor(2, new List<SensorValue>() {
      new SensorValue("CH4: NO2 (we)", "mV", x => 3 * (x[0] / 32768 - 1) * 1000),
      new SensorValue("CH4: NO2 (we-aux)", "mV", x => 3 * (x[1] / 32768 - 1) * 1000),
      new SensorValue("CH4: NO2 concentration (we)", "ppb", x => (3 * (x[0] / 32768 - 1) * 1000 - NO2_WE_0) / NO2_S),
      new SensorValue("CH4: NO2 concentration (we-aux)", "ppb", x => (3 * (x[1] / 32768 - 1) * 1000 - NO2_WE_0 + NO2_AUX_0) / NO2_S)
    }),
    new Sensor(2, new List<SensorValue>() {
      new SensorValue("CH5: NO (we)", "mV", x => 3 * (x[0] / 32768 - 1) * 1000),
      new SensorValue("CH5: NO (we-aux)", "mV", x => 3 * (x[1] / 32768 - 1) * 1000),
      new SensorValue("CH5: NO concentration (we)", "ppb", x => (3 * (x[0] / 32768 - 1) * 1000 - NO_WE_0) / NO_S),
      new SensorValue("CH5: NO concentration (we-aux)", "ppb", x => (3 * (x[1] / 32768 - 1) * 1000 - NO_WE_0 + NO_AUX_0) / NO_S)
    }),
    new Sensor(2, new List<SensorValue>() {
      new SensorValue("CH6: Ox (we)", "mV", x => 3 * (x[0] / 32768 - 1) * 1000),
      new SensorValue("CH6: Ox (we-aux)", "mV", x => 3 * (x[1] / 32768 - 1) * 1000),
      new SensorValue("CH6: Ox concentration (we)", "ppb", x => (3 * (x[0] / 32768 - 1) * 1000 - Ox_WE_0) / Ox_S),
      new SensorValue("CH6: Ox concentration (we-aux)", "ppb", x => (3 * (x[1] / 32768 - 1) * 1000 - Ox_WE_0 + Ox_AUX_0) / Ox_S)
    }),
    new Sensor(2, new List<SensorValue>() {
      new SensorValue("CH7: CO (we)", "mV", x => 3 * (x[0] / 32768 - 1) * 1000),
      new SensorValue("CH7: CO (we-aux)", "mV", x => 3 * (x[1] / 32768 - 1) * 1000),
      new SensorValue("CH7: CO concentration (we)", "ppb", x => (3 * (x[0] / 32768 - 1) * 1000 - CO_WE_0) / CO_S),
      new SensorValue("CH7: CO concentration (we-aux)", "ppb", x => (3 * (x[1] / 32768 - 1) * 1000 - CO_WE_0 + CO_AUX_0) / CO_S)
    }),
    new Sensor(1, new List<SensorValue>() {
      new SensorValue("Battery voltage", "V", x => x[0] / 1000)
    })
  };

  private static int ReadInt(Stream stream)
  {
    return (stream.ReadByte() << 8) + stream.ReadByte();
  }


  public static Dictionary<string, Tuple<double, string>> Decode(byte[] msg)
  {
    return Decode(new MemoryStream(msg));
  }

  public static Dictionary<string, Tuple<double, string>> Decode(String msg)
  {
    byte[] output = new byte[msg.Length / 2];
    for (int i = 0, j = 0; i < msg.Length; i += 2, j++)
    {
      output[j] = (byte)int.Parse(msg.Substring(i, 2), System.Globalization.NumberStyles.HexNumber);
    }
    return Decode(output);
  }

  public static Dictionary<string, Tuple<double, string>> Decode(Stream msg)
  {
    var version = msg.ReadByte();
    if (version != PROTOCOL_VERSION)
    {
      throw new InvalidDataException("protocol version " + version + " doesn't match v2");
    }

    var result = new Dictionary<string, Tuple<double, string>>();
    result["Protocol version"] = new Tuple<double, string>(version, null);

    var deviceId = ReadInt(msg);
    result["Device ID"] = new Tuple<double, string>(deviceId, null);

    var flags = ReadInt(msg);
    foreach (var sensor in SENSORS)
    {
      if ((flags & 1) == 1)
      {
        double[] x = new double[sensor.length];
        for (int i = 0; i < sensor.length; i++)
        {
          x[i] = ReadInt(msg);
        }
        foreach (var val in sensor.values)
        {
          if (val.convert != null)
          {
            result[val.name] = new Tuple<double, string>(val.convert(x), val.unit);
          }
        }
      }
      flags >>= 1;
    }

    return result;
  }
}
public class Program
{
  public static void Main()
  {
    var payloads = new string[] {
      "020fa0003f66b49b8c8966803c8cf580238a68804c903783f4158a",
      "020fa00020158a"
    };

    foreach (var pl in payloads)
    {
      var decoded = DecentlabDecoder.Decode(pl);
      foreach (var k in decoded.Keys)
      {
        Console.WriteLine(k + ": " + decoded[k]);
      }
      Console.WriteLine();
    }
  }
}
