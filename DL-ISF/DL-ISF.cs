
/* https://www.decentlab.com/products/sapflow-sensor-for-lorawan */

using System;
using System.IO;
using System.Collections.Generic;

public class DecentlabDecoder
{
  private delegate double conversion(double[] x);
  public const int PROTOCOL_VERSION = 2;

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
    new Sensor(16, new List<SensorValue>() {
      new SensorValue("Sap flow", "l⋅h⁻¹", x => (x[0] * 16 - 50000) / 1000),
      new SensorValue("Heat velocity (outer)", "cm⋅h⁻¹", x => (x[1] * 16 - 50000) / 1000),
      new SensorValue("Heat velocity (inner)", "cm⋅h⁻¹", x => (x[2] * 16 - 50000) / 1000),
      new SensorValue("Alpha (outer)", null, x => (x[3] * 32 - 1000000) / 100000),
      new SensorValue("Alpha (inner)", null, x => (x[4] * 32 - 1000000) / 100000),
      new SensorValue("Beta (outer)", null, x => (x[5] * 32 - 1000000) / 100000),
      new SensorValue("Beta (inner)", null, x => (x[6] * 32 - 1000000) / 100000),
      new SensorValue("Tmax (outer)", "s", x => (x[7] * 2) / 1000),
      new SensorValue("Tmax (inner)", "s", x => (x[8] * 2) / 1000),
      new SensorValue("Temperature (outer)", "°C", x => (x[9] - 32768) / 100),
      new SensorValue("Max voltage", "V", x => (x[10] - 32768) / 1000),
      new SensorValue("Min voltage", "V", x => (x[11] - 32768) / 1000),
      new SensorValue("Diagnostic", null, x => x[12] + x[13] * 65536),
      new SensorValue("Upstream Tmax (outer)", "s", x => (x[14] * 2) / 1000),
      new SensorValue("Upstream Tmax (inner)", "s", x => (x[15] * 2) / 1000)
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
      "023d0100030c290bab0c3e79707a1d78437991490845997e4cacdeaa6e00000000457e415a0b59",
      "023d0100020b59"
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
