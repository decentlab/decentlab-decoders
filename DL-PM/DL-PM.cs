
/* https://www.decentlab.com/products/particulate-matter-temperature-humidity-and-barometric-pressure-sensor-for-lorawan */

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
    new Sensor(1, new List<SensorValue>() {
      new SensorValue("Battery voltage", "V", x => x[0] / 1000)
    }),
    new Sensor(10, new List<SensorValue>() {
      new SensorValue("PM1.0 mass concentration", "µg⋅m⁻³", x => x[0] / 10),
      new SensorValue("PM2.5 mass concentration", "µg⋅m⁻³", x => x[1] / 10),
      new SensorValue("PM4 mass concentration", "µg⋅m⁻³", x => x[2] / 10),
      new SensorValue("PM10 mass concentration", "µg⋅m⁻³", x => x[3] / 10),
      new SensorValue("Typical particle size", "nm", x => x[4]),
      new SensorValue("PM0.5 number concentration", null, x => x[5] / 10),
      new SensorValue("PM1.0 number concentration", null, x => x[6] / 10),
      new SensorValue("PM2.5 number concentration", null, x => x[7] / 10),
      new SensorValue("PM4 number concentration", null, x => x[8] / 10),
      new SensorValue("PM10 number concentration", null, x => x[9] / 10)
    }),
    new Sensor(2, new List<SensorValue>() {
      new SensorValue("Air temperature", "°C", x => 175.72 * x[0] / 65536 - 46.85),
      new SensorValue("Air humidity", "%", x => 125 * x[1] / 65536 - 6)
    }),
    new Sensor(1, new List<SensorValue>() {
      new SensorValue("Barometric pressure", "Pa", x => x[0] * 2)
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
      "021b50000f0c25002500270027002701f50107012c012d012d012d67bd618dbd10",
      "021b50000d0c2567bd618dbd10",
      "021b5000010c25"
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
