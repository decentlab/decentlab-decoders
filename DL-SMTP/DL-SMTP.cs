
/* https://www.decentlab.com/products/soil-moisture-and-temperature-profile-for-lorawan */

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
      new SensorValue("Soil moisture at depth 0", "None", x => (x[0] - 2500) / 500),
      new SensorValue("Soil temperature at depth 0", "°C", x => (x[1] - 32768) / 100),
      new SensorValue("Soil moisture at depth 1", "None", x => (x[2] - 2500) / 500),
      new SensorValue("Soil temperature at depth 1", "°C", x => (x[3] - 32768) / 100),
      new SensorValue("Soil moisture at depth 2", "None", x => (x[4] - 2500) / 500),
      new SensorValue("Soil temperature at depth 2", "°C", x => (x[5] - 32768) / 100),
      new SensorValue("Soil moisture at depth 3", "None", x => (x[6] - 2500) / 500),
      new SensorValue("Soil temperature at depth 3", "°C", x => (x[7] - 32768) / 100),
      new SensorValue("Soil moisture at depth 4", "None", x => (x[8] - 2500) / 500),
      new SensorValue("Soil temperature at depth 4", "°C", x => (x[9] - 32768) / 100),
      new SensorValue("Soil moisture at depth 5", "None", x => (x[10] - 2500) / 500),
      new SensorValue("Soil temperature at depth 5", "°C", x => (x[11] - 32768) / 100),
      new SensorValue("Soil moisture at depth 6", "None", x => (x[12] - 2500) / 500),
      new SensorValue("Soil temperature at depth 6", "°C", x => (x[13] - 32768) / 100),
      new SensorValue("Soil moisture at depth 7", "None", x => (x[14] - 2500) / 500),
      new SensorValue("Soil temperature at depth 7", "°C", x => (x[15] - 32768) / 100)
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
      "020b50000309018a8c09438a9809278a920b3c8aa50c9c8a8c11e08aa500000000000000000b3b",
      "020b5000020b3b"
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
