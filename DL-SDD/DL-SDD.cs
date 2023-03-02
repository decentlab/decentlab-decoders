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
    new Sensor(18, new List<SensorValue>() {
      new SensorValue("Moisture at level 0", "%", x => (x[0] - 32768) / 100),
      new SensorValue("Moisture at level 1", "%", x => (x[1] - 32768) / 100),
      new SensorValue("Moisture at level 2", "%", x => (x[2] - 32768) / 100),
      new SensorValue("Moisture at level 3", "%", x => (x[3] - 32768) / 100),
      new SensorValue("Moisture at level 4", "%", x => (x[4] - 32768) / 100),
      new SensorValue("Moisture at level 5", "%", x => (x[5] - 32768) / 100),
      new SensorValue("Temperature at level 0", "°C", x => (x[6] - 32768) / 100),
      new SensorValue("Temperature at level 1", "°C", x => (x[7] - 32768) / 100),
      new SensorValue("Temperature at level 2", "°C", x => (x[8] - 32768) / 100),
      new SensorValue("Temperature at level 3", "°C", x => (x[9] - 32768) / 100),
      new SensorValue("Temperature at level 4", "°C", x => (x[10] - 32768) / 100),
      new SensorValue("Temperature at level 5", "°C", x => (x[11] - 32768) / 100),
      new SensorValue("Salinity at level 0", null, x => x[12] - 100),
      new SensorValue("Salinity at level 1", null, x => x[13] - 100),
      new SensorValue("Salinity at level 2", null, x => x[14] - 100),
      new SensorValue("Salinity at level 3", null, x => x[15] - 100),
      new SensorValue("Salinity at level 4", null, x => x[16] - 100),
      new SensorValue("Salinity at level 5", null, x => x[17] - 100)
    }),
    new Sensor(18, new List<SensorValue>() {
      new SensorValue("Moisture at level 6", "%", x => (x[0] - 32768) / 100),
      new SensorValue("Moisture at level 7", "%", x => (x[1] - 32768) / 100),
      new SensorValue("Moisture at level 8", "%", x => (x[2] - 32768) / 100),
      new SensorValue("Moisture at level 9", "%", x => (x[3] - 32768) / 100),
      new SensorValue("Moisture at level 10", "%", x => (x[4] - 32768) / 100),
      new SensorValue("Moisture at level 11", "%", x => (x[5] - 32768) / 100),
      new SensorValue("Temperature at level 6", "°C", x => (x[6] - 32768) / 100),
      new SensorValue("Temperature at level 7", "°C", x => (x[7] - 32768) / 100),
      new SensorValue("Temperature at level 8", "°C", x => (x[8] - 32768) / 100),
      new SensorValue("Temperature at level 9", "°C", x => (x[9] - 32768) / 100),
      new SensorValue("Temperature at level 10", "°C", x => (x[10] - 32768) / 100),
      new SensorValue("Temperature at level 11", "°C", x => (x[11] - 32768) / 100),
      new SensorValue("Salinity at level 6", null, x => x[12] - 100),
      new SensorValue("Salinity at level 7", null, x => x[13] - 100),
      new SensorValue("Salinity at level 8", null, x => x[14] - 100),
      new SensorValue("Salinity at level 9", null, x => x[15] - 100),
      new SensorValue("Salinity at level 10", null, x => x[16] - 100),
      new SensorValue("Salinity at level 11", null, x => x[17] - 100)
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
      "0243e300058000800080008000800080008741877b8749876c876c876600000000000000000000014a09e3",
      "0243e3000409e3"
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
