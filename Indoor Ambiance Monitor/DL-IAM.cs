using System;
using System.IO;
using System.Collections.Generic;

public class DecentlabDecoder
{
  private delegate double conversion(double[] x);

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
    new Sensor(2, new List<SensorValue>() {
      new SensorValue("Air temperature", "°C", x => 175 * x[0] / 65535 - 45),
      new SensorValue("Air humidity", "%", x => 100 * x[1] / 65535)
    }),
    new Sensor(1, new List<SensorValue>() {
      new SensorValue("Barometric pressure", "Pa", x => x[0] * 2)
    }),
    new Sensor(2, new List<SensorValue>() {
      new SensorValue("Ambient light (visible + infrared)", null, x => x[0]),
      new SensorValue("Ambient light (infrared)", null, x => x[1]),
      new SensorValue("Illuminance", "lx", x => Math.Max(Math.Max(1.0 * x[0] - 1.64 * x[1], 0.59 * x[0] - 0.86 * x[1]), 0) * 1.5504)
    }),
    new Sensor(3, new List<SensorValue>() {
      new SensorValue("CO2 concentration", "ppm", x => x[0] - 32768),
      new SensorValue("CO2 sensor status", null, x => x[1]),
      new SensorValue("Raw IR reading", null, x => x[2])
    }),
    new Sensor(1, new List<SensorValue>() {
      new SensorValue("Activity counter", null, x => x[0])
    }),
    new Sensor(1, new List<SensorValue>() {
      new SensorValue("Total VOC", "ppb", x => x[0])
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
    if (version != 2)
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
      "020bbd007f0b926a515d48bc4e0262006981c7000093d4000b0111",
      "020bbd006f0b926a515d48bc4e02620069000b0111",
      "020bbd00010b92"
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
