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
      new SensorValue("CH0: Temperature", "°C", x => (x[0] - 32768) / 100),
      new SensorValue("CH1: Temperature", "°C", x => (x[1] - 32768) / 100),
      new SensorValue("CH2: Temperature", "°C", x => (x[2] - 32768) / 100),
      new SensorValue("CH3: Temperature", "°C", x => (x[3] - 32768) / 100),
      new SensorValue("CH4: Temperature", "°C", x => (x[4] - 32768) / 100),
      new SensorValue("CH5: Temperature", "°C", x => (x[5] - 32768) / 100),
      new SensorValue("CH6: Temperature", "°C", x => (x[6] - 32768) / 100),
      new SensorValue("CH7: Temperature", "°C", x => (x[7] - 32768) / 100),
      new SensorValue("CH8: Temperature", "°C", x => (x[8] - 32768) / 100),
      new SensorValue("CH9: Temperature", "°C", x => (x[9] - 32768) / 100),
      new SensorValue("CH10: Temperature", "°C", x => (x[10] - 32768) / 100),
      new SensorValue("CH11: Temperature", "°C", x => (x[11] - 32768) / 100),
      new SensorValue("CH12: Temperature", "°C", x => (x[12] - 32768) / 100),
      new SensorValue("CH13: Temperature", "°C", x => (x[13] - 32768) / 100),
      new SensorValue("CH14: Temperature", "°C", x => (x[14] - 32768) / 100),
      new SensorValue("CH15: Temperature", "°C", x => (x[15] - 32768) / 100)
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
      "023e3e00038abc8a928aa08a848ab38a898ac38aad8ab78a928aa1000000000000000000000afc",
      "023e3e00020afc"
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
