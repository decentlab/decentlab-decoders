
/* https://www.decentlab.com/products/eleven-parameter-weather-station-for-lorawan */

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
    new Sensor(17, new List<SensorValue>() {
      new SensorValue("Solar radiation", "W⋅m⁻²", x => (x[0] - 32768) / 10),
      new SensorValue("Precipitation", "mm", x => x[1] / 1000),
      new SensorValue("Lightning strike count", "None", x => x[2] - 32768),
      new SensorValue("Lightning average distance", "km", x => x[3] - 32768),
      new SensorValue("Wind speed", "m⋅s⁻¹", x => (x[4] - 32768) / 100),
      new SensorValue("Wind direction", "°", x => (x[5] - 32768) / 10),
      new SensorValue("Maximum wind speed", "m⋅s⁻¹", x => (x[6] - 32768) / 100),
      new SensorValue("Air temperature", "°C", x => (x[7] - 32768) / 10),
      new SensorValue("Vapor pressure", "kPa", x => (x[8] - 32768) / 100),
      new SensorValue("Barometric pressure", "kPa", x => (x[9] - 32768) / 100),
      new SensorValue("Relative humidity", "%", x => (x[10] - 32768) / 10),
      new SensorValue("Internal temperature", "°C", x => (x[11] - 32768) / 10),
      new SensorValue("Tilt angle, X orientation", "°", x => (x[12] - 32768) / 10),
      new SensorValue("Tilt angle, Y orientation", "°", x => (x[13] - 32768) / 10),
      new SensorValue("Precipitation electrical conductivity", "µS⋅cm⁻¹", x => x[14] - 32768),
      new SensorValue("Cumulative precipitation", "mm", x => (x[15] + x[16] * 65536) / 1000)
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
      "025ef80003805c000080008000803484b3803680e78086a60181d680ed81c9809f8000117000010adc",
      "025ef800020adc"
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
