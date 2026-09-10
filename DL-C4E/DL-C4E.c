
/* https://www.decentlab.com/products/conductivity-salinity-and-temperature-sensor-for-lorawan */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>

#define ENOPROTO -1

struct dl_sensor_device_c4e_measurement {
    uint16_t device_id;

    bool sensor0_valid;
    float status;
    float temperature;
    float conductivity;
    float salinity;
    float tds;

    bool sensor1_valid;
    float battery_voltage;
};

static inline uint16_t sys_get_be16(const uint8_t src[2])
{
    return ((uint16_t)src[0] << 8) | src[1];
}

static inline int hex2bin(const char *hex_str, size_t len, uint8_t *result)
{
    if (len % 2 != 0) {
        return EINVAL;
    }

    for (size_t i = 0, k = 0; i < len; i += 2, k++) {
        char ch[3] = { hex_str[i], hex_str[i + 1] };
        char *ep;
        result[k] = strtol(ch, &ep, 16);
        if (*ep != 0) {
            return EINVAL;
        }
    }

    return 0;
}

static inline void bytes2words(const uint8_t bytes[], size_t bytes_len, float result[])
{
    for (size_t i = 0; i < bytes_len; i += 2) {
        *(result++) = (float)sys_get_be16(bytes + i);
    }
}

static inline size_t dl_decode_device_c4e_sensor_0(const float x[6], struct dl_sensor_device_c4e_measurement *result)
{
    result->sensor0_valid = true;
    result->status = x[0];
    result->temperature = (x[1] - 32768) / 100;
    result->conductivity = (x[2] + x[3] * 65536 - 2147483648) / 1000000;
    result->salinity = x[4] / 500;
    result->tds = x[5] / 0.4;
    return 6;
}

static inline size_t dl_decode_device_c4e_sensor_1(const float x[1], struct dl_sensor_device_c4e_measurement *result)
{
    result->sensor1_valid = true;
    result->battery_voltage = x[0] / 1000;
    return 1;
}

int dl_decode_device_c4e(const uint8_t payload[], size_t payload_len, struct dl_sensor_device_c4e_measurement *result)
{
    if (payload_len < 5 || payload_len > 19 || payload_len % 2 == 1) {
        return EINVAL;
    }

    if (payload[0] != 2) {
        return ENOPROTO;
    }

    result->device_id = sys_get_be16(payload + 1);

    uint16_t flag = sys_get_be16(payload + 3);

    size_t remaining_len = payload_len - 5;
    float words[remaining_len / 2];
    bytes2words(payload + 5, remaining_len, words);

    float *cur = words;
    
    if (flag & 1) {
        cur += dl_decode_device_c4e_sensor_0(cur, result);
    }
    flag >>= 1;
    
    if (flag & 1) {
        cur += dl_decode_device_c4e_sensor_1(cur, result);
    }
    flag >>= 1;
    

    return 0;
}


int main()
{
    const char *test_payload_hex[] = {
        "026934000300008A444F78806A0776064B0C0D",
        "02693400020C0D"
    };

    for (size_t i = 0; i < sizeof(test_payload_hex) / sizeof(const char*); i++) {
        size_t n = strlen(test_payload_hex[i]);
        uint8_t test_payload[n];
        int err = hex2bin(test_payload_hex[i], n, test_payload);
        if (err != 0) {
            return err;
        }

        struct dl_sensor_device_c4e_measurement result = { 0 };
        err = dl_decode_device_c4e(test_payload, sizeof(test_payload), &result);
        if (err != 0) {
            return err;
        }

        printf("\ndevice_id: %d\n", result.device_id);

        if (result.sensor0_valid) {
            printf("status: %f\n", result.status);
            printf("temperature: %f\n", result.temperature);
            printf("conductivity: %f\n", result.conductivity);
            printf("salinity: %f\n", result.salinity);
            printf("tds: %f\n", result.tds);
        }

        if (result.sensor1_valid) {
            printf("battery_voltage: %f\n", result.battery_voltage);
        }

    }

    return 0;
}