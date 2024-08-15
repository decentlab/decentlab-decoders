import sys
from . import get_decode_function

def main():
    device_name = sys.argv[1]
    decode_func = get_decode_function(device_name)
    data = decode_func(bytes(sys.argv[2], 'utf-8'), hex=True)
    print(data)

if __name__ == "__main__":
    main()