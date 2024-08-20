import importlib
from typing import Callable

def get_decode_function(device_name: str) -> Callable[['str | list | bytearray', bool], dict]:
    """Get the decode function for the given device name."""

    module_path = f".decoders.{device_name}.{device_name}"

    try:
        module = importlib.import_module(module_path, package=__package__)
    except ModuleNotFoundError as e:
        raise ImportError(f"Module for device '{device_name}' not found.") from e

    if not hasattr(module, 'decode'):
        raise ImportError(f"'decode' function not found in module for device '{device_name}'.")

    decode_function = getattr(module, 'decode')
    return decode_function
