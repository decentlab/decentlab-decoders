#!/usr/bin/env python3

import sys
import os
import logging
import itertools
import re
import shutil

import yaml
import json
from jinja2 import Environment


DEVICE_DEFS_FILE = './device-definitions.yaml'
PLATFORM_DEFS_FILE = './platform-definitions.yaml'
ROOT_DIR = './'

logger = logging.getLogger(__name__)
environment = Environment()
environment.filters['regex_replace'] = lambda s, find, replace: re.sub(find, replace, s)


def generate_snippet(device, platform):
    logger.debug("JSON-dump of sensors: %s", json.dumps(device['sensors']))
    return platform['compiled'].render(**device)


def main(args=sys.argv):
    with open(DEVICE_DEFS_FILE) as f:
        devices = yaml.load(f)

    with open(PLATFORM_DEFS_FILE) as f:
        platforms = yaml.load(f)

    for platform in platforms:
        platform["compiled"] = environment.from_string(platform["template"])

    if '-d' in args:
        for file_ in os.listdir(ROOT_DIR):
            path = os.path.join(ROOT_DIR, file_)
            if not file_.startswith('.') and os.path.isdir(path):
                shutil.rmtree(path)
                logger.info("removed: %s", path)

    for device in devices:
        if not os.path.exists(device["title"]):
            path = os.path.join(ROOT_DIR, device["title"])
            os.mkdir(path)
            logger.info("created: %s", path)

    for device, platform in itertools.product(devices, platforms):
        source = generate_snippet(device, platform)
        name = os.path.join(ROOT_DIR,
                            device["title"],
                            '{}.{}'.format(device["name"], platform["ext"]))
        with open(name, "w") as f:
            f.write(source)
        logger.info("created: %s", name)


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    main()
