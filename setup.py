#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys


try:
    from setuptools import setup, find_packages
except ImportError:
    from distutils.core import setup

if sys.argv[-1] == "publish":
    os.system("python setup.py sdist upload")
    sys.exit()

readme = open("README.rst").read()

setup(
    name="stalker_comment_editor",
    version="0.1.1",
    description="Cli tool for bulk editing of stalker ogg vorbis comments.",
    long_description=readme,
    author="Steve Casey",
    author_email="stevencasey21@gmail.com",
    url="https://github.com/sjcasey21/stalker_comment_editor",
    packages=find_packages(exclude=["tests"]),
    package_data={"stalker_comment_editor": ["*.hy"]},
    install_requires=["hy >= 0.17.0", "click", "pyyaml", "crcmod", "jsonschema"],
    entry_points={"console_scripts":
                  ["stalker_comment_editor=stalker_comment_editor:cli"]},
    license="BSD",
    keywords="stalker_comment_editor",
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: BSD License",
        "Programming Language :: Lisp",
        "Programming Language :: Python :: 3.7",
    ],
)
