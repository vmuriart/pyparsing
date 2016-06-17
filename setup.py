#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Setup script for the pyparsing module distribution."""

from setuptools import setup, find_packages

setup(
    name="pyparsing",
    version="2.1.5",
    description="Python parsing module",
    author="Paul McGuire",
    author_email="ptmcg@users.sourceforge.net",
    url="http://pyparsing.wikispaces.com/",
    download_url="http://sourceforge.net/project/showfiles.php?group_id=97203",
    license="MIT License",
    packages=find_packages('src'),
    package_dir={'': 'src'},
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Intended Audience :: Developers',
        'Intended Audience :: Information Technology',
        'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
    ]
)
