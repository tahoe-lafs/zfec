#!/usr/bin/env python

# zfec -- fast forward error correction library with Python interface
# 
# Copyright (C) 2007-2008 Allmydata, Inc.
# Author: Zooko Wilcox-O'Hearn
# 
# This file is part of zfec.
#
# See README.txt for licensing information.

import os, re, sys

miscdeps=os.path.join(os.getcwd(), 'misc', 'dependencies')

try:
    from ez_setup import use_setuptools
except ImportError:
    pass
else:
    # On cygwin there was a permissions error that was fixed in 0.6c6.
    use_setuptools(min_version='0.6c6', download_delay=0, to_dir=miscdeps)

from setuptools import Extension, find_packages, setup

if "--debug" in sys.argv:
    DEBUGMODE=True
    sys.argv.remove("--debug")
else:
    DEBUGMODE=("--debug" in sys.argv)

extra_compile_args=[]
extra_link_args=[]

extra_compile_args.append("-std=c99")

undef_macros=[]

if DEBUGMODE:
    extra_compile_args.append("-O0")
    extra_compile_args.append("-g")
    extra_compile_args.append("-Wall")
    extra_link_args.append("-g")
    undef_macros.append('NDEBUG')

trove_classifiers=[
    "Development Status :: 5 - Production/Stable",
    "Environment :: Console",
    "License :: OSI Approved :: GNU General Public License (GPL)", 
    "License :: DFSG approved",
    "License :: Other/Proprietary License",
    "Intended Audience :: Developers", 
    "Intended Audience :: End Users/Desktop",
    "Intended Audience :: System Administrators",
    "Operating System :: Microsoft",
    "Operating System :: Microsoft :: Windows",
    "Operating System :: Unix",
    "Operating System :: POSIX :: Linux",
    "Operating System :: POSIX",
    "Operating System :: MacOS :: MacOS X",
    "Operating System :: Microsoft :: Windows :: Windows NT/2000",
    "Operating System :: OS Independent", 
    "Natural Language :: English", 
    "Programming Language :: C", 
    "Programming Language :: Python", 
    "Topic :: Utilities",
    "Topic :: System :: Systems Administration",
    "Topic :: System :: Filesystems",
    "Topic :: System :: Distributed Computing",
    "Topic :: Software Development :: Libraries",
    "Topic :: Communications :: Usenet News",
    "Topic :: System :: Archiving :: Backup", 
    "Topic :: System :: Archiving :: Mirroring", 
    "Topic :: System :: Archiving", 
    ]

VERSIONFILE = "zfec/_version.py"
verstr = "unknown"
try:
    verstrline = open(VERSIONFILE, "rt").read()
except EnvironmentError:
    pass # Okay, there is no version file.
else:
    VSRE = r"^verstr = ['\"]([^'\"]*)['\"]"
    mo = re.search(VSRE, verstrline, re.M)
    if mo:
        verstr = mo.group(1)
    else:
        print "unable to find version in %s" % (VERSIONFILE,)
        raise RuntimeError("if %s.py exists, it is required to be well-formed" % (VERSIONFILE,))

dependency_links=[os.path.join(miscdeps, t) for t in os.listdir(miscdeps) if t.endswith(".tar")]
setup_requires = []

# darcsver is needed only if you want "./setup.py darcsver" to write a new
# version stamp in pycryptopp/_version.py, with a version number derived from
# darcs history.  http://pypi.python.org/pypi/darcsver
if "darcsver" in sys.argv[1:]:
    setup_requires.append('darcsver >= 1.0.0')

# setuptools_darcs is required to produce complete distributions (such as with
# "sdist" or "bdist_egg"), unless there is a PKG-INFO file present which shows
# that this is itself a source distribution.
# http://pypi.python.org/pypi/setuptools_darcs
if not os.path.exists('PKG-INFO'):
    setup_requires.append('setuptools_darcs >= 1.0.5')

setup(name='zfec',
      version=verstr,
      description='a fast erasure codec which can be used with the command-line, C, Python, or Haskell',
      long_description='Fast, portable, programmable erasure coding a.k.a. "forward error correction": the generation of redundant blocks of information such that if some blocks are lost then the original data can be recovered from the remaining blocks.  The zfec package includes command-line tools, C API, Python API, and Haskell API',
      author='Zooko O\'Whielacronx',
      author_email='zooko@zooko.com',
      url='http://allmydata.org/source/zfec',
      license='GNU GPL',
      dependency_links=dependency_links,
      install_requires=["argparse >= 0.8", "pyutil >= 1.3.5"],
      tests_require=["pyutil >= 1.3.5"],
      packages=find_packages(),
      include_package_data=True,
      setup_requires=setup_requires,
      classifiers=trove_classifiers,
      entry_points = { 'console_scripts': [ 'zfec = zfec.cmdline_zfec:main', 'zunfec = zfec.cmdline_zunfec:main' ] },
      ext_modules=[Extension('zfec._fec', ['zfec/fec.c', 'zfec/_fecmodule.c',], extra_link_args=extra_link_args, extra_compile_args=extra_compile_args, undef_macros=undef_macros),],
      test_suite="zfec.test",
      zip_safe=False, # I prefer unzipped for easier access.
      )
