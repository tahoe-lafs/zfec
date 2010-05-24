#!/usr/bin/env python

# zfec -- fast forward error correction library with Python interface
#
# Copyright (C) 2007-2010 Allmydata, Inc.
# Author: Zooko Wilcox-O'Hearn
#
# This file is part of zfec.
#
# See README.txt for licensing information.

import glob, os, re, sys

egg = os.path.realpath(glob.glob('setuptools-*.egg')[0])
sys.path.insert(0, egg)
import setuptools; setuptools.bootstrap_install_from = egg

from setuptools import Extension, find_packages, setup

if "--debug" in sys.argv:
    DEBUGMODE=True
    sys.argv.remove("--debug")
else:
    DEBUGMODE=False

extra_compile_args=[]
extra_link_args=[]

extra_compile_args.append("-std=c99")

define_macros=[]
undef_macros=[]

for arg in sys.argv:
    if arg.startswith("--stride="):
        stride = int(arg[len("--stride="):])
        define_macros.append(('STRIDE', stride))
        sys.argv.remove(arg)
        break

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
    "Programming Language :: Python :: 2",
    "Programming Language :: Python :: 2.4",
    "Programming Language :: Python :: 2.5",
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

PKG = "zfec"
VERSIONFILE = os.path.join(PKG, "_version.py")
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

setup_requires = []

# The darcsver command from the darcsver plugin is needed to initialize the
# distribution's .version attribute correctly. (It does this either by
# examining darcs history, or if that fails by reading the
# zfec/_version.py file). darcsver will also write a new version
# stamp in zfec/_version.py, with a version number derived from
# darcs history. Note that the setup.cfg file has an "[aliases]" section
# which enumerates commands that you might run and specifies that it will run
# darcsver before each one. If you add different commands (or if I forgot
# some that are already in use), you may need to add it to setup.cfg and
# configure it to run darcsver before your command, if you want the version
# number to be correct when that command runs.
# http://pypi.python.org/pypi/darcsver
setup_requires.append('darcsver >= 1.2.0')

# setuptools_darcs is required to produce complete distributions (such as with
# "sdist" or "bdist_egg"), unless there is a zfec.egg-info/SOURCE.txt file
# present which contains a complete list of files that should be included.
# http://pypi.python.org/pypi/setuptools_darcs
setup_requires.append('setuptools_darcs >= 1.1.0')

# stdeb is required to build Debian dsc files.
if "sdist_dsc" in sys.argv:
    setup_requires.append('stdeb')

data_fnames=[ 'COPYING.GPL', 'changelog', 'COPYING.TGPPL.html', 'TODO', 'README.txt' ]

# In case we are building for a .deb with stdeb's sdist_dsc command, we put the
# docs in "share/doc/$PKG".
doc_loc = "share/doc/" + PKG
data_files = [(doc_loc, data_fnames)]

def _setup(test_suite):
    setup(name=PKG,
          version=verstr,
          description='a fast erasure codec which can be used with the command-line, C, Python, or Haskell',
          long_description='Fast, portable, programmable erasure coding a.k.a. "forward error correction": the generation of redundant blocks of information such that if some blocks are lost then the original data can be recovered from the remaining blocks.  The zfec package includes command-line tools, C API, Python API, and Haskell API',
          author='Zooko O\'Whielacronx',
          author_email='zooko@zooko.com',
          url='http://allmydata.org/trac/'+PKG,
          license='GNU GPL',
          install_requires=["argparse >= 0.8", "pyutil >= 1.3.19"],
          tests_require=["pyutil >= 1.3.19"],
          packages=find_packages(),
          include_package_data=True,
          data_files=data_files,
          setup_requires=setup_requires,
          classifiers=trove_classifiers,
          entry_points = { 'console_scripts': [ 'zfec = %s.cmdline_zfec:main' % PKG, 'zunfec = %s.cmdline_zunfec:main' % PKG ] },
          ext_modules=[Extension(PKG+'._fec', [PKG+'/fec.c', PKG+'/_fecmodule.c',], extra_link_args=extra_link_args, extra_compile_args=extra_compile_args, undef_macros=undef_macros, define_macros=define_macros),],
          test_suite=test_suite,
          zip_safe=False, # I prefer unzipped for easier access.
          )

test_suite_name=PKG+".test"
try:
    _setup(test_suite=test_suite_name)
except Exception, le:
    # to work around a bug in Elisa v0.3.5
    # https://bugs.launchpad.net/elisa/+bug/263697
    if "test_suite must be a list" in str(le):
        _setup(test_suite=[test_suite_name])
    else:
        raise
