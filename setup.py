
# zfec -- fast forward error correction library with Python interface
#
# copyright © 2007-2013 Zooko Wilcox-O'Hearn
#
# This file is part of zfec.
#
# See README.rst for licensing information.

import glob, os, re, sys
import setuptools
import versioneer

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
    extra_compile_args.append("-Wextra")
    extra_link_args.append("-g")
    undef_macros.append('NDEBUG')

trove_classifiers=[
    "Development Status :: 5 - Production/Stable",
    "Environment :: Console",
    "License :: OSI Approved :: GNU General Public License (GPL)", # See README.rst for alternative licensing.
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
    "Programming Language :: Python :: 2.6",
    "Programming Language :: Python :: 2.7",
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

setup_requires = []

# stdeb is required to build Debian dsc files.
if "sdist_dsc" in sys.argv:
    setup_requires.append('stdeb')

data_fnames=[ 'COPYING.GPL', 'changelog', 'COPYING.TGPPL.rst', 'TODO', 'README.rst' ]

# In case we are building for a .deb with stdeb's sdist_dsc command, we put the
# docs in "share/doc/$PKG".
doc_loc = "share/doc/" + PKG
data_files = [(doc_loc, data_fnames)]

readmetext = open('README.rst').read()
if readmetext[:3] == '\xef\xbb\xbf':
    # utf-8 "BOM"
    readmetext = readmetext[3:]

try:
    readmetext = readmetext.decode('utf-8')
except UnicodeDecodeError:
    pass

install_requires=["pyutil >= 1.3.19"]

# argparse comes built into Python >= 2.7, and is provided by the "argparse"
# distribution for earlier versions of Python.
try:
    import argparse
    argparse # hush pyflakes
except ImportError:
    install_requires.append("argparse >= 0.8")

# distutils in Python 2.4 has a bug in that it tries to encode the long
# description into ascii. We detect the resulting exception and try again
# after squashing the long description (lossily) into ascii.

def _setup(longdescription):
    setup(name=PKG,
          description='a fast erasure codec which can be used with the command-line, C, Python, or Haskell',
          long_description=longdescription,
          author='Zooko O\'Whielacronx',
          author_email='zooko@zooko.com',
          url='https://tahoe-lafs.org/trac/'+PKG,
          license='GNU GPL', # See README.rst for alternative licensing.
          install_requires=install_requires,
          packages=find_packages(),
          include_package_data=True,
          data_files=data_files,
          setup_requires=setup_requires,
          classifiers=trove_classifiers,
          entry_points = { 'console_scripts': [ 'zfec = %s.cmdline_zfec:main' % PKG, 'zunfec = %s.cmdline_zunfec:main' % PKG ] },
          ext_modules=[Extension(PKG+'._fec', [PKG+'/fec.c', PKG+'/_fecmodule.c',], extra_link_args=extra_link_args, extra_compile_args=extra_compile_args, undef_macros=undef_macros, define_macros=define_macros, include_dirs=[PKG+'/']),],
          test_suite=PKG+".test",
          zip_safe=False, # I prefer unzipped for easier access.
          extras_require={
            'ed25519=ba95497adf4db8e17f688c0979003c48c76897d60e2d2193f938b9ab62115f59':[],
            },
          version=versioneer.get_version(),
          cmdclass=versioneer.get_cmdclass(),
         )

try:
    _setup(readmetext)
except UnicodeEncodeError:
    _setup(repr(readmetext))
