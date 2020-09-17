from setuptools import setup
from setuptools.extension import Extension

import sys
import os
import versioneer

DEBUGMODE = False

if "--debug" in sys.argv:
    DEBUGMODE = True
    sys.argv.remove("--debug")

extra_link_args = []
extra_compile_args = []
define_macros = []
undef_macros = []

for arg in sys.argv:
    if arg.startswith("--stride="):
        stride = int(arg[len("--stride="):])
        define_macros.append(('STRIDE', stride))
        sys.argv.remove(arg)
        break

extra_compile_args.append("-std=c99")
if DEBUGMODE:
    extra_compile_args.append("-O0")
    extra_compile_args.append("-g")
    extra_compile_args.append("-Wall")
    extra_compile_args.append("-Wextra")
    extra_link_args.append("-g")
    undef_macros.append('NDEBUG')

extensions = [
    Extension(
        "zfec._fec",
        [
            "zfec/fec.c",
            "zfec/_fecmodule.c"
        ],
        include_dirs=["zfec/"],
        extra_link_args=extra_link_args,
        extra_compile_args=extra_compile_args,
        define_macros=define_macros,
        undef_macros=undef_macros
    )
]

# Most of our metadata lives in setup.cfg [metadata]. We put "name" here
# because the setuptools-22.0.5 on slackware can't find it there, which breaks
# packaging. We put "version" here so that Versioneer works correctly, and
# "install_requires" because I don't know how to express the python_version
# clause within the setup.cfg syntax.

setup(
    name="zfec",
    version=versioneer.get_version(),
    description="An efficient, portable erasure coding tool",
    long_description=open('README.rst', 'r').read(),
    url="https://github.com/tahoe-lafs/zfec",
    install_requires=[
        "argparse >= 0.8",
    ],
    extras_require={
        "bench": ["pyutil >= 3.0.0"],
        "test": ["twisted", "setuptools_trial", "pyutil >= 3.0.0"],
    },
    ext_modules=extensions,
    cmdclass=versioneer.get_cmdclass(),
)
