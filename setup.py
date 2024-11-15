from setuptools import setup
from setuptools.extension import Extension

import platform
import sys
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
if platform.machine() == "x86_64" and platform.system().lower().startswith("linux"):
    # Only support CPUs starting from 2008/2009 or so, and run twice as fast as
    # a result! This requires sufficiently new gcc, 11 maybe?
    extra_compile_args.append("-march=x86-64-v2")

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
# packaging. We put "version" here so that Versioneer works correctly.
setup(
    name="zfec",
    version=versioneer.get_version(),
    description="An efficient, portable erasure coding tool",
    long_description=open('README.rst', 'r').read(),
    url="https://github.com/tahoe-lafs/zfec",
    extras_require={
        "bench": ["pyutil >= 3.0.0"],
        "test": ["twisted", "pyutil >= 3.0.0", "hypothesis"],
    },
    ext_modules=extensions,
    cmdclass=versioneer.get_cmdclass(),
)
