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

# Hack to override value detected by pbr which does not look at tags with zfec-
# prefix.
os.environ['PBR_VERSION'] = versioneer.get_version()

# Do not generate the ChangeLog and AUTHORS file
os.environ['SKIP_GENERATE_AUTHORS'] = "1"
os.environ['SKIP_WRITE_GIT_CHANGELOG'] = "1"

setup(
    setup_requires=['pbr'],
    pbr=True,
    ext_modules=extensions
)
