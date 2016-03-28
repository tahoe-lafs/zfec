#! /usr/bin/env python

import errno, locale, os, platform, subprocess, sys, traceback

def foldlines(s, numlines=None):
    lines = s.split("\n")
    if numlines is not None:
        lines = lines[:numlines]
    return " ".join(lines).replace("\r", "")

def print_platform():
    try:
        import platform
        out = platform.platform()
        print
        print "platform:", foldlines(out)
        print "machine: ", platform.machine()
        if hasattr(platform, 'linux_distribution'):
            print "linux_distribution:", repr(platform.linux_distribution())
    except EnvironmentError:
        sys.stderr.write("Got exception using 'platform'. Exception follows\n")
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
        pass

def print_python_ver():
    print "python:", foldlines(sys.version)
    print 'maxunicode: ' + str(sys.maxunicode)

def print_python_encoding_settings():
    print 'filesystem.encoding: ' + str(sys.getfilesystemencoding())
    print 'locale.getpreferredencoding: ' + str(locale.getpreferredencoding())
    try:
        print 'locale.defaultlocale: ' + str(locale.getdefaultlocale())
    except ValueError, e:
        print 'got exception from locale.getdefaultlocale(): ', e
    print 'locale.locale: ' + str(locale.getlocale())

def print_stdout(cmdlist, label=None, numlines=None):
    if label is None:
        label = cmdlist[0]
    try:
        res = subprocess.Popen(cmdlist, stdin=open(os.devnull),
                               stdout=subprocess.PIPE).communicate()[0]
        print label + ': ' + foldlines(res, numlines)
    except EnvironmentError, e:
        if isinstance(e, OSError) and e.errno == errno.ENOENT:
            print str(label) + ': ' + str(cmdlist[0]) + ': no such file or directory'
            return
        sys.stderr.write("\n%s: Got exception invoking '%s'. Exception follows.\n" % (label, cmdlist[0],))
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
        pass

def print_stderr(cmdlist, label=None):
    if label is None:
        label = cmdlist[0]
    try:
        res = subprocess.Popen(cmdlist, stdin=open(os.devnull),
                               stderr=subprocess.PIPE).communicate()[1]
        print label + ': ' + foldlines(res)
    except EnvironmentError, e:
        if isinstance(e, OSError) and e.errno == errno.ENOENT:
            print str(label) + ': ' + str(cmdlist[0]) + ': no such file or directory'
            return
        sys.stderr.write("\n%s: Got exception invoking '%s'. Exception follows.\n" % (label, cmdlist[0],))
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
        pass


def print_as_ver():
    if os.path.exists('a.out'):
        print "WARNING: a file named a.out exists, and getting the version of the 'as' assembler writes to that filename, so I'm not attempting to get the version of 'as'."
        return
    try:
        res = subprocess.Popen(['as', '-version'], stdin=open(os.devnull),
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
        print 'as: ' + foldlines(res[0]+' '+res[1])
        if os.path.exists('a.out'):
            os.remove('a.out')
    except EnvironmentError:
        sys.stderr.write("\nGot exception invoking '%s'. Exception follows.\n" % ('as',))
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
        pass

def print_setuptools_ver():
    try:
        import pkg_resources
        out = str(pkg_resources.require("setuptools"))
        print "setuptools:", foldlines(out)
    except (ImportError, EnvironmentError):
        sys.stderr.write("\nGot exception using 'pkg_resources' to get the version of setuptools. Exception follows\n")
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
        pass
    except pkg_resources.DistributionNotFound:
        print 'setuptools: DistributionNotFound'
        pass

def print_py_pkg_ver(pkgname, modulename=None):
    if modulename is None:
        modulename = pkgname
    print
    try:
        import pkg_resources
        out = str(pkg_resources.require(pkgname))
        print pkgname + ': ' + foldlines(out)
    except (ImportError, EnvironmentError):
        sys.stderr.write("\nGot exception using 'pkg_resources' to get the version of %s. Exception follows.\n" % (pkgname,))
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
        pass
    except pkg_resources.DistributionNotFound:
        print pkgname + ': DistributionNotFound'
        pass
    try:
        __import__(modulename)
    except ImportError:
        pass
    else:
        modobj = sys.modules.get(modulename)
        print pkgname + ' module: ' + str(modobj)
        try:
            print pkgname + ' __version__: ' + str(modobj.__version__)
        except AttributeError:
            pass

print_platform()
print
print_python_ver()
print
print_stdout(['locale'])
print_python_encoding_settings()
print
print_stdout(['buildbot', '--version'])
print_stdout(['buildslave', '--version'])
if 'windows' in platform.system().lower():
    print_stderr(['cl'])
print_stdout(['cc', '--version'], numlines=1)
print_stdout(['gcc', '--version'], numlines=1)
print_stdout(['git', '--version'])
print_stdout(['openssl', 'version'])
print_stdout(['flappclient', '--version'])
print_stdout(['valgrind', '--version'])

print_setuptools_ver()

print_py_pkg_ver('coverage')
print_py_pkg_ver('pyflakes')
print_py_pkg_ver('Twisted', 'twisted')
print_py_pkg_ver('TwistedCore', 'twisted.python')
