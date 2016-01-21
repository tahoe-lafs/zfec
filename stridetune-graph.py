#!/usr/bin/env python

from pyx import *
def g(f):
 g=graph.graphxy(width=16, x=graph.axis.linear(), y=graph.axis.linear())

 g.plot([graph.data.file(f, x=1, y=2)])
 g.writeEPSfile(f+'.eps')

g('stridetune.dat')
