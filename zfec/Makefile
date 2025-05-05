# This is not part of the main build but is included for reference only. 
# It is used to generate tables.c, but that's been done and the result committed to source control.

tables.c:	write.c fec.c fec.h Makefile
	touch tables.c
	gcc -o write write.c && ./write > tables.c
