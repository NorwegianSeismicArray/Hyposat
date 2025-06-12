##############################################################
#                                                            #
# Makefile for the program   hyposat                         #
#                                                            #
# July 2013 , Johannes Schweitzer, NORSAR                    #
#                                                            #
#  status: May 2025                                          #
#                                                            #
#  johannes@norsar.no                                        #
#                                                            #
# to be started with C-shell script make_hyposat             #
#                                                            #
##############################################################

DEST = ../bin

prog = hyposat
prog2 = hypomod

obj1 = hyposat.o
obj2 = hyposat_lib.o hyposat_file.o hyposat_cross.o hyposat_geo.o hyposat_geotab.o hyposat_phase.o hyposat_plane.o hyposat_inv.o
obj3 = hyposat_crust.o hyposat_crust_mod.o hyposat_mag.o hyposat_loc.o hyposat_depth.o hyposat_time.o  hyposat_out.o
obj4 = hyposat_numr.o hyposat_clib.o isf_in.o isf_out.o libtau_h.o jsonstorage.o
 
#

objm = hypomod.o
objm2 = hyposat_lib.o hyposat_file.o hyposat_geo.o hyposat_geotab.o hyposat_phase.o 
objm3 = hyposat_crust.o hyposat_crust_mod.o hyposat_mag.o hyposat_loc.o hyposat_depth.o hyposat_time.o 
objm4 = hyposat_numr.o hyposat_clib.o isf_in.o libtau_h.o 

hyposat : $(obj1) $(obj2) $(obj3) $(obj4) 
	$(LD) $(LDPP) $(LFLAG) -o $(prog) $(obj1) $(obj2) $(obj3) $(obj4) 

	mv $(prog) $(DEST)

hypomod : $(objm) $(objm2) $(objm3) $(objm4)
	$(LD) $(LFLAG) -o $(prog2) $(objm) $(objm2) $(objm3) $(objm4)

	mv $(prog2) $(DEST)

.SUFFIXES: .o

.c.o:
	$(CC) $(CFLAG) $(DEBUG) $(OPT) $*.c 

.cpp.o:
	$(CCPP) $(CFLAG) $(CPPFLAG) $(DEBUG) $(OPT) $*.cpp 

.f.o:
	$(F77) $(FFLAG) $(DEBUG) $(OPT) $*.f 

#
#
clean : 
	-rm -f *.o $(prog) $(prog2) core
