# HYPOSAT & HYPOMOD 

## DIRECTORY Hyposat-main. 

HYPOSAT version 6.2a2 and HYPOMOD version 2.2a main directory with following contents


### Subdirectory ./bin/

hyposat 
	HYPOSAT-executable compiled for Linux (Centos 7)

hypomod 
	HYPOMOD-executable compiled for Linux (Centos 7)


### Subdirectory ./data/ 
 
Data files used by HYPOSAT. For details and references see HYPOSAT_Manual.pdf

	ak135_A.hed 
		Model AK135 file
	ak135_A.tbl 
		Model AK135 file
	ek137_A.hed 
		Model EK137 file
	ek137_A.tbl 
		Model EK137 file
	iasp91_A.hed 
		Model IASP91 file
	iasp91_A.tbl 
		Model IASP91 file
	iasp91a_A.hed 
		Model IASP91a file, as IASP91 but different inner core
	iasp91a_A.tbl 
		Model IASP91a file, as IASP91 but different inner core
	jb_A.hed 
		Model Jeffreys-Bullen file
	jb_A.tbl 
		Model Jeffreys-Bullen file
	prem_A.hed 
		Model PREM file
	prem_A.tbl 
		Model PREM file
	sp6_A.hed 
		Model SP6 file
	sp6_A.tbl 
		Model SP6 file
	Plus several regional models in tau-spline file format (*_A.hed & *_A.tbl files).

	elcordir.tbl 
		data file to calculate ellipticity corrections

	stations.dat 
		NEIC formatted station coordinates 
	isc_stalist 
		ISC station list from September 2023

	MB_G-R.DAT 
		Gutenberg-Richter attenuation model for mb
	MB_V-C.DAT 
		Veith-Clawson attenuation model for mb
	MB_M-R.DAT 
		M. Rezapour's attenuation model for mb

	MLCORR.TABLE 
		Markus Baath's attenuation model for Ml 
	MLCORR.TABLE.wa 
		Charles Richter's Wood-Anderson attenuation model for Ml
	MLCORR.TABLE.wa.nicolas 
		Wood-Anderson-type attenuation model for Ml in Europe from Nicolas et al. (1982)

	REG_L3.DAT 
		Seismo-Tectonic Units, Level 3 (for parts of Europe)

	crust1.bnds 
		data file for model CRUST 1.0
	crust1.vp 
		data file for model CRUST 1.0
	crust1.vs 
		data file for model CRUST 1.0

	std_crusts.dat 
		crustal models of different spherically symetric Earth models

	isc_def_depths.dat 
		ISC default depths with 0.5 deg geographical resolution
	grn_default_depth.ak135.dat 
		ISC default depths for Flinn-Engdahl regions

### Subdirectory ./examples/

	README 
		some explanations

#### Examples for hyposat-in files

	hyposat-in.net 
	hyposat-in.regional 
	hyposat-in.single_array 
	hyposat-in.tele 


#### Examples for the corresponding hyposat-out files

	hyposat-out.net 
	hyposat-out.regional 
	hyposat-out.single_array 
	hyposat-out.tele 


#### Example for an ISF formatted output file

	hyposat-isf.net 

#### Examples for the corresponding hyposat-parameter files

	hyposat-parameter.net 
	hyposat-parameter.regional 
	hyposat-parameter.single_array 
	hyposat-parameter.tele 


#### Examples for HYPOMOD

	hypomod-in.tele 
		input file for HYPOMOD
	hyposat-parameter.tele.mod 
		corresponding parameter input file
	hypomod-out.tele 
		output for the HYPOMOD example

#### Other data files

	loc.dat 
		example file for a local/regional velocity model
	stations.cor 
		example file for station corrections


#### Run scripts

	run 
		script to run one of the HYPOSAT example
	run_hypomod_example 
		script to run the HYPOMOD example

### Subdirectory ./documentation/

	4823_Schweitzer1997_hyposat.pdf 
		Schweitzer, J. (1997). Hyposat - A new routine to locate seismic events. 
		NORSAR Sci. Rep. 1-97/98, doi: 10.21348/p.1997.0014 

	NMSOP-3_PD_11.1_june_2018.pdf 
		Schweitzer, J. (2018). Program Manual for HYPOSAT 6.0b (partly outdated) 
		in "New Manual of Observatory Practice", 3rd edition, doi: 10.2312/GFZ.NMSOP-3_PD_11.1

	HYPOSAT_Manual_6.2.pdf 
		Schweitzer, J. (2025). HYPOSAT 6.2 and HYPOMOD 6.2 - The User Manual
		doi: 10.21348/p.2025.0001

	HYPOSAT_HISTORY.md 
		History of main changes in the software package from version to version

	HYPOMOD_HISTORY.md 
		History of main changes in HYPOMOD from version to version

	hyposat-parameter_all 
		A file containing a list of all possible hyposat-parameter settings including their default values 

### Subdirectory ./msrc/ 
 
All source codes to compile your own executives of HYPOSAT and HYPOMOD on Linux and on Windows11 with Intel compilers

#### Source code of the main routines

	hyposat.f 
		HYPOSAT main program tested on Linux with gfortran
 
	hypomod.f 
		HYPOMOD main program tested on Linux with gfortran
 
#### Source code of the subroutines / functions

##### Using C:

	hyposat_clib.c 

##### Using C++:

	jsonstorage.cpp 

##### Using Fortran:

	hyposat_cross.f 
	using Fortran
	hyposat_crust.f
	hyposat_crust_mod.f
	hyposat_depth.f
	hyposat_file.f
	hyposat_geo.f
	hyposat_geotab.f
	hyposat_inv.f
	hyposat_lib.f
	hyposat_loc.f
	hyposat_mag.f
	hyposat_numr.f
	hyposat_out.f
	hyposat_phase.f
	hyposat_plane.f
	hyposat_time.f
	isf_in.f
	isf_out.f
	libtau_h.f

##### Include files used in different parts of the source code

	crust_10.h
	gm2.h
	gmi.h
	isf_head.h
	jsonstorage.h
	lsq.h
	magpar.h
	modelc.h
	modelg.h
	model.h
	phlist.h
	ref.h
	ttimes.h
	ttlim.h

#### Scripts for compilation on Linux systems

	run_make 
		Linux bash-script to run the Makefile for HYPOSAT or HYPOMOD
	Makefile 
		Makefile for HYPOSAT and HYPOMOD on LinuxA with gfortran and gcc

	CMakeLists.txt: CMake setup for builds on Windows and Linux

