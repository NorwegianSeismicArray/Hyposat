# Program History

## First version progammed in summer 1996

	Johannes Schweitzer
	Institute of Geophysics
	Ruhr-University Bochum
	D-44780 BOCHUM
	Germany

## Major improvements, corrections or changes since summer 1996:

### 12-15 February 1997

	czo = b means: starting with 'f' and ending with 'd'
	Flow of calculations for oscillating solutions changed, especially for depth determinations.
	Correcting to-time for Wadati formula; included some maximum values for to.
	Handling of dtm changed.
	P1 and S1 included as phase name for (unknown) first P- and S-onsets. The program choose the right name depending on travel-time table and distance.

### 13 March 1997

	Usage of PKiKP instead of PKPdf whenever PKPdf does not exist (for observations around the triplication). Similar changes for P/Pdif and S/Sdif.
	Startsolution always at the closest station, if no backazimuth information is available.

### 23 April 1997

	Some changes in hyposat_gmi to print out the resolution, covariance, correlation, and the information-density matrix.

### Version 2.2 May 8, 1997

	Station corrections included with file station.cor.
	Small bug to calculate dpdh removed.

### Version 3.0 June 2, 1997

	Local velocity model included.
	Checking if oscillating solution is running over 4 solutions.

==============================================================================

## Further Developments since 1 July 1997

All changes and extensions in the whole program package from version 3.0b on:

	Johannes Schweitzer
	NORSAR
	P.O.Box 53
	NO-2027 KJELLER
	Norway

e-mail: johannes.schweitzer@norsar.no

### Version 3.0b July 10, 1997

	Switch diffflag included. If set to .ne.0, no travel-time differences will be used.
 
	July 14, 1997

	Determined phase name only printed if different from input phase name.

	July 18-23, 1997

	Handling of dtm changed again. Now variable rms of resiudals of former solution (only if last change is smaller than 10*setcheck).
	Removing small bug in order of output listing. Smaller changes in output-file.

### Version 3.0c July 25-30, 1997

	Several mistypings removed. Phase-naming changed and logical errors removed.
	Smaller adjustments to relocate REB-events.
	ERROR due to -180 deg / +180 deg border for calculating a mean starting solution removed!
	Iteration also stops if the last change of hypocenter is smaller than 1/1000 of distance to the nearest station 
	(as long as SETCHECK has the default value of 1.).
	All these ditances are measured in [km].

### Version 3.1 August 2-8, 1997

	SDLATG corrected for events in the souther hemisphere.
 	Handling of multiple-core phases changed.
	Backazimuth handling of LR, LQ fixed.
	Calculating standard deviations fixed, if only two observations are available for Wadati-curve.
	Because no ellipticity corrections are available for events deeper than 700 km, we accept a smal error for events between 700 km and 800 km.

### Version 3.2 September, 21 - October, 16 1997

	Plotting removed and the parameter file decoupled from data file. Possibility to give a starting epicenter and its standard errors included.
	Usage of travel-time differences changed.
 
### Version 3.2a October, 29 - November 28, 1997

	Startsolution for station nets corrected. 
	Cases with low information (number of data very small) changed.
	Elevation correction corrected for unknown phase-type. 
	Inversion for underestimated events changed. 
	Handling of unstable depth estimates changed. 
	Reducing the output of 'bad'-solutions.
	In parameter file HEIGHT changed to ELEVATION.
	Missing backazimuth values corrected for LR phases in the REBs.
	Partial derivatives to define the source depth must be at least 0.00001 [s/km] or 0.00001 [(s/deg)/km].
	Comparing 'Wadati-source time' with 'final-source time' to get a better depth in the case of a large mean travel-time residual.
 
	December 16, - December 18, 1997

	Calculating the partial derivatives for 'tau-spline'-models numerically!
	Changed to new 'Flinn-Engdahl' regions.
 
	January 15, 1998

	DTKM adjusted for IASP91 crust.

### Version 3.2b June 17-22, 1998

	Smaller changes for one-phase observation at only one array (station).
 
### Version 3.2c September/October 1998

	Changes for local models (model can be given with file name although no phases are wanted), removing of smaller bugs in calculating 
	the starting source time.
 
### Version 3.2d December 1998

	New parameters in 'hyposat-parameter' and surface wave 'Rg' included using a constant group velocity.
	CSS 3.0 .site file format included.
	COMMON blocks for inversion calls included.

## Version 4

### Version 4.0 March 1999

	CRUST 5.1 after Mooney, Laske, and Masters (JGR, Jan. 1998) included.
	Input-parameter 'touse' included.

	May 1999

	CSS 3.0 .site file format corrected.
	Correction for different structures at reflection points of the Earth's surface.

### Version 4.1 September - January 2001

	Group velocities for LQ, LR, and Lg included. 
	Usage of (Lg-Pn) times for source time estimates included. 

	Epicenter uncertainty ellipse inluded. 
	Start values for source time and its standard deviations included. Travel-time table for local models expanded 
	(pS, sP, PmS, SmP,...).
	Whenever needed, changed from the standard Earth radius to the actual Earth radius as calculated with the standard 
	ellipsoid parameter (see FUNCTION RADLOC).
	HYPOSAT_CROSS corrected: error bars and non-crossing directions removed.
	RMS calculation also for slowness and backazimuth residuals.
	Calculation of magnitudes (Ms or mb) Attenuation model Gutenberg-Richter or Veith-Clawson for mb and for Ms the IASPEI 1967 formula.

### Version 4.2 June 2001 (distributed)

	Possibility to use two different global models during one inversion usage included.
	Handling of oscillating solutions changed and stabilized.

### Version 4.3 August/September 2001 (distributed)

	Calculation of weighted misfit for all data and of the azimuthal gap of defining observations included.
	Ms calculation with Rezapour/Pearce (BSSA 88, 43-61) formula added.

### Version 4.3a November 2001

	Calculation of azimuthal gap corrected, exponential output for large uncertainty ellipsis.

### Version 4.3b January - March 2002

	Plane wave fit included to get better initial solutions in the case of a distant event with respect to the network aperture.
	New version of libtau software (PKPdif!) included.

### Version 4.3c March 2002

	Re-compile and re-check of code for errors and inconsistencies.

### Version 4.3d July 2002

	New input format for starting source time. Usage of different global models extended to four!

### Version 4.4 October 2002 (distributed)

	Weighting of outlayers changed ( see DATMAX0 variable).
	Phase names changed as recommanded by IASPEI working group on phase names in location routine and libtau software, 
	including ellipticity corrections.
	Some other smaller changes and 'bugs' removed.

### Version 4.4a March - May 2003 (distributed)

	New parameter to use azimuth only for the inital solution.
	Single array, single phase solutions improved and corrected.
	Small error for usage of different models corrected.

### Version 4.4b July 2003 (distributed)

	Bug in negative station-elevation correction fixed.

### Version 4.4c September 2003

### Version 4.4d November 2003

	New environment variable 'HYPO_PARA' for non-standard hyposat-parameter files included (see hyposat_file.f).
	All common blocks moved in include files for easier maintenace 
	(i.e., changes in: hyposat.f, hyposat_gmi, hyposat_crust, hyposat_crust_mod.f, hyposat_lib.f, hyposat_gmi.f, 
	hyposat_time.f, and hyposat_loc.f).
	Check for coordinate changes at stations by time (CSS 3.0 Format input only!). 

### Version 4.4e December 2003

	Common block for LSQ corrected.
	Weighting of mean onset-time-residual vs. t0 changed.

#### January 2004

	Mehdi Rezapour's mb distance/depth corrections added.

#### March 2004

	Mean BAZ calculation at one station for single-array initial solution corrected.

### Version 4.5 March-August 2004

	More robust azimuth & ray parameter input.
	New option to list distance in [km].
	Static travel-time corrections for P and S included (see description for file station.cor).
	Option to get theoretical ray parameter and / or backazimuth in output listing.
	Option implemented to use double sided uncertainties for travel-time observations.
	Bugs in dirdel, hyposat_loc and hyposat_crust corrected.
	Function dpythag more systematically included for numerical stability reasons.
	Ellipticity corrections for Pdif and Sdif corrected (wrong since version 4.4).
	New option to constrain a solution by modifying the last iteration: 
	only these data are used which contribute at least with a choosen factor 
	(variable thrfixi) of the most important datum (analysis of information-density matrix) to the solution.

### Version 4.5a February 2005

	Size of uncertainty ellipse changed to two parameter of freedom statistics.
	Parameter for minimum and maximum travel-time difference for Wadati-diagram included.

### Version 4.5b March-April 2005

	Error in handling of information-density matrix (in particular for travel-time difference observations) corrected.
	Standard error for crossing BAZ observations set to a minimum of 5% of distance to closest station.

### Version 4.6 May-July 2005

	ISF formatted input and output included.

### Version 4.6a October 2006

	ISF formatted input cleaned.

### Version 4.6b-e November 2006

	In case of local models: output of Vp in source depth added.
	Fixed depth corrected for # of definings.
	Longitude for initial solution in case of the middle of a network corrected.

### Version 4.6f December 2006

	Smaller bugs removed regarding ISF i/o.

### Version 4.6gFebrury 2007

	ML calculations included.

### Version 4.6h May 2007

	New switch LOCGEO included.

### Version 4.6i June 2007

	Some bugs removed, found by Huang Wen Hui when moving HYPOSAT to MS Windows environment.

## Version 5.0

### Version 5.0 since September 2007

	General upgrade of the Manual and some code changes. 
	Seismic stations below surface for local models included (even deeper than source depth).

### Version 5.02 February 2010

### Version 5.04 November 2010 - 2011

### Version 5.1 November 2011 - October 2012

	ISF formatted I/O changed.

### Version 5.2 July 2013

### Version 5.2a September 2013

### Version 5.2b November 2014

	Reference event included.

### Version 5.2c Spring 2015

	Easier input handling (more default).
	ISF input changed.
	Phase naming in hyposat-parameter file extended.
	Secondary azimuthal gap included hyposat_name.f extended and changed to hyposat_phase.f.
	Additional options included, smaller errors with initial source time handling removed.

### Version 5.2d August 2015

	Multiple onsets can be merged.

### Version 5.2e September/October 2015

	P1 / S1 phase naming corrected.
	Remark: T phases can only be used for BAZ observations.

### Version 5.2f March/April 2016

	Handling of multiple entries changed.
	Some code cleaning.
	New functions UPPCAS & LOWCAS.
	Azimuthal gap moved into a subroutine and new option added.
	Number of stations added to output file.
	ISF event ID changed to 9 digits.
	Some corectios in the case of oscillating solutions.
	Some code cleaning.

### Version 5.2g May 2016

	Phase names for traveltime differences corrected.

### Version 5.2h June/September 2016

	Smaller adjustments for ISF i/o.
	Crust 5.1 input corrected for thin uppermost layer and topography .
	T phases as surface-wave type with constant group velocity added 
	(no raytracing in the ocean layer).

### Version 5.2i October 2016 - March 2017

	Switch to select const. station corrections.
	Corrections for multiple reading entries.
	Distance conversion deg to km corrected for very short distances.
	2ndary azimuthal gap corrected.

	Maximum time residual inferrd as switch to use onset for p/baz/mag.

### Version 5.3 March - April 2017

	libtau-tables changed to ASCII format and limits in ttlim.inc changed.
	libtau cleaned from not needed subroutines.
	TABs removed in all subroutines.
	FUNCTIONS ONERAY and TAUGET_RAY changed into SUBROUTINES.
	Resorting of subroutines/functions in libraries.
	Flag to calculate a location only based on BAZ observations.
	Elevation correction added for ray parameter observations.

### Version 5.3a May - July 2017

	Some changes in hyposat-paramter file handling (correction of one logical error).

### Version 5.4 August - September 2017

	DOUBLE PRECISION -> REAL*8.
	REAL -> REAL*4.
	hyposat_clib.c cleaned from old code.

### Version 5.4a October/November 2017

	i/o errors fixed in isf_i_o.f.
	FESCAN / BAREY / BAREZ & BARENTS16 added as standard programs to MLM data base local model usage 
	instead of MLM for corrections.
	Infrasound onsets are alowed as surface wave onsets with a constant group velocity 
	(no raytracing in the athmosphere).
	libtau package extended for double reflections at the Earth's surface for direct body waves 
	(e.g., PnPnPn, P'P'P, SSS, S'S'S',...).
	Function TRIMLE exchanged against intrinsic functions LEN_TRIM and TRIM (FORTRAN90).
	Split of hyposat_geo in two files: hyposat_geo and hyposat_geotab.

## Version 6.0

### Version 6.0 December 2017 - February 2018

	Review of hyposat-parameter file (consistency check between code and hyposat-parameter file).
	Exchange of CRUST 5.1 with CRUST 1.0.
	hyposat-in format slightly changed (switch 'M' for magnitude usage added).

### Version 6.0a March 2018

	New option added to calculate source emergence angles for all body wave phases.

### Version 6.0b May 2018 (distributed)

	New Manual!.

### Version 6.0c June 2018 (distributed)

	Some i/o related corrections.
	ARID added to hyposat-out & hypomod-out.

### Version 6.0d October 2018 (distributed)

	P1 & S1 slightly changed.
	Error in libtau_h corrected.

### Version 6.0e December 2018

	ISF input corrected.

### Version 6.0f April/May 2019

	Several small correction of code inconsistencies.
	More quality check of input data.

### Version 6.0g July 2019 - June 2020 (distributed)

	ISF output corrected.
	Start solution for single arrays corrected.
	Model uncertainties are now equally distributed between source time, epicenter (delta) and depth uncertainties.
	Some smaller code corrections (numerical stability issues).

### Version 6.1 June 2020 - January 2021 

	Used for ARCBUL paper in SRL September 2021.
	Phase L added and assumed to be an LR-type observation.
	Phase name 'X' added for unknown phase as alternative for IASPEI phase tx.
	ISC/ISF 'EVID' changed to 10 characters.
	MTYPE changed to array and made automatically accessible for usage by different standard models.
	Reading of local/regional model moved to separate subroutines: 
		hyposat_crust_mod.f all model input
		hyposat_crust.ffor crustal corrections (station & reflection)
		hyposat_loc.f TT calculations for crustal and regional phases
	Possible use of ISC default depth or in the middle crust (as defined in CRUST 1.0) added.
	Uncertainties of network magnitudes added.
	pwP-type phases included (accpeted by input and automatically recognized by using Crust 1.0 for 
	correction of reflection points).
	Application of Crust 1.0 changed: no longer interpolation between several points in the case of 
	reflection-point or station corrections.
	Spherical Earth model EK137 added.
	Changes in libtau_h.f for better stability.
	Taking also later onsets with identical phase name in account (i.e., splitted branches or 
	phases wrapped at 180 deg epicentral distance).

### Version 6.1a January 2021 - January 2022 

	Some clean-up in the code .
	More choices for default depths.
	Some phase name checks moved to new subroutine check_phase.
	Reading of ellipticity corrections changed.
	Local/regional model travel-time tables extended for double body wave multiples (e.g., PPP or SnSnSn) .
	Some corrections for uncertainties with single phase locations (i.e., array & ray parameter).
	dddp corrected/adjusted in travel-time routines.
	Ray parameter corrections for reflection-point corrections added.
	ISC-type RMS corrected.
	More concise language in output and Manual (error -> uncertainty / residual; azimuth -> backazimuth).

### Version 6.1b February 2022 - March 2023

	Small bug change in retrieving the right Crust-1.0-model (only for model output).
	Reading of CRUST 1.0-model and ISC-default- depth values optimized.
	Checked and reprogrammed reflection corrections.
	Corrected, new algorithm for calculation of uncertainty ellipses in the lat/lon plane in [km].
	Adding the new ellipticity travel-time corrections of Russel et al. (2022), but keeping PnS 
	from Kennett & Gudmundsson (1996).
	All file-name variables changed to 512 characters.
	Some new steering parameter options added to hyposat-parameter.
	libtau travel-time tables extended with: depth phases (no conversions): pPP, sSS, pPcP, sScS 
	and core multiples: S3KS.
	Proxies for ellipticity corrections checked and extended.
	Magnitudes for PKP-type onsets corrected.
	JSON output included.

### Version 6.1c September 2023

	Small bugs for magnitude output (ISF) and MS definition (R-P) corrected.
	Max magnitude limits included.
	Distancce limits for network magnitudes included.
	Usage of slowness values with high residuals changed.

### Version 6.1d April - June 2024

	Change of default network magnitude calculation Baath ML correction table usage limited .
	Automatic processing switch also changes onset measuring indicator in ISF files.
	Some rule ajustments for 'w' phases.

### Version 6.1e August - January 2025

	Error with weighting (model uncertainties) and other smaller bugs fixed.
	CPQ (see ISC publication) included as measure of location quality.
	hyposat_loc changed to handle better models without Conrad or Moho discontinuities.

### Version 6.1f February - May 2025

	Agreement between HYPOSAT-PARAMETER settings and new Manual carefully checked.
	do-loops closing statement sytematically changed 'continue' or 'enddo'
	Several smaller bugs corrected.

### Version 6.2 June 2025 First GitHub version

	Distribution with newly structured and extended Manual.

### Version 6.2a August 2025 

	For consisttancy reasons Earth figure changed from Stacey (1992) to WGS84.
        Magnitude and net-magnitude calculations moved in own subroutines.

### Version 6.2a1 October 2025
### Version 6.2a2 October 2025

        Some bug fixes in magnitude calculations and JSON output.
