c----------------------------------------------------------------------
c  
c          Johannes Schweitzer
c          NORSAR
c          P.O.Box 53
c          N-2027 KJELLER
c          Norway
c
c  e-mail: johannes.schweitzer@norsar.no
c
c----------------------------------------------------------------------
c
c
      program HYPOMOD_2_2

      implicit real*8 (a-h,o-z)
      implicit integer (i-n)

      character  version*25, VDATE*20
      parameter (version='HYPOMOD Version 2.2     ')
c     parameter (vdate=' ( 1 April 2025)' )
      parameter (vdate=' ' )

c
c     last changes:  1 April 2025
c
c----------------------------------------------------------------------
c
c                        Short desciption 
c
c     Short desciption - for more details see HYPOMOD-Manual
c
c     This program calculates for a given seismic event all residuals
c     observed travel times, backazimuth, and slowness values.
c
c     All input and output files are identical to hyposat.
c     See HYPOSAT manual for details. However some features are just
c     ignored because we do not invert any data!
c
c     HYPOMOD 2.1a is based on HYPOSAT 6.1f
c
c--------------------------------------------------------------------
c
c               Program History
c
c         see file hyposat_history!
c
c--------------------------------------------------------------------
c
c            Main Program dependencies:
c
c     calls:     azigap, check_phase, crustc, def_depth, delazd, depi, 
c                dlsq ellcal, ellip, elpcor, epmagc, fetoh2, fhtoe, 
c                findrange, get_mod_c10, get_mod_global, 
c                get_mod_reg, get_station, hyposat_cross, hyposat_geo, 
c                hyposat_gmi, indexx, magfact, mult_ons, 
c                plane, tauget_mod, tauget_ray, testphase, ttloc, zo2to
c
c     functions: alpha1, alpha2, convlat, phase_type, phasw,
c                dirdel, q2, radloc, rdig,
c                file_checkpara, read_event_id, read_origin, 
c                read_origin_head, read_phase_head, read_phase, 
c                write_isf_error, lowcas, uppcas
c
c     data exchange by common blocks in include files:
c                
c                include 'ttimes.h'
c                include 'model.h'
c                include 'modelg.h'
c
c     PARAMETER settings for: mstat, mread
c
c****6789012345678901234567890123456789012345678901234567890123456789012
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c     Functions called: variable definitions
c
      real*8           alpha1, alpha2, convlat, dirdel, q2, 
     +                 radloc, rdig
      character        phase_type*1, file_check*512, file_checkpara*512,
     +                 filepara*512, lowcas*40, uppcas*40, chgcas*40,
     +                 get_mtyp*3, phasw*8

c
c     mstat = maximum number of stations
c
      parameter (mstat = 2000)

      dimension stala(mstat),stalo(mstat),stael(mstat),del(mstat),
     +          azie(mstat),baz(mstat),stavp(mstat),
     +          stavs(mstat),istaph(mstat),stats(mstat),statp(mstat),
     +          delk(mstat),istfil(mstat),statr(mstat),
     +          stamb(mstat),stams(mstat),staml(mstat)

      character*5 sta(mstat),stat,stato,statw,stat1,stationfile*512,
     +          statcorfile*512,outputfile*512,inputfile*512,
     +          magfile*512,magmlfile*512
c
c     mread = maximum number of phases (hypomod-in file)
c
c     mrd2 = maximum number of observations per station
c
c     when changing these parameters remember to change also 
c     gmi.h & gm2.h
c
      parameter (mread = 4000, mrd2 = 100)

      character phase(mread)*8,phaseu(mread)*8,phid*8,used(mread)*6,
     +          phid2*8,text2(mrd2)*160,phid1*8,phase_t*1,
     +          string*210,touse(mread)*9,touse0*9,phidr*8,
     +          o_string*210,textout*160,text(mread)*160,
     +          arid(mread)*8,statcorstr*80, texth*160,phid0*8,
     +          usedm*6,phsearch*1,
     +          useds*6, usedr*1, usedsr*1, stringt*30, onflag(mread)*3

      dimension azi(mread),tt(mread),p(mread),
     +          period(mread),amplit(mread),
     +          iev(mread),indx(mread),
     +          indx2(mrd2),snr(mread),emeran(mread),
     +          stamag(mread),istmag(mread)

      real*4    arr(mread),epiaz(mread),dazgap,d1azi,
     +          d2azi,dazgap2,d1azi2,d2azi2

c
c     variables for travel-time calculations
c
      include 'ttimes.h'

      dimension dpdh(mphas)

      character art*16, mtyp0*3

      real*4 rzo,rdel,razi,rzo1,rdel1,rmcorr, rdelk

      logical first
      real*4  zso
      character modnamo*20
      common /bkin0/first,modnamo,zso

c
c     common blocks for local/regional models reside in the following 
c     three include files (varibales of crust_10.h are not needed in 
c     the main program):
c
c     include 'crust_10.h'
c
      include 'model.h'
      include 'modelg.h'

c
c     parameter nmod is defined in modelg.h
c
      dimension imodn(nmod)
      character*20 modnam(nmod), modn
      logical modflag(nmod)

      character cmod*1
c
c     variables used for ISF data handling
c
      integer   idum1, yyi, moni, ddi, itest, isf_null

      parameter (ISF_NULL=9999999)

      character cdum*1, cdum2*20, author*10, onscha*1,cevid*10, cdum3*2,
     +          phisf*10, isf_ref*10, phidd*10, author2*10, 
     +          corid*8, corid2*8, cpick*1, cpol*1

      real*4    rdum, rpa, ramp, rper, rsnr, rlati, rloni, rdepi,
     +          rdum0

c
c     Functions called from the ISF software libarary
c
      integer   read_event_id, read_origin, read_origin_head, 
     +          read_phase_head, read_phase, write_isf_error

c
c     other variables
c
      integer   yy,mon,dd,hh,idoy,ierr,typctl,mi,idum, isreg, regnum,
     +          y00, mon00, d00, h00, ierc

      character mm*4,name*48
      real*8    lat,lon,dlati,dloni,ddepi, elevs, cpq, cpq2
      real*4    sec, rlat, rlon, smag

      character title*140, czo*1, region*80, czo1*1, magtypp*3, 
     +          magtyps*6, magtypml*7, statmag*2

c
      dimension ttt(mread),tttr(mread)

      logical   vlflag  , stcorfl, iloc, surf, surff, surfm,
     +          diffflag, single , output, modout, 
     +          magflag, wflag,
     +          conr, rayok, 
     +          kmout, thbaz, thray, 
     +          isf_in, freeph, ref_eve,
     +          pflag, lgflag, sgflag, aziflag,
     +          sloflag, isf_epi, 
     +          firstph, first2, ldepth0,
     +          old_syntax, emerout, larid, 
     +          primef, 
     +          eflag , lmaxm

c
c     some constants and initial or default values
c
      first = .true.
      zso   = 0. 
      modnamo = '                    '

      pi      = 4.d0*datan(1.d0)
      deg2rad = pi / 180.d0
      rad2deg = 180.d0 / pi
c
      rearth  = 6371.d0

      grad1   = rad2deg/rearth

      epsilon = 1.0d-9

      ttray   = 0.d0

      cevid  = '9999999999'
      corid  = '_'
      corid2 = ' '
      AUTHOR = 'HYPOMOD'

      vlflag   = .true.
      stcorfl  = .false.
      diffflag = .true.
      iloc     = .false.
      single   = .false.
      output   = .false.
      isf_in   = .false.
      isf_epi  = .false.
      modout   = .false.
   
      kmout    = .false.
      thray    = .false.
      thbaz    = .false.
      locgeo   = .false.
      locsta   = .false.
      locmod   = .false.
      ref_eve  = .false.
      pflag    = .false.
      lgflag   = .false.
      sgflag   = .false.
      aziflag  = .true.
      sloflag  = .true.
      firstph = .false.
      emerout = .false.

      ldepth0 = .false.

      modnamo = '_'
      mtyp   = 'C10'
      mtyp0  = 'E6 '

      modnam(1) = 'ak135_A'
      modflag(1) = .true.
      imodn(1) = 1
      mtype(1)  = mtyp0

      do i=2,nmod
         modnam(i) = modnam(1)
         imodn(i) = 0
         mtype(i)  = mtype(1)
         modflag(i) = .false.
      enddo

      rmax = 0.d0
      rmax0 = 0.d0
      filloc = '_'
      imo     = 0
      stationfile = 'stations.dat'
      outputfile  = 'hypomod-out'
      inputfile   = 'hypomod-in'
      old_syntax = .false.
      statcorfile = ' '
      vpl = 5.8d0
      vsl = 3.46d0
      zo1 =  0.d0
      czo = 'F'
      typctl = 4
      islow = 1
      indph0 = 3333
      epilat0 = -999.d0
      epilon0 = -999.d0
      tome0   = -2840140801.d0

      iwl = 0

      string   = ' '
      o_string = ' '

      isf_ref  = ' '
      disfmod  = 999.d0

      vrg0    = 2.5d0
      vrg     = vrg0

      vlg0    = 3.5d0
      vlg     = vlg0

      vlr0    = 3.95d0
      vlr     = vlr0

      vlq0    = 4.4d0
      vlq     = vlq0

      vt0     = 1.45d0
      vt      = vt0

      vi0     = 0.33d0
      vi      = vi0

      lmaxm   = .true.
      magtypp  = 'G-R'
      magtyps  = 'IASPEI'
      magtypml = 'Bath'
      magmlfile = 'MLCORR.TABLE'

      delmbmin = 0.d0
      delmbmax = 180.d0
      delmsmin = 0.d0
      delmsmax = 180.d0
      delmlmin = 0.d0
      delmlmax = 180.d0

c
c     search file 'hyposat-parameter'
c

      filepara = file_checkpara()
      if(filepara.eq.' ') then
         go to 9999
      else
         open (unit=9,file=filepara,status='old')
      endif

c
c     read in steering parameters from parameter file (if found)
c

      do 1 jin = 1,1000

      read (9,'(a)',end=2) string

      icolon = index(string,':')

      if(icolon.ne.0) then
         icolon1 = icolon-1
         chgcas = uppcas(string(1:icolon1))
         string(1:icolon1) = chgcas(1:icolon1)
      endif

      if(string(1:1).eq.' ') go to 1
      if(string(1:1).eq.'*') go to 1
      if(string(1:1).eq.'?') go to 1

      if(icolon.eq.0) then
         print *,' Wrong syntax (ignored): ',trim(string)
         go to 1
      endif

      if(string(icolon+1:icolon+1).ne.' ') then
         print *,' Wrong syntax (ignored): ',trim(string)
         go to 1
      endif

      icolon2 = icolon+2

      if(string(1:14).eq.'GLOBAL MODEL  ') then
          read (string(icolon2:),'(a)') modnam(1)
          mtype(1) = get_mtyp(modnam(1))
          go to 1
      endif

      if(string(1:14).eq.'GLOBAL MODEL 1') then
          read (string(icolon2:),'(a)') modnam(1)
          mtype(1) = get_mtyp(modnam(1))
          go to 1
      endif

      if(string(1:14).eq.'GLOBAL MODEL 2') then
          read (string(icolon2:),'(a)') modnam(2)
          if(modnam(2) .ne. '_' .and. modnam(2).ne.' ') then
            modflag(2) = .true.
            imodn(2) = 1
            mtype(2) = get_mtyp(modnam(2))
          else
            modflag(2) = .false.
            modnam(2) = modnam(1)
            imodn(2) = 0
          endif
          go to 1
      endif

      if(string(1:14).eq.'GLOBAL MODEL 3') then
          read (string(icolon2:),'(a)') modnam(3)
          if(modnam(3) .ne. '_' .and. modnam(3).ne.' ') then
            modflag(3) = .true.
            imodn(3) = 1
            mtype(3) = get_mtyp(modnam(3))
          else
            modflag(3) = .false.
            modnam(3) = modnam(1)
            imodn(3) = 0
          endif
          go to 1
      endif

      if(string(1:14).eq.'GLOBAL MODEL 4') then
          read (string(icolon2:),'(a)') modnam(4)
          if(modnam(4) .ne. '_' .and. modnam(3).ne.' ') then
            modflag(4) = .true.
            imodn(4) = 1
            mtype(4) = get_mtyp(modnam(4))
          else
            modflag(4) = .false.
            modnam(4) = modnam(1)
            imodn(4) = 0
          endif
          go to 1
      endif

      if(string(1:25).eq.'GLOBAL CRUSTAL MODEL CODE') then
          read (string(icolon2:),'(a)') mtyp0
          go to 1
      endif

      if(string(1:23).eq.'LOCAL OR REGIONAL MODEL') then
          read (string(icolon2:),'(a)') filloc
          go to 1
      endif

      if(string(1:19).eq.'VERY LOCAL GEOMETRY') then
          intinp = 0
          locgeo = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) locgeo = .true.
          go to 1
      endif

      if(string(1:27).eq.'LOCAL STATION BELOW SURFACE') then
          intinp = 0
          locsta = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) locsta = .true.
          go to 1
      endif

      if(string(1:24).eq.'OUTPUT OF REGIONAL MODEL') then
          intinp = 0
          modout = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) modout = .true.
          go to 1
      endif

      if(string(1:9).eq.'CRUST 5.1') then
          print *, 'WARNING: CRUST 5.1 not longer supported,'
          print *, 'we try to use newer CRUST 1.0 instead!'
          string(1:10) = 'CRUST 1.0 '
      endif

      if(string(1:9).eq.'CRUST 1.0') then
          read (string(icolon2:),*) imo
          if(imo.lt.0) imo = 0
          if(imo.gt.6) then
             print *, 'WARNING: Wrong CRUST 1.0 parameter!, set to 0'
             imo = 0
          endif
          go to 1
      endif

      if(string(1:12).eq.'STATION FILE') then
          read (string(icolon2:),'(a)') stationfile
          stationfile = file_check(stationfile)
          go to 1
      endif

      if(string(1:19).eq.'STATION CORRECTIONS') then
          intinp = 0
          vlflag = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) vlflag = .true.
          go to 1
      endif

      if(string(1:23).eq.'STATION CORRECTION FILE') then
          stcorfl = .false.
          read (string(icolon2:),'(a)') statcorfile
          if(len_trim(statcorfile).gt.0 .and.
     +        trim(statcorfile).ne.'_'   ) then
              statcorfile = file_check(statcorfile)
              stcorfl = .true.
          endif
          go to 1
      endif

      if(string(1:27).eq.'STATION CORR ONLY 1ST PHASE') then
          intinp = 0
          firstph = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) firstph = .true.
          go to 1
      endif

      if(string(1:31).eq.'P-VELOCITY TO CORRECT ELEVATION') then
          read (string(icolon2:),*) vpl
          if(vpl.gt.99.d0 .or. vpl.lt.1.d-3) vpl=5.8d0
          go to 1
      endif

      if(string(1:31).eq.'S-VELOCITY TO CORRECT ELEVATION') then
          read (string(icolon2:),*) vsl
          if(vsl.gt.99.d0) vsl=3.46d0
          if(vsl.lt.1.d-3) then
             if(vsl.gt.-1.d-3 .and. (vpl.ge.2.d-3 .and. vpl.le.99.d0))
     +            then
                vsl = vpl / dsqrt(3.d0)
             else
                vsl=3.46d0
             endif
          endif
          go to 1
      endif

      if(string(1:20).eq.'STARTING SOURCE TIME') then
c
c     time formats supported: epochal time 
c          or ASCII formated: yyyy-doy?hh?mi?ss.sss
c                             yyyy-mm-dd?hh?mi?ss.sss
c
c          seconds may be omitted
c
          stringt = string(icolon2:icolon2+29)

          if(stringt(1:1) .eq. '_') go to 1
          if(stringt(1:1) .eq. '0') go to 1
          if(stringt(1:2) .eq. '0.') go to 1

          if(stringt(5:5).ne.'-') then
             read (stringt,*) timein
             if(timein.gt.tome0) tome0 = timein
          else

            mm   = ' '
            idum = 0
            idoy = 0
            mon  = 0
            dd   = 0
            hh   = 0
            mi   = 0
            sec  = 0.

            if(stringt(8:8).ne.'-') then
               if(len_trim(stringt).lt.14) then
                  print *,'Check format for source time in ',
     +                    'hyposat-parameter file'
                  go to 9999
               endif
              read (stringt(1:14),'(i4,x,i3,2(x,i2))') yy,idoy,hh,mi
              if(len_trim(stringt).gt.15) read (stringt(16:),*) sec
            else
              if(len_trim(stringt).lt.16) then
                 print *,'Check format for source time in ',
     +                   'hyposat-parameter file'
                 go to 9999
              endif
              read (stringt(1:16),'(i4,4(x,i2))') yy,mon,dd,hh,mi
              if(len_trim(stringt).gt.17) read (stringt(18:),*) sec
            endif

            if(sec.ge.60.0) then
               print *,'Check format for source time in ',
     +                 'hyposat-parameter file, seconds >= 60'
               print *,'Source time: ',yy,mon,dd,hh,mi,sec
               go to 9999
            endif

            call fhtoe(tome0,idum,yy,mon,mm,dd,idoy,hh,mi,sec)

          endif
          go to 1
      endif

      if(string(1:21).eq.'STARTING SOURCE DEPTH') then
          read (string(icolon2:),*) zo1
          go to 1
      endif

      if(string(1:24).eq.'STARTING SOURCE LATITUDE') then
          abc = -999.0d0
          read (string(icolon2:),*) abc
          if(abc.ge.-90.d0 .and. abc.le.90.d0) epilat0 = abc
          go to 1
      endif

      if(string(1:25).eq.'STARTING SOURCE LONGITUDE') then
          abc = -999.0d0
          read (string(icolon2:),*) abc
          if(abc.ge.-180.d0 .and. abc.le.180.d0) epilon0 = abc
          go to 1
      endif

      if(string(1:12).eq.'OUTPUT LEVEL') then
          read (string(icolon2:),*) typctl
          if(typctl.lt.-1)   typctl = 0
          if(typctl.ge.40)  typctl = 4
          if(typctl.gt.10) then
             itypn = mod(typctl,10)
             typctl = 4
             if(itypn.ge.5) typctl = itypn
          endif
          go to 1
      endif

      if(string(1:27).eq.'FLAG EMERGENCE ANGLE OUTPUT') then
          intinp = 0
          emerout = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) emerout = .true.
          go to 1
      endif

      if(string(1:16).eq.'SLOWNESS [S/DEG]') then
          intinp = 0
          read (string(icolon2:),*) intinp
          if(intinp.eq.0 .or. intinp.eq.1) islow = intinp
          if(isf_in) islow = 1
          go to 1
      endif

      if(string(1:34).eq.'FLAG USING TRAVEL-TIME DIFFERENCES') then
          intinp = 0
          diffflag = .true.
          read (string(icolon2:),*) intinp
          if(intinp.ne.1) diffflag = .false.
          go to 1
      endif

      if(string(1:22).eq.'FLAG FREE PHASE SEARCH') then
          intinp = 0
          freeph = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) freeph = .true.
          go to 1
      endif

      if(string(1:15).eq.'INPUT FILE NAME') then
          if(string(icolon2:icolon2).ne.' ' .and. 
     +       string(icolon2:icolon2) .ne.'_'  )
     +       read (string(icolon2:),*) inputfile
          go to 1
      endif

      if(string(1:21).eq.'HYPOSAT-IN OLD SYNTAX') then
          intinp = 0
          old_syntax = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) old_syntax = .true.
          go to 1
      endif

      if(string(1:16).eq.'INPUT FORMAT ISF') then
          intinp = 0
          isf_in = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) then
             isf_in = .true.
             islow = 1
          endif
          go to 1
      endif

      if(string(1:22).eq.'ISF REFERENCE LOCATION') then
          read (string(icolon2:),'(a)') isf_ref 
          if(isf_ref.eq.'_') isf_ref = ' '
          go to 1
      endif

      if(string(1:13).eq.'ISF EPICENTER') then
          intinp = 0
          isf_epi = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) isf_epi=.true.
          go to 1
      endif

      if(string(1:22).eq.'ISF_2ND MODEL DISTANCE') then
          abc = -999.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0 .and. abc.le.180.d0) disfmod = abc
          go to 1
      endif

      if(string(1:12).eq.'ISF EVENT ID') then
          lc = len_trim(string)
          read (string(icolon2:lc),*) cevid
          lc = len_trim(cevid)
          if(lc.lt.10) then
             cevid = ' '
             cevid(10-lc+1:10) = string(icolon2:icolon2+lc-1)
          endif
          go to 1
      endif

      if(string(1:13).eq.'ISF ORIGIN ID') then
          lc = len_trim(string)
          read (string(icolon2:lc),*) corid
          lc = len_trim(corid)
          if(lc.lt.8) then
             corid = ' '
             corid(8-lc+1:8) = string(icolon2:icolon2+lc-1)
          endif
          go to 1
      endif

      if(string(1:16).eq.'OUTPUT FILE NAME') then
          if(string(icolon2:icolon2).ne.' ' .and. 
     +       string(icolon2:icolon2) .ne.'_'  )
     +       read (string(icolon2:),*) outputfile
          go to 1
      endif

      if(string(1:13).eq.'OUTPUT SWITCH') then
          intinp = 0
          output = .true.
          read (string(icolon2:),*) intinp
          if(intinp.ne.1) output=.false.
          go to 1
      endif

      if(string(1:12).eq.'OUTPUT IN KM') then
          intinp = 0
          kmout = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) kmout = .true.
          go to 1
      endif

      if(string(1:21).eq.'OUTPUT OF THEO. BAZ+P') then
          intinp = 0
          thbaz = .false.
          thray = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) thbaz = .true.
          if(intinp.eq.2) thray = .true.
          if(intinp.eq.3) then
             thbaz = .true.
             thray = .true.
          endif
          go to 1
      endif

      if(string(1:18).eq.'AUTHOR OF SOLUTION') then
          read (string(icolon2:),'(a)') author
          go to 1
      endif

      if(string(1:17).eq.'LG GROUP-VELOCITY') then
         read (string(icolon2:),*) vlg
         if(vlg.le.0.d0) vlg = vlg0
         go to 1
      endif

      if(string(1:17).eq.'RG GROUP-VELOCITY') then
          read (string(icolon2:),*) vrg
          if(vrg.le.0.d0) vrg = vrg0
          go to 1
      endif

      if(string(1:17).eq.'LR GROUP-VELOCITY') then
          read (string(icolon2:),*) vlr
          if(vlr.le.0.d0) vlr = vlr0
          go to 1
      endif

      if(string(1:17).eq.'LQ GROUP-VELOCITY') then
          read (string(icolon2:),*) vlq
          if(vlq.le.0.d0) vlq = vlq0
          go to 1
      endif

      if(string(1:17).eq.'T-PHASE GROUP-VEL' .or.
     +   string(1:17).eq.'T PHASE GROUP-VEL') then
          read (string(icolon2:),*) vt
          if(vt.le.0.d0) vt = vt0
          go to 1
      endif

      if(string(1:18).eq.'IS-PHASE GROUP-VEL' .or.
     +   string(1:18).eq.'IS PHASE GROUP-VEL') then
          read (string(icolon2:),*) vi
          if(vi.le.0.d0) vi = vi0
          go to 1
      endif

      if(string(1:18).eq.'WATER LAYER ON TOP') then
          intinp = 0
          read (string(icolon2:),*) intinp
          if(intinp.gt.0 .and. intinp.lt.6) iwl = intinp
          go to 1
      endif

      if(string(1:21).eq.'MAGNITUDE CALCULATION') then
          intinp = 0
          magflag = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) magflag = .true.
          go to 1
      endif

      if(string(1:22).eq.'ALL STATION MAGNITUDES') then
          intinp = 0
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) lmaxm = .false.
          go to 1
      endif

      if(string(1:19).eq.'MIN DISTANCE FOR MS') then
          read (string(icolon2:),*) abc
          if(abc.ge.0.d0) delmsmin = abc
          go to 1
      endif

      if(string(1:19).eq.'MIN DISTANCE FOR MB') then
          read (string(icolon2:),*) abc
          if(abc.ge.0.d0) delmbmin = abc
          go to 1
      endif

      if(string(1:19).eq.'MIN DISTANCE FOR ML') then
          read (string(icolon2:),*) abc
          if(abc.ge.0.d0) delmlmin = abc
          go to 1
      endif

      if(string(1:19).eq.'MAX DISTANCE FOR MS') then
          read (string(icolon2:),*) abc
          if(abc.ge.0.d0) delmsmax = abc
          go to 1
      endif

      if(string(1:19).eq.'MAX DISTANCE FOR MB') then
          read (string(icolon2:),*) abc
          if(abc.ge.0.d0) delmbmax = abc
          go to 1
      endif

      if(string(1:19).eq.'MAX DISTANCE FOR ML') then
          read (string(icolon2:),*) abc
          if(abc.ge.0.d0) delmlmax = abc
          go to 1
      endif

      if(string(1:19).eq.'P-ATTENUATION MODEL' .or.
     +   string(1:17).eq.'ATTENUATION MODEL'    ) then
          read (string(icolon2:),*) magtypp
          go to 1
      endif

      if(string(1:20).eq.'MS-ATTENUATION MODEL' .or.
     +   string(1:19).eq.'S-ATTENUATION MODEL') then
          read (string(icolon2:),*) magtyps
          go to 1
      endif

      if(string(1:20).eq.'ML-ATTENUATION MODEL') then
          read (string(icolon2:),*) magtypml
          go to 1
      endif

      if(string(1:18).eq.'ML-CORRECTION FILE') then
          read (string(icolon2:),*) magmlfile
          go to 1
      endif

      if(string(1:15).eq.'REFERENCE EVENT') then
          intinp = 0
          ref_eve = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) ref_eve = .true.
          go to 1
      endif

      if(string(1:26).eq.'REFERENCE SOURCE LONGITUDE') then
          abc = -999.0d0
          read (string(icolon2:),*) abc
          if(abc.ge.-180.d0 .and. abc.le.180.d0) dloni = abc
          go to 1
      endif

      if(string(1:25).eq.'REFERENCE SOURCE LATITUDE') then
          abc = -999.0d0
          read (string(icolon2:),*) abc
          if(abc.ge.-90.d0 .and. abc.le.90.d0) dlati = abc
          go to 1
      endif

      if(string(1:22).eq.'REFERENCE SOURCE DEPTH') then
          read (string(icolon2:),*) ddepi
          go to 1
      endif

1     continue

2     close(9)

      if(typctl.gt.4) then
         print *,'modnam  : ',modnam(1)
         if(modflag(2)) print *,'modnam 2: ',modnam(2)
         if(modflag(3)) print *,'modnam 3: ',modnam(3)
         if(modflag(4)) print *,'modnam 4: ',modnam(4)
         print *,'filloc = ',filloc
         print *,'imo = ',imo
         print *,'stationfile = ',stationfile
         print *,'statcorfile = ',statcorfile 
         print *,'inputfile   = ',inputfile 
         print *,'outputfile  = ',outputfile 
         print *,'output switch ',output 
         print *,'vpl = ',vpl
         print *,'vsl = ',vsl
         print *,'vrg = ',vrg
         print *,'vlg = ',vlg
         print *,'vlq = ',vlq
         print *,'vlr = ',vlr
         print *,'vt  = ',vt 
         print *,'zo1   = ',zo1
         print *,'epilat0 = ',epilat0
         print *,'epilon0 = ',epilon0
         print *,'typctl = ',typctl
         print *,'islow = ',islow
         print *,'diffflag = ',diffflag
         print *,'Magnitude flags = ', magflag,' ',magtypp,' ',magtyps
     +           ,' ',magtypml,' ',lmaxm
      endif

c
c     reading ellipticity correction coefficients
c
      eflag = .false.
      du0 = 0.d0
      du1 = 0.d0
      idum = 1
      call elpcor('P',du0,du0,du0,du0,du1,eflag,idum)
c
c     initializing gobal model
c

      rzo1  = 0.
      rdel1 = 105.
      nphas = -999
      call tauget_mod(rzo1,rdel1,nphas,phcd,ttc,dtdd,dtdh,dddp,
     +                modnam(1))
c
      do 21 i=1,nmod

         if(imodn(i).eq.0) go to 21

         mtypg = mtype(i)
         
         call get_mod_global(typctl,ierr)

         if(ierr.ne.0) then

            write(*,'('' Cannot use crustal model corrections for '',
     +                a3,1x,a)') mtypg,modnam(i)

            if(i.le.1) then

               go to 9999

            else

               write(*,'('' We use instead '',a3,1x,a)') mtypg,modnam(1)
               ierr = 0

               jmodg(i) = jmodg(1)
               elevg(i) = elevg(1)
               zmaxg(i) = zmaxg(1)
               do 18 j = 1,jmodg(1)
                  v0g(i,1,j) = v0g(1,1,j)
                  v0g(i,2,j) = v0g(1,2,j)
                  zg(i,j)    = zg(1,j)
                  if(azo(j) .eq.'CONR') zconr(i) = zg(1,j)
                  if(azo(j) .eq.'MOHO') zmoho(i) = zg(1,j)
18             continue

            endif

         else
c
c     save the found Global Crustal Model in v0g / zg / ...
c
            jmodg(i) = jmod
            elevg(i) = elev
            zmaxg(i) = zmax
            do 19 j = 1,jmod
               v0g(i,1,j) = v0(1,j)
               v0g(i,2,j) = v0(2,j)
               zg(i,j)    = z(j)
               if(azo(j) .eq.'CONR') zconr(i) = z(j)
               if(azo(j) .eq.'MOHO') zmoho(i) = z(j)
19          continue
         endif

21    continue

      if(imo.eq.3 .or. imo.eq.4) then

         itrue = 0
         inum  = 1
         elatc = 0.d0
         elonc = 0.d0
         filloc = 'CRUST 1.0'
         mtyp = 'C10'

         call get_mod_c10(itrue,inum,typctl)

         iloc = .true.
         rmax0 = rmax

      else if(imo.eq.1 .or. imo.eq.2 .or. imo.eq.5) then

         if(len_trim(filloc).gt.0 .and. indph0.ge.0 .and.
     +      filloc(1:1).ne.'_'                ) then
         
            filloc = file_check(filloc)
            ierr   = 0
            rzo    = 0.
            rdel   = 0.
            czo1   = ' '
            indph  = 0
            elatc  = 0.d0
            elonc  = 0.d0
            sdep   = 0.d0
            rmax = 0.d0
            jmod = 0
            dconr = 0.d0
            dmoho = 0.d0

            nphas = 0
            mtyp = 'REG'

            call ttloc(rzo,rdel,czo1,nphas,ttc,dtdd,dtdh,dpdh,dddp,
     +                 phcd,sdep,typctl,ierr,indph,emerout,dconr,dmoho)

            if(ierr.ne.0) then 
               write(*,'('' Local/Regional Model Error: '',a)') filloc
               go to 9999
            endif

            rmax0 = rmax
            iloc = .true.

         else

            indph0 = 0
            rmax0 = 0.d0
            mtyp = 'C10'
            iloc = .false.
            
         endif
      
      endif

      if(ldepth0 .and. .not.iloc) then

         print *,'Source depth above sea level only possible for ',
     +           'receivers and source within local/regional ',
     +           'model distance!!!'
         go to 9999

      endif

      istcor = 0
      if(stcorfl) open(13,file=statcorfile)

      do 3 i=1,mstat
      sta(i) = ' '
3     continue

c
c     read in all available observed data
c

      open (unit=10,file=inputfile,status='old')

      title = trim(version) // trim(vdate)

      if(output) then
         open (unit=11,file=outputfile)
         write (11,'(a,/)') trim(title)
         write (11,'(''Event solution by '',a,/)') 
     +          trim(author)
      endif

      if(typctl.ge.0) then
         print *,'PROGRAM ',trim(title)
      endif

31    read (10,'(a)',end=9999) title

      if(isf_in) then

         chgcas = uppcas(title(1:6))
         if(chgcas(1:6).ne.'EVENT ') go to 31
         lc = len_trim(cevid)
         if(cevid(1:lc).eq.'         _' .or. cevid(1:10).eq.'9999999999'
     +      .or. lc.lt.1)     then
            itest = read_event_id(title(1:80),cevid,cdum2) 
            if(itest.eq.20) then
               itest = write_isf_error(typctl)
               go to 9999
            endif
         endif

         if(output) write (11,'(''ISF EVENT ID: '',a10,/)') cevid
   
32       read (10,'(a)',end=9999) string
         itest = read_origin_head(string(1:136))
         if(itest.eq.20) go to 32 

         author2 = ' '
         primef = .false.

33       o_string = string
         read (10,'(a)',end=9999) string

         if(isf_ref.eq.'PRIME') then
            if(string(1:9).eq.' (#PRIME)' .or.
     +          len_trim(string).le.1) then
               string = o_string
               primef = .true.
               go to 331
            endif
         endif

         if(string(1:2).eq.' (' .or. string(5:5).ne.'/') then
            string = o_string
            go to 33
         endif

331      itest = read_origin(string(1:136),yyi,moni,ddi,hh,mi,isec,msec,
     +   cdum,rdum,rdum,rlati,rloni,cdum,rdum,rdum,idum1,rdepi,
     +   cdum,rdum,idum1,idum1,idum1,rdum,rdum,cdum,cdum,
     +   cdum3,author2,corid2)

         if(itest.eq.20) then 
            itest = write_isf_error(typctl)
            go to 33
         endif

         if(author2.ne.isf_ref .and. isf_ref.ne.' ' .and.
     +      .not.primef) go to 33

         if((corid.eq.' ' .or. corid.eq.'_') .and.
     +      (corid2.ne.' ' .and. corid2.ne.'_') ) corid = corid2

         dlati = dble(rlati)
         dloni = dble(rloni)
         if(rdepi.eq.real(ISF_NULL)) rdepi = 0.
         ddepi = dble(rdepi)

         sec = real(isec)
         if(msec.ne.isf_null) then
            sec = sec + real(msec)/1000.
         endif
         mm = ' '
         idoy = 0
         jdate = 0
         timeoi = 0.d0
         call fhtoe(timeoi,jdate,yyi,moni,mm,ddi,idoy,hh,mi,sec)

         if(isf_epi) then
            epilat0 = dlati
            epilon0 = dloni
            tome0   = timeoi
            zo1     = ddepi
         endif

34       read (10,'(a)',end=9999) string
         itest = read_phase_head(string(1:122))

         if(itest.eq.20) then 
            go to 34
         endif

      else
         if(output) write (11,'(a,/)') trim(title)
         print *,'EVENT ',trim(title)
      endif

      timemin = 9999999999.d0

      terrm = 0.d0

      isnr = 0

      ii = 0
      string = ' '

      do 12 i=1,mread+200

      read (10,'(a)',end=14) string

      if(string.eq.o_string) go to 12
      if(string(1:4).eq.'STOP') go to 14
      if(string(1:1).eq.'*') go to 12
      if(string(1:1).eq.' ') go to 12
      if(string.eq.' ') go to 12

      ii = ii + 1

      if(ii.gt.mread) then
        print *,'Maximum number of input data reached: ',mread
        go to 9999
      endif

      ierr = 0

4     continue

      azi(ii) = -999.d0
      p(ii)   = -999.d0
      pin     = -999.d0
      snr(ii) = -999.d0
      amplit(ii) = -999.d0
      period(ii) = -999.d0
      touse0  = 'TASDRM1  '
      arid(ii)= ' '

      if(.not.isf_in) then

          lstring = len_trim(string)

          if(lstring.le.34) then
             go to 5
          else if(lstring.eq.35) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f3.0)',err=5)
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec
          else if(lstring.eq.36) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f4.1)',err=5)
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec
          else if(lstring.eq.37) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f5.2)',err=5)
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec
          else if(lstring.eq.38) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f6.3)',err=5)
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec
          else if(lstring.ge.69) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f6.3,1x,
     +             f5.3,1x,f6.2,3(1x,f5.2))',err=5) 
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec,
     +             rdum,azi(ii),rdum,pin
          else if(lstring.ge.63) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f6.3,1x,
     +             f5.3,1x,f6.2,2(1x,f5.2))',err=5) 
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec,
     +             rdum,azi(ii),rdum,pin
          else if(lstring.ge.57) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f6.3,1x,
     +             f5.3,1x,f6.2,f5.2)',err=5) 
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec,
     +             rdum,azi(ii)
          else if(lstring.ge.51) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f6.3,1x,
     +             f5.3,1x,f6.2)',err=5) 
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec,
     +             rdum,azi(ii)
          else if(lstring.ge.44) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f6.3,1x,
     +             f5.3)',err=5) 
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec
          else
             go to 5
          endif

          if(string(47:53).eq.'       ') azi(ii) = -999.d0
          if(string(60:66).eq.'       ') pin = -999.d0

          ipos = 71
          ipo2 = ipos + 6
          if(old_syntax) ipo2 = ipos + 5

          if(lstring.ge.ipos) then

             if(old_syntax) then
                read (string(ipos:ipo2),'(a6)',err=5) touse0
                touse0(7:7) = touse0(6:6)
                touse0(6:6) = 'M'
             else
                read (string(ipos:ipo2),'(a7)',err=5) touse0
             endif

             chgcas = uppcas(touse0)
             touse0 = chgcas(1:7) // '  '

          endif

          ipos = ipo2 + 2

          if(lstring.ge.ipos) then
             ipo2 = ipos + 5
             read (string(ipos:ipo2),'(f6.3)',err=5) abc
             if(abc.gt.0.d0) period(ii)= abc
          endif

          ipos = ipo2 + 2

          if(lstring.ge.ipos) then
             ipo2 = ipos + 11
             read (string(ipos:ipo2),'(f12.2)',err=5) abc
             if(abc.gt.0.d0) amplit(ii)= abc
          endif

          ipos = ipo2 + 2
          if(lstring.ge.ipos) then
             ipo2 = ipos + 6
             read (string(ipos:ipo2),'(f7.2)',err=5) abc
             if(abc.gt.0.d0) snr(ii)= abc
          endif

          ipos = ipo2 + 2
          if(lstring.ge.ipos) then
             ipo2 = ipos + 7
             read (string(ipos:ipo2),'(a8)',err=5) arid(ii)
          endif

          if(ii.eq.1) then
             y00   = yy
             mon00 = mon
             d00   = dd
             h00   = hh
          else
             if(string(16:19).eq.'    ') yy = y00
             if(string(21:22).eq.'  ')   mon = mon00
             if(string(24:25).eq.'  ')   dd = d00
             if(string(27:28).eq.'  ')   hh = h00
          endif

          ierr = 0
          go to 6 

5         continue

          if(ierr.le.1) then

             indcs= 200
             indcs = index(string,'#')
             if(indcs.le.120 .and. indcs.gt.0) then
                string(indcs:indcs)=' '
                go to 5
             endif
             ierr = ierr + 1
             go to 4

          else

             ii = ii - 1
             go to 12

          endif
      
      else

          onscha = ' '
          cpol = '_'

          if(string(21:23).eq.' DI') string(21:23) = '_DI'
          itest = read_phase (string(1:123),stat,rdum0,rdum,phisf,hh,mi,
     +    isec,msec,rdum,razi,rdum,rpa,rdum,touse0(1:1),touse0(2:2),
     +    touse0(3:3),rsnr,ramp,rper,cpick,cpol,onscha,cdum2,cdum,
     +    smag,arid(ii))

          if(itest.eq.20) then 
             itest = write_isf_error(typctl)
             ii = ii - 1
             go to 12
          endif

          if(hh+mi+isec+msec.eq.4*isf_null) then
             ii = ii - 1
             go to 12
          endif

          chgcas = lowcas(onscha)
          onscha = chgcas(1:1)

          if(onscha.ne.'i' .and. onscha.ne.'e' .and. onscha.ne.'q') 
     +       onscha='_'

          sec = real(isec)
          if(msec.ne.isf_null) then
             sec = sec +real(msec)/1000.
          else
             msec = 0
          endif

          mm = ' '
          idoy = 0
          jdate = 0
          timeop = 0.d0
          call fhtoe(timeop,jdate,yyi,moni,mm,ddi,idoy,hh,mi,sec)

          if( (timeop.lt.timeoi) .and.
     +        (timeop+86400.d0-timeoi.lt.7200.d0) ) then
              timeop = timeop + 86400.d0
          endif
          
          call fetoh2(timeop,idum,yy,mon,mm,dd,idoy,hh,mi,sec)

          if(phisf(1:1).eq.'(') then
             phidd=phisf(2:)
             ip = len_trim(phidd)
             if(phidd(ip:ip).eq.')') then
                phisf=phidd(1:ip-1)
             else
                phisf=phidd
             endif
          endif

          chgcas = uppcas(phisf(1:1))
          if(chgcas(1:1).eq.'I') then
             phidd=phisf(2:)
             ip = len_trim(phidd)
             if(ip.gt.1) then
                phisf=phidd(1:ip-1)
             else
                phisf='x'
             endif
             onscha='i'
          else if(chgcas(1:1).eq.'E') then
             phidd=phisf(2:)
             ip = len_trim(phidd)
             if(ip.gt.1) then
                phisf=phidd(1:ip-1)
             else
                phisf='x'
             endif
             onscha='e'
          endif

          onflag(ii)(1:1) = cpick
          onflag(ii)(2:2) = cpol
          onflag(ii)(3:3) = onscha

          chgcas = uppcas(phisf(1:3))
          if(phisf(1:2).eq.'P ') phisf='P1'
          if(chgcas(1:3).eq.'PN ') phisf='P1'
          if(phisf(1:3).eq.'P* ') phisf='Pb'
          if(chgcas(1:3).eq.'PG1') phisf='Pg '

          if(phisf(1:2).eq.'S ') phisf='S1'
          if(chgcas(1:3).eq.'SN ') phisf='S1'
          if(phisf(1:3).eq.'(S)') phisf='S1'
          if(phisf(1:3).eq.'S* ') phisf='Sb'
          if(chgcas(1:3).eq.'SG1') phisf='Sg '
          if(phisf(1:4).eq.'TSG1') phisf='Sg  '
          if(phisf(1:4).eq.'*ESG1') phisf='Sg   '

          if(phisf(1:9).eq.'(S)/(SKS)') phisf='S1'
          if(phisf(1:7).eq.'(S)/SKS') phisf='S1'
          if(phisf(1:7).eq.'S/(SKS)') phisf='S1'
          if(phisf(1:5).eq.'S/SKS') phisf='S1'

          if(phisf(1:3).eq.'SKS') phisf='S1'
          if(phisf(1:4).eq.'SKSa') phisf='S1'
          if(phisf(1:4).eq.'SKS2') phisf='SKSdf'
          if(phisf(1:4).eq.'sSKS') phisf='sSKSac'
          if(phisf(1:4).eq.'pSKS') phisf='pSKSac'

          if(phisf(1:4).eq.'SKKS') phisf='SKKSac'
          if(phisf(1:5).eq.'SKKS2') phisf='SKKSdf'
          if(phisf(1:5).eq.'SKKKS') phisf='S3KSac'
          if(phisf(1:6).eq.'SKKKS2') phisf='S3KSdf'

          if(phisf(1:4).eq.'SKP ') phisf='SKPab'
          if(phisf(1:5).eq.'SKP2 ') phisf='SKPdf'
          if(phisf(1:5).eq.'SKKP ') phisf='SKKPab'
          if(phisf(1:6).eq.'SKKP2 ') phisf='SKKPdf'

          if(phisf(1:6).eq.'P_DIFF') phisf='Pdif'
          if(phisf(1:5).eq.'P_DIF') phisf='Pdif'

          if(phisf(1:6).eq.'S_DIFF') phisf='Sdif'
          if(phisf(1:5).eq.'S_DIF') phisf='Sdif'

          if(phisf(1:4).eq.'PKP ') phisf='PKPdf'
          if(phisf(1:5).eq.'pPKP ') phisf='pPKPdf'
          if(phisf(1:5).eq.'sPKP ') phisf='sPKPdf'
          if(phisf(1:5).eq.'PKP2 ') phisf='PKPab'
          if(phisf(1:6).eq.'pPKP2 ') phisf='pPKPab'
          if(phisf(1:6).eq.'sPKP2 ') phisf='sPKPab'

          if(phisf(1:4).eq.'PKS ') phisf='PKSdf'
          if(phisf(1:5).eq.'PKS2 ') phisf='PKSab'

          if(phisf(1:6).eq.'PKKP ') phisf='PKKPdf'
          if(phisf(1:6).eq.'PKKP2 ') phisf='PKKPbc'
          if(phisf(1:6).eq.'PKKP3 ') phisf='PKKPab'

          if(phisf(1:6).eq.'PKKS ') phisf='PKKSdf'
          if(phisf(1:6).eq.'PKKS2 ') phisf='PKKSbc'
          if(phisf(1:6).eq.'PKKS3 ') phisf='PKKSab'

          if(phisf.eq.'PKHKP')  phisf='PKPpre'
          if(phisf.eq.'PKhKP')  phisf='PKPpre'

          chgcas = uppcas(phisf(1:1))
          if(chgcas(1:1).ne.'P' .and.  
     +       chgcas(1:1).ne.'S' .and. 
     +       chgcas(1:1).ne.'L' .and. 
     +       chgcas(1:1).ne.'R' .and. 
     +       chgcas(1:1).ne.'X' .and. 
     +       chgcas(1:2).ne.'IS' .and. 
     +       chgcas(1:1).ne.'T'   ) then

             ii = ii - 1
             go to 12

          endif

          if(phisf(1:1).eq.'p' .or. phisf(1:1).eq.'s') touse0(5:5) = 'R'

          phase(ii) = phisf(1:8)

          if(rpa.ne.real(isf_null)) then
              pin = dble(rpa)
c             touse0(3:3) = 'S'
          else
              pin    = -999.d0
              touse0(3:3) = ' '
          endif

          if(razi.ne.real(isf_null)) then
              azi(ii)  = dble(razi)
c             touse0(2:2) = 'A'
          else
              azi(ii)  = -999.d0
              touse0(2:2) = ' '
          endif

          if(rsnr.ne.real(isf_null)) then
              snr(ii) = dble(rsnr)
          else
              snr(ii) = -999.d0
          endif

          if(ramp.ne.real(isf_null)) then
              amplit(ii) = dble(ramp)
          else
              amplit(ii) = -999.d0
          endif

          if(rper.ne.real(isf_null)) then
              period(ii) = dble(rper)
          else
              period(ii) = -999.d0
          endif

          if(smag.gt.0.) touse0(6:6) = 'M'

          if(diffflag) touse0(4:4) = 'D'

          if(imo.gt.0 .and. imo.ne.3) touse0(5:5)='R'

      endif

6     mm = ' '
      idoy = 0
      jdate = 0
      timeo = 0.d0
c     print*, timeo,jdate,yy,mon,mm,dd,idoy,hh,mi,sec
      call fhtoe(timeo,jdate,yy,mon,mm,dd,idoy,hh,mi,sec)
      tt(ii) = timeo

      incap =  index(phidd,'w')
      if(incap.gt.0) then
         if(iwl.gt.0) touse0(5:5)='R'
      endif

      touse(ii)=   'TASDRM1  '

      if(touse0.ne.'         ') then
         if(touse0(1:1).ne.'T') touse0(1:1)=' '
         if(touse0(2:2).ne.'A') touse0(2:2)=' '
         if(touse0(3:3).ne.'S') touse0(3:3)=' '
         if(touse0(4:4).ne.'D') touse0(4:4)=' '
         if(touse0(5:5).ne.'R') touse0(5:5)=' '
         if(touse0(6:6).ne.'M') touse0(6:6)=' '
         if(touse0(7:7).ne.'1' .and. touse0(7:7).ne.'2' .and.
     +      touse0(7:7).ne.'3' .and. touse0(7:7).ne.'4') touse0(7:7)='1'

         if(pflag.and.phase_type(phase(ii)).ne.'P') 
     +      touse0(1:6) = '    RM'
         touse(ii)=touse0

      endif

      chgcas = uppcas(phase(ii)(1:2))
      if(chgcas(1:2).eq.'TX')    phase(ii)='tx tx'
      if(chgcas(1:2).eq.'SX')    phase(ii)='tx Sx'
      if(chgcas(1:2).eq.'PX')    phase(ii)='tx Px'
      if(chgcas(1:2).eq.'X ')    phase(ii)='tx x '
      if(chgcas(1:2).eq.'RX')    phase(ii)='tx rx'
      if(phase(ii)(1:2).eq.'p ') phase(ii)='tx Px'
      if(phase(ii)(1:2).eq.'s ') phase(ii)='tx Sx'

      chgcas = uppcas(phase(ii)(1:2))
      if(chgcas(1:1).ne.'P' .and. 
     +   chgcas(1:1).ne.'S' .and. phase(ii)(1:1).ne.'R' .and.
     +   phase(ii)(1:1).ne.'L'  .and. phase(ii)(1:2).ne.'IS'.and. 
     +   chgcas(1:1).ne.'T') touse(ii)(1:4) = ' A  '

      if(phase(ii)(1:1).eq.'R' .or. phase(ii)(1:2).eq.'LQ' .or.
     +   phase(ii)(1:2).eq.'LR' .or. phase(ii)(1:2).eq.'IS' .or.
     +   phase(ii)(1:2).eq.'T ') touse(ii)(3:3) = ' '

      if(.not.aziflag) touse(ii)(2:2) = ' '
      if(.not.sloflag) touse(ii)(3:3) = ' '

      if(phase(ii)(1:4).eq.'PKP ') phase(ii)='PKPdf'
      if(phase(ii).eq.'PKPPKP') phase(ii)="P'P'"
      if(phase(ii).eq.'SKS2')   phase(ii)="S'S'"
      if(phase(ii).eq.'SKSSKS') phase(ii)="S'S'"
      if(phase(ii).eq.'P3')     phase(ii)='PPP'
      if(phase(ii).eq.'S3')     phase(ii)='SSS'
      if(phase(ii).eq.'PKhKP')  phase(ii)='PKPpre'

62    phidd = phase(ii)

      incap = index(lowcas(phidd),'diff') 
      if(incap.ne.0) then 
         phase(ii)(incap:) = 'dif' // phidd(incap+4:)
         go to 62
      endif
      incap =  index(phidd,'N')
      if(incap.ne.0) then
         phase(ii)(incap:incap) = 'n'
         go to 62
      endif
      incap =  index(phidd,'B')
      if(incap.ne.0) then
         phase(ii)(incap:incap) = 'b'
         go to 62
      endif
      incap =  index(phidd,'A')
      if(incap.ne.0) then
         phase(ii)(incap:incap) = 'a'
         go to 62
      endif
      incap =  index(phidd,'G')
      if(incap.ne.0) then
         phase(ii)(incap:incap) = 'g'
         go to 62
      endif
      incap =  index(phidd,'C')
      if(incap.ne.0) then
         phase(ii)(incap:incap) = 'c'
         go to 62
      endif
      incap =  index(phidd,'M')
      if(incap.ne.0) then
         phase(ii)(incap:incap) = 'm'
         go to 62
      endif
      incap =  index(phidd,'D')
      if(incap.ne.0) then
         phase(ii)(incap:incap) = 'd'
         go to 62
      endif
      incap =  index(phidd,'F')
      if(incap.ne.0) then
         phase(ii)(incap:incap) = 'f'
         go to 62
      endif
      incap =  index(phidd,'l')
      if(incap.eq.1) then
         phase(ii)(incap:incap) = 'L'
         go to 62
      endif

      if(lgflag.and.phidd.eq.'Lg'.and..not.sgflag) phase(ii)='Sg'
      if(sgflag.and.phidd.eq.'Sg'.and..not.lgflag) phase(ii)='Lg'

      if(azi(ii).lt.0.d0) then
         azi(ii)       = -999.d0
         touse(ii)(2:2)= ' '
      endif

      chgcas = uppcas(stat)
      stat = chgcas(1:5)

      do 10 j=1,mstat

      if(stat.eq.sta(j)) then

        iev(ii) = j
        go to 11

      else if(sta(j).eq.' ') then

        call get_station(stationfile,stat,jdate,lat,lon,
     +                   elevs,name,ierr)

        if(ierr.ne.0) then
          print *,'Cannot find station: ',stat,' entry skipped'
          ii = ii - 1
          ierr = 0
          go to 12
        endif

        statpc = 0.d0
        statsc = 0.d0
        statrc = 0.d0
        ifil = 0

        if(vlflag .and. stcorfl) then
          rewind(13)

7         statcorstr = ' '
          read (13,'(a)',end=81,err=81) statcorstr
          if(statcorstr(1:1).eq.'*') go to 7
          if(len(trim(statcorstr)).eq.0) go to 7

          read(statcorstr,*,err=801,end=801) stat1,vpc,vsc,spc,ssc,src
          go to 69
801       src = 0.d0
          read(statcorstr,*,err=802,end=802) stat1,vpc,vsc,spc,ssc
          go to 69
802       spc = 0.d0
          ssc = 0.d0
          read(statcorstr,*,err=81,end=81) stat1,vpc,vsc

69        if(stat1.eq.stat) then

            istcor = istcor + 1
            statpc = spc
            statsc = ssc
            statrc = src
            if(vpc.gt.epsilon) then
              ifil = 1
              vp = vpc
              if(vsc.gt.epsilon) then
                vs = vsc
              else
                vs = vp / dsqrt(3.d0)
              endif
              go to 9
            else
              go to 81
            endif
          endif
          go to 7
        endif

81      if(vlflag) then
          vp = vpl
          vs = vsl
        endif

9       continue

        if(typctl.gt.8) then
c       print *,vlflag,vp,vs
           print *,j,stat,lat,lon,elevs,name,vp,vs,statpc,statsc
        endif

        sta(j)   = stat
        stala(j) = lat
        stalo(j) = lon
        stael(j) = elevs/1000.d0
        stavp(j) = vp
        stavs(j) = vs
        statp(j) = statpc
        stats(j) = statsc
        statr(j) = statrc
        stamb(j) = -10.d0
        stams(j) = -10.d0
        staml(j) = -10.d0
        istfil(j) = ifil
        istaph(j) = 0
        iev(ii) = j
        istat = j

        go to 11

      endif

10    continue

11    if(timeo.lt.timemin) then
         timemin = timeo
      endif

      if(azi(ii).lt.0.d0) then
         azi(ii)       = -999.d0
         touse(ii)(2:2)= ' '
      endif

      if(pin.le.0.d0) pin = -999.d0

      if(islow.eq.0 .and. pin.gt.0.0d0) then
         p(ii)  = radloc(stala(iev(ii)),1)*deg2rad/pin
      else
         p(ii) = pin
         if(pin.gt.0.d0) then
         else
            touse(ii)(3:3) = ' '
         endif
      endif

      chgcas = uppcas(phase(ii)(1:1))
      phase_t = chgcas(1:1)

      if(phase_t.eq.'P')  then
         if(istaph(iev(ii)).eq.0 .or. istaph(iev(ii)).eq.2) 
     +       istaph(iev(ii))=istaph(iev(ii)) + 1
      else if(phase_t.eq.'S') then
         if(istaph(iev(ii)).eq.0 .or. istaph(iev(ii)).eq.1)
     +       istaph(iev(ii))=istaph(iev(ii)) + 2
      endif

      if(phase(ii)(1:1).eq. 'P' .and. phase(ii)(3:3).eq. ' ')
     +             touse(ii)(8:8) = 'P'
      if(phase(ii)(1:3).eq. 'PKP' .and. phase(ii)(6:6).eq. ' ') 
     +             touse(ii)(8:8) = 'P'
      if(phase(ii)(1:4).eq. 'Pdif') touse(ii)(8:8) = 'P'

      if(typctl.gt.8) then
         print *,ii,stat,tt(ii),phase(ii),azi(ii),p(ii),
     +           touse(ii)
      endif

12    o_string = string

14    close(10)

      if(stcorfl) then

        if(istcor.gt.0 .and. output)
     +     write(11,'(''Station corrections were available for'',
     +           i5,'' station(s) as defined in '',a)') istcor,
     +           trim(statcorfile)

        close (13)
      endif

      if(nobsst.gt.0) then
         terrm = terrm / dble(nobsst)
      else
         terrm = 2.d0
      endif
      terrm = terrm * 10.d0 

      nobs  = ii
   
      nstat = istat

      larid = .false.
      do 15 i = 1,nobs
      tt(i) = tt(i)-timemin
      if(typctl.gt.8) then
        print*,i,sta(iev(i)),phase(i),tt(i),azi(i)
     +       ,p(i),touse(i)
      endif
      if(arid(i).ne.'        ') larid = .true.
15    continue

      tome = tome0

      zo = zo1

      elonm  = epilon0
      elatmg = epilat0
      elatm  = convlat(elatmg,1)

c
c     initializing ellipticity correction coefficients
c
      eflag = .false.
      du0 = 0.d0
      du1 = 0.d0
      idum = 2
      coelatm = 90.d0 - elatm
      coelatmr= deg2rad*coelatm
      call elpcor('P',du0,du0,coelatmr,du0,du1,eflag,idum)
c

      if(output) then

         if(iloc) then
            if( (modflag(2) .or. modflag(3) .or. modflag(4)) .and.
     +          (imodn(2).gt.1 .or. imodn(3).gt.1 .or. imodn(4).gt.1)
     +           ) then
                write(11,'(''First reference models  : '',a,'' and '',
     +                a)') trim(filloc),modnam(1)
                if(modflag(2) .and. imodn(2).gt.1) then
                   write(11,'(''Second reference model  : '',a)') 
     +                  modnam(2)
                endif
                if(modflag(3) .and. imodn(3).gt.1) then
                   write(11,'(''Third reference model   : '',a)') 
     +                  modnam(3)
                endif
                if(modflag(4) .and. imodn(4).gt.1) then
                   write(11,'(''Fourth reference model  : '',a)') 
     +                  modnam(4)
                endif
            else
                write(11,'(''Reference models  : '',a,'' and '',a)') 
     +                trim(filloc),modnam(1)
            endif
         else
            if( (modflag(2) .or.  modflag(3) .or. modflag(4)) .and.
     +          (imodn(2).gt.1 .or. imodn(3).gt.1 .or. imodn(4).gt.1)
     +           ) then

            write(11,'(   ''Main reference model    : '',a)') modnam(1)
              if(modflag(2) .and. imodn(2).gt.1) 
     +         write(11,'(''Second reference model  : '',a)') modnam(2)
              if(modflag(3) .and. imodn(3).gt.1) 
     +         write(11,'(''Third reference model   : '',a)') modnam(3)
              if(modflag(4) .and. imodn(4).gt.1) 
     +         write(11,'(''Fourth reference model  : '',a)') modnam(4)
            else
              write(11,'(''Reference model   : '',a)') modnam(1)
            endif
         endif

         if((imo.eq.2 .or. imo.eq.4 .or. imo.eq.5)) 
     +       write(11,'(''CRUST 1.0 used for Station corrections'')')

         if((imo.eq.2 .or. imo.eq.4 .or. imo.eq.6)) 
     +       write(11,'(''CRUST 1.0 used for Reflection Point '',
     +                  ''corrections'')')
      endif

      call fetoh2(tome,idum,yy,mon,mm,dd,idoy,hh,mi,sec)
         
c

      if(output) then

         write(11,'(/''The source parameters:'')')

         write(11,'(/''Source time  :'',i5,4i3.2,f7.3
     +           )')  yy,mon,dd,hh,mi,sec
         write(11,'(''        or'',12x,f16.3,'' [s]'')') tome


         isec1 = nint(sec*1000)
         isec  = isec1/1000
         msec  = isec1-isec*1000
         write(11,'(''        or'',7x,i4,''-'',i3.3,'':'',
     +         3(i2.2,''.''),i3.3/)') 
     +         yy,idoy,hh,mi,isec,msec
         
            write(11,'(''Epicenter lat:'',14x,f10.4,
     +              '' [deg]'')')  elatmg

            write(11,'(''Epicenter lon:'',14x,f10.4,
     +              '' [deg]'')')  elonm

            write(11,'(''Source depth :'',15x,f7.2,
     +               ''   [km]''/)') zo
      endif

c
c     let us now calculate the final residuals and print them out
c
      stmean    = 0.d0
      strmean   = 0.d0
      samean    = 0.d0
      sarmean   = 0.d0
      rmsazi    = 0.d0
      spmean    = 0.d0
      sprmean   = 0.d0
      rmsp      = 0.d0
      rms       = 0.d0

      loctts    = 0

c
c     misfit parameters for all input data!
c
      ndmisf    = 0

      nobst     = 0
      nobsa     = 0
      nobsp     = 0
      stato     = ' '

      if(magflag) then
         namp     = 0
         imsm     = 0
         dmsm     = 0.d0
         imbm     = 0
         dmbm     = 0.d0
         imlm     = 0
         dmlm     = 0.d0
      endif 

      do 450 i = 1,nobs

      epiaz(i) = -999.0
      emeran(i) = -999.d0

      dtmin     = 9999.d0
      dtnew2    = 9999.d0

      used(i) = touse(i)(1:6)
      phaseu(i) = phase(i)
      useds = used(i)
      usedm = ' '

      if(sta(iev(i)).ne.stato) then

         stato = sta(iev(i))

         call depi(stala(iev(i)),stalo(iev(i)),elatmg,elonm,
     +              del(iev(i)),delk(iev(i)),azie(iev(i)),baz(iev(i)),
     +        d2km)
         rzo   = sngl(zo)
         delta = del(iev(i))
         rdel  = sngl(delta)
         rdelk = sngl(delk(iev(i)))

         fla1 = deg2rad*(90.d0-elatm)
         razi = sngl(azie(iev(i)))

         if(isf_in.and.delta.ge.disfmod.and.modflag(2)) then
            modind = 2
         else
            read(touse(i)(7:7),'(i1)') modind
         endif

         modn = modnam(modind)
         imodn(modind) = 2

         if(iloc) then
            imod2 = 1
         else
            imod2 = 0
         endif

         modn = modnam(modind)

         loctt = 0
         nphas = 0

         if(imod2.eq.0 .or. delta.gt.rmax0 .or. zo.gt.zmax) then

           call tauget_mod(rzo,rdel,nphas,phcd,ttc,dtdd,
     +                         dtdh,dddp,modn)

         else

           ierr = 0
           indph = istaph(iev(i))*10000 + indph0
           elatc = elatmg
           elonc = elonm

           elat2 = stala(iev(i))
           elon2 = stalo(iev(i))
           sdep = 0.d0
           if(locsta) sdep  = - stael(iev(i))

           call ttloc(rzo,rdel,czo,nphas,ttc,dtdd,dtdh,dpdh,dddp,
     +                phcd,sdep,typctl,ierr,indph,emerout,dconr,dmoho)

           loctt = 1
           loctts = loctts + 1

         endif

         if(delk(iev(i)).gt.30.d0 .and. zo.lt.35.d0 .and.
     +      delk(iev(i)).gt.3.d0*zo ) then
            nphas       = nphas + 1
            ttc(nphas)  = delk(iev(i))/vlg
            dtdd(nphas) = d2km/vlg
            dtdd(nphas) = d2km/vlg
            dtdh(nphas) = 0.d0
            phcd(nphas) = 'Lg'
            dddp(nphas) = 0.d0
            dpdh(nphas) = 0.d0
         endif

      endif

      text(i) = ' '
      arr(i) = rdel

      phid1 = ' '
      phid2 = ' '

      if(phaseu(i).ne.' ') then
         phid = phaseu(i)

         ic = index(phid,'w')
         if(ic.gt.0) then
            phid(ic:8) = phaseu(i)(ic+1:8) // ' '
         endif

      else

         phid = phase(i)

      endif

      ttobs = timemin + tt(i)

      dpa   = 0.d0
      dpa2  = 0.d0
      dtdz  = 0.d0
      dtdz2 = 0.d0
      ttres  = -9999.d0
      dtnew  = -9999.d0
      ttres1 =  9999.d0
      pares  =  -1000.d0

      call fetoh2(ttobs,idum,yy,mon,mm,dd,idoy,hh,mi,sec)

      if(phase(i).eq.'tx rx' .and. delta.gt.25.d0) goto 433
      if(phase(i).eq.'tx tx' .and. delta.lt.10.d0) goto 433

      imin = 0
      if(freeph) imin = 1
      phsearch = ' '
      if(index(phase(i),'x').gt.0 .or. index(phaseu(i),'x').gt.0) then
         ttt(i) = 0.d0
         imin = 1
         if(index(phase(i),'P').gt.0) phsearch = 'P'
         if(index(phase(i),'S').gt.0) phsearch = 'S'
      endif

      surf = .false.
 
      if(phid.eq.'Rg') then
         surf = .true.
         vsurf = vrg
      endif
                
      if(phid.eq.'Lg' ) then
         surf = .true.
         vsurf = vlg
      endif
 
      if(phid.eq.'LR') then
         surf = .true.
         vsurf = vlr
      endif
                
      if(phid.eq.'LQ') then
         surf = .true.
         vsurf = vlq
      endif

      if(phid.eq.'T') then
         surf = .true.
         vsurf = vt
      endif

      if(phid.eq.'IS') then
         surf = .true.
         vsurf = vi
      endif

      if(surf .and. phid.ne.'Lg') then
         nphas       = nphas + 1
         ttc(nphas)  = delk(iev(i))/vsurf
         dtdd(nphas) = d2km/vsurf
         dtdh(nphas) = 0.d0
         phcd(nphas) = phid
         dddp(nphas) = 0.d0
         dpdh(nphas) = 0.d0
      endif
      nphass = nphas
      surfm = surf

      icha = 0
                
418   continue

c     print *,'---> (e-0) ', i,phase(i),phid,phaseu(i),useds,surf,icha

      surff = surf
      first2 = .false.
      j = 0

      do 420 j1 = 1,nphass

      j = j + 1
      if(j.gt.nphass) go to 421

c     Let's take the first phase from IASPEI-1991-type tables,
c     which fits the used phase name of the onset
c
      phid1 = phcd(j)

      phase_t = phase_type(phid1)
      if((phsearch.eq.'P' .and. phase_t.ne.'P') .or.
     +   (phsearch.eq.'S' .and. phase_t.ne.'S') ) go to 420

      dpa   = 0.d0
      dpaa  = 0.d0
      dtdz  = 0.d0

      if(phid1.eq.phid .or. imin.gt.0 ) then

c     print *,'---> (e-1) ',i,phase(i),phid,phid1,phaseu(i),useds,
c    +                     imin,surf,icha

c
c     checking : any ellipticity correction for this phase?
c

         if(phid1(1:1).eq.'L' .or. phid1(1:1).eq.'R' .or.
     +      phid1(1:1).eq.'T' .or. phid1(1:1).eq.'I')
     +      surff = .true.

         dpa   = dtdd(j)
         dpaa  = dabs(dpa)
         dtdz  = dtdh(j)
         dddpu = dddp(j)
         if(dabs(dddpu).lt.1.d-1) dddpu = 1.d-1

         if(single) then

c
c     too big ray parameter residuum for single array observation
c
            if(dabs(p(i)-dpaa) .ge. 0.01d0) go to 420

         endif

         ecor = 0.d0

         if(.not.surff .and. .not.locgeo) then
c
c           Ellipticity corrections are yet not available for sources 
c           deeper than 700 km. Therefore we accept a small error
c           and set the source depth to 700. km
c
            rzoe = zo
            if(rzoe.gt.700.d0) rzoe=700.d0
            ierre = 0

            call ellip(fla1,azie(iev(i)),delta,rzoe,phid1,dpa,ecor,
     +           ierre)

         endif

c
c     Any static station correction for this phase?
c

         statict = 0.d0

         if(firstph) then
            if(j.eq.1 .and. phase_t.eq.'P') statict = statp(iev(i))
            if(phid1(1:1).eq.'S' .and. .not. first2) then
               first2  = .true.
               statict = stats(iev(i))
            endif
         else
            if(phase_t.eq.'P') statict = statp(iev(i))
            if(phase_t.eq.'S') statict = stats(iev(i))
            if(phase_t.eq.'L') statict = statr(iev(i))
         endif

         th     = 0.d0
         tcrust = 0.d0
         vloc = 99999.d0
         vstat = 0.d0
         delc  = 0.d0
         dpc   = 0.d0
         dph = 0.d0

         hsta = stael(iev(i))

         if(vlflag .and. .not.surff) then

           if((imo.eq.2 .or. imo.eq.4 .or. imo.eq.5) .and. loctt.ne.1) 
     +        then

              elatc  = stala(iev(i))
              elonc  = stalo(iev(i))
              indr = 1
              zoc = 999.d0

              if(imo.eq.5) locmod= .false.

              call crustc(tcrust,delc,phase_t,dpaa,zoc,t2,d2,
     +                    modind,indr,iwl,typctl)

              if(dabs(tcrust).gt.epsilon) then
                 hsta = hsta - elev
c
c       crustal correction of ray parameter
c
                 dpc = delc / dddpu
                 dpa = dpa + dpc
                 dpaa = dabs(dpa)
              endif

           endif

           if(phase_t.eq.'P') vloc = stavp(iev(i))
           if(phase_t.eq.'S') vloc = stavs(iev(i))

           if(vstat.gt.0.d0 .and. istfil(iev(i)).eq.0) vloc=vstat

           if(locsta .and. hsta.lt.epsilon .and. loctt.eq.1) hsta = 0.d0

c          print *,'CRUST 2',tcrust,phid1,phase_t,dpaa,zoc,modind,
c    +              indr,hsta,elev,stael(iev(i)),loctt,imo


           if(vloc .lt. 999.d0 .and. dabs(hsta).gt.epsilon) then

              radkm = deg2rad*radloc(stala(iev(i)),1)
              phin = vloc*dpaa/radkm

              if(phin.lt.1.d0) then

                 dl = hsta / dcos(dasin(phin))

                 ddis2 = q2(dl)-q2(hsta)
                 if(ddis2.lt.1.d-3) ddis2 = 0.d0
                 ddis = dsqrt(ddis2)/radkm

                 if(dl.gt.0.d0) then
                    th = dl/vloc - dpaa*ddis
                    dph = -ddis / dddpu
                 else if(dl.lt.0.d0) then
                    th = dl/vloc + dpaa*ddis
                    dph = ddis / dddpu
                 endif
c
c       ray parameter correction due to elevation correction
c
                 dpa = dpa + dph
                 dpaa = dabs(dpa)

              endif

           endif

         endif

         if(typctl.ge.8) then
            print *,'i,phid1,vloc,phase_t,dpaa,ecor,tcrust,delc,dpc',
     +              ',hsta,th'
            print *,i,phid1,vloc,phase_t,dpaa,ecor,tcrust,delc,dpc,
     +             hsta,th
         endif

c
c        We have eventually to correct this phase for the local 
c        structure at the reflection point at the surface (if 
c        CRUST 1.0 or a local/regional model is available).
c
         trefl  = 0.d0
         trefl2 = 0.d0
         delr   = 0.d0
         delr2  = 0.d0
         dpr    = 0.d0
         usedr = ' '


         if( .not.surff .and. (useds(5:5).eq.'R' .or.
     +       (touse(i)(5:5).eq.'R' .and. useds(5:5).eq.' ' .and. 
     +        useds(1:1).eq.' ')   )
     +      .and. loctt.eq.0 .and. imo.gt.0 .and. imo.ne.3) then

            if(phid1(1:1).ne.'p' .and. phid1(1:1).ne.'s') then

c
c    no corrections in case of PPP, SSS, ...
c
               if(phid1(1:1).eq.phid1(2:2) .and. 
     +            phid1(1:1).eq.phid1(3:3))  go to  415
c
c    no corrections in case of PxPxPx, SxSxSx, ...
c
               if(phid1(1:2).eq.phid1(3:4) .and. 
     +            phid1(1:2).eq.phid1(5:6))  go to  415

               if(len_trim(phid1).eq.3 .and. (phid1(2:2).eq.'m' .or.
     +            phid1(2:2).eq.'c' .or. phid1(2:2).eq.'K' )) 
     +            go to 415

            else
c
c    no corrections in case of pPxPP_, sPxPP_, sSxSS_, pSxSS_, ...
c

               if(phid1(2:2).eq.phid1(4:4) .and. 
     +            phid1(2:2).eq.phid1(5:5))  then
                  go to  415
               endif
c
c    no corrections in case of pPxPxPx, sPxPxPx, sSxSxSx, pSxSxSx, ...
c

               if(phid1(2:3).eq.phid1(4:5) .and. 
     +            phid1(2:3).eq.phid1(6:7))  then
                  go to  415
               endif
c
c    no corrections in case of pPP, sPP, sSS, pSS, ...
c
               if(phid1(2:2).eq.phid1(3:3) .and.
     +            phid1(2:2).eq.phid1(5:5))  then
                  go to  415
               endif
            endif

            chgcas = uppcas(phid1)
            phase_t = chgcas(1:1)
             
c
c           Distance of reflection point from source
c
            fmult = 1.d0
            del0  = dirdel(dpaa,zo,fmult,phase_t)
            azi0 = azie(iev(i))

            if(phid1(1:1).ne.'p' .and. phid1(1:1).ne.'s') then
               if(dpa.ge.0.d0) then
                  del0 = (delta-del0)/2.d0
               else
                  del0 = (360.d0 - delta - del0)/2.d0
                  azi0 = alpha2(azie(iev(i)) + 180.d0)
               endif
            endif

c
c     The reflection point must lay within the valid distance of local/regionl model
c
            if(del0.gt.rmax0 .and. (imo.eq.1 .or. imo.eq.5)) go to 415

            inddel = 1
            call delazd(elatmg,elonm,azi0,del0,inddel,elatc,elonc)
    
c
c     correction for depth phases (e.g.: pP, pwP, sS...)
c

c
c        We have eventually to correct this phase for the local
c        structure at the reflection point at the surface (if
c        CRUST 1.0 or a local/regional model is available).
c

            if(phid1(1:1).eq.'p' .or. phid1(1:1).eq.'s' .and.
     +         phase_t.ne.' ')  then

               if((phid1(1:1).eq.'p' .and. phid1(2:2).eq.'P') .or.
     +             (phid1(1:1).eq.'s' .and. phid1(2:2).eq.'S') .or.
     +             (index(phid1,'w').gt.0)                       ) then
                   indr = 2
               else
                   indr = 4
               endif

               zoc    = zo

               if(imo.eq.5) locmod= .true.

               call crustc(trefl,delr,phase_t,dpaa,zoc,trefl2,delr2,
     +                     modind,indr,iwl,typctl)

               go to 415

            endif

c     correction for surface multiples (e.g.: PnPn,...,PP,SS,P'P', but
c     also PcPPcP and ScSScS)
c
            if( phid1(1:1).eq.phid1(2:2) .or. 
     +          phid1(1:2).eq.phid1(3:4) .or.
     +          phid1(1:3).eq.phid1(4:6)  ) then

                indr = 3
                zoc    = zo

                if(imo.eq.5) locmod= .false.

                call crustc(trefl,delr,phase_t,dpaa,zoc,trefl2,delr2,
     +                      modind,indr,iwl,typctl)

                go to 415

            endif

c
c      correction for converted surface multiples (e.g.: PnSn,...)
c
            conr = .false.
            if( (phid1(1:1).eq.'P' .or. phid1(2:2).eq.'P') .and.
     +          (phid1(1:1).eq.'S' .or. phid1(2:2).eq.'S') .and.
     +          (phid1(3:3).eq.' ' .or. phid1(3:3).eq.'g' .or.
     +           phid1(3:3).eq.'b' .or. phid1(3:3).eq.'n')) then
                   conr=.true.
                   phidr = phid1(2:)
            endif

c
c      We assume PbS or SbP are converted reflections from the Conrad
c      discontinuity and not converted surface refections.
c

            if( (phid1(1:1).eq.'P' .or. phid1(3:3).eq.'P') .and.
     +          (phid1(1:1).eq.'S' .or. phid1(3:3).eq.'S') .and.
     +           phid1(2:2).ne.'b' .and. phid1(2:2).ne.'m' .and.
     +           phid1(2:2).ne.'c' .and. phid1(2:2).ne.'k' .and.
     +          (phid1(2:2).eq.phid(4:4) .or. phid1(2:2).eq.'g' .or.
     +           phid1(2:2).eq.'n'                      )) then
                   conr=.true.
                   phidr = phid1(3:)
            endif

            if(conr) then

                zor = 0.d0
                call tauget_ray(phidr,phase_t,dpaa,modn,zor,
     +                        del0,ttray,rayok)

                azi0 = azie(iev(i))
                if(dpa.lt.0.d0) azi0 = alpha2(azie(iev(i))-180.d0)

                inddel = 1
                call delazd(stala(iev(i)),stalo(iev(i)),azi0,del0,
     +                      inddel,elatc,elonc)

                indr = 5
                zoc  = zo

                if(imo.eq.5) locmod= .false.

                call crustc(trefl,delr,phase_t,dpaa,zoc,trefl2,delr2,
     +                      modind,indr,iwl,typctl)

               go to 415

            endif

         endif

415      continue

         if(dabs(trefl)+dabs(trefl2).ge.epsilon) then
            usedr = 'R'
         else
            usedr = ' '
            trefl = 0.d0
            trefl2= 0.d0
         endif

         if(typctl.gt.6) then
            print *,'dirdel (end): ',phid1,' azi ',azi0,' del ',del0
            print *,'  lat ',elatc,' lon ',elonc,' trefl ',trefl,
     +             ' trefl2 ',trefl2,' phase_t ',phase_t
         endif

         tttn = tome + ttc(j) + ecor 
     +                 + th + tcrust + trefl - statict

         ttres  = ttobs -  tttn

         if(iwl.gt.0 .and. usedr.eq.'R' .and.
     +      dabs(trefl2).ge.epsilon) then

            fdt =  trefl2 - trefl
            dtt2 = ttres - fdt

            wflag = .false.

            if(index(phaseu(i),'w').gt.0) wflag = .true.

            if(phase(i)(1:3).eq.'tx ' .or. used(i)(1:1).eq.' ') then
               if(iwl.eq.1) then
                  wflag = .true.
               else 
                  if(dabs(dtt2).lt.dabs(ttres)) wflag = .true.
               endif
            endif

            if(wflag) then
              delr   = delr2
              trefl  = trefl2
              tttn   = tttn + fdt
              phid1  = phasw(phid1)
              ttres  = ttobs - tttn
            endif

         endif

c
c       reflection point correction of ray parameter
c
         if(usedr.eq.'R') then
            dpr = delr / dddpu
            dpa = dpa + dpr
            dpaa = dabs(dpa)
         endif

         dtnew = dabs(tttn-tome-ttt(i))

         if(typctl.gt.8) then
           print *,'phid1, i, tttn, t0, j, phase, TT, ECOR, Height,',
     +             ' Crust, Refl, Stat, Tobs, ttres'
           print
     +     *,phid1,i,tttn,tome,j,phcd(j),ttc(j),ecor,th,tcrust,trefl,
     +       statict,ttobs, ttres, dtmin, usedr,rdel,phase(i)
         endif

         ttres1 = dabs(ttc(j) - ttc(1))
         if(rdel.gt.113. .and. phcd(1)(1:3).eq.'Pdi' .and.
     +                         ttres1.gt.200.d0        ) then
           if(phcd(2)(1:2).eq.'PK') ttres1 = dabs(ttc(j) - ttc(2))
           if(phcd(3)(1:2).eq.'PK') ttres1 = dabs(ttc(j) - ttc(3))
           if(phcd(4)(1:2).eq.'PK') ttres1 = dabs(ttc(j) - ttc(4))
         endif

         if(dabs(ttres).lt.dabs(dtmin)) then
            phid2 = phid1
            dtmin = ttres
            dtnew2 = dtnew
            ttresm = ttres1
            dpa2  = dpa
            pares2 = p(i) - dabs(dpa)
            dtdz2 = dtdz
            usedm = useds
            usedsr = usedr
            tttm  = tttn
            surfm = surff
         endif

         do 419  j2 = j+1,nphass
            if(phid .eq. phcd(j2)) then
              j = j2 - 1
              go to 420
            endif
419      continue

         if(imin.eq.0 ) go to 422

      endif

420   continue

421   if(icha.ge.99 .or.imin.gt.0) go to 422

      if((dabs(dtmin).gt.999.d0 .or. dtnew2.ge.1.d0) .and.
     +   (usedm(1:1).ne.' ' .or. usedm(4:4).ne.' ')) then
         phid0 = phid
         call testphase (phid0,icha,delta)
         if(phid0.ne.phid .and. icha.lt.99) then
            phid = phid0
            go to 418
         endif
      endif

422   if(dabs(dtmin).gt.1.d0 .and. usedm(1:1).eq.' ' .and. 
     +   usedm(3:3).eq.' ' .and. imin.eq.0) then
         if((surfm .and. phid(1:2).ne.'Lg')) go to 423
         imin = 1
         go to 418
      endif

c     print *,'---> (e-2) ',i,phase(i),phid,phid1,phaseu(i),phid2,useds,
c    +                     imin,surf,icha,dtmin,dtnew2

423   if((dabs(dtmin).le.30.d0 .and. useds(1:1).eq.' ')      .or.
     +                    useds(1:1).ne.' '                   .or.
     +   (dabs(dtmin).le.300.d0 .and. surfm .and. phid2(2:2).ne.'g')) 
     +    then

         ttres  = dtmin
         ttres1 = ttresm
         pares  = pares2
         phid1  = phid2
         dpa    = dpa2
         dtdz   = dtdz2
         useds  = usedm
         useds(5:5) = usedsr
         tttr(i)= tttm
         phaseu(i) = phid2
      else
         ttres  = -9999.d0
         ttres1 =  9999.d0
         pares  = -999.d0
         phid1  = ' '
         dpa    = 0.d0
         dtdz   = 0.d0
         useds(1:1) = ' '
         useds(3:5) = '   '
         tttr(i) = 0.d0
         if(phase(i)(1:3).eq.'P1 ') then
           phaseu(i) = 'Px'
           phid1 = phaseu(i)
         else if(phase(i)(1:3).eq.'S1 ') then
           phaseu(i) = 'Sx'
           phid1 = phaseu(i)
         endif
      endif
      dpaa = dabs(dpa)

      surf   = surfm

      if(single) go to 430

      if(phase(i)(1:3).eq.'tx ' .and. phid1.ne.'        ') then
         phaseu(i) = phid1
      endif

430   continue

      if(useds(1:1).eq.'T') then
         stmean  = stmean + ttres
         strmean = strmean + dabs(ttres)
         rms     = rms    + q2(ttres)
         epiaz(i)= razi
         nobst   = nobst + 1
      endif

      if(useds(3:3).eq.'S') then
         spmean  = spmean + pares
         sprmean = sprmean + dabs(pares)
         rmsp    = rmsp + q2(pares)
         epiaz(i)= razi
         nobsp   = nobsp + 1
      endif

      if(emerout .and. .not.surf .and. index(phaseu(i),'x').le.0) then
c
c     we use global models only
c
        if(loctt.eq.0) then

          dtdx = dpaa*grad1
          if(dtdz.lt.0.d0) then
              emeran(i) = rad2deg*datan(dabs(dtdx/dtdz))
           else if(dtdz.gt.0.d0) then
              emeran(i) = 180.d0 - 
     +                    rad2deg*datan(dabs(dtdx/dtdz))
           else
              emeran(i) = 90.d0
           endif

        else

c    a local/regional model had been used to locate the source 
c
           vsource = -999.d0
           chgcas = uppcas(phid1(1:1))
           if(chgcas(1:1).eq.'P') vsource = rzv(1)
           if(chgcas(1:1).eq.'S') vsource = rzv(2)
           if(vsource.gt.0.d0) then
              fac = vsource*rad2deg*dpaa/
     +              (rearth-zo)
              if(dabs(fac).le.1.d0) then
                 if(dtdz.lt.0.d0) then
                    emeran(i) = rad2deg*dasin(fac)
                 else if(dtdz.gt.0.d0) then
                    emeran(i) = 180.d0 - rad2deg*dasin(fac)
                 else
                    emeran(i) = 90.d0
                 endif
              endif
           endif
        endif

c       print*,'e ',i,stato,phid1,phaseu(i),dtdx,dpa,dtdz,vsource,
c    +         fac,emeran(i)

      endif

      if(typctl.ge.8) then
         print *,'i,ttt,ttobs,ttres,ecor,th,tcrust,used,emer'
         print *,i,tttr(i),ttobs,ttres,ecor,th,tcrust,useds,
     +          emeran(i)
      endif

      if(useds(1:1).eq.'t') useds(1:1) = ' '

      if(dpa.lt.0.d0 .or.phid1(1:4).eq.'P3KP') then
         azires = alpha1(azi(i) - alpha2(baz(iev(i))-180.d0))
      else
         azires = alpha1(azi(i) - baz(iev(i)))
      endif

      if(useds(2:2).eq.'A') then
         samean  = samean + azires
         sarmean = sarmean + dabs(azires)
         rmsazi  = rmsazi + q2(azires)
         epiaz(i)= razi
         nobsa   = nobsa + 1
      endif

      cmod = ' '
      read(touse(i)(7:7),'(i1)') modind

      if(modind.gt.1 .and. modflag(modind)) write(cmod,'(i1)') modind

      statw = stato
      if(touse(i)(9:9).eq.'*') then
        chgcas = lowcas(stato)
        statw = chgcas(1:5)
      endif

433   continue
      ttres0 = rdig(ttres,3)
      azires0 = rdig(azires,2)
      pares0 = rdig(pares,2)
      write(text(i),'(a5,f8.3,f7.2,1x,a8,8x,2i3.2,f7.3,f8.3,
     +                f7.2,f8.2,f6.2,f7.2,1x,a5,a1)') 
     +                statw,rdel,razi,phase(i),hh,mi,sec,
     +                ttres0,azi(i),azires0,p(i),pares0,useds(1:5),cmod
      
      if(kmout) write(text(i)(6:13),'(f8.2)') rdelk

      if(phase(i)(1:3).eq.'tx ') then
         if(rdel.gt.20. .and. phase(i)(4:5).eq.'x ') 
     +      phase(i)(4:5) = 'tx'
         write(text(i)(22:29),'(a2,a6)') phase(i)(4:5), '      '
      endif

      if(phase(i).ne.phid1) write(text(i)(30:37),'(a8)') phid1

      if(ttres.lt.-99.999d0 .or. ttres.gt.999.999d0) 
     +   write(text(i)(51:58),'(1x,f7.2)') ttres
      if(ttres.lt.-999.99d0 .or. ttres.gt.9999.99d0) 
     +   write(text(i)(51:58),'(1x,f7.1)') ttres
      if(ttres.lt.-999.99d0 .or. ttres.gt.9999.99d0) 
     +   write(text(i)(51:58),'(1x,f7.0)') ttres

      if(dabs(ttres).gt.9990.d0) then
         text(i)(51:58)='        '
      endif
      if(dabs(ttres).lt.1.d-3) text(i)(51:58)='   0.000'

      if(azires.lt.-360.d0) text(i)(66:73)='        '
      if(azires.gt.360.d0)  text(i)(66:73)='        '
      if(azi(i).lt.0.d0) then
           text(i)(59:73)='               '
           if(thbaz) write (text(i)(60:65),'(F6.2)') baz(iev(i))
      endif

      if(pares.ge. 990.d0)  text(i)(80:86)='       '
      if(pares.le.-990.d0)  text(i)(80:86)='       '
      if(p(i).le.0.d0)   then
           text(i)(74:86)='             '
           if(thray) write (text(i)(75:79),'(F5.2)') dpa
      endif

      if(snr(i).gt.0.d0) then
         if(snr(i).lt.10000.d0) then
            write (text(i)(94:101),'(1x,f7.2)') snr(i)
         else
            if(snr(i).lt.100000.d0) then
               write (text(i)(94:101),'(1x,f7.1)') snr(i)
            else
               if(snr(i).lt.1000000.d0) then
                  write (text(i)(94:101),'(1x,f7.0)') snr(i)
               else
                  text(i)(94:101) = ' 999999.'
               endif
            endif
         endif
         isnr = isnr + 1
      endif

      if(amplit(i).gt.0.d0) then
         write (text(i)(102:114),'(1x,f12.2)') amplit(i)
      endif

      if(period(i).gt.0.d0) then
         write (text(i)(115:121),'(1x,f6.3)') period(i)
      endif

      text(i)(131:131) = onflag(i)(3:3)

      istmag(i) = 0

      if(amplit(i).gt.0.d0) then

      namp = namp + 1

      if(magflag .and. touse(i)(6:6).eq.'M') then

c
c     the standard IASPEI (1967) formula:
c     Ms = log (A / T )  + 1.66 log (DELTA) + 0.3 (A in nanometer)
c
c     or the Rezapour/Pearce(BSSA 88, 43-61) formula (18):
c
c     Ms = log (A / T ) + 1/3 log (DELTA) + 1/2 log (sin(DELA) +
c          0.0046 DELTA + 2.370  (A in nanometer)
cc

           dmag = -9.99d0
           statmag='  '

           if(phid1.eq.'LR' .and. period(i).ge.5.d0 ) then

              d1 = delta
c
c     to avoid problems with log(0.):
              if(d1.le.0.d0)   d1 =   0.001d0
              if(d1.ge.180.d0) d1 = 179.999d0
c
              if(magtyps(1:6).eq.'IASPEI') then
                dmag = dlog10(amplit(i)/period(i)) +
     +              dlog10(d1)*1.66d0  + 0.3d0
              else if(magtyps(1:3).eq.'R-P') then
                dmag = dlog10(amplit(i)/period(i)) +
     +              dlog10(d1)/3.d0 + dlog10(dsin(d1*deg2rad))/2.d0
     +              + 0.0046d0*d1 + 2.370d0
              else
                print *,' Ms attenuation model not defined!'
                go to 449
              endif

              if(dmag.lt.9.5d0 .and. dmag.gt.-9.9d0) then
                 if(delta.ge.delmsmin .and. delta.le.delmsmax) then
                    dmsm = dmsm + dmag
                    imsm = imsm + 1
                    if(dmag.gt.stams(iev(i))) stams(iev(i)) = dmag
                    stamag(i) = dmag
                    istmag(i) = 1
                 endif
                 statmag ='MS' 
              else
                 dmag = -9.999d0
              endif

           else if(dabs(ttres).lt.60.d0 .and. (phid1.eq.'Lg'   .or.
     +            (phid1(1:1).eq.'S'.and.(phid1(2:2).eq.'g'.or.
     +             phid1(2:2).eq.'b' .or.phid1(2:2).eq.'n'.or.
     +             phid1(2:2).eq.' '))           .or.
     +           ((phid1(1:2).eq.'pS'.or.phid1(1:2).eq.'sS').and.
     +            (phid1(3:3).eq.'g' .or.phid1(3:3).eq.'b' .or.
     +             phid1(3:3).eq.'n' .or.phid1(3:3).eq.' '))
     +            ))then
c
c             we will use ML attenuation file for S-type onsets
c

              magfile = file_check(magmlfile)
              ierc = 0

              if(magfile.ne.' ') then

                 if((magtypml(1:7).eq.'Richter').and.
     +               (period(i).le.0.d0)) period(i) = 1.d0

                 if(period(i).gt.0.d0) then
                    call epmagc(real(period(i)), rdelk, rmcorr, typctl, 
     +                       ierc, magfile)
                 else
                    ierc = 9
                 endif

                 if(ierc.eq.0) then

                    if(magtypml(1:5) .eq. 'Bath ') then
                       dmag = dlog10(amplit(i)*0.1d0) + dble(rmcorr)
                    else if(magtypml(1:7) .eq. 'Richter') then
                       dmag = dlog10(amplit(i)) + dble(rmcorr)
                    else
                       if(typctl . gt. 6 ) then 
                          print *,' Cannot find ML attenuation model '
     +                           , magtypml
                       endif
                       go to 449
                    endif
                    if(dmag.lt.7.5d0 .and. dmag.gt.-9.9d0) then
                       if(delta.ge.delmlmin .and. delta.le.delmlmax)then
                          dmlm = dmlm + dmag
                          imlm = imlm + 1
                          if(dmag.gt.staml(iev(i))) staml(iev(i)) = dmag
                          stamag(i) = dmag
                          istmag(i) = 2
                       endif
                       statmag = 'ML'
                    else
                       dmag = -9.999d0
                    endif

                 else

                    if(typctl . gt. 6 ) then
                      print *,' No ML attenuation corrections found!'
                    endif
                    go to 449

                 endif
              else
                 if(typctl . gt. 6 ) then 
                    print *,' No ML attenuation model defined!'
                 endif
              endif

           else if( ttres1.lt.9.d0 .and. dabs(ttres).le.8.d0 .and. 
     +             (phid1(1:1).eq.'P' .or. phid1(1:2).eq.'pP' .or. 
     +              phid1(1:2).eq.'sP') .and. period(i).gt.0.d0 ) then

              magfile = ' '

              if(rdel.le.110. ) then

                if(magtypp.eq.'G-R' .and. rdel.ge.11.) 
     +                        magfile = 'MB_G-R.DAT'

                if(magtypp.eq.'V-C') magfile = 'MB_V-C.DAT'

                if(magtypp.eq.'M-R' .and. (rdel.le.100. 
     +                        .and. rdel.ge.21.)) magfile = 'MB_M-R.DAT'

              else if( rdel.gt.110 .and. rdel.le.150. .and. 
     +                (phid1(1:3).eq.'PKP'  .or. phid1(1:3).eq.'PKi' 
     +            .or. phid1(1:4).eq.'pPKP' .or. phid1(1:4).eq.'sPKP'
     +            .or. phid1(1:4).eq.'pPKi' .or. phid1(1:4).eq.'sPKi')
     +               .and. magtypp.eq.'V-C' ) then

                magfile = 'MB_V-C.DAT'

              else if( rdel.gt.150 .and. 
     +               (phid1(1:5).eq.'PKPdf' .or. phid1(1:6).eq.'pPKPdf'
     +                 .or. phid1(1:6).eq.'sPKPdf') 
     +               .and. magtypp.eq.'V-C') then

                magfile = 'MB_V-C.DAT'

              endif

              if(magfile.ne.' ') then

                 magfile = file_check(magfile)
                 ierc = 0
                 call magfact(magfile, rdel, rzo, rmcorr, ierc)
                 if(ierc.eq.0) then

                    dmag = dlog10(amplit(i)/period(i)) + dble(rmcorr)

                    if(dmag.lt.8.5d0 .and. dmag.gt.-9.9d0) then
                       if(delta.ge.delmbmin .and. delta.le.delmbmax)then
                          dmbm = dmbm + dmag
                          imbm = imbm + 1
                          if(dmag.gt.stamb(iev(i))) stamb(iev(i)) = dmag
                          istmag(i) = 3
                          stamag(i) = dmag
                       endif
                       statmag = 'mb'
                    else
                       dmag = -9.999d0
                    endif

                 else
                    if(typctl . gt. 6 ) then 
                       print *,' No mb attenuation corrections found!'
                    endif
                 endif

              else
                 if(typctl . gt. 6 ) then 
                    print *,' No mb attenuation corrections file!'
                 endif
              endif

           endif

449        if(dmag.gt.-9.99d0) then
              if(dmag.lt.0.d0) then
                 write (text(i)(122:129),'(1x,f4.1,1x,a2)') dmag,statmag
              else
                 write (text(i)(122:129),'(1x,f4.2,1x,a2)') dmag,statmag
              endif
           endif
        endif
      endif

      if(emerout .and. emeran(i).ge.0.d0) then
        write(text(i)(132:138),'(1x,f6.2)') emeran(i)
      endif

      if(arid(i).ne.'        ') then
        write(text(i)(139:147),'(1x,a8)') arid(i)
      endif

      if(typctl.ge.10) then
         print*,i,text(i)
      endif

450   continue

      if(nobst.gt.0) then
         stmean  = stmean/dble(nobst)
         strmean = strmean/dble(nobst)
         rms     = dsqrt(rms/dble(nobst))
      endif 

      rlat = sngl(elatmg)
      rlon = sngl(elonm)
      call hyposat_geo( rlat,rlon, isreg, regnum, region , ierr )

      if(output) then
         write(11,'(''Flinn-Engdahl Region ('',i4,'' ): '',a/)')
     +            isreg, trim(region)
      endif

c
c     We will now calculate the maximum azimuthal gaps for 
c        - all as defining used observations
c        - all observations
c
c     If possible, also the secondary azimuthal gap is calculated and
c     the CPQ parameter.
c

      dazgap = 360.
      dazgap2 = 360.

      cpq  = 0.d0
      cpq2 = 0.d0

      if(nstat.gt.1) then

           call indexx(nobs,epiaz,indx)
           call azigap(epiaz,dazgap,d1azi,d2azi,dazgap2,d1azi2,
     +                 d2azi2,cpq,cpq2,nobs,indx,mread)

      endif

      call indexx(nobs,arr,indx)

      ntext = 0
      stato = sta(iev(indx(1)))
      lenons = 131

      texth = ' '
      texth(1:45)   = ' Stat  Delta   Azi   Phase   [used]    Onset '
      texth(46:91)  = 'time    Res     Baz     Res   Rayp   Res  Used' 
      texth(92:131) = '      SNR    Amplitude  Period  MAG    Q' 

      if(emerout) then
          texth(133:138) = 'Em-Ang'
          lenons = 138
      endif

      if(larid) then
          texth(139:143) = ' ARID'
          lenons = 147
      endif

      if(namp.gt.0 ) then

        if(imsm.gt.0) then
           if(lmaxm) then
              imsm = 0
              dmsm = 0.d0
              do 4511 im = 1,nstat
                 if(stams(im).gt.-9.9d0) then
                    imsm = imsm + 1
                    dmsm = dmsm + stams(im)
                 endif
4511          continue
           endif
           fac = dble(imsm)
           dmsm = dmsm / fac
           sdms = 0.d0
           do 4512 im = 1,nobs
             if(istmag(im).eq.1) sdms = sdms + q2(dabs(stamag(im)-dmsm))
4512       continue
           sdms = dsqrt(sdms/fac)
        endif

        if(imlm.gt.0) then
           if(lmaxm) then
              imlm = 0
              dmlm = 0.d0
              do 4513 im = 1,nstat
                 if(staml(im).gt.-9.9d0) then
                    imlm = imlm + 1
                    dmlm = dmlm + staml(im)
                 endif
4513          continue
           endif
           fac = dble(imlm)
           dmlm = dmlm / fac
           sdml = 0.d0
           do 4514 im = 1,nobs
             if(istmag(im).eq.2) sdml = sdml + q2(dabs(stamag(im)-dmlm))
4514       continue
           sdml = dsqrt(sdml/fac)
        endif

        if(imbm.gt.0) then
           if(lmaxm) then
              imbm = 0
              dmbm = 0.d0
              do 4515 im = 1,nstat
                 if(stamb(im).gt.-9.9d0) then
                    imbm = imbm + 1
                    dmbm = dmbm + stamb(im)
                 endif
4515          continue
           endif
           fac = dble(imbm)
           dmbm = dmbm / fac
           sdmb = 0.d0
           do 4516 im = 1,nobs
             if(istmag(im).eq.3) sdmb = sdmb + q2(dabs(stamag(im)-dmbm))
4516       continue
           sdmb = dsqrt(sdmb/fac)
        endif

        if(output) then

           nli = 0
           if(imsm.gt.0) then
              if(delmsmax.le.179.9999d0 .or. delmsmin.ge.0.0001d0) then
                 write(11,'(''Magnitude: '',f4.1,'' +/- '',f4.1,'' ('',
     +              i5,'' obs, MS, '',a,'', between'',f7.2,'' and'',
     +              f7.2,'' deg)'')')
     +               dmsm,sdms,imsm,trim(magtyps),delmsmin,delmsmax
              else
                 write(11,'(''Magnitude: '',f4.1,'' +/- '',f4.1,'' ('',
     +              i5,'' obs, MS, '',a,'')'')') dmsm,sdms,imsm,
     +              trim(magtyps)
              endif
              nli = 1
           endif
           if(imbm.gt.0) then
              if(delmbmax.le.179.9999d0 .or. delmbmin.ge.0.0001d0) then
                 write(11,'(''Magnitude: '',f4.1,'' +/- '',f4.1,'' ('',
     +              i5,'' obs, mb, '',a,'', between'',f7.2,'' and'',
     +              f7.2,'' deg)'')')
     +               dmbm,sdmb,imbm,trim(magtypp),delmbmin,delmbmax
              else
                 write(11,'(''Magnitude: '',f4.1,'' +/- '',f4.1,'' ('',
     +              i5,'' obs, mb, '',a,'')'')') dmbm,sdmb,imbm,
     +              trim(magtypp)
              endif
              nli = 1
           endif
           if(imlm.gt.0) then
              if(delmlmax.le.179.9999d0 .or. delmlmin.ge.0.0001d0) then
                 write(11,'(''Magnitude: '',f4.1,'' +/- '',f4.1,'' ('',
     +              i5,'' obs, ML, '',a,'', between'',f7.2,'' and'',
     +              f7.2,'' deg)'')')
     +               dmlm,sdml,imlm,trim(magtypml),delmlmin,delmlmax
              else
                 write(11,'(''Magnitude: '',f4.1,'' +/- '',f4.1,'' ('',
     +              i5,'' obs, ML, '',a,'')'')') dmlm,sdml,imlm,
     +              trim(magtypml)
              endif
              nli = 1
           endif
 
           if(nli.eq.1) write(11,'(/)')

           if(imsm.ne.0 .or. imbm.ne.0 .or. imlm.ne.0) 
     +        texth(124:126) = 'MAG'

        endif

      endif

      write (11,'(a/)') trim(texth)

      do 453 i=1,nobs

      if(sta(iev(indx(i))).ne.stato.or.i.eq.nobs) then

        ntext0 = ntext

        if(i.eq.nobs) then 

          ntext        = ntext + 1
          arr(ntext)   = sngl(tt(indx(i)))
          text2(ntext) = text(indx(i))

          ntext0       = ntext
          if(sta(iev(indx(i))).ne.stato) ntext0 = ntext - 1

        endif

        call indexx(ntext0,arr,indx2)

        do 452 j=1,ntext0
        textout = trim(text2(indx2(j)))
        if(textout(1:5).ne.'     ') then

           if(output) write(11,'(a)') textout(1:lenons)

        endif
452     continue

        if(i.eq.nobs) then
           if(ntext0.eq.ntext-1) then
             textout = trim(text(indx(i)))
             if(textout(1:5).ne.'     ') then

              if(output) write(11,'(a)') textout(1:lenons)

             endif
           endif
           go to 453
        endif

        stato = sta(iev(indx(i)))
        ntext = 1

      else

        ntext = ntext + 1

      endif

      arr(ntext)   = sngl(tt(indx(i)))
      text2(ntext) = text(indx(i))

453   continue
      
      if(nobsa.gt.0) then
         samean  = samean/dble(nobsa)
         sarmean = sarmean/dble(nobsa)
         rmsazi  = dsqrt(rmsazi/dble(nobsa))
      endif
      if(nobsp.gt.0) then
         spmean  = spmean/dble(nobsp)
         sprmean = sprmean/dble(nobsp)
         rmsp    = dsqrt(rmsp/dble(nobsp))
      endif
     
      if(.not.diffflag) go to 466

      if(output) then
        write(11,'(/''Defining travel-time differences:''/)')
        write(11,'('' Stat  Delta  Phases'',11x,''Observed   Res''/)')
      endif

      i2 = 0
      sdmean  = 0.d0
      sdrmean = 0.d0
      rmsdt   = 0.d0

      do 461 i = 1,nobs-1

      if(used(i)(4:4).ne.'D') go to 461

      do 460 j = i+1,nobs

         if(used(j)(4:4).ne.'D') go to 460

         if(iev(i).ne.iev(j)) go to 460
         if(phaseu(i).eq.phaseu(j)) go to 460

         i2    = i2 + 1
         arr(i2) = sngl(del(iev(i)))

         dtth  = tttr(j) - tttr(i)
         dtobs = tt(j) - tt(i)
         dtres = dtobs - dtth

         sdmean  = sdmean  + dtres
         sdrmean = sdrmean + dabs(dtres)
         rmsdt   = rmsdt   + q2(dtres)

         ndmisf = ndmisf + 1
 
         art = trim(phaseu(j))//' - '//trim(phaseu(i))

          if(typctl.gt.5) then
             print *,i,j,sta(iev(j)),del(iev(i)),
     +               trim(art),dtobs,dtres,tttr(j),tt(j),tttr(i),tt(i)
          endif

         if(output) then
            statw = sta(iev(i))
            if(touse(i)(9:9).eq.'*' .or. touse(j)(9:9).eq.'*') then
              chgcas = lowcas(statw)
              statw = chgcas(1:5)
            endif

            if(dabs(dtres).lt.1000.d0) then

               dtres = rdig(dtres,3)

               if(kmout) then
                  write(text(i2),'(a5,f8.2,1x,a16,f9.3,f8.3)') 
     +                  statw,delk(iev(i)),art,dtobs,dtres
               else
                  write(text(i2),'(a5,f8.3,1x,a16,f9.3,f8.3)') 
     +                  statw,del(iev(i)),art,dtobs,dtres
               endif
            else
               if(kmout) then
                  write(text(i2),'(a5,f8.2,1x,a16,f9.3,f8.1)') 
     +                  statw,delk(iev(i)),art,dtobs,dtres
               else
                  write(text(i2),'(a5,f8.3,1x,a16,f9.3,f8.1)') 
     +                  statw,del(iev(i)),art,dtobs,dtres
               endif
            endif
         endif

460   continue

461   continue

      sdmean  = sdmean / dble(ndmisf)
      sdrmean = sdrmean / dble(ndmisf)
      rmsdt   = dsqrt(rmsdt  / dble(ndmisf))

      if(output) then

         call indexx(i2,arr,indx)

         do 463 i=1,i2
         write(11,'(a)') trim(text(indx(i)))
463      continue
      endif

466   continue

      in = nobst + nobsp + nobsa + ndmisf

      if(output) then

         write(11,'(/''Number of usable stations: '',i4)') nstat

         if(nstat.gt.1) then
            write(11,'(/''Maximum azimuthal gap of all '',
     +        ''observations: '',f5.1,'' -> '',f5.1,'' [deg] = '',
     +        f5.1,'' [deg] CPQ ='',f7.3)') d1azi,d2azi,dazgap,cpq
         endif

         if(nstat.gt.2) then
             write(11,'(/''Maximum secondary azimuthal gap of all '',
     +         ''observations: '',f5.1,'' -> '',f5.1,
     +         '' [deg] = '',f5.1,'' [deg] CPQ ='',f7.3)') d1azi2,
     +         d2azi2,dazgap2,cpq2
         endif
c
c     output of mean residuals
c
         write(11,'(/''Residuals of defining data'',10x,
     +               ''RMS     MEAN-RES       MEAN'')')

         if(nobst.eq.1) 
     +   write(11,'(i6,'' onset time              : '',f8.3,x,2(3x,
     +         f8.3),''  [s]'')') nobst,rms,strmean,stmean

         if(nobst.gt.1) 
     +   write(11,'(i6,'' onset times             : '',f8.3,x,2(3x,
     +         f8.3),''  [s]'')') nobst,rms,strmean,stmean

         if(nobsa.eq.1) 
     +   write(11,'(i6,'' backazimuth value       : '',f8.3,x,2(3x,
     +         f8.3),''  [deg]'')') nobsa,rmsazi,sarmean,samean

         if(nobsa.gt.1) 
     +   write(11,'(i6,'' backazimuth values      : '',f8.3,x,2(3x,
     +         f8.3),''  [deg]'')') nobsa,rmsazi,sarmean,samean

         if(nobsp.eq.1)
     +   write(11,'(i6,'' ray parameter           : '',f8.3,x,2(3x,
     +         f8.3),''  [s/deg]'')') nobsp,rmsp,sprmean,spmean

         if(nobsp.gt.1)
     +   write(11,'(i6,'' ray parameters          : '',f8.3,x,2(3x,
     +         f8.3),''  [s/deg]'')') nobsp,rmsp,sprmean,spmean

         if(ndmisf.eq.1) 
     +   write(11,'(i6,'' travel-time difference  : '',f8.3,x,2(3x,
     +         f8.3),''  [s]'')') ndmisf,rmsdt,sdrmean,sdmean

         if(ndmisf.gt.1) 
     +   write(11,'(i6,'' travel-time differences : '',f8.3,x,2(3x,
     +         f8.3),''  [s]'')') ndmisf,rmsdt,sdrmean,sdmean

      endif

c
c     output of one line with all calculated source parameters and
c     quality parameters
c

      if(typctl.ge.0) then
         write(*,'(/''T0'',25x,''LAT'',6x,''LON'',7x,''Z'',5x,
     +      ''VPVS'',3x,''DLAT'',5x,''DLON'',6x,''DZ'',7x,''DT0'',4x,
     +      ''DVPVS DEF'',4x,''RMS'' )' )
      endif
      
      call fetoh2(tome,idum,yy,mon,mm,dd,idoy,hh,mi,sec)
 
      isec1 = nint(sec*1000)
      isec  = isec1/1000
      msec = isec1-isec*1000

      if(output) then

         write(11,'(/''T0'',25x,''LAT'',6x,''LON'',7x,''Z'',5x,
     +      ''VPVS'',3x,''DLAT'',5x,''DLON'',6x,''DZ'',7x,''DT0'',4x,
     +      ''DVPVS DEF'',4x,''RMS'' )' )

           write(11,'(i4,''-'',i2.2,''-'',i2.2,3i3.2,''.'',i3.3,
     +           2f9.3,f8.2,f7.2,2f9.4,a8,f9.3,f7.2,i5,f9.3)') 
     +           yy,mon,dd,hh,mi,isec,msec,elatmg,elonm,zo,0.,
     +           0.,0.,'  Fixed ',0.,0.,in,rms
      endif

      if(typctl.ge.0) then
           write(*,'(i4,''-'',i2.2,''-'',i2.2,3i3.2,''.'',i3.3,
     +        2f9.3,f8.2,f7.2,2f9.4,a8,f9.3,f7.2,i5,f9.3)') 
     +        yy,mon,dd,hh,mi,isec,msec,elatmg,elonm,zo,0.,
     +           0.,0.,'  Fixed ',0.,0.,in,rms
      endif

      if(ref_eve .and. output) then

         call depi(dlati,dloni,elatmg,elonm,del3,dk,ep2,ep1,d2km)


         if(isf_ref.ne.' ' .and. isf_in) then

            write(11,'(/,''Distance to ISF Ref ( '',a10,f9.4,f10.4,
     +           '' ):'',f8.2,'' km; DZ:'',f7.1,'' km, AZI:'',f6.1)')
     +           author2,dlati,dloni,dk,ddepi-zo,ep2
         else
            write(11,'(/,''Distance to Reference Event ('',f9.4,f10.4,
     +           '' ):'',f8.2,'' km; DZ:'',f7.1,'' km, AZI:'',f6.1)')
     +           dlati,dloni,dk,ddepi-zo,ep2
         endif

      endif

      if( output .and. iloc .and. modout .and. loctts.gt.0) then

        if(imo.eq.3 .or. imo.eq.4) then

           if(.not.kmout) then
             write(11,'(/,''CRUST 1.0 model for source-region (max. '',
     +              ''delta ='',f6.2,'' deg):'',/,''    DEPTH   '',
     +              ''    VP        VS    DISCON'')') rmax0
           else
             radkm = rmax0*deg2rad*radloc(elatmg,1)
             write(11,'(/,''CRUST 1.0 model for source-region (max. '',
     +              ''delta ='',f8.1,'' km):'',/,''    DEPTH   '',
     +              ''    VP        VS    DISCON'')') radkm
           endif
        
           itrue = 0
           elatc = elatmg
           elonc = elonm
           inum  = 2
           ierr = 0
           call get_mod_c10(itrue,inum,typctl)

           write(11,'(3f10.3,a6)') (z(i),v0(1,i),v0(2,i),
     +           azo(i),i=1,jmod)

        else if(imo.eq.1 .or. imo.eq.2 .or. imo.eq.5) then

           if(.not.kmout) then
              write(11,'(/,''Local model '',a,'' used (max. delta ='',
     +              f6.2,'' deg):'',/,''    DEPTH       VP'',
     +              ''        VS'',''    DISCON'')') 
     +              trim(filloc),rmax0
           else
              radkm = rmax0*deg2rad*radloc(elatmg,1)
              write(11,'(/,''Local model '',a,'' used (max. delta ='',
     +              f8.1,'' km):'',/,''    DEPTH       VP'',
     +              ''        VS'',''    DISCON'')') 
     +              trim(filloc),radkm
           endif

           if(mtyp.eq.'RER') call get_mod_reg(ierr)

           write(11,'(3f10.3,a6)') (z(i),v0(1,i),v0(2,i),
     +           azo(i),i=1,jmod)
        endif

        if(rzv(1).gt.0.d0) then
          write(11,'(/,''P velocity in source depth: '',f6.2)') rzv(1)
          if(rzv(2).gt.0.d0) then
            write(11,'(''S velocity in source depth: '',f6.2)') rzv(2)
          endif
        endif

      endif

9999  continue

      if(output) close(11)

      stop

c     end program HYPOMOD_2.1
      end 
