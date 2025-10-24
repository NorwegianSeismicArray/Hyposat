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
      program HYPOSAT_6_2a2

      implicit real*8 (a-h,o-z)
      implicit integer (i-n)

      character  version*25, VDATE*20, cprog*50
      parameter (version='HYPOSAT Version 6.2a2    ' )
c     parameter (vdate=' ( 23 October 2025)' )
      parameter (vdate=' ' )

c
c     last changes: 23 October 2025
c
c----------------------------------------------------------------------
c
c                        Short desciption 
c
c     (For more details see the newest HYPOSAT-Manual and HYPOSAT 
c      related publications.)
c
c     This program locates seismic events by iteratively inverting
c     observed onset times, backazimuths, and slowness values.
c
c     Different phases observed at one station can be used to
c     calculate travel-time differences. These differences are
c     then in addition used to invert for the hypocenter.
c
c     A preliminary epicenter will be defined with the backazimuth
c     observations, or with other available information.
c
c     If possible a preliminary source time will be estimated by a 
c     Wadati-Approach from the S-P travel-time difference(s) assuming 
c     a constant v(p)/v(s)=sqrt(3.) for each phase type separately.
c
c     The final location is done with a Single-Value-Decomposition
c     (SVD) algorithm, which results in a least squares fit for
c     (if possible) all four source parameters using travel-time 
c     models from tau-spline-type tables (i.e., IASP91, AK135, 
c     PREM, ...), and/or a local/regional model of horizontal layers.
c
c     All calculations are done for the Earth as a sphere and
c     travel times are corrected  for the ellipticity of the
c     Earth.
c
c     All available information can be used including standard
c     deviations for observed data and 'a priori' information
c     for the model parameter.
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
c                hyposat_gmi, indexx, isf_out_line, magfact, mult_ons, 
c                plane, tauget_mod, tauget_ray, testphase, ttloc, zo2to
c
c     functions: alpha1, alpha2, convlat, phase_type, phasw,
c                ddmax, dirdel, dmean, q2, radloc, wdepth, rdid
c                file_checkpara, getchi, dpythag,
c                read_event_id, read_origin, 
c                read_origin_head, read_phase_head, read_phase, 
c                write_origin, write_origin_head, lowcas, uppcas,
c                write_isf_error
c
c     data exchange by common blocks in include files:
c                
c                include 'gmi.h'
c                include 'lsq.h'
c                include 'ttimes.h'
c                include 'model.h'
c                include 'modelg.h'
c
c     PARAMETER settings for: mstat, mread, mvar, mosci0
c
c****6789012345678901234567890123456789012345678901234567890123456789012
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c     Functions called: variable definitions
c
      real*8           alpha1, alpha2, convlat, dirdel, q2, 
     +                 radloc, dmean, ddmax, getchi, dpythag, wdepth,
     +                 rdig
     
      character        phase_type*1, file_check*512, file_checkpara*512,
     +                 filepara*512, lowcas*1024, uppcas*1024, 
     +                 chgcas*1024, get_mtyp*3, phasw*8

c
c     mstat = maximum number of stations
c
      parameter (mstat = 2000)

      dimension stala(mstat),stalo(mstat),stael(mstat),del(mstat),
     +          azie(mstat),baz(mstat),stavp(mstat),
     +          stavs(mstat),istaph(mstat),stats(mstat),statp(mstat),
     +          delk(mstat),istad(mstat),istfil(mstat),statr(mstat),
     +          stamb(mstat),stams(mstat),staml(mstat)

      character*5 sta(mstat),stat,stato,statw,stat1,stationfile*512,
     +          statcorfile*512,outputfile*512,inputfile*512,
     +          magmlfile*512,outputisf*200,inputfilen*512,
     +          json_file*512,isf_file*512,statfile2*512
c
c     mread = maximum number of phases (hyposat-in file)
c
c     mrd2 = maximum number of observations per station
c
c
c     !!!! A T T E N T I O N !!!!
c
c     when changing these parameters remember to change also 
c     gmi.h & gm2.h
c

      parameter (mread = 4000, mr2=mread/2, mrd2 = 150)

      parameter (mloc  = (mread/2 + 3)*mread)

      character phase(mread)*8,phaseu(mread)*8,phid*8,used(mread)*6,
     +          phid2*8,text2(mrd2)*160,phid1*8,phase_t*1,phidr0*8,
     +          string*550,touse(mread)*9,touse0*9,phidr*8,phipl*8,
     +          phai*8,phaj*8,o_string*550,textout*160,text(mread)*160,
     +          arid(mread)*8,statcorstr*80, texth*160,phid0*8,
     +          comment(mread)*72,comm2(mrd2)*72,usedm*6,phsearch*1,
     +          useds*6, phcheck*8, usedr*1, usedsr*1, 
     +          textouts*1024,stringt*30,onflag(mread)*3

      dimension azi(mread),tt(mread),p(mread),azis(mread),tts(mread),
     +          ps(mread),period(mread),amplit(mread),dinv(mread,4),
     +          tt2(mread),ttu(mread),iev(mread),indx(mread),
     +          indx2(mrd2),idtu(mread),snr(mread),emeran(mread),
     +          stamag(mread),istmag(mread),tt3(mrd2),smagn(mread)

      real*4    arr(mread),epiaz(mread),epiaz2(mread),dazgap,d1azi,
     +          d2azi,dazgap2,d1azi2,d2azi2

      dimension elo(mloc),ela(mloc),elos(mloc),elas(mloc)
c
c     include file for common block and variable definition GMI 
c
      include 'gmi.h'

      dimension var2(mvar)

c
c     include file for common block and variable definition LSQ 
c
      include 'lsq.h'

c
c     mosci0 is the maximum number of consequent solutions checked for
c     oscillating results.
c
      parameter (mosci0=15)

      dimension dzoos(mosci0),dtos(mosci0),dlaos(mosci0),
     +          dlo1os(mosci0),dloos(mosci0),dlo2os(mosci0),
     +          rzos(mosci0),rtos(mosci0),rlaos(mosci0),rloos(mosci0)

c
c     variables for travel-time calculations
c
      include 'ttimes.h'

      dimension dpdh(mphas), dtdd1(mphas), dtdd2(mphas), ttc1(mphas),
     +          dtdh1(mphas), dddp1(mphas)

      character phcd1(mphas)*8,phcd2(mphas)*8,art*16, mtyp0*3

      real*4 rzo,rdel,razi,rzo1,rzo2,rdel1, rdelk

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
     +          phisf*10, isf_ref*10, phidd*10, author2*10, dformat*6,
     +          dsformat*5, corid*8, corid2*8, cpick*1, cduma*1, 
     +          cpol*1, cdumi*1

      real*4    rdum, rpa, ramp, rper, rsnr, rlati, rloni, rdepi,
     +          rdmi, rdma, rdum0, relmax, relmin

c
c     Functions called from the ISF software libarary
c
      integer   read_event_id, read_origin, read_origin_head, 
     +          read_phase_head, read_phase, write_origin, 
     +          write_origin_head, write_data_type, write_event_id,
     +          write_comment, write_netmag_head, write_netmag,
     +          write_phase_head, write_isf_error

c
c     other variables
c
      integer   yy,mon,dd,hh,idoy,ierr,typctl,mi,idum, isreg, regnum,
     +          typctlm, y00, mon00, d00, h00, idetyp, json_rc

      character mm*4,name*48
      real*8    lat,lon,kmdel,dlati,dloni,ddepi, elevs, dazir, cpq, cpq2
      real*4    sec, rlat, rlon, smag

      character title*140, czo*1, region*80, czo1*1, magtypp*3, cfix*8,
     +          magtyps*6, magtypml*7, statmag*2, c1typ*2, c1type*2,
     +          cfi*1, downw*4

c
c     idtmax = number of different travel-time-difference definitions
c              used for calculating a initial value for the source time
c              by using the Wadati-approach
c
      parameter (idtmax = 4)

      dimension dt(idtmax,mr2),idtp(idtmax,mr2),idts(idtmax,mr2),
     +          idt(idtmax),to(idtmax),tos(idtmax),vpvs(idtmax),
     +          vpvss(idtmax),datho(mread),datla(mread),datlo(mread),
     +          ttt(mread),tttr(mread)

      logical   zoflag , vlflag  , stcorfl, iloc, surf, surff, surfm,
     +          diffflag, dtmflag, epistart, single , output, modout, 
     +          last, magflag, lastfixi, direct, plflag, wflag,
     +          conr, rayok, rayokf, aziini, aziall,
     +          kmout, thbaz, thray, tresw, lastfixt, lastfixm,
     +          mttres, isf_in, isf_out, unc_out, fixinp, lsmu, 
     +          ref_eve, pflag, lgflag, sgflag, wadati, check_mult, 
     +          aziflag, sloflag, gapobs, isf_epi, isf_dep, isf_all, 
     +          new_in, lgsurf, rgsurf, tsurf, lpsurf, isurf,
     +          lrmsisc, firston, firstph, first2, azionlyf, ldepth0,
     +          old_syntax, emerout, larid, fsetloc, ldefd(3), ldefisc,
     +          lcomfix, primef, lstcor, ftvar, favar, fsvar, fdtvar,
     +          ftw, faw, fsw, fdtw, fth, fah, fsh, fdth, eflag, fconr,
     +          fmoho, ldist, llimdel, lwdepth, json_out, lauto, lautot,
     +          lautor, l_noepi, lwater, lmaxm, lpick

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
c     Earth figure after Stacey (1992), Physcis of the Earth
c
c     rada    = 6378.136d0
c     radb    = 6356.751d0
c
c    changed to WGS84 5 August 2025
c
      rada    = 6378.137d0
      radb    = 6356.7523142d0

      rearth  = 6371.d0

      eps     = q2(radb/rada)
      grad1   = rad2deg/rearth

      epsilon = 1.0d-9

      dtp0    = 600.d0
      dts0    = dtp0*1.4d0
      dtm2    = 2.2d0*dts0
      dtdt    = 10.d0

      dismin  = pi

      ttray   = 0.d0
      ddel    = 0.d0

      dchang0 = 1.d0

      check   = 9999.d0
      disper  = 0.001d0
      rmso    = 9999.d0
      rms1    = 0.d0
      rmsold  = 9999.d0
      datmax0 = 9999.d0
      nrms1   = 0

      miteras = 0
      iterz   = 0
      iteraz  = 0
      itso    = 0
      ibad0   = 0
      in0sw   = 0
      infind  = 0
      idepm   = 0

      insar   = 0

      nextiter1 = 0
      imaxiter  = 0
      ilastiter = 0
      mosci     = 4
      
      cevid  = '9999999999'
      corid  = '_'
      corid2 = ' '
      AUTHOR = 'HYPOSAT'

      vlflag   = .true.
      zoflag   = .false.
      stcorfl  = .false.
      diffflag = .true.
      iloc     = .false.
      epistart = .false.
      lauto    = .false.
      lautot   = .false.
      lautor   = .false.
      lpick    = .false.
      l_noepi  = .false.
      fsetloc  = .false.
      single   = .false.
      output   = .false.
      json_out = .false.
      isf_in   = .false.
      isf_out  = .false.
      unc_out = .false.
      isf_epi  = .false.
      isf_dep  = .false.
      isf_all  = .false.
      iellip   = .true.
      modout   = .false.
      lastfixi = .false.
      lastfixt = .false.
      lastfixm = .false.
      plflag   = .true.
      dtmflag  = .false.
      rayok    = .false.
      rayokf   = .false.
      aziini   = .false.
      aziall   = .false.
      kmout    = .false.
      thray    = .false.
      thbaz    = .false.
      tresw    = .false.
      mttres   = .true.
      fixinp   = .false.
      locgeo   = .false.
      locsta   = .false.
      locmod   = .false.
      ref_eve  = .false.
      pflag    = .false.
      lgflag   = .false.
      sgflag   = .false.
      lgsurf   = .true.
      rgsurf   = .true.
      lpsurf   = .false.
      tsurf    = .false.
      isurf    = .false.
      check_mult = .false.
      new_in   = .false.
      aziflag  = .true.
      azionlyf = .false.
      sloflag  = .true.
      gapobs  = .false.
      firstph = .false.
      emerout = .false.
      lcomfix = .false.
      ldefd(1)= .false.
      ldefd(2)= .false.
      ldefd(3)= .false.
      ldefisc = .false.

      ftvar   = .false.
      favar   = .false.
      fsvar   = .false.
      fdtvar  = .false.

      ftw     = .false.
      faw     = .false.
      fsw     = .false.
      fdtw    = .false.

      fth     = .false.
      fah     = .false.
      fsh     = .false.
      fdth    = .false.

      ldepth0 = .false.
      lwdepth = .false.
      lwater  = .false.
      zwater  = 0.d0

      ldist   =  .false.

      ideptyp = 1
      c1typ   = 'mc'

      lrmsisc = .false.

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
      statfile2   = 'stations.dat'
      outputfile  = 'hyposat-out'
      inputfile   = 'hyposat-in'
      isf_file = 'ISF'
      json_file = 'JSON'
      json_rc = 0
      json_prec = 12
      old_syntax = .false.
      statcorfile = ' '
      istcor = 0
      vpl = 5.8d0
      vsl = 3.46d0
      zo1 =  0.d0
      sdzo1 = 50.d0
      czo = 'F'
      depthmin = 0.d0
      depthmax = 800.d0
      maxiter = 80
      confl = 68.26895d0
      iellipi = 1
      dazim0 = 50.d0
      dpam0 = 10.d0
      typctl = 4
      islow = 1
      setcheck1 = -999.d0
      setcheck  = 1.d0
      thrfixi0 = 0.005d0
      indph0 = 3333
      epilat0 = -999.d0
      epilon0 = -999.d0
      sdlatg0  = 10.d0
      sdlatgi0 = 10.d0
      sdlat0   = -10.d0
      sdlon0   = 10.d0
      sdloni0  = 10.d0
      tome0   = -2840140801.d0
      stome0  = 120.d0
      wadmin  = 0.d0
      wadmax  = 300.d0
      dismaxst = 180.d0
      disminst = -1.d0

      iwl = 0
      dtdw = 2.d0

      elmax  = -999.d0
      elmin  = -999.d0
      ieazi  = -999

      dtphmax = 5.d0

      dtmaxazib = 30.d0
      dtmaxazil = 180.d0
      dtmaxslow = 15.d0

      resmaxp = 30.d0
      resmaxs = 30.d0

      sglgdis = -999.d0

      smpu = -9.d0
      smpnu = -9.d0 
      smpbu = -9.d0 
      smpgu = -9.d0 
      smsu = -9.d0 
      smsnu = -9.d0 
      smsbu = -9.d0 
      smsgu = -9.d0 
      smlgu = -9.d0 
      lsmu  =  .false.
      var2(1) = 0.d0
      var2(2) = 0.d0
      var2(3) = 0.d0
      var2(4) = 0.d0

      phidr0   = ' '
      string   = ' '
      o_string = ' '

      rdmi = 25000.
      rdma = 0.

      sisfi    = 0.1d0
      sisfe    = 1.0d0
      sisfo    = 2.0d0
      sisfaz   = 10.d0
      sisfsl   = 1.d0
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

      delmbmin = 0.d0
      delmbmax = 180.d0
      delmsmin = 0.d0
      delmsmax = 180.d0
      delmlmin = 0.d0
      delmlmax = 20.d0

      magtypp  = 'G-R'
      magtyps  = 'IASPEI'
      magtypml = 'Bath'
      magmlfile = 'MLCORR.TABLE'
      lmaxm    = .true.

      treswf   = 1.d0

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

      do 1 jin = 1,2000

      read (9,'(a)',end=2) string

      string = adjustl(string)

      if(string.eq.' ') go to 1
      if(string(1:1).eq.'*') go to 1
      if(string(1:1).eq.'?') go to 1

      icolon = index(string,':')

      if(icolon.gt.0) then
         icolon1 = icolon-1
         chgcas = uppcas(string(1:icolon1))
         string(1:icolon1) = chgcas(1:icolon1)
      else 
         print *,' Wrong syntax (ignored): ',trim(string)
         go to 1
      endif

      if(string(icolon+1:icolon+1).ne.' ') then
         print *,' Wrong syntax (ignored): ',trim(string)
         go to 1
      endif

      icolon2 = icolon+2

      if(string(1:14).eq.'GLOBAL MODEL 1' .or.
     +   (string(1:12).eq.'GLOBAL MODEL' .and. string(13:14).ne.' 2'
     +    .and. string(13:14).ne.' 3' .and. string(13:14).ne.' 4')
     +     ) then
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
          if(intinp.eq.1) then
             locsta = .true.
             locgeo = .true.
          endif
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
          string(1:9) = 'CRUST 1.0'
      endif

      if(string(1:20).eq.'CRUST 1.0 DEPTH TYPE') then
          c1typ = 'mc'
          read (string(icolon2:),'(a)') c1type
          chgcas = lowcas(c1type)
          if(chgcas(1:2).eq.'uc' .or. chgcas(1:2).eq.'mc' .or.
     +       chgcas(1:2).eq.'lc' .or. chgcas(1:2).eq.'mo' ) 
     +       c1typ = chgcas(1:2)
          go to 1
      endif
          
      if(string(1:14).eq.'CRUST 1.0 PATH') go to 1

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

      if(string(1:24).eq.'ALTERNATIVE STATION FILE') then
          read (string(icolon2:),'(a)') statfile2
          statfile2 = file_check(statfile2)
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

      if(string(1:17).eq.'PLANE WAVE APPROX') then
          intinp = 0
          read (string(icolon2:),*) intinp
          if(intinp.ne.1) plflag = .false.
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

      if(string(1:25).eq.'STARTING TIME UNCERTAINTY' .or.
     +   string(1:19).eq.'STARTING TIME ERROR') then
          read (string(icolon2:),*) stome0
          go to 1
      endif

      if(string(1:28).eq.'DOUBLE SIDED TIME UNCERTAINTY' .or.
     +   string(1:22).eq.'DOUBLE SIDED TIME ERROR') then
          tresw = .false.
          read (string(icolon2:),*) itresw
          if(itresw.eq.1 .or. itresw.eq.2) then
             tresw  = .true.
          endif
          go to 1
      endif

      if(string(1:28).eq.'DBLE SID. UNCERTAINTY FACTOR' .or.
     +   string(1:22).eq.'DBLE SID. ERROR FACTOR') then
          read (string(icolon2:),*) treswf
          if(treswf.le.0.d0) treswf = 1.d0
          go to 1
      endif

      if(string(1:21).eq.'CHANGE WEIGHTING TASD' .or.
     +   string(1:18).eq.'DOWNWEIGHTING TASD') then
          downw = ' '
          read (string(icolon2:),'(a4)') downw
          if(downw(1:1).eq.'1') ftvar = .true.
          if(downw(2:2).eq.'1') favar = .true.
          if(downw(3:3).eq.'1') fsvar = .true.
          if(downw(4:4).eq.'1') fdtvar = .true.
          if(downw(1:1).eq.'2') ftw   = .true.
          if(downw(2:2).eq.'2') faw   = .true.
          if(downw(3:3).eq.'2') fsw   = .true.
          if(downw(4:4).eq.'2') fdtw   = .true.
          if(downw(1:1).eq.'3') fth   = .true.
          if(downw(2:2).eq.'3') fah   = .true.
          if(downw(3:3).eq.'3') fsh   = .true.
          if(downw(4:4).eq.'3') fdth   = .true.
          go to 1
      endif

      if(string(1:21).eq.'STARTING SOURCE DEPTH') then
          read (string(icolon2:),*) zo1
          go to 1
      endif

      if(string(1:26).eq.'STARTING DEPTH UNCERTAINTY' .or.
     +   string(1:20).eq.'STARTING DEPTH ERROR') then
          read (string(icolon2:),*) sdzo1
          if(sdzo1.eq.0.d0) sdzo1 = 50.d0
          go to 1
      endif

      if(string(1:10).eq.'DEPTH FLAG') then
          ldefisc = .false.
          read (string(icolon2:),'(a)') czo
          if(czo .eq. 'i' .or. czo.eq.'I') then
             ldefisc = .true.
             czo = 'F'
          endif
          if(czo .eq. 'j' .or. czo.eq.'J') then
             ldefisc = .true.
             czo = 'B'
          endif
          go to 1
      endif

      if(string(1:18).eq.'DEFAULT DEPTH TYPE') then
          intinp = 0
          read (string(icolon2:),*) intinp
          if(intinp.gt.0 .and. intinp.lt.5) then
             ideptyp = intinp
          else
             ideptyp = 1
          endif
          go to 1
      endif

      if(string(1:27).eq.'CHECK DEPTH FOR WATER LAYER') then
          intinp = 0
          lwdepth   = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) lwdepth   = .true.
          go to 1
      endif

      if(string(1:20).eq.'DEPTH ALLOWED ABOVE 0') then
          intinp = 0
          ldepth0   = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) ldepth0   = .true.
          go to 1
      endif

      if(string(1:13).eq.'MINIMUM DEPTH') then
          read (string(icolon2:),*) depthmin
          if(depthmin.lt.0.d0) depthmin = 0.d0
          go to 1
      endif

      if(string(1:13).eq.'MAXIMUM DEPTH') then
          read (string(icolon2:),*) depthmax
          if(depthmax.gt.800.d0) depthmax = 800.d0
          go to 1
      endif

      if(string(1:25).eq.'AUTOMATIC PROCESSING TELE') then
          intinp = 0
          lautot = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) then
             lpick = .true.
             lautot  = .true.
          endif
          go to 1
      endif

      if(string(1:29).eq.'AUTOMATIC PROCESSING REGIONAL') then
          intinp = 0
          lautot = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) then
             lpick = .true.
             lautor  = .true.
          endif
          go to 1
      endif

      if(string(1:20).eq.'AUTOMATIC PROCESSING') then
          intinp = 0
          lauto  = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) then
             lpick = .true. 
             lauto   = .true.
             lautot  = .true.
             lautor  = .true.
          endif
          go to 1
      endif

      if(string(1:24).eq.'STARTING SOURCE LATITUDE') then
          abc = -999.0d0
          if(string(icolon2:icolon2).eq.'_') then
            epilat0 = abc
            go to 1
          endif
          read (string(icolon2:),*) abc
          if(abc.ge.-90.d0 .and. abc.le.90.d0) epilat0 = abc
          go to 1
      endif

      if(string(1:29).eq.'STARTING LATITUDE UNCERTAINTY' .or.
     +   string(1:23).eq.'STARTING LATITUDE ERROR') then
          read (string(icolon2:),*) sdlatgi0
          go to 1
      endif

      if(string(1:25).eq.'STARTING SOURCE LONGITUDE') then
          abc = -999.0d0
          if(string(icolon2:icolon2).eq.'_') then
            epilon0 = abc
            go to 1
          endif
          read (string(icolon2:),*) abc
          if(abc.ge.-180.d0 .and. abc.le.180.d0) epilon0 = abc
          go to 1
      endif

      if(string(1:30).eq.'STARTING LONGITUDE UNCERTAINTY' .or.
     +   string(1:24).eq.'STARTING LONGITUDE ERROR') then
          read (string(icolon2:),*) sdloni0
          go to 1
      endif

      if(string(1:27).eq.'INCLUDING MODEL UNCERTAINTY') then
          intinp = 0
          lsmu   = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) lsmu   = .true.
          go to 1
      endif

      if(string(1:29).eq.'MEAN P-WAVE MODEL UNCERTAINTY') then
          abc = 0.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) smpu = abc
          go to 1
      endif

      if(string(1:31).eq.'MEAN PN TRAVEL-TIME UNCERTAINTY') then
          abc = 0.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) smpnu = abc
          go to 1
      endif

      if(string(1:31).eq.'MEAN PB TRAVEL-TIME UNCERTAINTY') then
          abc = 0.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) smpbu = abc
          go to 1
      endif

      if(string(1:31).eq.'MEAN PG TRAVEL-TIME UNCERTAINTY') then
          abc = 0.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) smpgu = abc
          go to 1
      endif

      if(string(1:29).eq.'MEAN S-WAVE MODEL UNCERTAINTY') then
          abc = 0.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) smsu = abc
          go to 1
      endif

      if(string(1:31).eq.'MEAN SN TRAVEL-TIME UNCERTAINTY') then
          abc = 0.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) smsnu = abc
          go to 1
      endif

      if(string(1:31).eq.'MEAN SB TRAVEL-TIME UNCERTAINTY') then
          abc = 0.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) smsbu = abc
          go to 1
      endif

      if(string(1:31).eq.'MEAN SG TRAVEL-TIME UNCERTAINTY') then
          abc = 0.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) smsgu = abc
          go to 1
      endif

      if(string(1:31).eq.'MEAN LG TRAVEL-TIME UNCERTAINTY') then
          abc = 0.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) smlgu = abc
          go to 1
      endif

      if(string(1:17).eq.'MIN DT FOR WADATI') then
          read (string(icolon2:),*) wadmin
          if(wadmin.lt.0.d0) wadmin = 0.d0
          go to 1
      endif

      if(string(1:17).eq.'MAX DT FOR WADATI') then
          read (string(icolon2:),*) wadmax
          go to 1
      endif

      if(string(1:20).eq.'MAX EPI DIST OF STAT') then
          read (string(icolon2:),*) dismaxst
          go to 1
      endif

      if(string(1:20).eq.'MIN EPI DIST OF STAT') then
          read (string(icolon2:),*) disminst
          go to 1
      endif

      if(string(1:20).eq.'OUTPUT ALL DISTANCES') then
          intinp = 0
          ldist   = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) ldist   = .true.
          go to 1
      endif

      if(string(1:23).eq.'MAXIMUM # OF ITERATIONS') then
          intinp = 0
          read (string(icolon2:),*) intinp
          if(intinp.gt.0) maxiter = intinp
          go to 1
      endif

      if(string(1:33).eq.'ITERATIONS TO SEARCH OSCILLATIONS' .or.
     +   string(1:24).eq.'# TO SEARCH OSCILLATIONS') then
          intinp = 0
          read (string(icolon2:),*) intinp
          if(intinp .gt. mosci0) then
              print *, 'MAX # TO SEARCH OSCILLATIONS SET TO:',mosci0
              mosci = mosci0
          else if(intinp .lt. 1) then
             mosci = 1
          else
             mosci = intinp
          endif
          go to 1
      endif

      if(string(1:16).eq.'CONFIDENCE LEVEL') then
          read (string(icolon2:),*) confl
          if(confl.lt.68.26895d0)  confl = 68.26895d0
          if(confl.gt.99.99d0)     confl = 99.99d0
          go to 1
      endif

      if(string(1:18).eq.'CONSTRAIN SOLUTION' .and. .not.fixinp) then
          intinp = 0
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) then
             lastfixt = .true.
             lastfixi = .false.
             lastfixm = .false.
          else if(intinp.eq.2) then
             lastfixt = .false.
             lastfixi = .true.
             lastfixm = .false.
          else if(intinp.eq.3) then
             lastfixt = .true.
             lastfixi = .true.
             lastfixm = .false.
          else if(intinp.eq.4) then
             lastfixt = .true.
             lastfixi = .false.
             lastfixm = .true.
          else if(intinp.eq.5) then
             lastfixt = .true.
             lastfixi = .true.
             lastfixm = .true.
          else
             lastfixt = .false.
             lastfixi = .false.
             lastfixm = .false.
          endif
          go to 1
      endif

      if(string(1:26).eq.'MAXIMUM ALLOWED P RESIDUAL' .or.
     +   string(1:26).eq.'MAXIMUM ALLOWED P RESIDUUM') then
          abc = -9.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) then
             resmaxp = abc
          else
             resmaxp = 30.d0
          endif
          go to 1
      endif

      if(string(1:26).eq.'MAXIMUM ALLOWED S RESIDUAL' .or.
     +   string(1:26).eq.'MAXIMUM ALLOWED S RESIDUUM') then
          abc = -9.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) then
             resmaxs = abc
          else
             resmaxs = 30.d0
          endif
          go to 1
      endif

      if(string(1:20).eq.'MEAN T-T RES. CORREC') then
          intinp = 0
          mttres = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) mttres = .true.
          go to 1
      endif

      if(string(1:29).eq.'INF. DENSITY MATRIX THRESHOLD') then
          read (string(icolon2:),*) thrfixi0
          if(thrfixi0.gt.1.d0) then
             thrfixi0  = 1.d0
          else if(thrfixi0.le.0.d0) then
             thrfixi0  = -1.d0
          endif
          go to 1
      endif

      if(string(1:30).eq.'EPICENTER UNCERTAINTY ELLIPSE' .or.
     +   string(1:23).eq.'EPICENTER ERROR ELLIPSE') then
          iellip = .true.
          read (string(icolon2:),*) iellipi
          if(iellipi.ne.1) iellip = .false.
          go to 1
      endif

      if(string(1:30).eq.'AZIMUTHAL GAP FOR OBSERVATIONS') then
          intinp = 0
          gapobs = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) gapobs = .true.
          go to 1
      endif

      if(string(1:20).eq.'MAXIMUM BAZ RESIDUAL' .or.
     +   string(1:20).eq.'MAXIMUM BAZ RESIDUUM' .or.
     +   string(1:21).eq.'MAXIMUM AZIMUTH ERROR') then
          read (string(icolon2:),*) dazim0
          go to 1
      endif

      if(string(1:12).eq.'BAZ ONLY LOC' .or.
     +   string(1:16).eq.'AZIMUTH ONLY LOC') then
          intinp = 0
          azionlyf = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1)  azionlyf = .true.
          go to 1
      endif

      if(string(1:15).eq.'BAZ AS DEFINING' .or.
     +   string(1:19).eq.'AZIMUTH AS DEFINING') then
          intinp = 0
          aziflag = .true.
          read (string(icolon2:),*) intinp
          if(intinp.ne.1) aziflag = .false.
          go to 1
      endif

      if((string(1:26).eq.'MAX T RES FOR BAZ OF B USE' .or.
     +   string(1:26).eq.'MAX T RES FOR AZI OF B USE' ) .and.
     +   .not.aziall) then
          read (string(icolon2:),*) dtmaxazib 
          go to 1
      endif

      if((string(1:26).eq.'MAX T RES FOR BAZ OF L USE' .or.
     +   string(1:26).eq.'MAX T RES FOR AZI OF L USE' ) .and.
     +   .not.aziall) then
          read (string(icolon2:),*) dtmaxazil
          go to 1
      endif

      if(string(1:23).eq.'USE ALL BAZ INFORMATION' .or.
     +   string(1:27).eq.'USE ALL AZIMUTH INFORMATION') then
          intinp = 0
          aziall = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1 .and. .not.aziini) then
            aziall = .true.
            dtmaxazib = 99999.d0
            dtmaxazil = 99999.d0
          endif
          go to 1
      endif

      if(string(1:17).eq.'BAZ ONLY INIT SOL' .or.
     +   string(1:21).eq.'AZIMUTH ONLY INIT SOL') then
          intinp = 0
          aziini = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) then
            aziini = .true.
            aziall = .false.
          endif
          go to 1
      endif

      if(string(1:20).eq.'SLOWNESS AS DEFINING') then
          intinp = 0
          sloflag = .true.
          read (string(icolon2:),*) intinp
          if(intinp.ne.1) sloflag = .false.
          go to 1
      endif

      if(string(1:25).eq.'MAXIMUM SLOWNESS RESIDUAL' .or.
     +   string(1:25).eq.'MAXIMUM SLOWNESS RESIDUUM' .or.
     +   string(1:22).eq.'MAXIMUM SLOWNESS ERROR') then
          read (string(icolon2:),*) dpam0
          go to 1
      endif

      if(string(1:26).eq.'MAX T RES FOR SLOWNESS USE') then
          read (string(icolon2:),*) dtmaxslow
          go to 1
      endif

      if(string(1:12).eq.'OUTPUT LEVEL') then
          read (string(icolon2:),*) typctl
          typctlm = typctl
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

      if(string(1:17).eq.'LOCATION ACCURACY') then
          read (string(icolon2:),*) setcheck1
          go to 1
      endif

      if(string(1:27).eq.'PHASE INDEX FOR LOCAL MODEL') then
          read (string(icolon2:),*) indph0
          if(indph0.gt.3333) then
            print *,'Wrong input: PHASE INDEX FOR LOCAL MODEL'
            go to 9999
          endif
          go to 1
      endif

      if(string(1:34).eq.'FLAG USING TRAVEL-TIME DIFFERENCES') then
          intinp = 0
          diffflag = .true.
          read (string(icolon2:),*) intinp
          if(intinp.ne.1) diffflag = .false.
          go to 1
      endif

      if(string(1:22).eq.'FLAG USING INPUT FIXED') then
          intinp = 0
          fixinp = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) then
            fixinp = .true.
            lastfixt = .false.
            lastfixi = .false.
            lastfixm = .false.
            dtmaxazib = 50.d0
            dtmaxazil = 50.d0
            dtmaxslow = 10.d0
          endif
          go to 1
      endif

      if(string(1:29).eq.'FLAG CHECKING MULTIPLE ONSETS') then
          intinp = 0
          check_mult = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) check_mult = .true.
          go to 1
      endif

      if(string(1:19).eq.'FLAG NEW INPUT FILE') then
          intinp = 0
          new_in = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) new_in = .true.
          go to 1
      endif

      if(string(1:26).eq.'MAX DT FOR MULTIPLE ONSETS') then
          read (string(icolon2:),*) dtphmax
          if(dtphmax.le.0.d0) dtphmax = 5.0d0
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

      if(string(1:9).eq.'ISF DEPTH') then
          intinp = 0
          isf_dep =  .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) isf_dep = .true.
          go to 1
      endif

      if(string(1:14).eq.'ISF ALL PHASES') then
          intinp = 0
          isf_all =  .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) isf_all=.true.
          go to 1
      endif

      if(string(1:23).eq.'TT DATA UNCERTAINTY OUT') then
          intinp = 0
          unc_out = .false.
          read (string(icolon2:),*) intinp 
          if(intinp.eq.1) unc_out = .true.
          go to 1
      endif

      if(string(1:5).eq.'ISF_i') then
          read (string(icolon2:),*) sisfi 
          if(sisfi.le.0.d0) sisfi = 0.1d0
          go to 1
      endif

      if(string(1:5).eq.'ISF_e') then
          read (string(icolon2:),*) sisfe
          if(sisfe.le.0.d0) sisfe = 1.0d0
          go to 1
      endif

      if(string(1:5).eq.'ISF_o') then
          read (string(icolon2:),*) sisfo
          if(sisfo.le.0.d0) sisfo = 2.d0
          go to 1
      endif

      if(string(1:7).eq.'ISF_baz') then
          read (string(icolon2:),*) sisfaz
          if(sisfaz.le.0.d0) sisfaz = 10.d0
          go to 1
      endif

      if(string(1:7).eq.'ISF_slo') then
          read (string(icolon2:),*) sisfsl
          if(sisfsl.le.0.d0) sisfsl = 1.d0
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

      if(string(1:17).eq.'OUTPUT FORMAT ISF') then
          intinp = 0
          isf_out = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) isf_out=.true.
          go to 1
      endif

      if(string(1:15).eq.'ISF OUTPUT FILE') then
          if(string(icolon2:icolon2).ne.' ' .and. 
     +       string(icolon2:icolon2) .ne.'_'  )
     +       read (string(icolon2:),*) isf_file
          go to 1
      endif

      if(string(1:16).eq.'ISC-TYPE ISF RMS') then
          intinp = 0
          lrmsisc = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) lrmsisc=.true.
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

      if(string(1:18).eq.'OUTPUT FORMAT JSON') then
          intinp = 0
          json_out =.false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) json_out = .true.
          go to 1
      endif

      if(string(1:16).eq.'JSON OUTPUT FILE') then
          if(string(icolon2:icolon2).ne.' ' .and. 
     +       string(icolon2:icolon2) .ne.'_'  )
     +       read (string(icolon2:),*) json_file
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

      if(string(1:25).eq.'REGIONAL SURFACE WAVES LG') then
          intinp = 0
          lgsurf = .true.
          read (string(icolon2:),*) intinp
          if(intinp.ne.1) lgsurf = .false.
          go to 1
      endif

      if(string(1:17).eq.'LG GROUP-VELOCITY') then
         read (string(icolon2:),*) vlg
         if(vlg.le.0.d0) vlg = vlg0
         go to 1
      endif

      if(string(1:25).eq.'REGIONAL SURFACE WAVES RG') then
          intinp = 0
          rgsurf = .true.
          read (string(icolon2:),*) intinp
          if(intinp.ne.1) rgsurf = .false.
          go to 1
      endif

      if(string(1:17).eq.'RG GROUP-VELOCITY') then
          read (string(icolon2:),*) vrg
          if(vrg.le.0.d0) vrg = vrg0
          go to 1
      endif

      if(string(1:24).eq.'LP SURFACE WAVES (LQ/LR)') then
          intinp = 0
          lpsurf = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) lpsurf = .true.
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

      if(string(1:13).eq.'T-PHASE USAGE') then
          intinp = 0
          tsurf = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) tsurf = .true.
          go to 1
      endif

      if(string(1:17).eq.'T-PHASE GROUP-VEL' .or.
     +   string(1:17).eq.'T PHASE GROUP-VEL') then
          read (string(icolon2:),*) vt
          if(vt.le.0.d0) vt = vt0
          go to 1
      endif

      if(string(1:14).eq.'IS-PHASE USAGE') then
          intinp = 0
          isurf = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) isurf = .true.
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

      if(string(1:14).eq.'WATER LAYER DT') then
          abc = -999.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) dtdw = abc
          go to 1
      endif
      
      if(string(1:18).eq.'P-TYPE ONSETS ONLY') then
          intinp = 0
          pflag = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) pflag = .true.
          go to 1
      endif

      if(string(1:14).eq.'LG-PHASE TO SG') then
          intinp = 0
          lgflag = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) lgflag = .true.
          go to 1
      endif

      if(string(1:14).eq.'SG-PHASE TO LG') then
          intinp = 0
          sgflag = .false.
          read (string(icolon2:),*) intinp
          if(intinp.eq.1) sgflag = .true.
          go to 1
      endif

      if(string(1:15).eq.'SG--LG DISTANCE') then
          abc = -999.d0
          read (string(icolon2:),*) abc
          if(abc.gt.0.d0) sglgdis = abc
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

c
c     OUTFILES
c

      cprog = trim(version) // trim(vdate)

      if(output) then
         open (unit=11,file=outputfile)
         write (11,'(a,/)') trim(cprog)
         write (11,'(''Event solution by '',a,/)') 
     +          trim(author)
      endif

      if(typctl.ge.0) then
         print *,'PROGRAM ',trim(cprog)
      endif

      if(kmout) then
        dismaxst = dismaxst * 111.5d0
        disminst = disminst * 110.5d0
      endif

      if(isf_out) then

        if(.not.kmout) then
           
           if(trim(isf_file).eq.'ISF') then
              if(trim(outputfile).eq.'hyposat-out') then
                 isf_file = 'hyposat-out.isf'
              else
                 isf_file = trim(outputfile) // '.isf'
              endif
           else
              isf_file = trim(isf_file)
           endif

        else
           isf_out = .false.
           print *, 'ISF output not possible: distance units are',
     +           ' in [km]!'
           isf_file = 'no ISF out'
        endif
      endif

      if(json_out) then

        if(trim(json_file).eq.'JSON') then
           if(trim(outputfile).eq.'hyposat-out') then
              json_file = 'hyposat-out.json'
           else
              json_file = trim(outputfile) // '.json'
           endif
        else
           json_file = trim(json_file)
        endif

        call json_start_dict(json_file, json_prec, json_rc)
        call json_add_string("version", cprog, json_rc)
        call json_add_string("author", author, json_rc)

      endif

      fchi1 = dsqrt(getchi(1.0d0,confl))
      fchi2 = dsqrt(getchi(2.0d0,confl))

      if(zo1.lt.depthmin) zo1 = depthmin
      if(zo1.gt.depthmax) zo1 = depthmax

      if(typctl.gt.4) then
         print *,'modnam  : ',modnam(1)
         if(modflag(2)) print *,'modnam 2: ',modnam(2)
         if(modflag(3)) print *,'modnam 3: ',modnam(3)
         if(modflag(4)) print *,'modnam 4: ',modnam(4)
         print *,'filloc = ',filloc
         print *,'imo = ',imo
         print *,'stationfile = ',stationfile
         print *,'statfile2 = ',statfile2
         print *,'statcorfile = ',statcorfile 
         print *,'inputfile   = ',inputfile 
         print *,'outputfile  = ',outputfile 
         print *,'output switch ',output 
         print *,'output json ',json_out
         print *,'JSON out file  = ',json_file 
         print *,'output isf ',isf_out 
         print *,'ISF out file  = ',isf_file 
         print *,'vpl = ',vpl
         print *,'vsl = ',vsl
         print *,'vrg = ',vrg
         print *,'vlg = ',vlg
         print *,'vlq = ',vlq
         print *,'vlr = ',vlr
         print *,'vt  = ',vt 
         print *,'zo1   = ',zo1
         print *,'sdzo1 = ',sdzo1
         print *,'czo   = ',czo
         print *,'epilat0 = ',epilat0
         print *,'sdlatgi0  = ',sdlatgi0
         print *,'epilon0 = ',epilon0
         print *,'sdloni0   = ',sdloni0
         print *,'maxiter = ',maxiter
         print *,'lastfixi = ',lastfixi
         print *,'lastfixi threshold= ',thrfixi0
         print *,'lastfixt = ',lastfixt
         print *,'lastfixm = ',lastfixm
         print *,'confl   = ',confl  
         print *,'dazim0 = ',dazim0
         print *,'dpam0  = ',dpam0
         print *,'typctl = ',typctl
         print *,'islow = ',islow
         print *,'setcheck1 = ',setcheck1
         print *,'indph0 = ',indph0 
         print *,'diffflag = ',diffflag
         print *,'Magnitude flags = ', magflag,' ',magtypp,' ',magtyps
     +           ,' ',magtypml,' ',lmaxm
         print *,'Plane wave = ', plflag
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
      pdif  = dtdd(1)

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

      if(stcorfl) open(13,file=statcorfile)

      if(setcheck1.gt.0.d0) then
         setcheck  = setcheck1
      else
         setcheck1 = setcheck
      endif

      rminh    = setcheck
      if(rminh.lt.0.1d0) rminh = 0.1d0
      rmint    = rminh/5.d0
      rming    = rminh*grad1

      setcheck2 = 15.d0*setcheck

      chgcas = uppcas(czo)
      czo = chgcas(1:1)
      if(czo.ne.'F' .and. czo.ne.'D' .and. czo.ne.'B' ) czo='F'
      zo = zo1

      if(epilat0.ge.-90.d0 .and. epilon0.ge.-180.0d0 .and.
     +   epilat0.le.90.d0 .and. epilon0.le.180.0d0 ) epistart = .true.

      do 3 i=1,mstat
      sta(i) = ' '
3     continue

c
c     read in all available observed data
c

      open (unit=10,file=inputfile,status='old')

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
         if(json_out) call json_add_int("isf_event_id", ievid, json_rc)
   
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

331      rlati = 0.
         rloni = 0.
         rdepi = 0.
         itest = read_origin(string(1:136),yyi,moni,ddi,hh,mi,isec,msec,
     +           cdum,rdum,rdum,rlati,rloni,cdum,rdum,rdum,idum1,rdepi,
     +           cdum,rdum,idum1,idum1,idum1,rdum,rdum,cdum,cdum,
     +           cdum3,author2,corid2)

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
            epistart = .true.
         endif
         if(isf_dep) then
            zo1     = ddepi
            if(zo1.lt.depthmin) zo1=depthmin
            if(zo1.gt.depthmax) zo1=depthmax
            zo      = zo1
         endif

34       read (10,'(a)',end=9999) string
         itest = read_phase_head(string(1:122))

         if(itest.eq.20) then 
c           itest = write_isf_error(typctl)
            go to 34
         endif

      else
         if(output) write (11,'(a,/)') trim(title)
         if(json_out) call json_add_string('description',
     +       trim(title), json_rc)
         print *,'EVENT ',trim(title)
      endif

      timemin = 9999999999.d0

      stalam = 0.d0
      stalom = 0.d0
      stalom1 = 0.d0
      stalom2 = 0.d0

      terrm = 0.d0
      nobsst = 0

      isnr = 0

      sdpmean = 0.d0
      nsdp    = 0
      sdsmean = 0.d0
      nsds    = 0
      sdmeans = 0.d0

      ii = 0
      string = ' '

      do 13 i=1,mread+200

      read (10,'(a)',end=14) string

      if(string(1:4).eq.'STOP') go to 14

      if(string(1:1).eq.'*') go to 12
      if(string(1:1).eq.' ') go to 12
      if(string.eq.o_string) go to 12
      if(string.eq.' ') go to 12

      ii = ii + 1

      comment(ii) = ' '

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
      tt2(ii) = -1.d0
      onflag(ii) = '___'


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
     +             tts(ii),azi(ii),azis(ii),pin,ps(ii)
          else if(lstring.ge.63) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f6.3,1x,
     +             f5.3,1x,f6.2,2(1x,f5.2))',err=5) 
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec,
     +             tts(ii),azi(ii),azis(ii),pin
          else if(lstring.ge.57) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f6.3,1x,
     +             f5.3,1x,f6.2,f5.2)',err=5) 
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec,
     +             tts(ii),azi(ii),azis(ii)
          else if(lstring.ge.51) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f6.3,1x,
     +             f5.3,1x,f6.2)',err=5) 
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec,
     +             tts(ii),azi(ii)
          else if(lstring.ge.44) then
             read (string,'(a5,1x,a8,1x,i4,4(1x,i2),1x,f6.3,1x,
     +             f5.3)',err=5) 
     +             stat,phase(ii),yy,mon,dd,hh,mi,sec,tts(ii)
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

          ipos = ipo2 + 2
          if(itresw.eq.1 .and. lstring.ge.ipos) then
             ipo2 = ipos + 4
             read (string(ipos:ipo2),'(f5.2)',err=5) abc
             if(abc.gt.0.d0) tt2(ii) = abc
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
          razi = -9.
          rpa = -9.
          rper = -9.
          ramp = 0.
          rsnr = 0.
          smag = 0.
          cpick = ' '
          cpol = ' '

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

          lc = len_trim(string)
          if(lc.gt.122) comment(ii) = string(123:lc)

c         print *,stat,rdum0,hh,mi,isec,msec,phisf,razi,rpa,touse0

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
              ps(ii) = sisfsl
c             touse0(3:3) = 'S'
          else
              pin    = -999.d0
              ps(ii) = 0.d0
              touse0(3:3) = ' '
          endif

          if(razi.ne.real(isf_null)) then
              azi(ii)  = dble(razi)
              azis(ii) = sisfaz
c             touse0(2:2) = 'A'
          else
              azis(ii) = 0.d0
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

          tts(ii) = sisfo
          if(onscha.eq.'i') tts(ii) = sisfi
          if(onscha.eq.'e') tts(ii) = sisfe
          if(mod(msec,100).gt.0 .and. mod(msec+1,10).gt.0) 
     +           tts(ii) = 0.8d0 * tts(ii)

          onflag(ii)(1:1) = cpick
          onflag(ii)(2:2) = cpol
          onflag(ii)(3:3) = onscha

          if(tts(ii).lt.0.1d0) tts(ii)=0.1d0

          if(phase_type(phase(ii)).ne.'P') tts(ii) = tts(ii) * 2.d0

          if(isf_all) touse0(1:3) = 'TAS'

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
     +   phase(ii)(1:2).eq.'T ') touse(ii)(3:4) = '  '

      if(.not.aziflag) touse(ii)(2:2) = ' '
      if(.not.sloflag) touse(ii)(3:3) = ' '

      if((.not.isurf .and. phase(ii)(1:1).eq.'I' ) .or.
     +   (.not.tsurf .and. phase(ii)(1:1).eq.'T' ) .or.
     +   (.not.rgsurf .and. phase(ii)(1:2).eq.'Rg' ) .or.
     +   (.not.lpsurf .and. phase(ii)(1:2).eq.'LQ' ) .or.
     +   (.not.lpsurf .and. phase(ii)(1:2).eq.'LR' )) then
           if(aziall) then
              touse(ii)(1:5) = ' A   ' 
           else
              touse(ii)(1:5) = '     '
           endif
      endif

      if(touse(ii)(1:1).eq.'T' .or. touse(ii)(4:4).eq.'D') then
         if(tts(ii).le.0.d0) tts(ii) = 2.d0
         terrm  = terrm + tts(ii)
         nobsst = nobsst + 1
      endif

      if(tresw) then
          if(tt2(ii).le.0.d0) tt2(ii) = tts(ii)
          if(itresw.eq.2) then
              tt2(ii) = tts(ii)*treswf
          endif
      else
          tt2(ii) = tts(ii)
      endif

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
      if(incap.gt.0 .and. (uppcas(phidd(1:1)).eq.'P' .or.
     +   uppcas(phidd(1:1)).eq.'S')) then
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

      if(touse(ii)(1:1).ne.' ' .or. touse(ii)(4:4).ne.' ') then
         phase_t = phase_type(phidd)
         if(phase_t.eq.'P') then
            nsdp    = nsdp + 1
            sdpmean = sdpmean + q2(tts(ii))
         endif
         if(phase_t.eq.'S') then
            nsds    = nsds + 1
            sdsmean = sdsmean + q2(tts(ii))
         endif
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
          
          if(statfile2.ne.stationfile) then
            ierr = 0
            call get_station(statfile2,stat,jdate,lat,lon,
     +                   elevs,name,ierr)
          endif

          if(ierr.ne.0) then
            print *,'Cannot find station: ',stat,' entry skipped'
            ii = ii - 1
            ierr = 0
            go to 12
          endif

        endif

        if(typctl.gt.5) print *,stat,jdate,lat,lon,elevs,name

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
        istad(j)  = 0
        stalam = stalam + convlat(lat,1)
        p1 = deg2rad*lon
        stalom1 = stalom1 + dcos(p1)
        stalom2 = stalom2 + dsin(p1)
        iev(ii) = j
        istat = j

        go to 11

      endif

10    continue

11    if(timeo.lt.timemin) then
         timemin = timeo
         istatmin = iev(ii)
      endif

      if(azi(ii).lt.0.d0) then
         azi(ii)       = -999.d0
         touse(ii)(2:2)= ' '
         azis(ii)      =    0.d0
      endif

c
c     The standard deviations for baz or ray parameter may not yet
c     been given: set default values (30 or 40 [deg] and 5 [s/deg])!
c
      if(azi(ii).ge.0.d0 .and. azis(ii).le.0.d0) then

          azis(ii)= 30.d0

          if(phase(ii)(1:2).eq.'LR'.or.phase(ii)(1:2).eq.'LQ' .or.
     +       phase(ii)(1:2).eq.'L '.or.phase(ii)(1:2).eq.'M ') 
     +        azis(ii)=40.d0

      endif

      if(pin.le.0.d0) pin = -999.d0

      if(islow.eq.0 .and. pin.gt.0.0d0) then
         p(ii)  = radloc(stala(iev(ii)),1)*deg2rad/pin
         ps(ii) = ps(ii)*p(ii)/pin
      else
         p(ii) = pin
         if(pin.gt.0.d0) then
            if(ps(ii).le.0.d0) ps(ii)= 5.d0
         else
            touse(ii)(3:3) = ' '
            ps(ii) =  0.d0
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
         print *,ii,stat,tt(ii),phase(ii),azi(ii),azis(ii),p(ii),ps(ii),
     +           touse(ii)
      endif

12    o_string = string

c     end of onset reading loop
13    continue

14    close(10)

      if(stcorfl) then

         if(istcor.gt.0)  then

           write(textouts,'(''Station corrections were available for'',
     +           i5,'' station(s) as defined in '',a)') istcor,
     +           trim(statcorfile)

           if(output) write(11,'(a/)') trim(textouts)

           if(json_out) call json_add_string('station corrections file',
     +        trim(statcorfile), json_rc)

         endif

         close(13)

      endif


      if(nobsst.gt.0) then
         terrm = terrm / dble(nobsst)
      else
         terrm = 2.d0
      endif
      terrm = terrm * 10.d0 

      nobs  = ii

      if(nobs.le.3) fixinp = .true.

      nstat = istat
      stalam  = stalam / dble(nstat)
      stalom1 = stalom1 / dble(nstat)
      stalom2 = stalom2 / dble(nstat)
      stalom  = rad2deg*datan2(stalom2,stalom1)

      if(nsdp.gt.0) then
         sdmeans = sdpmean
         sdpmean = 4.d0*dsqrt(sdpmean / dble(nsdp))
         if(sdpmean.lt.1.d0) sdpmean = 1.d0
      else
         sdpmean = 2.d0
      endif

      if(nsds.gt.0) then
         sdmeans = sdmeans + sdsmean
         sdsmean = 4.d0*dsqrt(sdsmean / dble(nsds))
         if(sdsmean.lt.2.0d0) sdsmean = 2.0d0
      else
         sdsmean = 4.0d0
      endif

      if(nsdp.gt.0 .or. nsds.gt.0 ) then
         sdmeans = dsqrt( sdmeans / dble(nsdp+nsds) )
      else
         sdmeans = 5.d0
      endif

      if(check_mult) then
         nobs0 = nobs
         call mult_ons(nobs,nobs0,iev,phase,tt,tts,tt2,azi,azis,p,ps,
     +                 touse,amplit,period,dtphmax,mread,arid,comment)
         if(nobs.ne.nobs0) nobs = nobs0
      endif

      if(nobs.eq.1) then

         rzo1  = 0.

         rdel1 = 3.
         call tauget_mod(rzo1,rdel1,nphas,phcd,ttc,dtdd,
     +                   dtdh,dddp,modnam(1))
         pmoh  = dtdd(1)

         single = .true.

         if(typctl.gt.4) print *, 'Case: single array observation!'

      endif

c
c     Fix the time system at the earliest onset time for this event.
c

c     print *,istatmin,sta(iev(istatmin)),timemin
      larid = .false.
      iazin = 0
      do 15 i = 1,nobs
      tt(i) = tt(i)-timemin
      ttu(i) = tts(i)
      if(azi(i).ge.0.d0) iazin = iazin+1
      if(typctl.gt.8) then
        print*,i,sta(iev(i)),phase(i),tt(i),tts(i),tt2(i),azi(i),azis(i)
     +       ,p(i),ps(i),touse(i)
      endif
      if(arid(i).ne.'        ') larid = .true.
15    continue

      if(epistart) then
         elatmf  = convlat(epilat0,1)
         elatmgf = epilat0
         elonmf  = epilon0
      endif

c
c     At first, let us try to calculate an epicenter from all 
c     available baz observations.
c

      sela  = 0.d0
      svla  = 0.d0
      selo1 = 0.d0
      selo2 = 0.d0
      svlo  = 0.d0
      azims = 0.d0
      azimc = 0.d0
      azimr = 0.d0
      iazim = 0
      rpar  = 0.d0
      istater = 0

      jj = 0
      ja = 0

      if(nobs.eq.1) then
         if(azi(i).gt.-999.d0) then
            azims = dsin(deg2rad*azi(1))
            azimc = dcos(deg2rad*azi(1))
            if(p(1).gt.0.d0) rpar = p(1)
            iazim = 1
            istataz = iev(1)
            go to 51
         else
            print *,' Too little input data for a location'
            go to 9999
         endif
      endif

      if(azionlyf) typctl = 6

      do 50 i=1,nobs-1

      if(touse(i)(2:2).ne.'A') go to 50
      azi1 = azi(i)
      if(index(phase(i),'pre').gt.0) go to 50
      if(index(phase(i),'KK').gt.0 .and. phase(j)(1:1).ne.'S' .and. 
     +         phase(i)(1:2).ne.'sS')   azi1 = alpha2(azi1-180.d0)
      if(index(phase(i),'P2K').gt.0) azi1 = alpha2(azi1-180.d0)
      if(index(phase(i),'P3K').gt.0) azi1 = alpha2(azi1-180.d0)
      if(index(phase(i),"P'P'").gt.0) azi1 = alpha2(azi1-180.d0)
      if(index(phase(i),"S'S'").gt.0) azi1 = alpha2(azi1-180.d0)

      azims = azims + dsin(deg2rad*azi1)
      azimc = azimc + dcos(deg2rad*azi1)
      iazim = iazim + 1
      istataz = iev(i)

      slat1  = stala(iev(i))
      slat1e = convlat(slat1,1)
      slon1  = stalo(iev(i))

      do 20 j=i+1,nobs

      if(touse(j)(2:2).ne.'A') go to 20
      azi2 = azi(j)
      if(index(phase(j),'pre').gt.0) go to 20
      if(index(phase(j),'KK').gt.0 .and. phase(j)(1:1).ne.'S' .and. 
     +         phase(j)(1:2).ne.'sS')   azi1 = alpha2(azi1-180.d0)
      if(index(phase(j),'P2K').gt.0) azi2 = alpha2(azi2-180.d0)
      if(index(phase(j),'P3K').gt.0) azi2 = alpha2(azi2-180.d0)
      if(index(phase(j),"P'P'").gt.0) azi2 = alpha2(azi2-180.d0)
      if(index(phase(j),"S'S'").gt.0) azi2 = alpha2(azi2-180.d0)

      if(i.eq.nobs-1 .and. j.eq.nobs) then
         azims = azims + dsin(deg2rad*azi2)
         azimc = azimc + dcos(deg2rad*azi2)
         iazim = iazim + 1
         istataz = iev(i)
      endif
      if(iev(i).eq.iev(j)) go to 20

      slat2  = stala(iev(j))
      slon2  = stalo(iev(j))

      jj = jj + 1
      ja = ja + 1

      if(jj.gt.mloc) then
         print *,'Something wrong with number of locations!'
         go to 9999
      endif

      if(typctl.gt.8 .or. azionlyf) then

         print *,' '
         print *,'    Station BAZ                       dBAZ' 
         print *,'(1) ',sta(iev(i)),azi1,azis(i)
         print *,'(2) ',sta(iev(j)),azi2,azis(j)

      endif

c
c     Calculate distance and angles between the 2 stations
c

      call depi (slat1,slon1,slat2,slon2,del3,dk,ep2,ep1,d2km)

c     if(typctl.gt.7) then
c        print *,'station 1 (lat,lon,distance [km,deg],azimuth: ',
c    +           slat1,slon1,dk,del3,ep1
c        print *,'station 2 (lat,lon,azimuth; ',slat2,slon2,ep2
c     endif

c
c     Now an initial epicenter will be calculated
c

      ierr = 0
      call hyposat_cross(slat1e,slon1,azi1,azis(i),
     +               azi2,azis(j),del3,ep1,ep2,ela(jj),elas(jj),
     +               elo(jj),elos(jj),dismin,typctl,ierr)

      if(ierr.gt.0) then
         jj = jj - 1
         go to 20
      endif

      sela  = sela + ela(jj)/elas(jj)
      svla  = svla + 1.d0 / elas(jj)

      p1 = deg2rad*elo(jj)
      p2 = rad2deg/elos(jj)

      selo1  = selo1 + dcos(p1)*p2
      selo2  = selo2 + dsin(p1)*p2
      svlo   = svlo  + p2

      if(lauto) l_noepi = .true.
      if(dismin.lt.0.166d0*pi .and. lautor) l_noepi = .true.
      if(dismin.gt.0.1d0*pi .and. lautot) l_noepi = .true.

20    continue
50    continue

51    nloc = jj


      if(nloc.eq.0) then

        if(ja.gt.0) istater = 1

        if(rpar .gt. 0.d0) then

           phase_t = ' '
           phidr0 = phase(1)
           if(phidr0.eq.'P1') phidr0 = 'P'
           if(phidr0.eq.'S1') phidr0 = 'S'

           call tauget_ray(phidr0,phase_t,rpar,modnam(1),zo,ddel,ttray,
     +                     rayokf)

           if(.not.rayokf) then

             ddel =  (14.d0 - rpar)*9.8d0 + 2.0d0
             phidr0 = 'P'

             if(rpar.lt.pdif) then
                ddel = 150.d0
                phidr0 = 'PKPdf'
             endif

             if(rpar.ge.10.0d0) then
                ddel = 23.d0
             endif

             if(rpar.ge.13.0d0) then
                ddel = 10.d0
                phidr0 = 'Pn'
             endif

             if(rpar.ge.14.8d0) then
                ddel = 2.d0
                phidr0 = 'Pb'
             endif

             if(rpar.ge.17.0d0) then
                ddel = 1.d0
                phidr0 = 'Pg'
             endif
           
           endif
         
           if(typctl.gt.0 .and. .not.rayokf) then
                print *,'No distance found. Missing slowness values?'
                print *,'Distance set to ',ddel
           else if(typctl.gt.4) then
              print *,'Initial distance from Station(net): ',ddel
           endif

        endif
  
        if(iazim.le.0 .or. istater.gt.0) then

c
c       Choose a point in the vicinity (1 deg) of the closest
c       station as initial solution:
c

           istatd = istatmin
           azim = 315.d0

           if(typctl.gt.0 .and. .not.rayokf) then
                print *,' '
                print *,'No initial epicenter found. Missing ',
     +                  'backazimuth values?'
                print *,'Backazimuth set to ',azim,' degrees'
           endif

        else

           istatd = istataz
           azimr  = datan2(azims,azimc)
           azim   = alpha2(rad2deg*azimr)

        endif

        if(ddel.le.0.d0) ddel = 0.5d0

        inddel = 1
        call delazd(stala(istatd),stalo(istatd),azim,ddel,
     +               inddel,elatmgd,elonmd)
        elatmd = convlat(elatmgd,1)

        if(lauto) l_noepi = .true.
        if(ddel.lt.30.d0 .and. lautor) l_noepi = .true.
        if(ddel.gt.18.d0 .and. lautot) l_noepi = .true.

        go to 65

      endif

c
c     Now mean source coordinates have to be calculated.
c

      elatmd = sela / svla
      elatmgd = convlat(elatmd,2)

      elonmd = rad2deg*datan2(selo2/svlo,selo1/svlo)

      if(nloc.eq.1) then

        sdlat0  = elas(1)
        sdlon0  = elos(1)

      else if(nloc.gt.1) then

        dla = 0.d0
        dlo = 0.d0
        do 60 i =1,nloc
        dla = dla + q2(ela(i)-elatmd) / elas(i)
        p1 = alpha1(elo(i)-elonmd)
        dlo = dlo + (p1*p1) / elos(i)
60      continue

        if(dabs(dla).le.epsilon) then
          sdlat0  = elas(1)
        else
          sdlat0  = dsqrt(dla / svla)
        endif
        if(dabs(dlo).le.epsilon) then
          sdlon0  = elos(1)
        else
          sdlon0  = dsqrt(dlo / svlo)
        endif

      endif

65    continue

c     The next step is to calculate a first source time. If already 
c     given with input parameter file, this given value will be used.
c
c     We are using the method of Wadati. If we have only one travel
c     time difference S-P we assume Vp/Vs = sqrt(3.). Otherwise
c     we calculate Vp/Vs as a constants for each specific phase type
c     (Pg,Pb,Pn,P,P1)-(Sg,Sb,Sn,S,S1).
c
c     Which travel-time differences between S and P onsets do we have?
c     These travel differences are also used to calculate a source 
c     distance from a station or an array.
c

      dtkm  = 0.d0
      idtkm = 0

      do 66 i = 1,idtmax
      idt(i)=0
66    continue

      istatd = istatmin
      if(iazim.gt.0 .and. istater.le.0) istatd = istataz

      if(nobs.eq.1) go to 71

      do 702 i = 1,nobs-1

      if(touse(i)(1:1).ne.'T') go to 702

      do 701 j = i+1,nobs

      if(touse(j)(1:1).ne.'T') go to 701

      kmdel = 0.d0

      if(iev(i).eq.iev(j)) then

         phai = phase(i)
         phaj = phase(j)

         dtwad = dabs(tt(j)-tt(i))

         if(stcorfl) then
            st1=0.d0
            st2=0.d0
            if(phase_type(phase(i)).eq.'P') st1 = statp(iev(i))
            if(phase_type(phase(i)).eq.'S') st1 = stats(iev(j))
            if(phase_type(phase(j)).eq.'P') st2 = statp(iev(j))
            if(phase_type(phase(j)).eq.'S') st2 = stats(iev(j))
            dtwad = dtwad + st1 - st2
         endif

        wadati = .true.
        if(dtwad.gt.wadmax) wadati = .false.
        if(dtwad.lt.wadmin) wadati = .false.

        if((phai.eq.'P       ' .and. phaj.eq.'S       ') .or.
     +     (phai.eq.'S       ' .and. phaj.eq.'P       ')) then

          if(wadati) then
             idt(1)        = idt(1) + 1
             dt(1,idt(1))  = dtwad

             if(phai(1:1).eq.'P') then
                idtp(1,idt(1))= i
                idts(1,idt(1))= j
             else
                idtp(1,idt(1))= j
                idts(1,idt(1))= i
             endif
          endif

          if(iev(i).eq.istatd) then
            kmdel = (dtwad/60.d0-1.d0)*1000.d0
            if(kmdel.le.2000.d0) kmdel = dtwad*10.2d0
          endif

        else if(( phai.eq.'P1      ' .and. (phaj.eq.'S1      ' .or.
     +            phaj.eq.'Sg      '  .or. phaj.eq.'Sb      '  .or.
     +            phaj.eq.'Sn      '  .or. phaj.eq.'S       ' )) .or.
     +          ( phai.eq.'S1      ' .and. (phaj.eq.'P1      ' .or.
     +            phaj.eq.'Pg      '  .or. phaj.eq.'Pb      '  .or.
     +            phaj.eq.'Pn      '  .or. phaj.eq.'P       ' )) .or.
     +          ((phai.eq.'Pg      '  .or. phai.eq.'Pb      '  .or. 
     +            phai.eq.'Pn      '  .or. phai.eq.'P       ') .and. 
     +            phaj.eq.'S1      ' ) .or.
     +          ((phai.eq.'Sg      '  .or. phai.eq.'Sb      '  .or. 
     +            phai.eq.'Sn      '  .or. phai.eq.'S       ') .and. 
     +            phaj.eq.'P1      ' ) ) then
c
c         For P1 and S1 it is not automatically known, which phase it will
c         become. We use AK135 to choose the most
c         presumable phase type for the Wadati Approach.
c
          if(wadati) then
             idtc = 1
             if(dtwad.lt.200.0d0) idtc = 3
             if(dtwad.lt.17.6d0)  idtc = 2

             idt(idtc)          = idt(idtc) + 1
             dt(idtc,idt(idtc)) = dtwad

             if(phai(1:1).eq.'P') then
                idtp(idtc,idt(idtc))= i
                idts(idtc,idt(idtc))= j
             else
                idtp(idtc,idt(idtc))= j
                idts(idtc,idt(idtc))= i
             endif
          endif

          if(iev(i).eq.istatd) then
             if(idtc.eq.1) then
               kmdel = (dtwad/60.d0-1.d0)*1000.d0
               if(kmdel.le.2000.d0) kmdel = dtwad*10.2d0
             else if(idtc.eq.2) then
               kmdel = dtwad*8.58d0
             else if(idtc.eq.3) then
               kmdel = dtwad*10.2d0
             endif
          endif

        else if((phai.eq.'Pg      '                      .and.
     +          (phaj.eq.'Sg      '.or.phaj.eq.'Lg      ')) .or.
     +          (phaj.eq.'Pg      '                      .and.
     +          (phai.eq.'Sg      '.or.phai.eq.'Lg      ')))then

          if(wadati) then
             idt(2)        = idt(2) + 1
             dt(2,idt(2))  = dtwad

             if(phai(1:1).eq.'P') then
                idtp(2,idt(2))= i
                idts(2,idt(2))= j
             else
                idtp(2,idt(2))= j
                idts(2,idt(2))= i
             endif
          endif

          if(iev(i).eq.istatd) kmdel = dtwad*8.58d0

        else if((phai.eq.'Pn      '.and.phaj.eq.'Sn      ') .or.
     +          (phaj.eq.'Pn      '.and.phai.eq.'Sn      '))then

          if(wadati) then
             idt(3)        = idt(3) + 1
             dt(3,idt(3))  = dtwad

             if(phai(1:1).eq.'P') then
                idtp(3,idt(3))= i
                idts(3,idt(3))= j
             else
                idtp(3,idt(3))= j
                idts(3,idt(3))= i
             endif
          endif

          if(iev(i).eq.istatd) kmdel = dtwad*10.2d0

        else if((phai.eq.'Pb      '.and.phaj.eq.'Sb      ') .or.
     +          (phaj.eq.'Pb      '.and.phai.eq.'Sb      '))then

          if(wadati) then
             idt(4)        = idt(4) + 1
             dt(4,idt(4))  = dtwad

             if(phai(1:1).eq.'P') then
                idtp(4,idt(4))= i
                idts(4,idt(4))= j
             else
                idtp(4,idt(4))= j
                idts(4,idt(4))= i
             endif
          endif

          if(iev(i).eq.istatd) kmdel = dtwad*9.47d0

        else if(phai.eq.'Pn      '                          .and.
     +      (phaj.eq.'Sg      '.or. phaj.eq.'Lg      ')) then

          if(iev(i).eq.istatd) kmdel = dtwad*6.02d0

        else if(phaj.eq.'Pn      '                          .and.
     +      (phai.eq.'Sg      '.or. phai.eq.'Lg      ')) then

          if(iev(i).eq.istatd) kmdel = dtwad*6.02d0

        endif

        if(kmdel.gt.0.d0) then
           idtkm = idtkm + 1
           dtkm  = dtkm + kmdel
        endif

      endif

701   continue
702   continue

71    inet = 0

      if(idtkm.gt.0 .and. nloc.eq.0) then

         dtkm = dtkm / dble(idtkm)

         if(iazim.gt.0 .and. istater.le.0) then

            inddel = 2
            call delazd(stala(istataz),stalo(istataz),azim,dtkm,
     +               inddel,elatmgd,elonmd)
            elatmd = convlat(elatmgd,1)

            if(typctl.gt.0) then
               if(dtkm.le.0d0) then
                 print *,'Epicenter set to station ', sta(istataz)
               else
                 print *,'Epicenter set from station ',
     +               sta(istataz),': backazimuth',azim,' deg, delta',
     +               dtkm,' km' 
               endif
            endif

            sdlatg0 = dtkm*grad1
            sdlon0  = dtkm*grad1

            if(lauto) l_noepi = .true.
            if(ddel.lt.2500.d0 .and. lautor) l_noepi = .true.


         else 

            if(plflag) then

               call plane(stala,stalo,iev,tt,nobs,azim,dazir,
     +                    ray,dray,phipl,touse,phase,jref,typctl)

               if(jref.gt.0 .and. dazir.lt.50.d0 .and.
     +            dray.lt.4.d0) then

                   inddel = 2
                   call delazd(stala(iev(jref)),stalo(iev(jref)),azim,
     +                         dtkm,inddel,elatmgd,elonmd)
                   elatmd = convlat(elatmgd,1)

                   if(typctl.gt.0) then
                      if(dtkm.le.0d0) then
                        print *,'Epicenter set to station ',sta(jref)
                      else
                        print *,'Epicenter set from station ',
     +                            sta(jref),' after plane wave fit: ',
     +                      'backazimuth',azim,' deg, delta',dtkm,' km' 
                      endif
                   endif

                   sdlatg0 = dazir*dtkm*grad1
                   sdlon0  = dazir*dtkm*grad1
 
                   if(lauto) l_noepi = .true.
                   if(ddel.lt.2500.d0 .and. lautor) l_noepi = .true.

                   go to 72

               endif

            endif

            elatmd = stalam
            elonmd = stalom

            inet = 1

            if(lauto) l_noepi = .true.

            if(typctl.gt.0) 
     +         print *, 'Epicenter set in center of station net '

         endif
            
      else if(nloc.eq.0 .and. nstat.gt.1) then
            
         elatmd = stalam
         elonmd = stalom
         inet = 1

         if(lauto) l_noepi = .true.

         if(typctl.gt.0) 
     +      print *, 'Epicenter set in center of station net '

      endif

72    continue

      if(l_noepi .and. epistart) epistart = .false.

      if(epistart) then
        elatm  = elatmf
        elonm  = elonmf
        elatmg = elatmgf
      else
        elatm  = elatmd
        elonm  = elonmd
        elatmg = elatmgd
      endif

      elatmr  = deg2rad*elatm
      elatmgr = deg2rad*elatmg
      coelatm = 90.d0 - elatm
      coelatmr= deg2rad*coelatm

      fsetloc = .true.

      if(sdlat0.gt.0.d0) then
         sdlatg0 = sdlat0  / (eps*q2(dcos(elatmr))+q2(dsin(elatmr)))
      else
         sdlat0  = sdlatg0 * eps / (q2(dcos(elatmgr)) + 
     +                              eps*q2(dsin(elatmgr)))
      endif

      if(dismin.lt.pi .and. nloc.gt.0) then
         dismin = dismin * rad2deg * 5.d-2
         if(sdlatg0 .lt. dismin) then
            sdlatg0  = dismin
            sdlat0  = sdlatg0 * eps / (q2(dcos(elatmgr)) + 
     +                                 eps*q2(dsin(elatmgr)))
         endif 
         if(sdlon0 .lt. dismin) sdlon0  = dismin
      endif

      if(typctl.gt.0) then
        print *,' '
        if(nloc.gt.0) then
           print*,'Mean epicenter calculated from ',nloc,
     +                  ' observation(s)'
        else
           print*,'Initial epicenter:'
        endif
        print*,'(Mean) epicenter lat: ',elatmgd,' +/- ',sdlatg0
        print*,'(Mean) epicenter lon: ',elonmd,' +/- ',sdlon0
      endif

      if(output) then

        if(nloc.gt.0) then

           write(11,'(/''Parameters of initial solution ('',
     +              ''+/- 1 standard deviation):''/)') 
           write(11,'(''Mean epicenter calculated from'',i6,
     +              '' backazimuth observation pairs'')') nloc
           write(11,'(''Mean epicenter lat:'',f9.3,'' +/- '',f9.3,
     +              '' [deg]'')')        elatmgd,sdlatg0
           write(11,'(''Mean epicenter lon:'',f9.3,'' +/- '',f9.3,
     +              '' [deg]''/)')        elonmd,sdlon0

        else if(.not.(epistart .and.  .not.
     +         (nstat.eq.1.and.(dtkm.gt.0.d0 .or. ddel.gt.0.d0))))  then

           write(11,'(''No initial solution from multiple backazimuth ''
     +            ,''observations possible.''/)')

           if(inet.ne.0) then

               write(11,'(''Epicenter set in the center of station'',
     +                   '' net '')')

           else if(iazim.gt.0 .and. istater.le.0) then

               if(idtkm.gt.0 .and. dtkm.gt.0.d0) then
                  write(11,'(''Epicenter set from station '',
     +               a,'' with backazimuth'',f6.1,'' [deg], delta'',
     +               f8.2,'' [km]'')') sta(istatd),azim,dtkm
               else if(ddel.gt.0.d0) then
                  write(11,'(''Epicenter set from station '',
     +               a,'' with backazimuth'',f6.1,'' [deg], delta'',
     +               f7.2,'' [deg]'')') sta(istatd),azim,ddel
               else
                  write(11,'(''Epicenter set to station '',a)') 
     +               sta(istatd)
               endif
           endif
  
           write(11,'(''Epicenter lat:'',f9.3,'' [deg]'')')  elatmgd
           write(11,'(''Epicenter lon:'',f9.3,'' [deg]''/)') elonmd

        endif

      endif

      if(json_out .and. nloc.gt.0) then
         call json_start_dict_group("Location_with_BAZ observations",
     +        json_rc)

         call json_add_int("BAZ_observations", nloc,json_rc)

         call json_add_double("BAZ_latitude", elatmgd, json_rc)
         call json_add_double("BAZ_latitude_uncertainty", sdlatg0, 
     +        json_rc)
         call json_add_double("BAZ_longitude", elonmd, json_rc)
         call json_add_double("BAZ_longitude_uncertainty", sdlon0,
     +        json_rc)
         call json_end_group(json_rc)
      endif

      if(azionlyf) go to 9999

      if(epistart) then

        sdlatg0 = sdlatgi0
        sdlon0  = sdloni0

        write(11,'(''Initial Epicenter set by input file'')')
        write(11,'(''Epicenter lat:'',f9.3,'' [deg]'')')  elatmgf
        write(11,'(''Epicenter lon:'',f9.3,'' [deg]''/)') elonmf

        if(typctl.gt.0) then
          print *,' '
          print *, 'Initial Epicenter set by input file'
          print *, 'Epicenter lat: ',elatmgf,' [deg]'
          print *, 'Epicenter lon: ',elonmf,' [deg]'
          print *,' '
        endif
      endif

      if(typctl.gt.8) then

        do 75 i=1,nstat

        call depi(stala(i),stalo(i),elatmg,elonm,del(i),dk,azie(i),
     +                  baz(i),d2km)

        if(azi(i).ge.0.d0) then
          print *,sta(i),del(i),azie(i),baz(i),azi(i),azi(i)-baz(i)
        else
          print *,sta(i),del(i),azie(i),baz(i)
        endif

75      continue

      endif

      do 80 i=1,idtmax

      vpvs(i) = 0.d0

      if(idt(i).le.0) go to 80

      if(idt(i).eq.1) then

        vpvs(i) = dsqrt(3.d0)
        f1      = 1.d0/(vpvs(i)-1.d0)

        to(i)   = tt(idtp(i,1)) - dt(i,1)*f1
        tos(i)  = dpythag((1.d0+f1)*tts(idtp(i,1)),f1*tts(idts(i,1)))
        vpvss(i)= 0.5d0

      else if(idt(i).eq.2) then

        if( dabs(tt(idtp(i,2))-tt(idtp(i,1))).lt.0.01d0) then
           idt(i) = 0
           go to 80
        endif

        f1 = dt(i,2)-dt(i,1)
        f2 = 1.d0 / (tt(idtp(i,2))-tt(idtp(i,1)))

        am      = f1*f2
        am1     = 1.d0 / am
        vpvs(i) = am + 1.d0
        to(i)   = tt(idtp(i,1)) - dt(i,1)*am1

        f3 = am*f2
        f4 = am1*am1
        vpvss(i)= dsqrt ( q2(( f2+f3)*tts(idtp(i,1))) +
     +                    q2((-f2-f3)*tts(idtp(i,2))) +
     +                    q2(  f2    *tts(idts(i,1))) +
     +                    q2( -f2    *tts(idts(i,2))) )
        
        tos(i) = dsqrt( q2( (1.d0+am1+dt(i,1)*(f2+f3)*f4 )
     +                                         *tts(idtp(i,1)) )   +
     +                  q2( (         dt(i,1)*(-f2-f3)*f4 )
     +                                   *tts(idtp(i,2)) )   +
     +                  q2( (    -am1+dt(i,1)*f2     *f4 )
     +                                   *tts(idtp(i,2)) )   +
     +                  q2( (        -dt(i,1)*f2     *f4 )
     +                                 *tts(idtp(i,2)) )   )

      else 

        do 77 j=1,idt(i)

        f1 = 1.d0
        if(tts(idtp(i,j)).ne.0.d0 .or. tts(idts(i,j)).ne.0.d0)
     +     f1 = 1.d0/dpythag(tts(idtp(i,j)),tts(idts(i,j)))

         f2 = dt(i,j) * f1
         f3 = tt(idtp(i,j)) * f1

        if(dabs(f3).le.epsilon) f3=epsilon

        ddl(j) = f2
        ggl(j,1) = f3
        ggl(j,2) = f1

c       if(typctl.gt.6) then
c         print *,j,f1,f3,f2,tt(idtp(i,j))
c       endif

77      continue
        
        im = 2
        in = idt(i)
        call dlsq(in,im)

        if(lsqerr.gt.0) then
          if(typctl.gt.4) print*,'Wadati TYPE ',i,' LSQ-uncertainty: ',
     +       lsqerr
          idt(i) = 0
          go to 80
        endif

c        if(typctl.gt.6) then
c          do 78 j=1,idt(i)
c          print *,aal(1),vvl(1)
c          print *,aal(2),vvl(2)
c78        print *,ddl(j),rrl(j)
c        endif

        vpvs(i) = aal(1) + 1.d0
        vpvss(i)= vvl(1)

        to(i)   = -aal(2) / aal(1)
        tos(i)  = dpythag(to(i)*vvl(2)/aal(2),to(i)*vvl(1)/aal(1))

      endif

      if(i.eq.1 .and. dabs(to(i)).gt.1300.d0) then
         to(i)=dsign(1300.d0,to(i))
         tos(i)=dabs(to(i))
      endif
      if(i.eq.2 .and. dabs(to(i)).gt.150.d0) then
         to(i)=dsign(150.d0,to(i))
         tos(i)=dabs(to(i))
      endif
      if(i.eq.3 .and. dabs(to(i)).gt.400.d0) then
         to(i)=dsign(400.d0,to(i))
         tos(i)=dabs(to(i))
      endif
      if(i.eq.4 .and. dabs(to(i)).gt.250.d0) then
         to(i)=dsign(250.d0,to(i))
         tos(i)=dabs(to(i))
      endif

      if(typctl.gt.0) then
         print *,'S-P Travel-time difference type ',i
         print *,'Source time from ',idt(i),' observation(s)'
         print *,'   to= ',to(i),  ' +/- ',tos(i)
         print *,'Vp/Vs= ',vpvs(i),' +/- ',vpvss(i)
         if(vpvs(i).lt.1.2d0.or.vpvs(i).gt.2.5d0) print *,'Not used!'
         print *,' '
      endif

      if(vpvs(i).lt.0.5d0) vpvs(i)=0.5d0
      if(vpvs(i).gt.5.0d0) vpvs(i)=5.0d0

      if(vpvss(i).gt.9.99d0) vpvss(i)=9.99d0
      if(vpvss(i).lt.0.01d0) vpvss(i)=0.01d0

      if(tos(i).gt.9999.9d0) tos(i)=9999.9d0
      if(tos(i).lt.1.0d0) tos(i)=1.0d0

      if(output) then
         if(vpvs(i).lt.1.2d0.or.vpvs(i).gt.2.5d0) then
           write(11,'(''S-P Travel-time difference type'',i2,
     +      '' with'',i4,'' observation(s)''/''   to='',f14.1,'' +/-'',
     +      f6.1,'' [s] Vp/Vs= '',f4.2,'' +/- '',f4.2,'' not used!'')')
     +      i,idt(i),to(i)+timemin,tos(i),vpvs(i),vpvss(i)
         else
           write(11,'(''S-P Travel-time difference type'',i2,
     +      '' with'',i4,'' observation(s)''/''   to='',f14.1,'' +/-'',
     +      f6.1,'' [s] Vp/Vs= '',f4.2,'' +/- '',f4.2)') i,idt(i),
     +      to(i)+timemin,tos(i),vpvs(i),vpvss(i)
         endif

      endif

      if(json_out) then
        call json_start_dict_group("s_p_travel_time_difference_type",
     +  json_rc)
        call json_add_int("type", i, json_rc)
        call json_add_int("observations", idt(i), json_rc)
        call json_add_double("to", dble(to(i)+timemin), json_rc)
        call json_add_double("to_uncertainty", dble(tos(i)), json_rc)
        call json_add_double("vp_vs", dble(vpvs(i)), json_rc)
        call json_add_double("vp_vs_uncertainty", dble(vpvss(i)), 
     +       json_rc)

        if(vpvs(i).lt.1.2d0.or.vpvs(i).gt.2.5d0) then
           call json_add_bool("used", .FALSE., json_rc)
        else
           call json_add_bool("used", .TRUE., json_rc)
        endif

        call json_end_group(json_rc)
      endif

      fsetloc = .false.

80    continue

c
c     now follows the statistics over all estimated to-values
c
      sto     = 0.d0
      stos    = 0.d0
      svpvs   = 0.d0
      svpvss  = 0.d0
      ito     = 0
      iwa     = 0
 
      do 82 i = 1,idtmax
 
      if(idt(i).le.0) go to 82
      if(vpvs(i).lt.1.2d0.or.vpvs(i).gt.2.5d0) then
         idt(i) = 0
         if(output) then
           write(11,'(''S-P Travel-time difference type'',i2,
     +      '' not used for mean value calculations''
     +      '' because results are not trustable'')') i
         endif
         go to 82
      endif

      ito = ito + 1

      iwa = iwa + idt(i)

      sto   = sto  + idt(i)*to(i)/tos(i)
      stos  = stos + idt(i)/tos(i)
 
      svpvs = svpvs  + idt(i)*vpvs(i)/vpvss(i)
      svpvss= svpvss + idt(i)/vpvss(i)
 
82    continue
 
      if(ito.eq.0) then

         if(fsetloc .or. nloc.ge.1) then

           call depi(stala(istatmin),stalo(istatmin),elatmg,elonm,
     +               del(istatmin),dk,azie(istatmin),baz(istatmin),d2km)
           rzo1 = 0.
           rdel = real(del(istatmin))

           call tauget_mod(rzo1,rdel,nphas,phcd1,ttc1,dtdd1,
     +                         dtdh1,dddp1,modnam(1))

           tom  = - ttc1(1)

         else

           if(ttray.gt.0d0) then
              tom  = -ttray
           else
              tom  = -dtp0/2.d0
           endif

         endif
         vpvsm = 0.d0
         sdto = dtp0
         go to 85

      else

         tom   = sto / stos
         vpvsm = svpvs / svpvss

      endif
 
      if(ito.eq.1) then
         sdto   = iwa / stos
         sdvpvs = iwa / svpvss
         go to 85
      endif

      dto   = 0.d0
      dvpvs = 0.d0
 
      do 83 i =1,idtmax

      if(idt(i).le.0) go to 83
 
      dto   = dto   + idt(i)*q2(to(i)-tom)     / tos(i)
      dvpvs = dvpvs + idt(i)*q2(vpvs(i)-vpvsm) / vpvss(i)
 
83    continue
 
      sdto    = dsqrt(dto / stos)
      sdvpvs  = dsqrt(dvpvs / svpvss)
 
85    continue

      tome = tom + timemin

      if(ito.gt.0) then
         if(output) then
            if(typctl.gt.0) then
               print*,'Mean source time: ',tome,' +/- ',sdto
               print*,'Mean       vp/vs: ',vpvsm,' +/- ',sdvpvs
            endif
            if(sdto.lt.1000.d0) then
               write(11,'(''Mean source time:'',f15.3,'' +/- '',f7.3,
     +                 '' [s]'')')  tome,sdto
            else
               write(11,'(''Mean source time:'',f15.3,'' +/- '',f7.1,
     +                 '' [s]'')')  tome,sdto
            endif
            if(sdvpvs.lt.1000.d0) then
               write(11,'(''Mean       vp/vs:'',f15.3,'' +/- '',f7.3)') 
     +               vpvsm,sdvpvs
            else
               write(11,'(''Mean       vp/vs:'',f15.3,'' +/- '',f7.1)') 
     +               vpvsm,sdvpvs
            endif

         endif
      endif

      if(json_out) then
        if(ito.gt.0) then
          call json_add_double("mean_source_time", tome, json_rc)
          call json_add_double("mean_source_time_uncertainty", sdto, 
     +         json_rc)
          call json_add_double("mean_vp_vs", vpvsm, json_rc)
          call json_add_double("mean_vp_vs_uncertainty", sdvpvs, 
     +         json_rc)
        else
          call json_add_double("initial_source_time", tome, json_rc)
        endif

      endif

c
c     In any case, we use (if set) the initial source time and its standard
c     deviation from the hyposat-parameter or ISF-input file.
c     
      if(tome0 .gt. -2840140800.0d0) then

         tome = tome0

         tom = tome - timemin

         if(stome0 .gt. 0.d0) then
            sdto = stome0
         endif

         if(typctl.gt.0) then
            print*,'Source time (from input): ',tome,' +/- ',sdto
         endif
         if(output) then
            write(11,'(/''Source time (from input):'',f15.3,'' +/- '',
     +                f7.3,'' [s]''/)') tome,sdto
         endif

      else if(ito.le.0) then
         if(typctl.gt.0) then
            print *,'Source time (set): ',tome,' [s]'
         endif
         if(output) then
            write(11,'(''Source time (set):'',f15.3,'' [s]'')') tome
         endif

      endif

c
c     Now the new source parameters can be calculated by
c     several iterations using the GMI algorithm. 
c
c     For the first iteration we use as initial solution the read in 
c     source depth (or the ISC default depth), the source time to, and 
c     the epicenter coordinates elatmg and elonm.
c

      if(ldefisc) then
         call def_depth(defdep,elatmg,elonm,idetyp,ldefd,c1typ,ideptyp)
         zo = defdep
      endif

      if(lwdepth ) then
          water = wdepth(elatmg,elonm)
          if(water.gt.zo) then
             zo = dble(dnint(water+0.5d0))
             if(typctl.gt.4) then
                print *, 'water depth at source:', water,' km'
                print *, 'depth fixed at: ',zo,' km'
             endif
             lwater = .true.
             zwater = 1000.d0*water
          endif
      endif

      sdzo   = sdzo1
      sdlat  = sdlat0
      sdlon  = sdlon0
      sdlatg = sdlatg0

      if(json_out) then

         call json_start_dict_group("initial_solution_used",json_rc)
         call json_add_double("initial_source_time", tome, json_rc)
         call json_add_double("initial_source_time_uncertainty",
     +       sdto, json_rc)
         call json_add_double("initial_latitude", elatmg, json_rc)
         call json_add_double("initial_latitude_uncertainty", 
     +       sdlatg, json_rc)
         call json_add_double("initial_longitude", elonm, json_rc)
         call json_add_double("initial_longitude_uncertainty", 
     +       sdlon, json_rc)
         call json_add_double("initial_depth", zo, json_rc)
         call json_add_double("initial_depth_uncertainty", 
     +       sdzo, json_rc)

         call json_end_group(json_rc)
      endif

c     Add list of origins to output before loop begins
      if(json_out) call json_start_list_group("origins", json_rc);

99    continue

      rs(1) = sdto
      rs(2) = sdlat
      rs(3) = sdlon
      rs(4) = sdzo

      var(1) = sdto
      var(2) = sdlat
      var(3) = sdlon
      var(4) = sdzo

      iter = 0
      nextiter = 0
      in = 0
      dtp = dtp0
      dts = dts0

      dchang = dchang0

      rmso   = 9999.d0
      rmsold  = 9999.d0
      datmax0 = 9999.d0

      dazim1 = dazim0*2.0d00
      dpam1  = dpam0*2.0d0

      dtmp = dtp
      dtms = dts
      dtm0 = dtm2

      dazim = dazim1
      dpam  = dpam1

c
c     At first, we build the Jacobi-matrix
c     (loop 300 and 301)
c
      if(output) print*,' '

100   continue

      if(ibad0.gt.3) then
         print *,'Could not find a stable solution for these data'
         if(output) then
            write(11,'(/''Could not invert these data!'')')
         endif
         go to 9999
      endif

      last = .false.

      iter = iter + 1

      ifixaz = 0
      ifixto = 0

      rmsold = rmso
      iremo  = 0

101   continue

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

c     nextiter.lt.0 if oszillating solutions


      if((check.le.setcheck2 .or. nextiter1.eq.1) .and. 
     +    iteraz.eq.0 ) dtmflag = .true.

      if(ilastiter.eq.1) then
        dazim = dazim1
        dpam  = dpam1
        dtmp  = resmaxp
        dtms  = resmaxs
        dtm0  = 2.2d0*resmaxs
      endif

      if(lastfixm) then
         dazim = dmin1(dazim0,dazim1)
         dpam  = dmin1(dpam0,dpam1)
         dtmp  = resmaxp
         dtms  = resmaxs
         dtm0  = 2.2d0*resmaxs
      endif

      if(lastfixt .or. dtmflag) then

         if(nrms1.le.5) then
            dazim = dmin1(20.d0,dazim1,dazim)
            dpam  = dmin1(2.d0,dpam1,dpam)
         else if(nrms1.gt.5 .and. nrms1.le.10) then
            dazim = dmin1(15.d0,dazim1,dazim)
            dpam  = dmin1(1.5d0,dpam1,dpam)
         else if(nrms1.gt.10) then
            dazim = dmin1(10.d0,dazim1,dazim)
            dpam  = dmin1(1.d0,dpam1,dpam)
         endif
       
         dtm  = dmax1(rmso*.8d0,0.7d0)
         if(nrms1.gt.200) dtm  = dmax1(rmso*.7d0,0.6d0)

         dtmp = dmin1(dtm,dtp)
         dtms = dmin1(2.d0*dtm,dts)

         if(dtmp.lt.sdpmean) dtmp = sdpmean
         if(dtms.lt.sdsmean) dtms = sdsmean

         dtm0 = 2.2d0*dtm + var(1)

      endif

      if(dtmp.gt.1000.d0) dtmp = 1000.d0
      if(dtms.gt.1400.d0) dtms = 1400.d0
      if(dtm0.gt.1400.d0) dtm0 = 1400.d0
      if(dpam.gt.10.d0)  dpam = 10.d0
      if(dazim.gt.50.d0) dazim = 50.d0

      stato = ' '
      jj    = 0
      jdt   = 0
      jazi  = 0
      jpa   = 0
      rms1  = 0.d0
      nrms1 = 0
      datmax = 0.001d0
      nzo    = 0

      lstcor = .false.
      nstref = 0

      rzv(1)    = -999.d0
      rzv(2)    = -999.d0

      do 300 i = 1,nobs

      used(i)= '      '
      usedsr = ' '
      phaseu(i) = ' '

      if(touse(i)(1:4).eq.'    ') go to 300

      if(sta(iev(i)).ne.stato) then

         call depi(stala(iev(i)),stalo(iev(i)),elatmg,elonm,del(iev(i)),
     +              dk,azie(iev(i)),baz(iev(i)),d2km)

         delta = del(iev(i))

         if(typctl.gt.8) then
           print *,'STATION EPI: ',i,stato,delta,dk
         endif

         if(kmout) then
            if(dk .gt. dismaxst ) go to 300
            if(dk .lt. disminst ) go to 300
         else
            if(delta .gt. dismaxst ) go to 300
            if(delta .lt. disminst ) go to 300
         endif
         if(dk .lt. 1.d-4)       dk = 1.d-4
         if(delta .lt. 1.d-6) delta = 1.d-6

         stato = sta(iev(i))

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

         rzo = sngl(zo)
         rdel = sngl(delta)

         costalat  = 90.d0-convlat(stala(iev(i)),1)
         costalatr = deg2rad*costalat

         loctt = 0

         fconr = .false.
         fmoho = .false.

         if(imod2.eq.0 .or. delta.gt.rmax0 .or. zo.gt.zmax) then

            nphas0 = 0
            call tauget_mod(rzo,rdel,nphas0,phcd,ttc,dtdd,
     +                      dtdh,dddp,modn)

            if(czo.eq.'D') then

               rzo1 = rzo-1.
c              no event above the surface!
               if(rzo1.lt.0.0) rzo1 = 0.0
c
               rzo2 = rzo+1.
c              no event deeper than 799. km !
               if(rzo2.ge.800.0) rzo2=799.0

               if(rzo-rzo1.gt.0.0) then
                  nphas1 = 0
                  call tauget_mod(rzo1,rdel,nphas1,phcd1,ttc1,dtdd1,
     +                            dtdh1,dddp1,modn)
               endif

               if(rzo2-rzo.gt.0.0) then
                  nphas2 = 0
                  call tauget_mod(rzo2,rdel,nphas2,phcd2,ttc1,dtdd2,
     +                            dtdh1,dddp1,modn)
               endif

               do 280 k=1,nphas0

               dpdh(k) = 0.d0

               if(abs(rzo2-rzo1).le.0.001) go to 280

               if(nphas1.gt.0 .and. nphas2.gt.0) then
                  do 251 ki=1,nphas1
                  do 250 kj=1,nphas2
                  if(phcd1(ki).eq.phcd(k)  .and. 
     +               phcd(k).eq.phcd2(kj)) then
                     dpdh(k) = (dtdd2(kj)-dtdd1(ki))/dble(rzo2-rzo1)
                     go to 280
                  endif
250               continue
251               continue
               endif

               if(rzo.gt.rzo1 .and. nphas1.gt.0) then
                  do 260 ki=1,nphas1
                  if(phcd(k).eq.phcd1(ki)) then
                     dpdh(k) = (dtdd(k)-dtdd1(ki))/dble(rzo-rzo1)
                     go to 280
                  endif
260               continue
               endif

               if(rzo2.gt.rzo .and. nphas2.gt.0) then
                  do 270 kj=1,nphas2
                  if(phcd(k).eq.phcd2(kj)) then
                     dpdh(k) = (dtdd2(kj)-dtdd(k))/dble(rzo2-rzo)
                     go to 280
                  endif
270               continue
               endif

280            continue

            endif

            nphas = nphas0

            if(zo.ge.zconr(modind)) fconr = .true.
            if(zo.ge.zmoho(modind)) fmoho = .true.

         else 

            ierr = 0
            indph = istaph(iev(i))*10000 + indph0
            elatc = elatmg
            elonc = elonm

            elat2 = stala(iev(i))
            elon2 = stalo(iev(i))
            sdep = 0.d0
            if(locsta) sdep  = - stael(iev(i))

            nphas = 0
            call ttloc(rzo,rdel,czo,nphas,ttc,dtdd,dtdh,dpdh,dddp,
     +                 phcd,sdep,typctl,ierr,indph,emerout,dconr,dmoho)

            if(ierr.ne.0) then
               print *, 'Error in getting travel-time tables for:'
               print *, sta(iev(i)), ' in ',rdel,' deg --> not used!'
               go to 299
            endif
            loctt = 1

            if(zo.ge.dconr) fconr = .true.
            if(zo.ge.dmoho) fmoho = .true.

         endif

         if(dk.gt.30.d0 .and. zo.lt.35.d0 .and. 
     +      dk.gt.3.d0*zo ) then
            nphas       = nphas + 1
            ttc(nphas)  = dk/vlg
            dtdd(nphas) = d2km/vlg
            dtdd(nphas) = d2km/vlg
            dtdh(nphas) = 0.d0
            phcd(nphas) = 'Lg'
            dddp(nphas) = 9.999d99
            dpdh(nphas) = 0.d0
         endif

         f1 = dcos(coelatmr)
         f3 = dsin(coelatmr)

         f2 = dcos(costalatr)
         f4 = dsin(costalatr)

         f5 = dabs(deg2rad*alpha1(stalo(iev(i))-elonm))

         f6 = dcos(f5)
         f8 = dsin(f5)

         f7 = dsin(deg2rad*delta)

         if(baz(iev(i)).le.180.d0) then
            alpha = deg2rad*baz(iev(i))
         else
            alpha = deg2rad*(360.d0-baz(iev(i)))
         endif

         deldla =  (f4*f1*f6 - f3*f2) / f7
         deldlo =  f4*dsin(alpha)

         f9  = dcos(alpha) * f7*f7
         f10 = dcos(deg2rad*delta)

         dazidla =  -f8*(f1*f7+f3*f10*deldla)/f9
         dazidlo =   f3*(f6*f7-f8*f10*deldlo)/f9

          if(baz(iev(i)).gt.180.d0) then
            dazidla = -dazidla
            deldlo  = -deldlo
          endif
          
c         if(typctl.gt.8) then
c             print *,'baz ',baz(iev(i)),deldla,deldlo,dazidla,
c    +              dazidlo,(stalo(iev(i))-elonm),delta
c
c         endif
      endif
  
      phid = phase(i)

      dinv(i,1) = 0.d0
      dinv(i,2) = 0.d0
      dinv(i,3) = 0.d0
      dinv(i,4) = 0.d0

      ttt(i) = 0.d0

      idtu(i) = 0

      dtt  = -9999.d0
      dtts = -9999.d0
      dpa  = -9999.d0
      dtdhu = 0.d0
      dtdhs = 0.d0
      dpdhu = 0.d0
      dpdhs = 0.d0
      dddpu = 9.999d99
      dddps = 9.999d99

      surf = .false.

      if(phid(1:3).eq.'tx ') go to 299

      if(phid.eq.'P1      ' .or. phid.eq.'S1      ') then
         firston= .true.
      else
         firston= .false.
      endif

      incap = index(phid,'w')
      if(incap.gt.0) 
     +   phid = phase(i)(1:incap-1) // phase(i)(incap+1:8) // ' '

      chgcas = uppcas(phid(1:1))
      if(chgcas(1:1).ne.'P' .and. chgcas(1:1).ne.'S' .and.
     +   phid(1:1).ne.'R' .and. phid(1:1).ne.'L' .and. 
     +   phid(1:1).ne.'T' .and. phid(1:2).ne.'IS'  ) go to 299

      if(sglgdis.gt.0.d0) then
         
         dttlg = tt(i) - tom - dk/vlg

         if(phid.eq.'Sg' .and. dk.ge.sglgdis .and. dttlg.gt.-15.d0) 
     +      phid = 'Lg'
         if(phid.eq.'Lg' .and. dk.lt.sglgdis) phid = 'Sg'

      endif

      if(fconr .and. .not. fixinp) then
        ipg = index(phid(1:3),'Pg')
        if(ipg.gt.0) phid(ipg:ipg+1) = 'Pb'
        isg = index(phid(1:3),'Sg')
        if(isg.gt.0) phid(isg:isg+1) = 'Sb'
      endif

      if(fmoho .and. .not. fixinp) then
        ipg = index(phid(1:3),'Pg')
        if(ipg.gt.0) phid(ipg:ipg+1) = 'Pn'
        ipb = index(phid(1:3),'Pb')
        if(ipb.gt.0) phid(ipb:ipb+1) = 'Pn'

        isg = index(phid(1:3),'Sg')
        if(isg.gt.0) phid(isg:isg+1) = 'Sn'
        isb = index(phid(1:3),'Sb')
        if(isb.gt.0) phid(isb:isb+1) = 'Sn'
      endif

      if(touse(i)(1:1).eq.'m') go to 299

c
c     LR is also assumed for far-distant Lg and Rg
c

      if(phid.eq.'Rg') then
         if(.not.rgsurf) then
            dtt=tt(i) - tom - dk/vrg + statr(iev(i))
            phaseu(i) = phase(i)
            go to 299
         endif
         if(dk.le.400.d0) then
            surf = .true.
            vsurf = vrg
         else
            phid = 'LR'
         endif
      endif
                
      if(phid.eq.'Lg' ) then
         if(.not.lgsurf) then
            dtt=tt(i) - tom - dk/vlg + statr(iev(i))
            phaseu(i) = phase(i)
            go to 299
         endif
         if(dk.le.3000.d0) then
            surf = .true.
            vsurf = vlg
         else 
            phid= 'LR'
         endif
      endif
                
      if((phid(2:3).eq.'n ' .or. phid(2:3).eq.'g ' .or. phid(2:3).eq.
     +   'b ') .and. rdel.gt.30. ) phid(2:3)='  '

      if((phid(1:5).eq.'PKPab' .or. phid(1:5).eq.'PKPbc') .and. 
     +    delta.le.143.d0) phid(1:5) = 'PKPdf'

      phid0 = phid

      icha = 0
      imin = 0
      dtts = 999.d99

c     print *,i,sta(iev(i)),phase(i),phid0,phid,icha,imin,nphass,
c    +        phaseu(i),delta

c
c     Let's take the first phase from IASPEI-1991-type tables,
c     which fits the phase name of the onset
c

      if(single .and. insar.le.0 .and. iter.eq.1) then
         phid = phidr0
         phase(1) = phid
      endif

      phid1 = ' '
      
c     print *,'---> (0) phid0,phid,phid1 ',i,delta,phid0,phid,phid1,
c    +        firston

      if(firston) then

         phid1 = phcd(1)

c     print *,'---> (1a) phid0,phid,phid1 ',i,phid0,phid,phid1,firston

         if(phid(1:1).eq.'P' .and. phid1(1:1).eq.'P' .and.
     +      (phid1(3:3).eq.' ' .or. phid1(2:4).eq.'dif' .or.
     +       phid1(1:3).eq.'PKP' .or. phid1(1:2).eq."P'")) then

           phid = phid1

c     print *,'---> (1b) phid0,phid,phid1 ',i,phid0,phid,phid1,firston

           if(p(i).lt.0.9d0*pdif .and. phid1(1:3).eq.'Pdi') then
              if(delta.gt.113.d0) then
                phid = 'PKPdf'
              endif
           endif

c     print *,'---> (1c) phid0,phid,phid1 ',i,phid0,phid,phid1,firston

         endif
      endif

294   continue

c
c     we assume for unspecified 'L ' that it is LR
c
      if(phid.eq.'L ' .or. phid.eq.'M ') then
         phid= 'LR'
      endif

      if(phid.eq.'LR') then
c     print *,'---> (0-1) phid,surf ',i,phid,surf
         if(.not.lpsurf) then
            touse(i)(1:1) = ' '
            touse(i)(3:4) = '  '
            dtt=tt(i) - tom - dk/vlr + statr(iev(i))
            phaseu(i) = phase(i)
            go to 299
         endif
         surf = .true.
         vsurf = vlr
      endif
                
      if(phid.eq.'LQ') then
         if(.not.lpsurf) then
            touse(i)(1:1) = ' '
            touse(i)(3:4) = '  '
            dtt=tt(i) - tom - dk/vlq + statr(iev(i))
            phaseu(i) = phase(i)
            go to 299
         endif
         surf = .true.
         vsurf = vlq
      endif

      if(phid.eq.'T') then
         if(.not.tsurf) then
            touse(i)(1:1) = ' '
            touse(i)(3:4) = '  '
            dtt=tt(i) - tom - dk/vt
            phaseu(i) = phase(i)
            go to 299
         endif
         surf = .true.
         vsurf = vt
      endif

      if(phid.eq.'IS') then
         if(.not.isurf) then
            touse(i)(1:1) = ' '
            touse(i)(3:4) = '  '
            dtt=tt(i) - tom - dk/vi
            phaseu(i) = phase(i)
            go to 299
         endif
         surf = .true.
         vsurf = vi
      endif

      if(surf .and. phid.ne.'Lg') then
         nphas       = nphas + 1
         ttc(nphas)  = dk/vsurf
         dtdd(nphas) = d2km/vsurf
         dtdh(nphas) = 0.d0
         phcd(nphas) = phid
         dddp(nphas) = 9.999d99
         dpdh(nphas) = 0.d0
      endif

      phcheck = ' '
                
295   continue

      nphass = nphas

c     print *,'---> (2) phid0,phid,phid1 ',i,phid0,phid,phid1,firston

      j = 0
      first2 = .false.

      do 297 j1 = 1,nphass

      j = j + 1
      if(j.gt.nphass) go to 2975

      phid1 = phcd(j)

c     print *,'---> (3) phid0,phid,phid1 ',i,j,j1,phid0,phid,phid1,
c    +        firston,icha,imin

      if(firston) then

         if(phid(1:1).eq.'P' .and. phid1(1:1).eq.'S') go to 2975

         if(phid(1:1).eq.'S' .and. phid1(1:1).ne.'S') go to 297 

         if(phid(1:1).eq.'S' .and. phid1(1:1).eq.'S' .and. 
     +      phid1(2:2).ne.'S' .and. phid1(2:2).ne.'P' .and.
     +     (phid1(3:3).eq.' ' .or. phid1(1:3).eq.'SKS') .and.
     +      imin.eq.0) phid = phid1

      endif
    
      if(phid1(2:4).eq.'dif') then
         if(phid(1:2).eq.'P ') phid='Pdif'
         if(phid(1:2).eq.'S ') phid='Sdif'
      endif

c     print *,'---> (4) phid0,phid,phid1 ',i,j,j1,phid0,phid,phid1,
c    +        firston,icha,imin

      if(phid.eq.phid1) then

         dpa   = dtdd(j)
         dpaa  = dabs(dpa)

         if(single .and. rayokf) then

            if(dabs(p(i)-dpaa) .ge. 0.01d0) go to 297

         endif

         dtdhu = dtdh(j)
         dpdhu = dpdh(j)
         dddpu = dddp(j)
         if(dabs(dddpu).lt.1.d-1) dddpu = 1.d-1

c
c     Now the phase is identified and several corrections can be applied
c     (if possible and/or requested).
c

c
c     Any static station correction for this phase?
c
         statict = 0.d0

         phase_t = phase_type(phid1)
         if(firstph) then
            if(j.eq.1 .and. phase_t.eq.'P') statict = statp(iev(i))
            if(phase_t.eq.'S' .and. phid1(1:1).eq.'S' .and. .not.
     +         first2) then
               first2  = .true.
               statict = stats(iev(i))
            endif
         else
            if(phase_t.eq.'P') statict = statp(iev(i))
            if(phase_t.eq.'S') statict = stats(iev(i))
            if(phase_t.eq.'L') statict = statr(iev(i))
         endif

c
c     Any ellipticity correction for this phase?
c

         ecor = 0.d0

         if(.not.surf .and. .not.locgeo) then
c
c          Ellipticity corrections are yet not available for sources 
c          deeper than 700 km. Therefore we accept a small error
c          and set the source depth to 700. km
c
c          No ellipticity corrections for locations in the case that 
c          the 'local geometry' switch is set.
c
           rzoe = zo
           if(rzoe.gt.700.d0) rzoe=700.d0
           ierre = 0

           call ellip(coelatmr,azie(iev(i)),delta,rzoe,phid1,dpa,ecor,
     +                ierre)

         endif

c
c     Any elevation from station info or Crust 1.0 correction for this phase
c     (only if phase path is not within rmax of local/regional model) ?
c
         th     = 0.d0
         tcrust = 0.d0
         vloc = 99999.d0
         vstat = 0.d0
         delc  = 0.d0
         dpc   = 0.d0
         dph   = 0.d0

         hsta = stael(iev(i))

         if(vlflag .and. .not.surf) then

           if((imo.eq.2 .or. imo.eq.4 .or. imo.eq.5) .and. loctt.ne.1) 
     +         then

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
                 lstcor = .true.
              endif

           endif

           if(phase_t.eq.'P') vloc = stavp(iev(i))
           if(phase_t.eq.'S') vloc = stavs(iev(i))

           if(vstat.gt.0.d0 .and. istfil(iev(i)).eq.0) vloc=vstat

           if(locsta .and. hsta.lt.epsilon .and. loctt.eq.1) hsta = 0.d0

c          print *,'CRUST 1',tcrust,phid1,phase_t,dpaa,zoc,modind,
c    +              indr,hsta,elev,stael(iev(i))

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
            print *,'i,sta(iev(i)),phid1,phase_t,dpaa,ecor,tcrust,',
     +             'delc,dpc,hsta,vloc,th,dph,dddpu,ddis,dl'
            print *,i,sta(iev(i)),phid1,phase_t,dpaa,ecor,tcrust,
     +             delc,dpc,hsta,vloc,th,dph,dddpu,ddis,dl
         endif

c
c        We have eventually to correct this phase for the local
c        structure at the reflection point at the surface (if
c        CRUST 1.0 or a local/regional model is available).
c

         trefl  = 0.d0
         trefl2 = 0.d0
         usedr  = ' '
         dpr    = 0.d0
         delr   = 0.d0
         delr2  = 0.d0

         if( .not.surf .and. touse(i)(5:5).eq.'R' .and. loctt.eq.0 .and.
     +      imo.gt.0 .and. imo.ne.3) then

            if(phid1(1:1).ne.'p' .and. phid1(1:1).ne.'s') then
c
c    no corrections in case of PPP, SSS, ...
c
               if(phid1(1:1).eq.phid1(2:2) .and. 
     +            phid1(1:1).eq.phid1(3:3))  go to  296
c
c    no corrections in case of PxPxPx, SxSxSx, ...
c
               if(phid1(1:2).eq.phid1(3:4) .and. 
     +            phid1(1:2).eq.phid1(5:6))  go to  296

               if(len_trim(phid1).eq.3 .and. (phid1(2:2).eq.'m' .or.
     +            phid1(2:2).eq.'c' .or. phid1(2:2).eq.'K' )) 
     +            go to 296

            else

c
c    no corrections in case of pPxPP_, sPxPP_, sSxSS_, pSxSS_, ...
c
               if(phid1(2:2).eq.phid1(4:4) .and. 
     +            phid1(2:2).eq.phid1(5:5))  then
                  go to  296
               endif
c
c    no corrections in case of pPxPxPx, sPxPxPx, sSxSxSx, pSxSxSx, ...
c
               if(phid1(2:3).eq.phid1(4:5) .and. 
     +            phid1(2:3).eq.phid1(6:7))  then
                  go to  296
               endif
c
c    no corrections in case of pPP, sPP, sSS, pSS, ...
c
               if(phid1(2:2).eq.phid1(3:3) .and. 
     +            phid1(2:2).eq.phid1(5:5))  then
                  go to  296
               endif
            endif

            chgcas = uppcas(phid1)
            phase_t = chgcas(1:1)

c
c           Distance of reflection point from source
c
            fmult = 1.d0
            del0  = dirdel(dpaa,zo,fmult,phase_t)
            azi0  = azie(iev(i))

            if(phid1(1:1).ne.'p' .and. phid1(1:1).ne.'s') then
               if(dpa.ge.0.d0) then
                  del0 = (delta-del0)/2.d0
               else
                  del0 = (360.d0 - delta - del0)/2.d0
                  azi0 = alpha2(azie(iev(i)) + 180.d0)
               endif
            endif
                
c
c     The reflection point must lay within the valid distance of 
c     local/regional model
c

            if(del0.gt.rmax0 .and. (imo.eq.1 .or. imo.eq.5)) go to 296
    
c
c     Reflection point lat/lon
c
            inddel = 1
            call delazd(elatmg,elonm,azi0,del0,inddel,elatc,elonc)

c
c     Correction for depth phases (e.g.: pP, pwP, sS, pS, sP, ...)
c 

            if(phid1(1:1).eq.'p' .or. phid1(1:1).eq.'s' .and.
     +         phase_t.ne.' ')  then

              if( phid1(1:2).eq.'pP' .or. phid1(1:2).eq.'sS' .or.
     +             phid1(1:3).eq.'pwP' ) then
                  indr = 2
              else 
                  indr = 4
              endif

              zoc    = zo

              if(imo.eq.5) locmod= .true.

              call crustc(trefl,delr,phase_t,dpaa,zoc,trefl2,delr2,
     +                    modind,indr,iwl,typctl)

              go to 296

            endif

c
c     correction for surface multiples (e.g.: PnPn,...,PP,SS,P'P', but
c     also PcPPcP and ScSScS)
c
            if( phid1(1:1).eq.phid1(2:2) .or. 
     +          phid1(1:2).eq.phid1(3:4) .or.
     +          phid1(1:3).eq.phid1(4:6)  ) then

                indr = 3
                zoc  = zo

                if(imo.eq.5) locmod= .false.

                call crustc(trefl,delr,phase_t,dpaa,zoc,trefl2,delr2,
     +                      modind,indr,iwl,typctl)
   
                go to 296

            endif

c
c      correction for converted surface multiples (e.g.: PnS,...)
c
            conr = .false.

            if( (phid1(1:1).eq.'P' .or. phid1(2:2).eq.'P') .and.
     +          (phid1(1:1).eq.'S' .or. phid1(2:2).eq.'S') ) then
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
     +          (phid1(2:2).eq.phid1(4:4) .or. phid1(2:2).eq.'g' .or.
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
                go to 296

            endif

         endif

296      continue

         if(dabs(trefl)+dabs(trefl2).ge.epsilon) then
            usedr = 'R'
            nstref = nstref + 1 
         else
            usedr = ' '
            trefl = 0.d0
            trefl2= 0.d0
         endif

         if(usedr.eq.'R' .and. typctl.gt.6) then
             print *,'dirdel: ',phid1,' azi ',azi0,' del ',del0
             print *,'  lat ',elatc,' lon ',elonc,' trefl ',trefl,
     +               ' trefl2 ',trefl2, 'usedr ',usedr
         endif

         ttt(i) = tom + ttc(j) + ecor 
     +                + th + tcrust + trefl - statict

         dtt    = tt(i) - ttt(i)
 
         if(iwl.gt.0 .and. usedr.eq.'R' .and. phase_t.eq.'P'
     +      .and. dabs(trefl2).ge.epsilon) then

            fdt =  trefl2 - trefl
            dtt2 = dtt - fdt

            wflag = .false.

            if(iwl.eq.1) then
               wflag = .true.
            else if(iwl.eq.2) then
               if(dabs(dtt2).lt.dabs(dtt)) wflag = .true.
            else if(iwl.eq.3) then
               if(dabs(fdt).ge.dtdw .and. dabs(dtt2).lt.dabs(dtt)) 
     +            wflag = .true.
            else if(iwl.eq.4) then
               wflag = .true.
               if(dabs(fdt).ge.dtdw .and. dabs(dtt2).gt.dabs(dtt)) 
     +            wflag = .false.
            else if(iwl.eq.5) then
               if(index(phase(i),'w').gt.0) wflag=.true.
            endif

c           print *,'water layer: ',i,iwl,dtt,dtt2,fdt,dtdw,wflag

            if(wflag) then
               delr   = delr2
               trefl  = trefl2
               ttt(i) = ttt(i) + fdt
               phid   = phasw(phid)
               dtt    = dtt2
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

         if(typctl.ge.8) then
             print *,'i, stat, phid, ttt, t0, TT, ECOR, Height, Crust, '
     +               ,'Refl, Stat, DTT, DTTS, touse'  
             print *,i,sta(iev(i)),' ',phid,ttt(i),tom,ttc(j),
     +               ecor,th,tcrust,trefl,statict,dtt,dtts,touse(i),
     +               j,phcd(j)
         endif

         if(dabs(dtt).lt.dabs(dtts)) then
            dtts  = dtt
            ttts  = ttt(i)
            dtdhs = dtdhu
            dpdhs = dpdhu
            dddps = dddpu
            dpas  = dpa
            phid0 = phid 
            phaseu(i) = phid 
            usedsr = usedr
         endif

         if(imin.le.1 .and. .not.fixinp .and. (ilastiter.gt.0 .or.
     +      iter.gt.5) .and. .not.(iwl.eq.5 .and. 
     +      (phid(1:1).eq.'p'.or.phid(1:1).ne.'s')) ) then

           phid2 = phid
           call check_phase(phid,imin,delta,dk,dtt,surf,fconr,fmoho)

c          print *,'---> (5--) phid0,phid,phid2 ',i,j,j1,phid0,phid,
c    +           phid2,delta,dk,dtt,surf,fconr,fmoho,imin

           if(phid.ne.phid2 .and. imin.gt.0) go to 295
           if(imin.lt.0) imin = -imin
           phid = phid2
           
         endif

c        print *,'---> (5-) phid0,phid,phid1 ',i,j,j1,phid0,phid,phid1,
c    +           dtt,dtts,dtm,touse(i),fixinp,delta,imin

         if(phid.ne.phaseu(i)) then
            dtt   = dtts
            ttt(i)= ttts
            dtdhu = dtdhs 
            dpdhu = dpdhs
            dddpu = dddps
            dpa   = dpas
            dpaa  = dabs(dpa)
            phid  = phaseu(i)
            usedr = usedsr
         endif

         dtm = dtm0
         if(phase_t.eq.'P') dtm  = dtmp + var(1)
         if(phase_t.eq.'S') dtm  = dtms + var(1)
         dtm = dmin1(dtm,datmax0)

c        print *,'---> (5) phid0,phid,phid1 ',i,j,j1,phid0,phid,phid1,
c    +        dtt,dtm,touse(i),fixinp,delta

         if((dabs(dtt).le.dtm .or. fixinp ) 
     +       .and. touse(i)(1:1).eq.'T') then

           jj  = jj + 1
           jdt = jdt + 1
           rms1 = rms1 + q2(dtt)
           nrms1= nrms1+ 1

c          print *,'---> (5a) phid0,phid,phid1 ',i,j,jj,phid0,phid,
c    +        phid1,dtt,dtm,delta,touse(i)

           dat(jj) = dtt

c
c          setting uncertainty for this onset time
c
c          if(tresw) was set, tt2(i) may have a different 
c          value than tts(i) !
c

           if(dtt.gt.0.d0) then
               dats(jj) = tts(i)
           else
               dats(jj) = tt2(i)
           endif

c
c     adding theoretical model uncertainties to the data uncertainty
c
           if(lsmu ) then

              f1 = 0.d0
              if(phase_t.eq.'P') then
                 f1 = smpu
                 if(phid.eq.'Pn') then
                    if(smpnu.gt.0.d0) f1 = smpnu
                 else if(phid.eq.'Pb') then
                    if(smpbu.gt.0.d0) f1 = smpbu
                 else if(phid.eq.'Pg') then
                    if(smpgu.gt.0.d0) f1 = smpgu
                 endif
              else if(phase_t.eq.'S') then
                 f1 = smsu
                 if(phid.eq.'Sn') then
                    if(smsnu.gt.0.d0) f1 = smsnu
                 else if(phid.eq.'Sb') then
                    if(smsbu.gt.0.d0) f1 = smsbu
                 else if(phid.eq.'Sg') then
                    if(smsgu.gt.0.d0) f1 = smsgu
                 else if(phid.eq.'Lg') then
                    if(smlgu.gt.0.d0) f1 = smlgu
                 endif
              endif

c
c          if possible model uncertainties were found, they are added 
c          to the observation uncertainties:
c
              if(f1.gt.0.d0) dats(jj) = dpythag(dats(jj),f1)
 
           endif
             
c
c          (ierre.ne.0) > No ellipticity correction is available for 
c                         this onset. We assume a larger uncertainty!
c
           if(ierre.ne.0 .and. .not.surf) then
              dats(jj)= dats(jj) + 0.5d0
              ierre = 0
           endif

c
c          different data-types can be weighted unequally (see Manual)
c
           if(ftvar) dats(jj) = dpythag(dats(jj),dtt)
           if(ftw)   dats(jj) = dats(jj) / 2.d0
           if(fth)   dats(jj) = dats(jj) * 2.d0

c
c          we have to know the actually used uncertainty for later 
c          statistics
c
           ttu(i) = dats(jj)

           if(dabs(dtt/ttu(i)).gt.datmax) datmax=dabs(dtt/ttu(i))

           a(jj,1) = 1.d0
           a(jj,2) = dpa*deldla
           a(jj,3) = dpa*deldlo

           if(dabs(dtdhu).lt.1.d-5) then
              a(jj,4)=0.d0
           else
              a(jj,4) = dtdhu
              nzo = nzo + 1
           endif

           used(i)(1:1) = 'T'
           if(imin.eq.4) used(i)(1:1) = '2'
           used(i)(5:5) = usedr
           phaseu(i) = phid

           dinv(i,1) = dble(jj)

           datla(i) = a(jj,2)
           datlo(i) = a(jj,3) 
           datho(i) = a(jj,4)

           if(typctl.gt.5) then
                print *,jj,' tt ',tt(i),a(jj,1),a(jj,2),a(jj,3),
     +                     a(jj,4),dat(jj),used(i),dpa
           endif

         else if(dabs(dtt).le.dtm+dtdt .and. touse(i)(4:4).eq.'D') then

c
c          the phase may be later usable for a travel-time-
c          difference observation.
c

           used(i)(1:1) = 't'
           if(imin.eq.4) used(i)(1:1) = '3'

           used(i)(5:5) = usedr
           phaseu(i) = phid
      
           datla(i) = dpa*deldla
           datlo(i) = dpa*deldlo

           if(dabs(dtdhu).lt.1.d-5) then
              datho(i) =0.d0
           else
              datho(i) = dtdhu
           endif

c
c          setting uncertainty for this onset time
c
c          if(tresw) was set, tt2(i) may have a different 
c          value than tts(i) !
c

           if(dtt.gt.0.d0) then
               ttu(i) = tts(i)
           else
               ttu(i) = tt2(i)
           endif

c
c          (ierre.ne.0) > No ellipticity correction is available for this
c                         phase. We assume a larger data uncertainty!
c
           if(ierre.ne.0 .and. .not.surf) then
              ttu(i)= ttu(i) + 0.5d0
              ierre = 0
           endif

         endif

         if(used(i)(1:1).eq.' ' .and. touse(i)(1:1).eq.'T') then
            do 2969 j2 = j+1,nphass
               if(phid .eq. phcd(j2)) then
                 j = j2 - 1
                 imin = 4
                 go to 297
               endif
2969        continue
         endif

c        print *,'---> (5b) phid0,phid,phid1 ',i,j,jj,phid0,phid,phid1,
c    +        dtt,dtm,delta,touse(i),dtmaxslow,surf

         used(i)(3:3) = ' '
         if(touse(i)(3:3).eq.'S' .and. dabs(dtt).lt.dtmaxslow .and.
     +      .not.surf) then

           ddpa = p(i) - dpaa

c          print *,'ray p',ddpa,p(i),dpaa,dpam,fixinp

           if(dabs(ddpa).lt.dpam .or. 
     +        (fixinp .and. dabs(ddpa).lt.10d0) ) then
             jj  = jj + 1
             jpa = jpa + 1
             dat(jj)  = ddpa
             dats(jj) = ps(i)
             if(fsvar) dats(jj) = dpythag(dats(jj),ddpa)
             if(fsw)   dats(jj) = dats(jj) / 2.d0
             if(fsh)   dats(jj) = dats(jj) * 2.d0

             a(jj,1) = 0.d0
             a(jj,2) = deldla / dddpu
             a(jj,3) = deldlo / dddpu

             partabl = dddpu

             if(dabs(dpdhu).lt.1.d-5) then 
                a(jj,4)=0.d0
             else
                a(jj,4) = dpdhu
                nzo = nzo + 1
             endif

             used(i)(3:3) = 'S'
             phaseu(i) = phid

             dinv(i,3) = dble(jj)

             if(typctl.gt.5) then
                   print *,jj,' p ',p(i),a(jj,1),a(jj,2),
     +                    a(jj,3),a(jj,4),dat(jj),used(i)
             endif

           endif

         endif

         if(used(i)(1:1).ne.' ' .or. used(i)(3:3).ne.' ') go to 299

         if(imin.eq.0) go to 2975

      endif
      
      if(imin.le.1 .and. used(i)(1:1).eq.' ' .and. used(i)(3:3).eq.' '
     +   .and. j1.eq.nphass .and. dtt.gt.-9999.d0) then

         phid2 = phid
         call check_phase(phid,imin,delta,dk,dtt,surf,fconr,fmoho)

c        print *,'---> (5d) phid2,phid ',i,j,jj,phid2,phid,imin

         if(phid.ne.phid2 .and. imin.gt.0 .and. phid.ne.phcheck) then
            phcheck = phid
            go to 295
         endif
         if(imin.lt.0) imin = -imin
         phid = phid2

      endif

c     print *,'---> (5e) phid0,phid,phid1 ',i,j,jj,phid0,phid,phid1,
c    +     imin,used(i),j1,dtt

c
c     end of loop over all theoretical phases
c
297   continue

2975  if(single) go to 299
      if(used(i)(1:1).ne.' ' .or. used(i)(3:3).ne.' ') go to 299

c
c     Try it with another phase-name from the same phase-type.
c

c     print *,'--> (7)', i,phid,phid0,icha

      phid2 = phid
      call testphase(phid2,icha,delta)

c     print *,'--> (8)', i,phid2,phid,icha

      if(icha.lt.99 .and. phid.ne.phid2) then
         if(phid(1:1).eq.'L' .or. phid(1:1).eq.'R') go to 294
         phid = phid2
         imin = 3
         go to 295
      endif

299   continue

c     print *,'after 299',i,phase(i),sta(iev(i)),' ',
c    +        phid,phid0,phaseu(i),used(i),dtt

      used(i)(2:2) = ' '

      if( ( touse(i)(2:2).eq.'A' .and. .not.aziini ) .and. 
     +    ( dabs(dtt).lt.dtmaxazib .or. aziall .or. 
     +      ((phid(1:2).eq.'LR' .or. phid(1:2).eq.'LQ')
     +      .and.dabs(dtt).lt.dtmaxazil)  ))  then

         if(used(i)(1:1).eq.'T' .and. (dpa.lt.0.d0 
     +                              .or.phase(i)(1:4).eq.'P3KP') ) then
           ddazi = alpha1(azi(i) - alpha2(baz(iev(i))-180.d0))
         else
           ddazi = alpha1(azi(i) - baz(iev(i)))
         endif

         if(dabs(ddazi).lt.dazim  .or. 
     +     (fixinp .and. dabs(ddazi).lt.50.d0) ) then
           jj = jj + 1
           jazi = jazi + 1
           dat(jj)  = ddazi
           dats(jj) = azis(i)
           if(favar) dats(jj) = dpythag(dats(jj),ddazi)
           if(faw)   dats(jj) = dats(jj) / 2.d0
           if(fah)   dats(jj) = dats(jj) * 2.d0

           a(jj,1) = 0.d0
           a(jj,2) = dazidla
           a(jj,3) = dazidlo
           a(jj,4) = 0.d0
           used(i)(2:2) = 'A'

           dinv(i,2) = dble(jj)

           if(typctl.gt.5) then
              print *,jj,' baz ',azi(i),a(jj,1),a(jj,2),a(jj,3),
     +                     a(jj,4),dat(jj),used(i)
           endif
         endif

      endif

300   continue

      if(single) go to 302

      if(jazi .gt. jdt .and. jdt.le.3 .and. ifixaz.le.5) then
         ifixaz = ifixaz + 1
         dtmp   = dtmp * 1.5d0
         dtms   = dtms * 1.5d0
         dtm0   = dtmp + dtms
         dazim  = dazim * 1.5d0
         dpam   = dpam * 1.5d0
         datmax0 = datmax0 * 5.d0
         go to 101
      endif

      if(nrms1.gt.1) rmso = dsqrt(rms1/dble(nrms1))

      if(jj.ge.3) then
         if(ilastiter.eq.1 .and. iremo.lt.2 .and. lastfixt) then
            rms0 = 1.5d0 * rmso / sdmeans
            if(datmax.ge.rms0) then
               datmax0 = rmso*2.d0
               if(datmax0.lt.1.d-1) datmax0=1.d-1
               iremo = iremo+1
               go to 101
            endif
         endif
         iteraz = 0 
      else 
         if(rmso.le.50.d0) then
            rmso = rmso*10.d0
         else 
            rmso = 9999.d0
         endif
         iteraz = iteraz + 1
      endif

      if(.not.diffflag) go to 302
c
c     Add possible travel-time difference(s) as additional 
c     condition(s) to the equation system to be solved.
c
c     Travel-time differences can only be used in the case that we 
c     have more than 2 different phase observations at one station.
c
      
      if(jdt.le.2) go to 302

      ndt = 0

      do 3011 i = 1,nobs-1

      if(touse(i)(4:4).ne.'D') go to 3011
      if(used(i)(1:1).eq.' ') go to 3011

      do 301 j = i+1,nobs

         if(touse(j)(4:4).ne.'D' ) go to 301
         if(used(j)(1:1).eq.' ') go to 301

         if(iev(i).ne.iev(j)) go to 301

         if(phaseu(i).eq.phaseu(j)) go to 301
         if(dabs(tt(i)-tt(j)).le.1.d-3)     go to 301
         if(dabs(ttt(i)-ttt(j)).le.1.d-3)   go to 301

         if(((tt(j).gt.tt(i)) .and. (ttt(j).lt.ttt(i))) .or.
     +       ((tt(i).gt.tt(j)) .and. (ttt(i).lt.ttt(j)))   )  go to 301

         dtt = (tt(j) - ttt(j)) - (tt(i) - ttt(i))

         if(dabs(dtt).le.dtm0 .or. fixinp) then

           jj = jj + 1

           ndt = ndt + 1
           idtu(ndt) = j*i + j+i

           dat(jj) = dtt

           dats(jj)= dpythag(ttu(i),ttu(j))

           if(fdtvar) dats(jj) = dpythag(dats(jj),dtt)
           if(fdtw)   dats(jj) = dats(jj) / 2.d0
           if(fdth)   dats(jj) = dats(jj) * 2.d0

           a(jj,1) = 0.d0
           a(jj,2) = datla(j) - datla(i)
           a(jj,3) = datlo(j) - datlo(i)

           a(jj,4) = datho(j) - datho(i)
           if(dabs(a(jj,4)).lt.1.d-5) then
              a(jj,4)=0.d0
           else
              nzo = nzo + 1
           endif

           dinv(i,4) = dble(jj) + dble(ndt)*1.D-3

           if(typctl.gt.5) then
              print *,jj,' dt ',i,j,phaseu(i),phaseu(j),a(jj,1),a(jj,2),
     +                a(jj,3),a(jj,4),dat(jj),dats(jj),used(i),used(j),
     +                dtm0,fixinp
           endif

          endif

301   continue
3011  continue

c
c     Everything is ready for a next or  a 'final' inversion
c
c     hyposat_gmi will do it
c

302   in = jj

      if(in.gt.mread2) then

         print *, 'Inversion matrix: ',in,' (data) ',mread2,
     +            ' (wrong dimension of Jacobian)'
         go to 9999

      endif

      if(czo.eq.'D') then

        if(nobs.lt.4 .or. jdt.lt.4) then
           if(zo/(deg2rad*radloc(stala(istatmin),1)).lt.0.25d0 .and. 
     +        iter.gt.2) then
             if( zoflag ) go to 9998
             zo = dmax1(0.d0,depthmin)
             czo = 'B'
             lcomfix = .true.
             rs(4)  = 1.d0
             var(4) = 0.d0
             if(typctl.gt.0) then
                print *,'(1) No depth resolution, fixed at',zo,' [km]'
             endif

             if(output) then
                write(11,'(/''No resolution for depth, fixed at''
     +                      ,f7.2)') zo
             endif

             go to 101
           endif
        endif

        im = 4

        if(nzo .le. 0 .or. in.le. 3) then

           if( zoflag ) go to 9998

           czo = 'B'
           lcomfix = .true.
           rs(4)  = 1.d0
           var(4) = 0.d0
           im  = 3

           if(typctl.gt.0) then
              print *,'No depth determination possible, fixed at',
     +                zo,' [km]'
           endif
           if(output) then
              write(11,'(/''No depth determination possible, '',
     +                    ''fixed at'',f7.2)') zo
           endif

        endif

      else if(czo.eq.'F' .or. czo.eq.'B') then

        im = 3
        if(czo.eq.'B' .and. dabs(zo-zo1).lt.5.d-3) lcomfix = .false.

      endif

      if(in.le.1 .and. .not.single) then

         if(in0sw .ge. 6) then
            print*,'Inversion failed'
            print*,'No data fit within limits of initial model!'
            go to 9999
         endif

         if(plflag .and. in0sw.eq.5) then

           call plane(stala,stalo,iev,tt,nobs,azim,dazir,
     +                ray,dray,phipl,touse,phase,jref,typctl)

           if(jref.gt.0 .and. dazir.lt.50.d0 .and. dray.lt.4.d0) then

             phase_t = ' '
             itray = 1
             rayok = .false.
3021         call tauget_ray(phipl,phase_t,ray,modn,zo,
     +                        ddel,ttray,rayok)

             if(rayok) then

               inddel = 1

c               print *,' ---> [delazd] B',stala(iev(jref)),
c     +              stalo(iev(jref)),azim,ddel,inddel,
c     +              elatmg,elonm
               
               call delazd(stala(iev(jref)),stalo(iev(jref)),azim,
     +                     ddel,inddel,elatmg,elonm)
               
c               print *,' ---> [delazd] A',stala(iev(jref)),
c     +              stalo(iev(jref)),azim,ddel,inddel,
c     +              elatmg,elonm


               elatm = convlat(elatmg,1)

               if(lwdepth ) then
                   water = wdepth(elatmg,elonm)
                   if(water.gt.zo) then
                      zo = dble(dnint(water+0.5d0))
                      rayok = .false.
                      if(typctl.gt.4) then
                         print *, 'water depth at source:', water,' km'
                         print *, 'depth fixed at: ',zo,' km'
                      endif
                      lwater = .true.
                      zwater = 1000.d0*water
                      go to 3021
                   else
                      lwater = .false.
                   endif
               endif

               if(typctl.gt.0) then
                  if(ddel.gt.0d0) then
                    print *,'Epicenter set from station ',
     +               sta(iev(jref)),'after plane wave fit: backazimuth',
     +               azim,' deg, delta',ddel,' deg'
                  else
                    print *,'Epicenter set to station ',sta(iev(jref))
                  endif
               endif

               sdlatg = 45.d0/ray
               sdlon  = 90.d0/ray

               tome = tt(jref) + timemin - ttray

               if(output) then
                  if(ddel.gt.0d0) then
                       write(11,'(''Epicenter set from station '',a8)')
     +                   sta(iev(jref))
                  else
                       write(11,'(''Epicenter set to station '',a8,
     +                   '' deg, delta'',f7.2,'' deg'')') sta(iev(jref))
                  endif
                  write(11,'(''Epicenter lat:'',f9.3,'' [deg]'')')  
     +              elatmg
                  write(11,'(''Epicenter lon:'',f9.3,'' [deg]''/)') 
     +                     elonm
                  write(11,'(''Source time set to: '',f15.2)') tome
               endif

               in0sw = in0sw + 1
               go to 101

             else
               
               if(itray.lt.2) then

                  itray = itray + 1
                  ray = ray - dray
                  go to 3021

               endif

             endif

           endif

         endif

         in0sw = in0sw + 1

         if(iter.ge.3) then

           if(iter.gt.mosci) then
               ilas = 1
           else
               ilas = iter-2
           endif

           tom     = dtos(ilas)
           tome    = tom + timemin
           elonm   = dloos(ilas)
           elatm   = dlaos(ilas)
           elatmr  = deg2rad*elatm
           elatmg  = convlat(elatm,2)
           coelatm = 90.d0 - elatm
           coelatmr= deg2rad*coelatm

         endif

         dtmp   = dtmp * 2.d0
         dtms   = dtms * 2.d0
         dtm0   = dtms
         dazim  = dazim * 2.0d0
         dpam   = dpam * 2.0d0
         datmax0 = datmax0 * 10.d0

         if(typctl.ge.4) then
            print *, 'Inversion matrix error: in = ',in
            print *, '(too less data to invert!)' 
            print *, 'Time boundaries changed'
         endif

         go to 101

      endif

c
c     special case: only one, single array observation
c
      if(single .and. in.eq.3) then

        dtmflag = .true.

        ddel = dat(2) / partabl

        if(p(1).lt.pdif .and. dabs(ddel).gt.1.d0) 
     +     ddel=dmax1(ddel/2.d0,1.d0)
        if(p(1).gt.9.d0 ) then 
           if(ddel.gt.1.d0 ) ddel=1.d0
           if(ddel.lt.-1.d0) ddel=-1.d0
        endif

        deln = del(1) + ddel 

        inddel = 1
        call delazd(stala(1),stalo(1),azi(1),deln,inddel,elat1,elon1)

        elatm1 = convlat(elat1,1)

        r(1) = dat(1)
        r(2) = elatm1 - elatm
        r(3) = elon1 - elonm
        r(4) = 0.d0

        dvar = dabs(dats(2) / partabl)
        ddel1 = deln + dvar
        ddel2 = deln - dvar

        dpaa1 = dpaa + dats(2)
        if(dpaa1.gt.40.d0) dpaa1 = 40.d0
        dpaa2 = dpaa - dats(2)
        if(dpaa2.lt.1.d-2) dpaa2 = 1.d-2

        var(1) = dats(1)

        zor = zo
        phase_t = ' '
        call tauget_ray(phaseu(1),phase_t,dpaa1,modn,zor,
     +                  ddel0,ttray1,rayok)

        if(rayok) then
           ddel2 = ddel0
           call tauget_ray(phaseu(1),phase_t,dpaa2,modn,zor,
     +                     ddel0,ttray2,rayok)

           if(rayok) then
              ddel1 = ddel0
              var(1) = dpythag(var(1),0.5d0*(ttray1-ttray2))
           endif

        endif

        aziv1 = alpha2(azi(1) + dats(3))
        aziv2 = alpha2(azi(1) - dats(3))

        call delazd(stala(1),stalo(1),aziv1,ddel1,inddel,elat1,elon1)
        call delazd(stala(1),stalo(1),aziv2,ddel1,inddel,elat2,elon2)
        call delazd(stala(1),stalo(1),aziv1,ddel2,inddel,elat3,elon3)
        call delazd(stala(1),stalo(1),aziv2,ddel2,inddel,elat4,elon4)

        vara = dmax1(elat1,elat2,elat3,elat4)
        varb = dmax1(elon1,elon2,elon3,elon4)

        varc = dmin1(elat1,elat2,elat3,elat4)
        vard = dmin1(elon1,elon2,elon3,elon4)

        var(2) = 0.5d0 * alpha1(vara-varc)
        var(3) = 0.5d0 * alpha1(varb-vard)

        var(4) = 0.d0

        res(1) = 0.d0
        res(2) = 0.d0
        res(3) = 0.d0
        res(4) = 0.d0

        go to 304

      else if(single .and. in.lt.3) then

           if(insar.le.15) then

              ddel = deln

              if(insar.le.0) then 

                 phase(1) = 'P'
                 ddel = 50.d0

              else

                if(p(1).le.pdif) then

                   if(insar.ge.7) go to 3028

                   ddel = 148.d0

                   if(phid(1:2).eq.'P ')   phase(1) = 'Pdif'
                   if(phid(1:3).eq.'Pdif') phase(1) = 'PKPab'
                   if(phid.eq.'PKPab')     phase(1) = 'PKPdif'
                   if(phid.eq.'PKPdif')    phase(1) = 'PKPbc'
                   if(phid.eq.'PKPbc')     phase(1) = 'PKPdf'
                   if(phid.eq.'PKPdf')     phase(1) = 'PKiKP'
              
                else if(p(1).gt.9.d0) then

                   if(insar.ge.4) go to 3028

                   if(phid(1:2).eq.'P ' .or. phid(1:2).eq.'P1')  then
                      phase(1) = 'Pn'
                      ddel = 10.d0
                   endif
                   if(phid.eq.'Pn' .and. .not.fmoho) then
                      phase(1) = 'Pb'
                      ddel = 2.d0
                   endif
                   if(phid.eq.'Pn' .and. .not.fconr) then
                      phase(1) = 'Pg'
                      ddel = 1.0d0
                   endif
                   if(phid.eq.'Pb' .and. .not.fconr) then
                      phase(1) = 'Pg'
                      ddel = 1.0d0
                   endif

                endif

              endif

              insar = insar + 1
              inddel = 1
              call delazd(stala(1),stalo(1),azi(1),ddel,inddel,
     +              elatmg,elonm)
              elatm = convlat(elatmg,1)

              phaseu(1) = phase(1)
C Close origin group before jumping out
              if(json_out) call json_end_group(json_rc)
              go to 100

           endif

3028           print*,'Single phase case, but no inversion possible'
           if(output) then
              write(11,*)'Single phase case, but no inversion ',
     +                         'possible'
           endif

           if(p(1).ge. pmoh) then
              print*, 'due to missing direct',
     +                     ' crustal phase matching theoretical ray',
     +               ' parameter in chosen model.'
              if(output) then
                 write(11,*) 'due to missing direct',
     +                     ' crustal phase matching theoretical ray',
     +               ' parameter in chosen model.'
              endif
           else if(p(1).gt. 10.1d0 .and. p(1).lt. pmoh) then
              print*, 'due to missing direct upper mantle',
     +                     ' crustal phase matching theoretical ray',
     +               ' parameter in chosen model.'
              if(output) then
                 write(11,*) 'due to missing direct upper mantle',
     +                     ' crustal phase matching theoretical ray',
     +               ' parameter in chosen model.'
              endif
           else
              print *, 'due to core phase travel-time-curve ',
     +                     'triplication!'
              if(output) then
                 write(11,*) 'due to core phase travel-time-curve ',
     +                     'triplication!'
              endif
           endif
           go to 9999
           
      endif

      in0sw = 0

      if(iellipi.eq.1 .and. .not.iellip) iellip=.true.

      ilastfixi = 0
      nq = im

c     print *,'--->  [gmi]',in,im,nq, iter
c     do i=1,in,1
c        print *,i,(a(i,j),j=1,im),dat(i),dats(i)
c     enddo
c     print *,' '
     
303   continue

      call hyposat_gmi(in,im,nq,ierr,typctlm)

c
c     Matrix A contains now the Correlation Matrix
c
c     do i=1,im,1
c        print *,i,(a(i,j),j=1,im)
c     enddo
c     print *,' '
      
      if(ierr.ne.0) then
         if(output) then
            write(11,*) 'GMI failed: no new solution!'
         endif
         print*,'GMI failed: no new solution!'
         go to 9999
      endif

304   continue
      
      if(typctl.gt.4) then
          print *, 'iter,in,im,nq',iter,in,im,nq
          print *,r(1),var(1)
          print *,r(2),var(2)
          print *,r(3),var(3)
          print *,r(4),var(4)
          do 305 j=1,jj
          f1 = dat(j)-res(j)
          print *,j,dat(j),res(j),f1
305       continue
      endif

c
c     estimating the new hypocenter
c
         
c
c     the new source depth
c

      if(im.eq.4) then

        ar4 = dabs(r(4))
        if(ar4.ge.300d0 ) then
           f1 = 0.d0
           f2 = 0.d0
           if(czo.eq.'D') then
              czo='B'
              if(output)
     +           write(11,'(/''No resolution for depth, fixed at''
     +                   ,f7.2)') zo
              if(typctl.gt.0) 
     +           print *,'No resolution for depth, fixed at ',zo
              iterz = iterz + 1
           endif
        else if(ar4.gt.200.d0 .and.ar4.lt.300d0 ) then
           f1 = 128.175d0
           f2 = 0.1875d0
        else if(ar4.le.200.d0 .and. ar4.gt.100.d0 ) then
           f1 = 85.d0
           f2 = 0.375d0
        else if(ar4.le.100.d0 .and. ar4.gt.40.d0) then
           f1 = 40.d0
           f2 = 0.75d0
        else
           f1 = 0.d0
           f2 = 1.d0
        endif

        if(r(4).ge.0.d0) then
           r40 =   f1 + (ar4-f1) * f2
        else
           r40 = - f1 - (ar4-f1) * f2
        endif

        depuse = r40*dchang

        zo = zo + depuse

c       print *, 'Depth (1)',zo,r40,r(4),depuse

        if(var(4).gt.1.d-5) then
           rs(4) = var(4)
        else
           rs(4) = sdzo
        endif

        r40 = 0.d0

        if(zo.lt.depthmin) then
           idepm = idepm + 1
           r40 = depthmin - zo
           depuse = depuse + r40
           zo = depthmin
        else if(zo.gt.depthmax) then
           idepm = idepm + 1
           r40 = zo - depthmax
           depuse = depuse - r40
           zo = depthmax
        else
           idepm = 0
        endif

c       print *, 'Depth (2)',zo,r40,r(4),depuse

        rs(4) = dpythag(rs(4),r40)
        if(rs(4).gt.250.d0) rs(4) = 250.d0

        if(dabs(depuse).gt.0.d0) call zo2to(depuse-r(4),tome,r(1))

c       print *, 'Depth (3)',zo,r40,r(4),depuse,tome,r(1)

      endif
c
c     save the old epicenter solution 
c
      elatmgo = elatmg
      elonmo  = elonm

c
c     the new epicenter and source time
c

      if(r(2).gt.180.d0) r(2) = 180.d0
      if(r(3).gt.180.d0) r(3) = 180.d0
      if(r(2).lt.-180.d0) r(2) = -180.d0
      if(r(3).lt.-180.d0) r(3) = -180.d0

      elatt = elatm + r(2)*dchang
      elont = elonm + r(3)*dchang

      elont = alpha1(elont)

      ilon = 0
      if(elatt.gt. 90.d0) then
         elatt = 180.d0 - elatt
         ilon  = 1
      else if(elatt.lt.-90.d0) then
         elatt = -(elatt + 180.d0)
         ilon  = 1
      endif
      elattg= convlat(elatt,2)

      if(ilon.eq.1) elont = alpha1(elont+180.d0)

      call depi(elattg,elont,elatmgo,elonmo,delta,dk,ep2,ep1,d2km)

      r10 = dabs(r(1))

      if(iter.le.5) then

         if(delta.le.10.d0 .or. single) then
            elatm = elatt
            elonm = elont
            elatmg =  convlat(elatm,2)
         else 
            deltn = 0.d0
            if(delta.le.30.0d0) then
               deltn = 5.d0 + delta/2.0d0
            else
               deltn = 20.d0
            endif
            inddel = 1
            call delazd(elatmgo,elonmo,ep2,deltn,inddel,elatmg,elonm)
            elatm = convlat(elatmg,1)
         endif

         if(r10.lt.140.d0) then
            f1 = 0.d0
            f2 = 1.d0
         else
            if(r10.lt.400.d0) then
               f1 = 140.d0
               f2 = 0.5d0
            else
               f1 = 270.d0
               f2 = 0.d0
            endif
         endif
      
      else

         if(delta.le.2.d0 .or. single) then
            elatm = elatt
            elonm = elont
            elatmg =  convlat(elatm,2)
         else 
            deltn = 0.d0
            if(delta.le.8.0d0) then
               deltn = 1.d0 + delta/2.0d0
            else
               deltn = 5.d0
            endif
            inddel = 1
            call delazd(elatmgo,elonmo,ep2,deltn,inddel,elatmg,elonm)
            elatm = convlat(elatmg,1)
         endif

         if(r10.lt.35.d0) then
            f1 = 0.d0
            f2 = 1.d0
         else
            if(r10.lt.115.d0) then
               f1 = 35.d0
               f2 = 0.5d0
            else
               f1 = 75.d0
               f2 = 0.d0
            endif
         endif
      
      endif
 
      if(r(1).ge.0.d0) then
         r12  = + f1 + (r10-f1)*f2
      else
         r12  = - f1 - (r10-f1)*f2
      endif

      tome    = tome + r12*dchang 

      if(ldefisc .and. (czo.eq.'F' .or. czo.eq.'B')) then
         call def_depth(defdep,elatmg,elonm,idetyp,ldefd,c1typ,ideptyp)
         call zo2to(defdep-zo,tome,var(1))
         zo = defdep
      endif
 
      if(lwdepth ) then
         water = wdepth(elatmg,elonm)
         if(water .gt. zo ) then
            call zo2to(water-zo,tome,var(1))
            zo = dble(dnint(water+0.5d0))
            if(typctl.gt.4) then
               print *, 'water depth at source:', water,' km'
               print *, 'depth fixed at: ',zo,' km'
            endif
            lwater = .true.
            zwater = 1000.d0*water
         else if (water .le. epsilon) then
            lwater = .false.
         endif
      endif

      tom     = tome - timemin

      elatmr  = deg2rad*elatm
      sdlatg  = var(2) /( eps*q2(dcos(elatmr))  + q2(dsin(elatmr)) )


      coelatm = 90.d0 - elatm
      coelatmr= deg2rad*coelatm

      if(var(1).gt. 1.d-3) rs(1) = var(1)
      if(rs(1).gt.250.d0) rs(1) = 250.d0

      if(var(2).gt. 1.d-5) rs(2) = var(2)
      if(rs(2) .gt.180.d0)  rs(2) = 180.d0

      if(var(3).gt. 1.d-5) rs(3) = var(3)
      if(rs(3).gt.180.d0) rs(3) = 180.d0

      last = .false.

      if(iter.eq.maxiter) then
         dtmflag = .true.
      else if(iter.gt.maxiter) then
         print *,'Location stopped, more than ',maxiter,
     +           ' iterations'
         if(output) then
            write(11,'(/''Location stopped, more than '',i4,
     +              '' iterations'')') maxiter
         endif

         mosci2 = 1
         go to 399
      endif

      if(iter.le.mosci) then

        dtos(iter)  = tom
        dlaos(iter) = elatm
        dloos(iter) = elonm
        dlo1os(iter)= dcos(deg2rad*elonm)
        dlo2os(iter)= dsin(deg2rad*elonm)
        dzoos(iter) = zo
        rtos(iter)  = dabs(r(1)) + var(1)
        rlaos(iter) = dabs(r(2)) + var(2)
        rloos(iter) = dabs(r(3)) + var(3)
        rzos(iter)  = dabs(r(4)) + var(4)

      endif
 
c
c     We will check if the new solution is close to the former solution.
c     If CHECK [km] is smaller than SETCHECK [km] we will stop.
c

c     The change in the horizontal plane (the epicenter)

      dk = 0.d0
      call depi (elatmg,elonm,elatmgo,elonmo,del3,dk,ep2,ep1,d2km)

c     The change in source time is compensated eventually by a
c     change in depth

      dtokm = 0.d0
      if(im.eq.4 .and. var(4).gt.1.d-5) dtokm = depuse

      check = dpythag(dtokm,dk)

      call depi(elatmg,elonm,stala(istatmin),stalo(istatmin),del3,
     +                    dk,ep2,ep1,d2km)

      ilastiter = 0

      if(   check.le.setcheck .or. 
     +      (check.le.disper*dk .and. 
     +        (iteraz.ge.5 .or. imaxiter.ge.5 .or. 
     +         dble(maxiter)/dble(iter).lt.1.4d0   ) ) 
     +                                                   ) then

         ilastiter = 1
         if((dtmflag .or. .not.lastfixt .or. in.le.im .or. 
     +       nstat.eq.1) .and. 
     +      (rmsold/rmso.lt.1.2d0 .or. rmso.lt.0.1d0)) then

            if(ilastfixi.eq.0 .and. lastfixi .and. 
     +         in/im.gt.3                          ) then

               ilastfixi = 1

               if(thrfixi0.le.0.d0) then
                  thrfixi = 1.d0 / dble(in)
               else
                  thrfixi = thrfixi0
               endif

               infind = 0
               do 365 j=1,in

                  if(dinf(j)/dinfm .lt. thrfixi) then

                     if(typctl.gt.8) print *,j,dinf(j),dinf(j)/dinfm

                     infind = infind + 1
                     do 360 j3=1,im
                        a(j,j3) = 0.d0
360                  continue

                     do 363 j3=1,nobs
                        do 361 j4=1,4
                           if(int(dinv(j3,j4)).eq.j) then
                              if(j4.le.3) then
                               used(j3)(j4:j4) = ' '
                              else
                               idum = int( (dinv(j3,4)-
     +                                int(dinv(j3,4)))*1000.d0+0.01d0)
                               idtu(idum) = 0
                              endif
                              dinv(j3,j4)   = 0.d0
                              ndt = ndt - 1
                              go to 365
                           endif
361                     continue
363                  continue
                  endif

365            continue

               if(infind.gt.0) go to 303

            endif

            go to 400

         else

            if(var(4).le.0.d0 .and. czo.eq.'D') then

               czo = 'B'
               rs(4)  = 1.d0
               var(4) = 0.d0

               if(ldefisc) then
                  call def_depth(defdep,elatmg,elonm,idetyp,ldefd,c1typ,
     +                 ideptyp)

                  call zo2to(defdep-zo,tome,var(1))
                  if(var(1).gt. 1.d-3) rs(1) = var(1)
                  if(rs(1).gt.250.d0) rs(1) = 250.d0

                  zo   = defdep
                  tom  = tome - timemin
                  
                  if(output) then
                     write(11,'(/''No depth resolution, '',
     +               ''fixed at ISC default depth:'',f6.1,
     +               '' km'')') zo
                  endif

               else

                  lcomfix = .true.

                  if(typctl.gt.0) then
                     print *,'(3) No depth resolution, fixed at',zo,
     +                      ' [km]'
                  endif

                  if(output) then
                     write(11,'(/''No resolution for depth, fixed at''
     +                          ,f7.2)') zo
                  endif

               endif

            endif

            setcheck2 = check*1.1d0

            dchang = dchang0

            go to 390

         endif

      endif

      dtmflag = .false.
      direct  = .false.

      if( ifixto.le.5  .and. (var(1).le.0.d0 .or. 
     +    (jdt.lt.in/3 .and. jdt.lt.nstat/2)) ) then

         ifixto = ifixto + 1

         rzo = sngl(zo)
         rdel = sngl(del3)
         call tauget_mod(rzo,rdel,nphas,phcd,ttc,dtdd,
     +                           dtdh,dddp,modnam(1))

         tom  = - ttc(1)
         tome = timemin + tom

         rs(1) = rs(1) + (-tom)

         dtp   = dtp * 1.2d0
         dts   = dts * 1.2d0
         dtm2  = dtp + dts

         setcheck = setcheck*1.2d0
         setcheck2= setcheck*10.d0

         go to 101

      endif

      if(iter.gt.mosci) then
        moscil = mosci
      else
        moscil = iter-1
      endif

      nextiter1 = 0

c
c     check for oscillating solutions
c

      mosci2 = 0

c     print *, 'MOSCI2-0: ', mosci2,'mosci: ',mosci,' iter: ', iter,
c    +         ' moscil,',moscil

      if(moscil.gt.2) then
         do 370 i = moscil,1,-1

           if( dabs(dzoos(i)-zo).le.rminh            .and.
     +         dabs(dtos(i)-tom).le.rmint            .and.
     +         dabs(dlaos(i)-elatm).le.rming         .and.
     +         dabs(alpha1(dloos(i)-elonm)).le.rming        ) then

               mosci2 = i
c              print *,dzoos(i)-zo,rminh,dtos(i)-tom,rmint,
c    +          dlaos(i)-elatm,dloos(i)-elonm,rming
               go to 371

           endif

370      continue
      endif

371   continue

c     print *, 'MOSCI2-1: ', mosci2,'mosci: ',mosci,' iter: ', iter

      if(mosci2.gt.0) then
c
c     we have to calculate a new initial solution from all
c     oscillating solutions!
c

         nextiter = nextiter + 1
         if(nextiter.gt.8) then
            if(output) then
               idosci = mosci2-mosci-1
               write(11,'(/''Oscillating between'',i3,'' solutions: '',
     +                  ''after '',i3,'' iterations stopped!'')')
     +         idosci, iter
            endif
            go to 399
         endif

         nextiter1 = 1

         if(czo.eq.'D') then

             zo  = dmean(dzoos,moscil,mosci2) 
             rs(4) = ddmax(rzos,moscil,mosci2)
             if(rs(4).le.1.d-5) rs(4) = sdzo

             if(nextiter.gt.moscil*2/3 .or. idepm.gt.0) then

                if(iterz.eq.1 .and. zoflag) go to 9998

                iterz = iterz + 1
                czo = 'B'
                var(4) = 0.d0
                rs(4)  = 1.d0

                call findrange(zmin,zmax1,dzoos,moscil,mosci2,1)

                if(zmax1-zmin.gt.1.d-5) then
                  if(ldefisc) then
                    call def_depth(defdep,elatmg,elonm,idetyp,ldefd,
     +                             c1typ,ideptyp)
                    zo   = defdep
                    if(output) then
                         write(11,'(/''No resolution for depth, '',
     +                        ''oscillating between:'',f6.1,
     +                        '' and'',f6.1,'' [km]''/
     +                   ''depth fixed at ISC default depth:'',f6.1,
     +                   '' km'')') zmin,zmax1,zo
                    endif
                  else

                    lcomfix = .true.

                    if(output) then
                       write(11,'(/''No resolution for depth, '',
     +                         ''oscillating between:'',f6.1,
     +                         '' and'',f6.1,'' [km]''/
     +                ''depth fixed at:'',f6.1,'' km'')')zmin,zmax1,zo
                    endif

                  endif
                endif

                if(typctl.gt.0) then
                   print *,'(2) No depth resolution, fixed at',zo,
     +                    ' [km]'
                endif

             endif

         endif

         tom = dmean(dtos,moscil,mosci2)
         tome = tom  + timemin

         rs(1) = ddmax(rtos,moscil,mosci2)
         if(rs(1).lt.1.d-3) rs(1) = sdto

         elatm = dmean(dlaos,moscil,mosci2)
         elatmr= deg2rad*elatm
         elatmg  = convlat(elatm,2)
         coelatm = 90.d0 - elatm
         coelatmr= deg2rad*coelatm
         rs(2) = ddmax(rlaos,moscil,mosci2)
         if(rs(2).lt.1.d-5) rs(2) = sdlat

         elonm1= dmean(dlo1os,moscil,mosci2)
         elonm2= dmean(dlo2os,moscil,mosci2)
         elonm = rad2deg*datan2(elonm2,elonm1)

         rs(3) = ddmax(rloos,moscil,mosci2)
         if(rs(3).lt.1.d-5) rs(3) = sdlon

         if(nrms1.gt.5) then

             dtp = dtp * 0.9d0
             dtp = dmin1(dtp,rmso*2.d0)
             if(dtp.lt.3.d0) dtp = 3.d0

             dts = dts * 0.9d0
             dts = dmin1(dts,rmso*4.d0)
             if(dts.lt.6.d0) dts = 6.d0

             dtm2  = dtp + dts

         endif

         if(nobs.gt.1) then

            dazim1 = dazim1*0.9d0
            if(dazim1.lt.3.d0) dazim1 = 3.d0

            dpam1  = dpam1 *0.9d0
            if(dpam1.lt.1.0d0) dpam1 = 1.0d0

            rminh = rminh*1.2d0
            rming = rming*1.2d0
            rmint = rmint*1.2d0

            disper = disper*1.1d0
            if(disper.gt.0.05d0) disper=0.05d0

            setcheck = setcheck*1.5d0
            setcheck2= setcheck*10.d0

            dchang = dchang * 0.8d0

            direct = .true.

         endif
         
         if(((im.eq.4.and.var(4).gt.4.d0*zo) .or. idepm.gt.0)  .and.
     +      iter.gt.5 ) then

            if(zoflag .and. iterz.ge.6) go to 9998

            czo = 'B'
            rs(4) = 1.d0
            var(4) = 0.d0

            iterz = iterz + 1

            if(ldefisc) then
               call def_depth(defdep,elatmg,elonm,idetyp,ldefd,c1typ,
     +                        ideptyp)
               call zo2to(defdep-zo,tome,var(1))
               if(var(1).gt. 1.d-3) rs(1) = var(1)
               if(rs(1).gt.250.d0) rs(1) = 250.d0

               zo   = defdep
               tom  = tome - timemin
               if(output) then
                  write(11,'(/''Bad resolution for depth; '',
     +            ''depth fixed at ISC default depth:'',f6.1,
     +            '' km'')') zo
               endif
            else

               lcomfix = .true.

               if(mosci2.gt.0)then
                  zo  = dmean(dzoos,moscil,mosci2)
               else
                  zo  = dmean(dzoos,moscil,1)
               endif

               if(output) then
                  write(11,'(/''Bad resolution for depth; depth fixed'',
     +                      '' at '',f6.1,'' km'')') zo
               endif

            endif

            if(typctl.gt.0) then
               print *,'Bad resolution for depth; depth fixed at',zo
            endif

            direct = .true.

            go to 380

         endif

         if(czo.eq.'D' .and. nextiter1.eq.0 .and. iterz.le.3 .and.
     +      moscil.ge.1) then

c
c           check for oscillation in the solutions for the focal depth
c
            mosci3 = 0

            do 377 i = moscil,1,-1

              if(dabs(dzoos(i)-zo).le.rminh) then
                 mosci3 = i
                 go to 378
              endif

377         continue

378         if((mosci3.ne.0      .and. mosci3.ne.moscil) .or. 
     +         (mosci3.eq.moscil .and. zo.le.depthmin)         ) then

c
c           we have to calculate a new initial value for the depth and 
c           fix it 
c

              call findrange(zmin,zmax1,dzoos,moscil,mosci3,1)

              rs(4)  = 1.d0
              var(4) = 0.d0
              czo = 'B'

              iterz = iterz + 1

              zo = dmean(dzoos,moscil,mosci3)

              if(dabs(zmin-zmax1).gt.setcheck) then

                 if(output) then
                    write(11,'(/''Oscillating solution between:'',f6.1,
     +                      '' and'',f6.1,'' [km] depth'')') zmin,zmax1
                 endif

                 if(ldefisc) then
                    call def_depth(defdep,elatmg,elonm,idetyp,ldefd,
     +                             c1typ,ideptyp)
                    call zo2to(defdep-zo,tome,var(1))
                    if(var(1).gt. 1.d-3) rs(1) = var(1)
                    if(rs(1).gt.250.d0) rs(1) = 250.d0
                    zo   = defdep
                    tom  = tome - timemin
                    if(output) then
                       write(11,'(/''Depth fixed at ISC default depth:''
     +                         ,f6.1,'' km'')') zo
                    endif
                 else
                    lcomfix = .true.
                    if(output) then
                       write(11,'(''Depth fixed at:'',f6.1,'' km'')') zo
                    endif
                 endif

              else

                 if(typctl.gt.0) then
                    print *,'Oscillating solution, '
                    print *,'Depth fixed at: ',zo,' km'
                 endif

              endif

              direct = .true.

            endif
         endif

      endif

380   continue
        
      if(iter.gt.mosci) then
        do 381 i = 1,mosci-1

        i2 = i + 1

        dtos(i)  = dtos(i2)
        dlaos(i) = dlaos(i2)
        dloos(i) = dloos(i2)
        dlo1os(i)= dlo1os(i2)
        dlo2os(i)= dlo2os(i2)
        dzoos(i) = dzoos(i2)
        rtos(i)  = rtos(i2)
        rlaos(i) = rlaos(i2)
        rloos(i) = rloos(i2)
        rzos(i)  = rzos(i2)

381     continue

        dtos(mosci)  = tom
        dlaos(mosci) = elatm
        dloos(mosci) = elonm
        dlo1os(mosci)= dcos(deg2rad*elonm)
        dlo2os(mosci)= dsin(deg2rad*elonm)
        dzoos(mosci) = zo
        rtos(mosci)  = dabs(r(1)) + var(1)
        rlaos(mosci) = dabs(r(2)) + var(2)
        rloos(mosci) = dabs(r(3)) + var(3)
        rzos(mosci)  = dabs(r(4)) + var(4)

      endif

      if(direct) go to 100

390   if(typctl.ge.4) then
         print*,'Iteration: ',iter,'   # of def.: ',in
         print*,'New source time  : ',tome,' +/- ',var(1)
         print*,'New epicenter lat: ',elatmg,' +/- ',sdlatg
         print*,'New epicenter lon: ',elonm,' +/- ',var(3)
         print*,'New source depth : ',zo,' +/- ',var(4)
      endif
c
      if(iter.gt.nint(maxiter*0.75) .and. imaxiter.lt.5 .and.
     +   ilastiter.eq.0) then

c
c     If we were coming close to the end of all iterations,
c     let's try it with a mean solution of the last 
c     4 solutions as new initial solution.
c

         imaxiter = imaxiter + 1
         maxiter  = maxiter + nint(maxiter*0.25/imaxiter)

         if(czo.eq.'D') then
 
            zo  = dmean(dzoos,mosci,1)
            rs(4) = ddmax(rzos,mosci,1)
            if(rs(4).le.1.d-3) rs(4) = sdzo

         endif
 
         tom = dmean(dtos,mosci,1)
         tome = tom  + timemin
         rs(1) = ddmax(rtos,mosci,1)
         if(rs(1).le.1.d-3) rs(1) = sdto
 
         elatm = dmean(dlaos,mosci,1)
         elatmr= deg2rad*elatm
         elatmg  = convlat(elatm,2)
         coelatm = 90.d0 - elatm
         coelatmr= deg2rad*coelatm
         rs(2) = ddmax(rlaos,mosci,1)
         if(rs(2).le.1.d-4) rs(2) = sdlat
 
         elonm1= dmean(dlo1os,mosci,1)
         elonm2= dmean(dlo2os,mosci,1)
         elonm = rad2deg*datan2(elonm2,elonm1)

         rs(3) = ddmax(rloos,mosci,1)
         if(rs(3).le.1.d-4) rs(3) = sdlon

         dazim1 = dazim1*2.d0
         if(dazim1.gt.50.d0) dazim1 = 50.d0

         dpam1  = dpam1 *2.d0
         if(dpam1.gt.10.d0) dpam1 = 10.d0

         dtp   = dtp * 2.d0
         dts   = dts * 2.d0
         dtm2  = dtp + dts

         setcheck = setcheck*1.5d0
         setcheck2= setcheck*15.d0
         disper   = disper*2.d0
         if(disper.gt.0.05d0) disper=0.05d0

         rminh = rminh*1.5d0
         rming = rming*1.5d0
         rmint = rmint*1.5d0

      endif

      go to 100

399   continue

      last = .true.

      call findrange(tomin,tomax,dtos,mosci,mosci2,1)
      call findrange(dlamin,dlamax,dlaos,mosci,mosci2,1)
      call findrange(dlomin,dlomax,dloos,mosci,mosci2,2)
      call findrange(zmin,zmax1,dzoos,mosci,mosci2,1)

      if(output) then
         write(11,'(''Rel. source time between'',f9.2,'' and'',
     +           f9.2,'' [s]'')') tomin,tomax
      endif

      flamin = convlat(dlamin,2)
      flamax = convlat(dlamax,2)
      if(output) then
         write(11,'(''Latitude         between'',f8.2,'' and'',
     +           f8.2,'' [deg]'')') flamin,flamax

         write(11,'(''Longitude        between'',f8.2,'' and'',
     +           f8.2,'' [deg]'')') dlomin,dlomax

         if(czo.eq.'D' .and. (zmax-zmin).gt.0.05d0) then
            write(11,'(''Depth            between'',f7.1,''  and'',
     +              f7.1,''  [km]''/)') zmin,zmax1
         endif

            write(11,'(//''Following the last (must not be the '',
     +                  ''best!) solution:''/)') 

      endif

400   continue

c
c     initializing ellipticity-correction coefficients
c
      eflag = .false.
      du0 = 0.d0
      du1 = 0.d0
      idum = 2
      call elpcor('P',du0,du0,coelatmr,du0,du1,eflag,idum)

      if(output) then
         write(11,'(/''Iterations        :'',i5)') iter
      endif

      ibad = 0

      if(idepm.gt.0) then
        czo = 'F'
        lcomfix = .true.
        ibad = ibad + 1
      endif

      if(rmso .gt. 50.d0) ibad = ibad + 1

      if(infind.gt.0) in = in - infind

      if(json_out) then
        call json_start_dict_group("origin", json_rc)
        call json_add_int("number_of_iterations", iter, json_rc)
        call json_add_int("number_of_defining", in, json_rc)
      endif

      if(output) then

         write(11,'(''Number of defining:'',i5)') in

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

         if((imo.eq.2 .or. imo.eq.4 .or. imo.eq.5) .and. lstcor) 
     +       write(11,'(''CRUST 1.0 used for Station corrections'')')

         if((imo.eq.2 .or. imo.eq.4 .or. imo.eq.6) .and. nstref.gt.0) 
     +       write(11,'(''CRUST 1.0 used for Reflection Point '',
     +                  ''corrections'')')
      endif

      if(json_out) then
         call json_add_string("global_reference_model", modnam, json_rc)
         if(iloc) call json_add_string("regional_model", trim(filloc),
     +       json_rc)
         if((imo.eq.2 .or. imo.eq.4 .or. imo.eq.5) .and. lstcor) 
     +      call json_add_bool("CRUST 1.0 used for Station corrections",
     +            .TRUE., json_rc)

         if((imo.eq.2 .or. imo.eq.4 .or. imo.eq.6) .and. nstref.gt.0) 
     +      call json_add_bool(
     +      "CRUST 1.0 used for Reflection Point corrections",
     +            .TRUE., json_rc)

         if(ndt.gt.0)
     +      call json_add_int("Travel-time differences used as defining"
     +      ,ndt, json_rc)

      endif

      call fetoh2(tome,idum,yy,mon,mm,dd,idoy,hh,mi,sec)
         
c

      var2(1) = fchi1 * var(1)
      if(var2(1).gt.9999.999d0) var2(1) = 9999.999d0

      f1      = var(2) /( eps*q2(dcos(elatmr))+q2(dsin(elatmr)) ) 
      sdlatg  = fchi1 * f1

      var2(3) = fchi1 * var(3)

      var2(4) = fchi1 * var(4)

      if(output) then

         write(11,'(/''The new source parameters:'')')

         write(11,'(/''Confidence level of given uncertainties:'',
     +         f7.2,'' %'')') confl

         if(kmout) then
            if(disminst.gt.0.d0 .and. dismaxst.ge.20100.d0) then
               write(11,'(/''Location for observations at distances '',
     +               ''larger than'',f8.1,'' [km]'')') disminst
            endif
            if(disminst.le.0.d0 .and. dismaxst.lt.20100.d0) then
               write(11,'(/''Location for observations at distances '',
     +               ''shorter than'',f8.1,'' [km]'')') dismaxst
            endif
            if(disminst.gt.0.d0 .and. dismaxst.lt.20100.d0) then
               write(11,'(/''Location for observations at distances '',
     +               ''between'',f8.1,'' and'',f8.1,'' [km]'')') 
     +               disminst,dismaxst
            endif
         else
            if(disminst.gt.0.d0 .and. dismaxst.ge.180.d0) then
               write(11,'(/''Location for observations at distances '',
     +               ''larger than'',f6.1,'' [deg]'')') disminst
            endif
            if(disminst.le.0.d0 .and. dismaxst.lt.180.d0) then
               write(11,'(/''Location for observations at distances '',
     +               ''shorter than'',f6.1,'' [deg]'')') dismaxst
            endif
            if(disminst.gt.0.d0 .and. dismaxst.lt.180.d0) then
               write(11,'(/''Location for observations at distances '',
     +               ''between'',f6.1,'' and'',f6.1,'' [deg]'')') 
     +               disminst,dismaxst
            endif
         endif

         write(11,'(/''Source time  :'',i5,4i3.2,f7.3,'' +/- '',
     +           f8.3,'' [s]'')')  yy,mon,dd,hh,mi,sec,var2(1)
         write(11,'(''        or'',12x,f16.3,'' +/- '',f8.3,
     +           '' [s]'')') tome,var2(1)


         isec1 = nint(sec*1000)
         isec  = isec1/1000
         msec  = isec1-isec*1000
         write(11,'(''        or'',7x,i4,''-'',i3.3,'':'',
     +         3(i2.2,''.''),i3.3,'' +/- '',f8.3,'' [s]''/)') 
     +         yy,idoy,hh,mi,isec,msec,var2(1)
         
      endif

      if(json_out) then
        call json_add_double("confidence_level", confl*1.d-2, json_rc)
      endif

      if(var2(1).gt.zo/6.d0 .and. czo.eq.'D') ibad = ibad + 1

      if(sdlatg.lt.45.d0) then
         if(output) then
            write(11,'(''Epicenter lat:'',14x,f10.4,'' +/- '',f8.4,
     +              '' [deg]'')')  elatmg,sdlatg
         endif
      else
         ibad = ibad + 1
         if(sdlatg.gt.180.d0) sdlatg = 180.d0
         if(output) then
            write(11,'(''Epicenter lat:'',14x,f10.4,'' +/- '',f8.4,
     +              '' [deg] (no resolution!)'')') elatmg,sdlatg
         endif
      endif

      if(var2(3).lt.90.d0) then
         if(output) then
            write(11,'(''Epicenter lon:'',14x,f10.4,'' +/- '',f8.4,
     +              '' [deg]'')')  elonm,var2(3)
         endif
      else
         if(var2(3).gt.180.d0) var2(3) = 180.d0
         if(elatmg+sdlatg.gt.89.d0 .or. elatmg-sdlatg.lt.-89.d0) then
           if(output) then
              write(11,'(''Epicenter lon:'',14x,f10.4,'' +/- '',f8.4,
     +              '' [deg] (not well defined, source too close '',
     +              ''to the Pole!)'')') elonm,var2(3)
           endif
         else
           ibad = ibad + 1
           if(output) then
              write(11,'(''Epicenter lon:'',14x,f10.4,'' +/- '',f8.4,
     +              '' [deg] (no resolution!)'')') elonm,var2(3)
           endif
         endif
      endif

      if(czo.eq.'D') then

         if((var2(4).ge.zo       .and. zo.ge. 50.d0) .or. 
     +      (var2(4).ge..75d0*zo .and. zo.gt.300.d0) .or.
     +      (var2(4).gt.660.d0)                            ) then

            ibad = ibad + 1

            if(var2(4).gt.660.d0) var2(4) = 660.d0

            if(output) then
               write(11,'(''Source depth :'',15x,f7.2,''   +/- '',f6.2,
     +               ''   [km] (no resolution!)''/)') zo,var2(4)
            endif

         else

            if(output) then
               write(11,'(''Source depth :'',15x,f7.2,''   +/- '',
     +               f6.2,''   [km]''/)') zo,var2(4)
            endif

         endif

      else if(czo.eq.'F' .or. czo.eq.'B') then

         if(output) then
            if(.not.ldefisc) then
               if(lcomfix) then
                  write(11,'(''Source depth :'',15x,f7.2,''   [km] '',
     +                    ''Fixed by HYPOSAT''/)') zo
               else
                  if(lwater) then
                     write(11,'(''Source depth :'',15x,f7.2,''   [km]'',
     +                  '' Fixed below water level of'',f7.0,'' m''/)')
     +                    zo,zwater
                  else
                     write(11,'(''Source depth :'',15x,f7.2,''   [km]'',
     +                  '' Fixed by input''/)') zo
                  endif
               endif
            else
               if(idetyp.eq.1) then
                  write(11,'(''Source depth :'',15x,f7.2,''   [km] '',
     +                    ''Fixed at ISC default depth grid''/)') zo
               else if(idetyp.eq.2) then

                  if(c1typ.eq.'uc') then
                     write(11,'(''Source depth :'',15x,f7.2,''   [km] ''
     +                     ,''Depth fixed at 1/4 of CRUST 1.0 Moho '',
     +                     ''depth''/)') zo
                  else if(c1typ.eq.'mc') then
                     write(11,'(''Source depth :'',15x,f7.2,''   [km] ''
     +                     ,''Depth fixed at 1/2 of CRUST 1.0 Moho '',
     +                     ''depth''/)') zo
                  else if(c1typ.eq.'lc') then
                     write(11,'(''Source depth :'',15x,f7.2,''   [km] ''
     +                     ,''Depth fixed at 3/4 of CRUST 1.0 Moho '',
     +                     ''depth''/)') zo
                  else if(c1typ.eq.'mo') then
                     write(11,'(''Source depth :'',15x,f7.2,''   [km] ''
     +                     ,''Depth fixed at CRUST 1.0 Moho '',
     +                     ''depth''/)') zo
                  endif

               else if(idetyp.eq.3) then
                  write(11,'(''Source depth :'',15x,f7.2,''   [km] '',
     +                    ''Fixed at FE-region default depth''/)') zo
               else 
                  write(11,'(''Source depth :'',15x,f7.2,''   [km] '',
     +                    ''Fixed by HYPOSAT''/)') zo
               endif
            endif
         endif
      endif

c
c     let us now calculate the final residuals and print them out
c
405   stmean    = 0.d0
      strmean   = 0.d0
      samean    = 0.d0
      sarmean   = 0.d0
      rmsazi    = 0.d0
      spmean    = 0.d0
      sprmean   = 0.d0
      rmsp      = 0.d0
      rms       = 0.d0
      rmsisc    = 0.d0
      wisc      = 0.d0

      loctts    = 0

c
c     misfit parameters for all input data!
c
      tmisf     = 0.d0
      tmisfl    = 0.d0
      ntmisf    = 0
      dmisf     = 0.d0
      dmisfl    = 0.d0
      ndmisf    = 0
      amisf     = 0.d0
      amisfl    = 0.d0
      namisf    = 0
      pmisf     = 0.d0
      pmisfl    = 0.d0
      npmisf    = 0
      wmisf     = 0.d0
      wmisfl    = 0.d0
      nwmisf    = 0

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

      i3 = 1

      do 450 i = 1,nobs

      epiaz(i) = -999.0
      epiaz2(i) = -999.0
      emeran(i) = -999.d0

      dtmin     = 9999.d0
      dtnew2    = 9999.d0

      if(phase(i).eq.'P1      ' .or. phase(i).eq.'S1      ') then
         firston= .true.
      else
         firston= .false.
      endif

c     print *,'---> (e--0) ', i,sta(iev(i)),phase(i),phaseu(i),used(i),
c    +        firston,touse(i)

      if(phaseu(i).eq.'P1      ' .or. phaseu(i).eq.'S1      ' .or.
     +   (phaseu(i).eq.' '.and. touse(i)(1:1).ne.' ')  )
     +    phaseu(i) = 'x'

      if(phase(i)(1:3).eq.'tx ') then
         phaseu(i)    = 'x'
         used(i)(1:1) = ' '
         used(i)(3:5) = '   '
      endif

      chgcas = uppcas(phaseu(i)(1:1))
      cdum = chgcas(1:1)

      if((cdum.ne.'P' .and. cdum.ne.'S' .and. cdum.ne.'R' .and.
     +    cdum.ne.'L' ) .or. used(i)(1:1).eq.' ') go to 413

c
c     Mark all phases, which have been used as part of a defining
c     travel-time difference measure.
c
      do 412 j = i+1,nobs

         if(iev(i).ne.iev(j)) go to 412

         chgcas = uppcas(phaseu(j)(1:1))
         cdum = chgcas(1:1)
         if((cdum.ne.'P' .and. cdum.ne.'S' .and. cdum.ne.'R' .and.
     +       cdum.ne.'L' ) .or. used(j)(1:1).eq.' ') go to 412

         if(phaseu(i).eq.phaseu(j)) go to 412
         if(idtu(i3).eq.(j*i + j+i)) then
            used(i)(4:4) = 'D'
            used(j)(4:4) = 'D'
            i3 = i3 + 1
         endif
412   continue

413   useds = used(i)
      usedm = ' '
      usedsr = ' '

      if(sta(iev(i)).ne.stato) then

         istad(iev(i)) = 0

         stato = sta(iev(i))

         call depi(stala(iev(i)),stalo(iev(i)),elatmg,elonm,
     +              del(iev(i)),delk(iev(i)),azie(iev(i)),baz(iev(i)),
     +        d2km)
         rzo   = sngl(zo)
         delta = del(iev(i))
         rdel  = sngl(delta)
         rdelk = sngl(delk(iev(i)))

         if(rdel.lt.rdmi) rdmi = rdel
         if(rdel.gt.rdma) rdma = rdel

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

      llimdel = .false.

      if(kmout) then
         if((delk(iev(i)) .gt. dismaxst+10.d0) .or.
     +      (delk(iev(i)) .lt. disminst-10.d0) ) then
            if(.not.ldist) go to 450
            llimdel = .true.
         endif
      else
         if((delta .gt. dismaxst+1.d0) .or.
     +      (delta .lt. disminst-1.d0) ) then
            if(.not.ldist) go to 450
            llimdel = .true.
         endif
      endif

      epiaz2(iev(i)) = razi

      istad(iev(i)) = 1

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
         phsearch = 'L'
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
      if(phsearch.ne.' ' .and. phsearch.ne.phase_t) go to 420

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

         if(single .and. rayokf) then

c
c     too big ray parameter residual for single array observation
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
            if(phase_t.eq.'S' .and. phid1(1:1).eq.'S' .and. .not. 
     +         first2) then
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
            nstref = nstref + 1 
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
                  if(dabs(dtt2).lt.dabs(ttres)) then
                     if(iwl.eq.2) then
                        wflag = .true.
                     else if(iwl.eq.3) then
                        if(dabs(fdt).ge.dtdw ) wflag = .true.
                     else if(iwl.eq.4) then
                        wflag = .true.
                        if(dabs(fdt).ge.dtdw)  wflag = .false.
                     else if(iwl.eq.5) then
                        if(index(phase(i),'w').gt.0) wflag=.true.
                     endif
                  endif
               endif
            endif

            if(wflag) then
              delr   = delr2
              trefl  = trefl2
              tttn   = tttn + fdt
              phid1  = phasw(phid1)
              ttres  = dtt2
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

         dtnew = dabs(tttn-tome-ttt(i)+tom)

         if(typctl.gt.8) then
           print *,'phid1, i, tttn, t0, j, phase, TT, ECOR, Height,',
     +             ' Crust, Refl, Stat, Tobs, ttres'
           print
     +     *,phid1,i,tttn,tome,j,phcd(j),ttc(j),ecor,th,tcrust,trefl,
     +       statict,ttobs, ttres, dtmin, usedr,rdel,phase(i)
         endif

         ttres1 = dabs(ttc(j) - ttc(1))
         if(rdel.gt.113. .and. phcd(1)(1:3).eq.'Pdi' .and.
     +                         ttres1.gt.50.d0        ) then
           if(phcd(2)(1:2).eq.'PK' .and. j.ge.2) then
              ttres1 = dabs(ttc(j) - ttc(2))
              go to 419
           else if(phcd(3)(1:2).eq.'PK' .and. j.ge.3) then
              ttres1 = dabs(ttc(j) - ttc(3))
              go to 419
           else if(phcd(4)(1:2).eq.'PK' .and. j.ge.4) then
              ttres1 = dabs(ttc(j) - ttc(4))
           endif
         endif

419      if(dabs(ttres).lt.dabs(dtmin)) then
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

         if(useds(1:1).eq.'2' .or. useds(1:1).eq.'3') then
            do 4191  j2 = j+1,nphass
               if(phid .eq. phcd(j2)) then
                 j = j2 - 1
                 go to 420
               endif
4191        continue
         endif

         if(imin.eq.0 ) go to 422

      endif

420   continue

421   if(icha.ge.99 .or.imin.gt.0) go to 422

      if((dabs(dtmin).gt.999.d0 .or. dtnew2.ge.1.d0) .and.
     +   (usedm(1:1).ne.' ' .or. usedm(4:4).ne.' ') .and. 
     +    .not.llimdel ) then
         phid0 = phid
         call testphase (phid0,icha,delta)
         if(phid0.ne.phid .and. icha.lt.99) then
            phid = phid0
            go to 418
         endif
      endif

422   if(dabs(dtmin).gt.1.d0 .and. usedm(1:1).eq.' ' .and. 
     +   usedm(3:3).eq.' ' .and. imin.eq.0) then
         if((surfm .and. phid(1:2).ne.'Lg') .or. llimdel) go to 423
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

      if(useds(1:1).eq.'T' .or. useds(1:1).eq.'2') then
         stmean  = stmean + ttres
         strmean = strmean + dabs(ttres)
         rms     = rms    + q2(ttres)
         rmsisc  = rmsisc + q2(ttres)/ttu(i)
         wisc    = wisc   + 1.d0/ttu(i)
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
         print *,'i,ttt,ttobs,ttres,ecor,th,tcrust,used,tts,tt2,ttu'
         print *,i,tttr(i),ttobs,ttres,ecor,th,tcrust,useds,tts(i),
     +          tt2(i),ttu(i),emeran(i)
      endif

      if(useds(1:1).eq.'t' .or. useds(1:1).eq.'3') useds(1:1) = ' '

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

      if(.not.llimdel) then
         if(touse(i)(1:1).eq.'T' .and. dabs(ttres).lt.999.d0) then
            fmis    = ttres/ttu(i)
            tmisfl  = tmisfl + dabs(fmis)
            tmisf   = tmisf + q2(fmis)
            ntmisf  = ntmisf + 1
         endif

         if(touse(i)(3:3).eq.'S' .and. pares.gt.-99.d0 ) then
            fmis    = pares/ps(i)
            pmisfl  = pmisfl + dabs(fmis)
            pmisf   = pmisf + q2(fmis)
            npmisf  = npmisf + 1
         endif

         if(touse(i)(2:2).eq.'A' ) then
            fmis    = azires/azis(i)
            amisfl  = amisfl + dabs(fmis)
            amisf   = amisf + q2(fmis)
            namisf  = namisf + 1
         endif
      endif

      cmod = ' '
      read(touse(i)(7:7),'(i1)') modind

      if(modind.gt.1 .and. modflag(modind)) write(cmod,'(i1)') modind

      do 432 iu = 1,4
      if(useds(iu:iu).eq.' ' .and. touse(i)(iu:iu).eq.'m') then
         useds(iu:iu) = 'm'
      endif
432   continue

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

      onscha = onflag(i)(3:3)

      if(unc_out .and. (useds(1:1).ne.' ' .or. useds(4:4).ne.' ')) then
         onscha = '_'
         if(phase_type(phid1).eq.'P') then
            if(ttu(i).le.sisfo) onscha = 'Q'
            if(ttu(i).le.sisfe) onscha = 'E'
            if(ttu(i).le.sisfi) onscha = 'I'
         else if(phase_type(phid1).eq.'S' .or. phid1(1:3).eq.'Rg ') then
            if(ttu(i).le.sisfo*2.d0) onscha = 'Q'
            if(ttu(i).le.sisfe*2.d0) onscha = 'E'
            if(ttu(i).le.sisfi*2.d0) onscha = 'I'
         endif
         onflag(i)(3:3) = onscha
      endif

      text(i)(131:131) = onflag(i)(3:3)

      istmag(i) = -9
      stamag(i) = -10.d0

      if(amplit(i).gt.0.d0) then

         namp = namp + 1

         if(magflag .and. touse(i)(6:6).eq.'M') then

            io = 0

            call get_stat_mag(phid1,amplit(i),period(i),delta,
     +        delk(iev(i)),zo,magtyps,magtypml,magtypp,magmlfile,dmag,
     +        statmag,typctl)

            if(statmag.eq. 'MS') then

               if(delta.ge.delmsmin .and. delta.le.delmsmax) then
                  imsm = imsm + 1
                  if(dmag.gt.stams(iev(i))) stams(iev(i)) = dmag
                  stamag(i) = dmag
                  istmag(i) = 1
                  io = 1
               endif
               go to 448
            endif

            if(statmag.eq. 'ML') then

               if(dabs(tres).lt.60.d0 ) then
                  if(delta.ge.delmlmin .and. delta.le.delmlmax) then
                     imlm = imlm + 1
                     if(dmag.gt.staml(iev(i))) staml(iev(i)) = dmag
                     stamag(i) = dmag
                     istmag(i) = 2
                     io = 1
                  endif
               endif
               go to 448
            endif

            if(statmag.eq. 'mb') then

               if(ttres1.lt.9.d0 .and. dabs(ttres).le.8.d0 ) then
                  if(delta.ge.delmbmin .and. delta.le.delmbmax ) then
                     imbm = imbm + 1
                     if(dmag.gt.stamb(iev(i))) stamb(iev(i)) = dmag
                     stamag(i) = dmag
                     istmag(i) = 3
                     io = 1
                  endif
               endif
            endif

448         if(dmag.gt.-9.99d0 .and. io.gt.0) then
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
         rmsisc  = dsqrt(rmsisc/wisc)
      endif 

      if(dabs(stmean).gt.0.025d0 .and. mttres .and. itso.lt.2) then

         if(output) then
            write(11,'(''Source time corrected for mean '', 
     +           ''travel-time residual ( '',f9.3,'' [s])'')') stmean
         endif

         if(dabs(stmean).gt.var(1) .and. itso.lt.1) then
            
            miteras  = miteras + iter
            iter     = 0
            nextiter = 0
            dchang   = dchang0
            iteraz   = 0
            itso     = itso + 1

            in  = 0

            dtp = dtp0
            dts = dts0

            dtm0 = dtm2
            dtm = dtm0

            rmso   = 9999.d0
            rmsold  = 9999.d0
            datmax0 = 9999.d0

            dazim1 = dazim0*2.0d00

            tom  = tom  + stmean
            tome = tom  + timemin

            rs(1) = dpythag(var(1),stmean)
            rs(2) = var(2)
            rs(3) = var(3)
            rs(4) = var(4)

            go to 100

         endif

         itso     = itso + 1

         tome = tome + stmean
         
         call fetoh2(tome,idum,yy,mon,mm,dd,idoy,hh,mi,sec)

         if(output) then

            write(11,'(/''Source time  :'',i5,4i3.2,f7.3,'' +/- '',
     +              f8.3,'' [s]'')')  yy,mon,dd,hh,mi,sec,var2(1)
            write(11,'(''        or'',12x,f16.3,'' +/- '',f8.3,
     +              '' [s]'')') tome,var2(1)

            isec1 = nint(sec*1000)
            isec  = isec1/1000
            msec  = isec1-isec*1000
            write(11,'(''        or'',7x,i4,''-'',i3.3,'':'',
     +            3(i2.2,''.''),i3.3,'' +/- '',f8.3,'' [s]''/)') 
     +            yy,idoy,hh,mi,isec,msec,var2(1)

         endif

         go to 405

      endif

      if(ibad.ge.3 .and. .not.last) go to 455

      if(ibad.ge.3) then
         ibad0 = ibad0 + 1
      else if(ibad.eq.0) then
         ibad0 = 0
      endif

      if(nq.lt.3 .and.single) iellip =.false.

      earea = 9.99d6
      if(iellip .and. .not.last) then

         call ellcal(elatmg,ax1,ax2,eps,fchi2,elmax,elmin,eazi,earea)

c            print *,iellip,single,iellip1,nq,elatmg,ax1,ax2,fchi2,
c    +       elmax,elmin,eazi,earea,var(2),var(3)

         if(output) then

            if(earea.lt.10000.d0) then
              write(11,'( ''Epicenter uncertainty ellipse:''/
     +           ''Major half axis: '',f8.2,
     +           '' [km]  Minor half axis: '',f8.2,
     +           '' [km]'',/''Azimuth:'',f11.1,''  [deg] '',
     +           ''Area: '',f14.2,'' [km^2]''/)')
     +           elmax,elmin,eazi,earea
            else
              write(11,'( ''Epicenter uncertainty ellipse:''/
     +           ''Major half axis: '',f8.0,
     +           '' [km]  Minor half axis: '',f8.0,
     +           '' [km]'',/''Azimuth:'',f11.1,''  [deg] '',
     +           ''Area: '',1p e14.5,'' [km^2]''/)')
     +           elmax,elmin,eazi,earea
            endif

         endif

         if(json_out) then
           call json_start_dict_group("uncertainty_ellipse", json_rc)
           call json_add_double("major_half_axis", dble(elmax), json_rc)
           call json_add_double("minor_half_axis", dble(elmin), json_rc)
           call json_add_double("azimuth", dble(eazi), json_rc)
           call json_add_double("area", dble(earea), json_rc)
           call json_end_group(json_rc)
         endif

c        radsource = radloc(elatmg,1)
c        print *,'Earth radius in source region: ',radsource

      else if(iellipi.eq.1 .and. .not.iellip) then

         if(output .and. .not.single) then

            write(11,'( ''Epicenter uncertainty ellipse calculation '',
     +         ''not possible (too less parameter resolution)''/)')

         endif

      endif

      rlat = sngl(elatmg)
      rlon = sngl(elonm)
      call hyposat_geo( rlat,rlon, isreg, regnum, region , ierr )

      if(output) then
         write(11,'(''Flinn-Engdahl Region ('',i4,'' ): '',a/)')
     +            isreg, trim(region)
      endif

      if(json_out) then
        call json_start_dict_group("flinn_engdahl_region", json_rc)
        call json_add_int("number", isreg, json_rc)
        call json_add_string("name", trim(region), json_rc)
        call json_end_group(json_rc)
      endif

c
c     We will now calculate the maximum azimuthal gaps for 
c        - all as defining used observations
c
c     or
c
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

         if(.not.gapobs) then

           call indexx(nobs,epiaz,indx)
           call azigap(epiaz,dazgap,d1azi,d2azi,dazgap2,d1azi2,
     +                 d2azi2,cpq,cpq2,nobs,indx,mread)

         else

           call indexx(nobs,epiaz2,indx)
           call azigap(epiaz2,dazgap,d1azi,d2azi,dazgap2,d1azi2,
     +                 d2azi2,cpq,cpq2,nobs,indx,mread)

         endif

      endif

c     now we count the number of stations available in the chosen 
c     distance range

      nstata = 0
      do 451 i = 1, nstat
        nstata = nstata + istad(i)
451   continue

c
c     if chosen: open the 'new' input file
c
      if(new_in) then

         inputfilen = trim(inputfile) // '_rev'
         open (unit=31,file=trim(inputfilen))
         write (31,'(a)') trim(title)

      endif

c
c     if chosen: open the ISF formatted output file
c
      if(isf_out) then

        open (unit=12,file=isf_file)

        cdum = '_'
        cdum2 = 'BULLETIN'
        dformat = 'IMS1.0'
        dsformat = 'short'
        itest = write_data_type(12,cdum2,cdum,dformat,dsformat)

        cdum2 = ' '


        if(corid.eq.' ' .or. corid.eq.'_') then

          corid = '99999999'
c
c       no hypocenter id was given. We use the eventid reduced to 8
c       characters
c
          lc = len_trim(cevid)
          if(lc.gt.8) then
             corid = cevid(lc-7:lc)
          else if(lc.gt.0) then
             corid = cevid(1:lc)
          endif

        endif

        lc = len_trim(corid)
        if(lc.lt.8) then
           cdum2 = corid
           corid = ' '
           corid(8-lc+1:8) = cdum2(1:lc)
        endif

c       cdum = ' '
c       write(12,'(a)') cdum

        itest = write_event_id(12,cevid,region)

        if(itest.eq.20) then
           itest = write_isf_error(typctl)
        endif

        itest = write_origin_head(12)

        call fetoh2(tome,idum,yy,mon,mm,dd,idoy,hh,mi,sec)

        isec1 = nint(sec*1000)
        isec  = isec1/1000
        msec  = isec1-isec*1000

        relmax = real(elmax)
        if(relmax.lt.0.) relmax = 0.
        if(relmax.gt.9999.) relmax = 9999.

        relmin = real(elmin)
        if(relmin.lt.0.) relmin = 0.
        if(relmin.gt.9999.) relmin = 9999.

        ieazi = nint(eazi)
        if(ieazi.lt.0) ieazi = 0

        if(var2(1).gt.999.9d0) var2(1) = 999.9d0

        rmsisf = rms
        if(lrmsisc) rmsisf = rmsisc

        cduma = ' '
        if(lpick) cduma = 'a'
        cdumi = 'i'
        cdum3 = '  '
        cdum = ' '
        rdum = REAL(ISF_NULL)

        if(czo.eq.'D') then

           itest = write_origin(12,yy,mon,dd,hh,mi,isec,msec,cdum,
     +     real(var2(1)),real(rmsisf),real(elatmg),real(elonm),cdum,
     +     relmax,relmin,ieazi,real(zo),cdum,
     +     real(var2(4)),in,nstata,nint(dazgap),rdmi,rdma,cduma,cdumi,
     +     cdum3,author,corid)
 
           if(itest.eq.20) then
              itest = write_isf_error(typctl)
           endif

        else if(czo.eq.'F' .or. czo.eq.'B') then

           cfi = 'f'
           itest = write_origin(12,yy,mon,dd,hh,mi,isec,msec,cdum,
     +     real(var2(1)),real(rmsisf),real(elatmg),real(elonm),cdum,
     +     relmax,relmin,ieazi,real(zo),cfi,rdum,
     +     in,nstata,nint(dazgap),rdmi,rdma,cduma,cdumi,
     +     cdum3,author,corid)

           if(itest.eq.20) then
              itest = write_isf_error(typctl)
           endif

           if(ldefisc .and. .not.lcomfix) then
              if(idetyp.eq.1) then
                write(outputisf,'(''Depth fixed at ISC default depth'',
     +              '' grid'')') 
              else if(idetyp.eq.2) then

                if(c1typ.eq.'uc') then
                   write(outputisf,'(
     +                ''Depth fixed at 1/4 of CRUST 1.0 Moho depth'')')
                else if(c1typ.eq.'mc') then
                   write(outputisf,'(
     +                ''Depth fixed at 1/2 of CRUST 1.0 Moho depth'')')
                else if(c1typ.eq.'lc') then
                   write(outputisf,'(
     +                ''Depth fixed at 3/4 of CRUST 1.0 Moho depth'')')
                else if(c1typ.eq.'mo') then
                   write(outputisf,'(
     +                ''Depth fixed at CRUST 1.0 Moho depth'')')
                endif

              else if(idetyp.eq.3) then
                write(outputisf,'(''Depth fixed at default FE-region'',
     +              '' depth'')') 
              endif
           else if(lcomfix .and. .not.ldefisc) then
                write(outputisf,'(''Depth fixed by HYPOSAT'')') 
           else if(.not.lcomfix .and. .not.ldefisc) then
                if(lwater) then
                   write(outputisf,
     +               '(''Depth fixed by input below water level'')') 
                else
                   write(outputisf,'(''Depth fixed by input'')') 
                endif
           endif
           itest = write_comment(12,trim(outputisf))
        endif

        write(outputisf,'(''Confidence level of given uncertainties:'',
     +         f7.2,'' %'')') confl
        itest = write_comment(12,trim(outputisf))
        if(itest.eq.20) then
           itest = write_isf_error(typctl)
        endif

        write(outputisf,'(''CPQ:'',f6.3)') cpq
        itest = write_comment(12,trim(outputisf))
        if(itest.eq.20) then
           itest = write_isf_error(typctl)
        endif

        if(stcorfl .and. istcor.gt.0) then
           itest = write_comment(12,trim(textouts))
           if(itest.eq.20) then
              itest = write_isf_error(typctl)
           endif
        endif

        if(diffflag .and. ndt.gt.0) then
          if(ndt.eq.1) then
             write(outputisf,'(i4,'' Travel-time difference used as '',
     +         ''defining'')') ndt
          else
             write(outputisf,'(i4,'' Travel-time differences used as '',
     +         ''defining'')') ndt
          endif
          itest = write_comment(12,trim(outputisf))
          if(itest.eq.20) then
             itest = write_isf_error(typctl)
          endif

        endif

        if(iloc) then
           if( (modflag(2) .or. modflag(3) .or. modflag(4)) .and.
     +         (imodn(2).gt.1 .or. imodn(3).gt.1 .or. imodn(4).gt.1)
     +          ) then

               write(outputisf,'(''First reference models  : '',a,
     +               '' and '',a)') trim(filloc),modnam(1)
               itest = write_comment(12,outputisf)

               if(modflag(2) .and. imodn(2).gt.1) then
                 if(.not.isf_in) then
                   write(outputisf,'(''Second reference model  : '',a)')
     +                   modnam(2)
                 else
                   write(outputisf,'(''Second reference model  : '',a,
     +                   '' from'',f7.2,'' deg'')')
     +                  modnam(2),disfmod
                 endif
                 itest = write_comment(12,outputisf)
               endif

               if(modflag(3) .and. imodn(3).gt.1) then
                  write(outputisf,'(''Third reference model   : '',a)')
     +                 modnam(3)
                  itest = write_comment(12,outputisf)
               endif

               if(modflag(4) .and. imodn(4).gt.1) then
                  write(outputisf,'(''Fourth reference model  : '',a)')
     +                 modnam(4)
                  itest = write_comment(12,outputisf)
               endif
           else
               write(outputisf,'(''Reference models  : '',a,'' and '',
     +               a)') trim(filloc),modnam(1)
               itest = write_comment(12,outputisf)
           endif
        else
           if( (modflag(2) .or.  modflag(3) .or. modflag(4)) .and.
     +         (imodn(2).gt.1 .or. imodn(3).gt.1 .or. imodn(4).gt.1)
     +          ) then

               write(outputisf,'(''First reference model   : '',a)') 
     +               modnam(1)
               itest = write_comment(12,outputisf)

               if(modflag(2) .and. imodn(2).gt.1) then
                 if(.not.isf_in) then
                   write(outputisf,'(''Second reference model  : '',a)')
     +                   modnam(2)
                 else
                   write(outputisf,'(''Second reference model  : '',a,
     +                   '' from'',f7.2,'' deg'')')
     +                  modnam(2),disfmod
                 endif
                 itest = write_comment(12,outputisf)
               endif

               if(modflag(3) .and. imodn(3).gt.1) then
                  write(outputisf,'(''Third reference model   : '',a)')
     +                 modnam(3)
                  itest = write_comment(12,outputisf)
               endif

               if(modflag(4) .and. imodn(4).gt.1) then
                  write(outputisf,'(''Fourth reference model  : '',a)')
     +                 modnam(4)
                  itest = write_comment(12,outputisf)
               endif
           else
               write(outputisf,'(''Reference model   : '',a)') modnam(1)
               itest = write_comment(12,outputisf)
           endif
        endif

      endif

      if(itest.eq.20) then
         itest = write_isf_error(typctl)
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
           imsm = 0
           dmsm = 0.d0
           sdms = 0.d0
           if(.not.lmaxm) then
              do 4511 im = 1,nstat
                 if(stams(im).gt.-9.9d0) then
                    imsm = imsm + 1
                    smagn(im) = stams(im)
                 endif
4511          continue
           else
              do 4512 im = 1,nobs
                 if(stamag(im).gt.-9.9d0 .and. istmag(im).eq.1) then
                    imsm = imsm + 1
                    smagn(imsm) = stamag(im)
                 endif
4512          continue
           endif
           if(imsm.gt.0) call get_netmag(smagn,imsm,dmsm,sdms)
        endif

        if(imlm.gt.0) then
           imlm = 0
           dmlm = 0.d0
           sdml = 0.d0
           if(.not.lmaxm) then
              do 4513 im = 1,nstat
                 if(staml(im).gt.-9.9d0) then
                    imlm = imlm + 1
                    smagn(imlm) = staml(im)
                 endif
4513          continue
           else
              do 4514 im = 1,nobs
                 if(stamag(im).gt.-9.9d0 .and. istmag(im).eq.2) then
                    imlm = imlm + 1
                    smagn(imlm) = stamag(im)
                 endif
4514          continue
           endif
           if(imlm.gt.0) call get_netmag(smagn,imlm,dmlm,sdml)
        endif

        if(imbm.gt.0) then
           imbm = 0
           dmbm = 0.d0
           sdmb = 0.d0
           if(.not.lmaxm) then
              do 4515 im = 1,nstat
                 if(stamb(im).gt.-9.9d0) then
                    imbm = imbm + 1
                    smagn(imbm) = stamb(im)
                 endif
4515          continue
           else
              do 4516 im = 1,nobs
                 if(stamag(im).gt.-9.9d0 .and. istmag(im).eq.3) then
                    imbm = imbm + 1
                    smagn(imbm) = stamag(im)
                 endif
4516          continue
           endif
           if(imbm.gt.0) call get_netmag(smagn,imbm,dmbm,sdmb)
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

        if(json_out) then
          call json_start_list_group("magnitudes", json_rc)
          if(imsm.gt.0) then
            call json_start_dict_group("magnitude", json_rc)
            call json_add_string("type", "Ms", json_rc)
            call json_add_double("value", dble(dmsm), json_rc)
            call json_add_double("magnitude_uncertainty",sdms,json_rc)
            call json_add_int("magnitude_number_of_observations",
     +           imsm,json_rc)
            call json_add_string("model", trim(magtyps), json_rc)
            call json_end_group(json_rc)
          endif
          if(imlm.gt.0) then
            call json_start_dict_group("magnitude", json_rc)
            call json_add_string("type", "ML", json_rc)
            call json_add_double("value", dble(dmlm), json_rc)
            call json_add_double("magnitude_uncertainty",sdml,json_rc)
            call json_add_int("magnitude_number_of_observations",
     +           imlm,json_rc)
            call json_add_string("model", trim(magtypml), json_rc)
            call json_end_group(json_rc)
           endif
           if(imbm.gt.0) then
             call json_start_dict_group("magnitude", json_rc)
             call json_add_string("type", "mb", json_rc)
             call json_add_double("value", dble(dmbm), json_rc)
             call json_add_double("magnitude_uncertainty",sdmb,json_rc)
             call json_add_int("magnitude_number_of_observations",
     +            imbm,json_rc)
             call json_add_string("model", trim(magtypp), json_rc)
             call json_end_group(json_rc)
           endif
           call json_end_group(json_rc)
        endif

      endif

      write (11,'(a/)') trim(texth)

      cdum = ' '

      if(isf_out .and. (imsm.gt.0 .or. imbm.gt.0 .or. imlm.gt.0)) then

        itest = write_netmag_head(12)

        if(imbm.gt.0) then
           itest = write_netmag(12,'mb', cdum, real(dmbm), real(sdmb), 
     +             imbm, trim(magtypp),corid)
        endif

        if(imsm.gt.0) then
           itest = write_netmag(12,'MS', cdum, real(dmsm), real(sdms), 
     +             imsm, trim(magtyps),corid)
        endif

        if(imlm.gt.0) then
           itest = write_netmag(12,'Ml', cdum, real(dmlm), real(sdml), 
     +             imlm, trim(magtypml),corid)
        endif

        if(itest.eq.20) then
           itest = write_isf_error(typctl)
        endif

      endif

      if(isf_out) then
        
        itest = write_phase_head(12)

      endif

      if(json_out) call json_start_list_group("arrivals", json_rc)

      do 453 i=1,nobs

      if(new_in) then

c        print *,i,touse(i),' , ',useds,text(i)
         if(touse(i)(1:1).eq.'T' .or. touse(i)(2:2).eq.'A' .or.
     +      touse(i)(3:3).eq.'S' .or. touse(i)(4:4).eq.'D'   ) then

            string = ' '

            string(1:5) = text(i)(1:5)
            string(7:14) = text(i)(22:29)
            if(text(i)(30:37).ne.'        ') 
     +         string(7:14) = text(i)(30:37)

            string(71:77) = touse(i)(1:7)

            if(touse(i)(1:1).eq.'m') then
               chgcas = lowcas(string(7:14))
               string(7:14) = chgcas(1:8)
               string(71:71) = 'x'
            endif

            ttobs  = timemin + tt(i)

            call fetoh2(ttobs,idum,yy,mon,mm,dd,idoy,hh,mi,sec)

            write(string(16:44),'(i4,4(1x,i2),1x,f6.3,1x,f5.3)') 
     +            yy,mon,dd,hh,mi,sec,ttu(i)

            if(azi(i).ge.0.d0 .and. touse(i)(2:2).ne.'m') then
               write(string(46:57),'(f6.2,1x,f5.2)') azi(i),azis(i)
               string(72:72) = 'A'
            else if(touse(i)(2:2).eq.'m') then
               string(72:72) = 'x'
            else
               string(72:72) = '_'
            endif

            if(p(i).ge.0.d0 .and. touse(i)(3:3).ne.'m') then
               write(string(59:69),'(f5.2,1x,f5.2)') p(i),ps(i)
               string(73:73) = 'S'
            else if(touse(i)(3:3).eq.'m') then
               string(73:73) = 'x'
            else
               string(73:73) = '_'
            endif

            if(amplit(i).gt.0.d0 .and. period(i).gt.0.d0) then
               write(string(79:97),'(f6.3,1x,f12.2)')period(i),amplit(i)
            endif

            if(snr(i).gt.0.d0) then
               write(string(99:105),'(f7.2)') snr(i)
            else
               string(99:105) = '       '
            endif

            if(arid(i).ne.'        ') then
               write(string(106:114),'(1x,a8)') arid(i)
            endif

            if((dabs(tt2(i)-tts(i)).gt.0.001d0) .and. itresw.eq.1) then
               write(string(115:120),'(1x,f5.2)') tt2(i)
            endif

            write(31,'(a)') trim(string)

         endif

      endif
      
      if(sta(iev(indx(i))).ne.stato.or.i.eq.nobs) then

        ntext0 = ntext

        if(i.eq.nobs) then 

          ntext        = ntext + 1
          arr(ntext)   = sngl(tt(indx(i)))
          text2(ntext) = text(indx(i))
          comm2(ntext) = comment(indx(i))
          tt3(ntext)   = timemin + tt(indx(i))

          ntext0       = ntext
          if(sta(iev(indx(i))).ne.stato) ntext0 = ntext - 1

        endif

        call indexx(ntext0,arr,indx2)

        do 452 j=1,ntext0
        textout = trim(text2(indx2(j)))
        if(textout(1:5).ne.'     ') then

           if(output) write(11,'(a)') textout(1:lenons)
           if(isf_out) call isf_out_line(12,onflag(indx2(j)),textout,
     +        comm2(indx2(j)))
           if(json_out) call json_out_line(textout,comm2(indx2(j)),
     +        tt3(indx2(j)),onflag(indx(i)),json_rc)

        endif
452     continue

        if(i.eq.nobs) then
           if(ntext0.eq.ntext-1) then
             textout = trim(text(indx(i)))
             if(textout(1:5).ne.'     ') then

              if(output) write(11,'(a)') textout(1:lenons)
              if(isf_out) call isf_out_line(12,onflag(indx(i)),textout,
     +           comment(indx(i)))
              if(json_out) call json_out_line(textout,comment(indx(i)),
     +           timemin+tt(indx(i)),onflag(indx(i)),json_rc)

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
      comm2(ntext) = comment(indx(i))
      tt3(ntext)   = timemin + tt(indx(i))

453   continue
      
455   continue

      if(new_in) close(31)

      if(json_out) call json_end_group(json_rc)

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

      if(ibad.ge.3 .and. .not.last) go to 470

c
      if(ndt.le.0) go to 466

      if(output) then
        write(11,'(/''Defining travel-time differences:''/)')
        write(11,'('' Stat  Delta  Phases'',11x,''Observed   Res''/)')
      endif

      i2 = 0
      sdmean  = 0.d0
      sdrmean = 0.d0
      rmsdt   = 0.d0
      i3 = 1

      do 461 i = 1,nobs-1

      if(used(i)(4:4).ne.'D') go to 461

      do 460 j = i+1,nobs

         if(used(j)(4:4).ne.'D') go to 460

         if(iev(i).ne.iev(j)) go to 460
         if(phaseu(i).eq.phaseu(j)) go to 460
         if(idtu(i3).ne.(j*i + j+i)) go to 460

         i2    = i2 + 1
         i3    = i3 + 1
         arr(i2) = sngl(del(iev(i)))

         dtth  = tttr(j) - tttr(i)
         dtobs = tt(j) - tt(i)
         if(dtth.lt.0.d0) then
            dtres = dtth - dtobs
         else
            dtres = dtobs - dtth
         endif
            
         sdmean  = sdmean  + dtres
         sdrmean = sdrmean + dabs(dtres)
         rmsdt   = rmsdt   + q2(dtres)

         fmis   = dtres/dpythag(ttu(i),ttu(j))
         dmisfl = dmisfl + dabs(fmis)
         dmisf  = dmisf + q2(fmis)
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

      if(output) then
         write(11,'(/''Number of usable stations: '',i4)') nstata
      endif

      if(json_out) then
        call json_add_int("num_usable_stations", nstata, json_rc)
      endif

      miteras = miteras + iter

      if(output) then

        if(miteras.gt.iter) then
            write(11,'(/''Total number of iterations: '',i5)') miteras
        endif

        if(nstat.gt.1) then
           if(.not.gapobs) then
             write(11,'(/''Maximum azimuthal gap of defining '',
     +        ''observations: '',f5.1,'' -> '',f5.1,'' [deg] = '',
     +        f5.1,'' [deg]; CPQ ='',f7.3)') d1azi,d2azi,dazgap,cpq
             if(json_out)  then
               call json_add_double("max_azimuthal_gap of defing data",
     +             dble(dazgap), json_rc)
               call json_add_double("CPQ",cpq, json_rc)
             endif
           else
             write(11,'(/''Maximum azimuthal gap for all '',
     +        ''observing stations: '',f5.1,'' -> '',f5.1,'' [deg] = '',
     +        f5.1,'' [deg]; CPQ ='',f7.3)') d1azi,d2azi,dazgap,cpq
             if(json_out)  then
               call json_add_double("max_azimuthal_gap of all data",
     +              dble(dazgap), json_rc)
               call json_add_double("CPQ",cpq, json_rc)
             endif
          endif
        endif

        if(nstat.gt.2) then
          if(.not.gapobs) then
             write(11,'(/''Maximum secondary azimuthal gap of '',
     +         ''defining observations: '',f5.1,'' -> '',f5.1,
     +         '' [deg] = '',f5.1,'' [deg]; CPQ ='',f7.3)') d1azi2,
     +         d2azi2,dazgap2,cpq2
             if(json_out)  then
               call json_add_double(
     +           "max_secondary_azimuthal_gap of defining data",
     +            dble(dazgap2), json_rc)
               call json_add_double("CPQ2",cpq2, json_rc)
             endif
          else
             write(11,'(/''Maximum secondary azimuthal gap for '',
     +         ''all observing stations: '',f5.1,'' -> '',f5.1,
     +         '' [deg] = '',f5.1,'' [deg]; CPQ ='',f7.3)') d1azi2,
     +         d2azi2,dazgap2,cpq2
             if(json_out)  then
               call json_add_double(
     +           "max_secondary_azimuthal_gap of all data",
     +           dble(dazgap2), json_rc)
               call json_add_double("CPQ2",cpq2, json_rc)
             endif
          endif
        endif
c
c     output of mean residuals
c
        write(11,'(/''Residuals of defining data'',10x,
     +              ''RMS     MEAN-RES       MEAN'')')

        stmean0 = rdig(stmean,3) 
        if(nobst.eq.1) 
     +  write(11,'(i6,'' onset time              : '',f8.3,x,2(3x,
     +        f8.3),''  [s]'')') nobst,rms,strmean,stmean0

        if(nobst.gt.1) 
     +  write(11,'(i6,'' onset times             : '',f8.3,x,2(3x,
     +        f8.3),''  [s]'')') nobst,rms,strmean,stmean0

        samean0 = rdig(samean,3) 
        if(nobsa.eq.1) 
     +  write(11,'(i6,'' backazimuth value       : '',f8.3,x,2(3x,
     +        f8.3),''  [deg]'')') nobsa,rmsazi,sarmean,samean0

        if(nobsa.gt.1) 
     +  write(11,'(i6,'' backazimuth values      : '',f8.3,x,2(3x,
     +        f8.3),''  [deg]'')') nobsa,rmsazi,sarmean,samean0

        spmean0 = rdig(spmean,3) 
        if(nobsp.eq.1)
     +  write(11,'(i6,'' ray parameter           : '',f8.3,x,2(3x,
     +        f8.3),''  [s/deg]'')') nobsp,rmsp,sprmean,spmean0

        if(nobsp.gt.1)
     +  write(11,'(i6,'' ray parameters          : '',f8.3,x,2(3x,
     +        f8.3),''  [s/deg]'')') nobsp,rmsp,sprmean,spmean0

        sdmean0 = rdig(sdmean,3) 
        if(ndmisf.eq.1) 
     +  write(11,'(i6,'' travel-time difference  : '',f8.3,x,2(3x,
     +        f8.3),''  [s]'')') ndmisf,rmsdt,sdrmean,sdmean0

        if(ndmisf.gt.1) 
     +  write(11,'(i6,'' travel-time differences : '',f8.3,x,2(3x,
     +        f8.3),''  [s]'')') ndmisf,rmsdt,sdrmean,sdmean0

        write(11,'(/''Weighted RMS of onset times (ISC type): '',
     +             f8.3,'' [s]''/)') rmsisc

        write(11,'(''Weighted misfit of input data'',9x,
     +             ''L1      L2'')')

        if(ntmisf.ge.1) then
           tmisf1 = dsqrt(tmisf/ntmisf)
           tmisfl1 = tmisfl/ntmisf
c          PRINT *,'tmisf tmisfl ntmisf tmisf1 tmisfl1'
c          PRINT *,tmisf,tmisfl,ntmisf,tmisf1,tmisfl1 
           write(11,'(i6,'' onset times             :'',2(x,f8.3))')
     +     ntmisf,tmisfl1,tmisf1
        endif

        if(namisf.ge.1) then
           amisf1 = dsqrt(amisf/namisf)
           amisfl1 = amisfl/namisf
           write(11,'(i6,'' backazimuth values      :'',2(x,f8.3))')
     +     namisf,amisfl1,amisf1
        endif

        if(npmisf.ge.1) then
           pmisf1 = dsqrt(pmisf/npmisf)
           pmisfl1 = pmisfl/npmisf
           write(11,'(i6,'' ray parameters          :'',2(x,f8.3))')
     +     npmisf,pmisfl1,pmisf1
        endif

        if(ndmisf.ge.1) then
           dmisf1 = dsqrt(dmisf/ndmisf)
           dmisfl1 = dmisfl/ndmisf
           write(11,'(i6,'' travel-time differences :'',2(x,f8.3))')
     +     ndmisf,dmisfl1,dmisf1
        endif

        nwmisf = ntmisf + namisf + npmisf + ndmisf
        wmisf = dsqrt((tmisf + amisf + pmisf + dmisf)/nwmisf)
        wmisfl = (tmisfl + amisfl + pmisfl + dmisfl)/nwmisf
        write(11,'(i6,'' misfit over all         :'',2(x,f8.3))')
     +  nwmisf,wmisfl,wmisf

c       qp = dble(in*nobst) /
c    +       (dble(nwmisf)*rms * var2(1)*wmisfl*earea*dazgap2)
c       if(czo.eq.'D') qp = qp * zo *4.d0 / (3.d0 *var2(4))
c       write(11,'(/''Quality: '',e15.10/)') qp

      endif

      if(json_out) then
        call json_add_double("weighted_rms_of_onset_times",
     +       rmsisc, json_rc)
        call json_start_dict_group("residuals_of_defining_data",
     +       json_rc)

        call json_start_dict_group("onset_times", json_rc)
        call json_add_int("num", nobst, json_rc)
        call json_add_double("rms", rms, json_rc)
        call json_add_double("mean_uncertainty", strmean, json_rc)
        call json_add_double("mean", stmean, json_rc)
        call json_end_group(json_rc)

        call json_start_dict_group("backazimuth_values", json_rc)
        call json_add_int("num", nobsa, json_rc)
        call json_add_double("rms", rmsazi, json_rc)
        call json_add_double("mean_uncertainty",sarmean, json_rc)
        call json_add_double("mean", samean, json_rc)
        call json_end_group(json_rc)

        call json_start_dict_group("ray_parameters", json_rc)
        call json_add_int("num", nobsp, json_rc)
        call json_add_double("rms", rmsp, json_rc)
        call json_add_double("mean_uncertainty", sprmean, json_rc)
        call json_add_double("mean", spmean, json_rc)
        call json_end_group(json_rc)

        call json_start_dict_group("traveltime_differences", json_rc)
        call json_add_int("num", ndmisf, json_rc)
        call json_add_double("rms", rmsdt, json_rc)
        call json_add_double("mean_uncertainty", sdrmean, json_rc)
        call json_add_double("mean", sdmean, json_rc)
        call json_end_group(json_rc)

        call json_end_group(json_rc)
        call json_start_dict_group("misfit_of_input_data", json_rc)

        call json_start_dict_group("onset_times", json_rc)
        call json_add_int("num", ntmisf, json_rc)
        call json_add_double("L1", tmisfl1, json_rc)
        call json_add_double("L2", tmisf1, json_rc)
        call json_end_group(json_rc)

        call json_start_dict_group("backazimuth_values", json_rc)
        call json_add_int("num", namisf, json_rc)
        call json_add_double("L1", amisfl1, json_rc)
        call json_add_double("L2", amisf1, json_rc)
        call json_end_group(json_rc)

        call json_start_dict_group("ray_parameters", json_rc)
        call json_add_int("num", npmisf, json_rc)
        call json_add_double("L1", pmisfl1, json_rc)
        call json_add_double("L2", pmisf1, json_rc)
        call json_end_group(json_rc)

        call json_start_dict_group("traveltime_differences", json_rc)
        call json_add_int("num", ndmisf, json_rc)
        call json_add_double("L1", dmisfl1, json_rc)
        call json_add_double("L2", dmisf1, json_rc)
        call json_end_group(json_rc)

        call json_start_dict_group("misfit_over_all", json_rc)
        call json_add_int("num", nwmisf, json_rc)
        call json_add_double("L1", wmisfl, json_rc)
        call json_add_double("L2", wmisf, json_rc)
        call json_end_group(json_rc)

        call json_end_group(json_rc)
      endif

c
c     output of one line with all calculated source parameters and
c     quality parameters
c

      if(output) then
         write(11,'(/''T0'',25x,''LAT'',6x,''LON'',7x,''Z'',5x,
     +      ''VPVS'',3x,''DLAT'',5x,''DLON'',6x,''DZ'',7x,''DT0'',4x,
     +      ''DVPVS DEF'',4x,''RMS'' )' )
      endif

      if(typctl.ge.0) then
         write(*,'(/''T0'',25x,''LAT'',6x,''LON'',7x,''Z'',5x,
     +      ''VPVS'',3x,''DLAT'',5x,''DLON'',6x,''DZ'',7x,''DT0'',4x,
     +      ''DVPVS DEF'',4x,''RMS'' )' )
      endif
      
      call fetoh2(tome,idum,yy,mon,mm,dd,idoy,hh,mi,sec)
 
      isec1 = nint(sec*1000)
      isec  = isec1/1000
      msec = isec1-isec*1000

      if(var(1).gt.9999.999d0) var2(1)=9999.999d0

      if(json_out) then
         call json_add_double("time", tome, json_rc)
         call json_add_double("dt0", var2(1), json_rc)
         call json_add_double("lat", elatmg, json_rc)
         call json_add_double("dlat", sdlatg, json_rc)
         call json_add_double("lon", elonm, json_rc)
         call json_add_double("dlon", var2(3), json_rc)
         call json_add_double("z", zo, json_rc)
      endif

      if(czo.eq.'D') then

        if(output) then
           write(11,'(i4,''-'',i2.2,''-'',i2.2,3i3.2,''.'',i3.3,
     +        2f9.3,f8.2,f7.2,2f9.4,f8.2,f9.3,f7.2,i5,f9.3)') 
     +        yy,mon,dd,hh,mi,isec,msec,elatmg,elonm,zo,vpvsm,
     +        sdlatg,var2(3),var2(4),var2(1),sdvpvs,in,rms

        endif

        if(typctl.ge.0) then
           write(*,'(i4,''-'',i2.2,''-'',i2.2,3i3.2,''.'',i3.3,2f9.3,
     +           f8.2,f7.2,2f9.4,f8.2,f9.3,f7.2,i5,f9.3)') 
     +           yy,mon,dd,hh,mi,isec,msec,elatmg,elonm,zo,vpvsm,
     +           sdlatg,var2(3),var2(4),var2(1),sdvpvs,in,rms
        endif
        
        if(json_out) call json_add_double("dz", var2(4), json_rc)

      else if(czo.eq.'F' .or. czo.eq.'B') then

        cfix = '  Fixed '
        if(ldefisc .and. .not.lcomfix) then
           if(idetyp.eq.1) cfix = '  F-ISC '
           if(idetyp.eq.2) cfix = '  F-CR1 '
           if(idetyp.eq.3) cfix = '  F-FER '
        endif
        if(output) then
           write(11,'(i4,''-'',i2.2,''-'',i2.2,3i3.2,''.'',i3.3,
     +           2f9.3,f8.2,f7.2,2f9.4,a8,f9.3,f7.2,i5,f9.3)') 
     +           yy,mon,dd,hh,mi,isec,msec,elatmg,elonm,zo,vpvsm,
     +           sdlatg,var2(3),cfix,var2(1),sdvpvs,in,rms
        endif

        if(typctl.ge.0) then
           write(*,'(i4,''-'',i2.2,''-'',i2.2,3i3.2,''.'',i3.3,
     +        2f9.3,f8.2,f7.2,2f9.4,a8,f9.3,f7.2,i5,f9.3)') 
     +        yy,mon,dd,hh,mi,isec,msec,elatmg,elonm,zo,vpvsm,
     +        sdlatg,var2(3),cfix,var2(1),sdvpvs,in,rms
        endif

        if(json_out) call json_add_string("dz", cfix, json_rc)

      endif

      if(json_out) then
         call json_add_double("vpvs", vpvsm, json_rc)
         call json_add_double("dvpvs", sdvpvs, json_rc)
         call json_add_int("def", in, json_rc)
         call json_add_double("rms", rms, json_rc)
      endif

      if(isf_out) then
         write(12,'(/,''STOP'')')
         close(12)
      endif

      if(ref_eve .and. output) then

         call depi(dlati,dloni,elatmg,elonm,del3,dk,ep2,ep1,d2km)

      
         if(isf_ref.ne.' ' .and. isf_in) then

            write(11,'(/,''Distance to ISF Ref ( '',a10,f9.4,f10.4,
     +           '' ):'',f8.2,'' km; DZ:'',f7.1,'' km, AZI:'',f6.1,
     +           '' MA: '',f7.1,'' MI: '',f7.1,'' AZI: '',f6.1)') 
     +           author2,dlati,dloni,dk,ddepi-zo,ep2,elmax,elmin,eazi
         else
            write(11,'(/,''Distance to Reference Event ('',f9.4,f10.4,
     +           '' ):'',f8.2,'' km; DZ:'',f7.1,'' km, AZI:'',f6.1,
     +           '' MA: '',f8.1,'' MI: '',f7.1,'' AZI: '',f6.1)') 
     +           dlati,dloni,dk,ddepi-zo,ep2,elmax,elmin,eazi
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

470   continue

c     end origin JSON object
      if(json_out) call json_end_group(json_rc)

c
c     Now let us try to locate the event better without a fixed depth.
c

c     print *,'czo ',czo,' iterz ',iterz,' zoflag ',zoflag, idepm,zo

      if(czo.eq.'B' .and. ((.not. zoflag) .or. iterz.lt.2) .and.
     +   idepm.eq.0) then

         zoflag=.true.
         czo ='D'

         miteras = miteras + iter

         if(zo.le.depthmin .or. zo.ge.depthmax) then
            call def_depth(defdep,elatmg,elonm,idetyp,ldefd,c1typ,
     +                     ideptyp)
            call zo2to(defdep-zo,tome,var(1))
            zo   = defdep
            tom  = tome - timemin
         endif

         sdzo  = sdzo1

         sdto  = dpythag(var2(1),25.d0)
         sdlat = sdlatg*5.d0
         sdlon = var2(3)*5.d0
         sdlat = sdlat0
         sdlon = sdlon0

         disper  = 0.001d0

         itso = 0
         nextiter1= 0
         iteraz = 0
         ibad   = 0
         ibad0  = 0
         ilastiter = 0

         dtp0  = 600.d0
         dts0  = dtp0*2.d0
         dtmin = 9999.d0
         dtm2  = dts

         check  = 9999.d0
         setcheck = setcheck1
         setcheck2 = 15.d0*setcheck

         go to 99

      endif

      go to 9999

9998  continue
      if(output) then
         write(11,'(/''Could not find a better or non-oscillating'',
     +            '' solution without fixed depth'')')
      endif
      write(*,'(/''Could not find a better or non-oscillating'',
     +            '' solution without fixed depth'')')

9999  continue

      if(json_out) then
         call json_end_group(json_rc);
         call json_end(json_rc);
      endif

      if(output) close(11)

      stop

c     end program HYPOSAT_6_1
      end 
