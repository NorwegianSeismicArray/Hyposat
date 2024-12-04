c
c     SUBROUTINE DEF_DEPTH
c
c     read the ISC file with default depths at locations given by
c     lat & lon and the long record of lacated events at ISC.
c
c     If no default dept vaue is available, the Moho depth from 
c     Crust 1.0 is used as default.
c
c     input:
c     input:
c
c              dlat - event latitude
c              dlon - event longitude
c
c     output:
c              defdep - found default depth value
c              idetyp - type of default depth
c                       = 1 from ISC file
c                       = 2 from Crust 1.0 Moho depth
c                       = 3 from default depth in FE Region for AK135
c                       = 4 no value found. defdep set to 0.
c
c     data are read from file 'isc_def_depths.dat' (modified from 
c     'default.depth0.5.grid'), grn_default_depth.ak135.dat and the 
c     Crust 1.0 files
c
c     Johannes Schweitzer, NORSAR August 2020
c
c     changes: 11 July 2022 after changing the reading to direct
c              accessing the correct record.
c           
c
c     calls: get_moho_depth, hyposat_geo
c

      subroutine def_depth(defdep,dlat,dlon,idetyp,ldefd,c1t,ityp)
c
      implicit real*8 (a-h,o-z)
      implicit integer (i-n)

      real*8  dlat, dlon, defdep, depmoh
      integer idetyp, i1, is, ierr, ityp
      character c1t*2

      integer idetypo
      real*8  dlato, dlono, elev, dwa
      character*512 file, file_check
      character line*58, name*80
      logical ldefd(3)

      parameter (nfe = 1000)
      dimension fez(nfe)

      save

      idetyp = 4
      defdep = 0.d0

      if(.not.ldefd(1) .and. .not.ldefd(2) .and. .not.ldefd(3)) then
         dlato   = 0.d0
         dlono   = 0.d0
         defdepo = 0.d0
         idetypo = 4
      endif

      if(dabs(dlat-dlato).le.1.d-1) then
        if(dabs(dlon-dlono).le.1.d-1) then
          go to 899
        endif
      endif

      if(ityp.lt.3) then

c
c     ideptyp = ISC database
c

         irec = idint(180.d0 - dlat*2.d0)*721 + 
     +          idint(360.d0 + alpha1(dlon)*2.d0) + 1

         if(irec.eq.ireco)  go to 90

         file = file_check('isc_def_depths.dat')
         open(unit=33,file=trim(file),status='old',access='direct',
     +        recl=59,form='formatted')
   
         read(33,'(a)',rec=irec) line
         close (33)
         
         read(line,'(f5.1,f7.1,5f7.2,i5,f6.1)') plat0,plon0,pz0,zmin,
     +        q25,q75,zmax,n,zrange

         ireco   = irec

90       if(zrange.lt.100.d0 .and. n.gt.0) then
            defdep = pz0
            idetyp = 1
            ldefd(1) = .true.
            goto 900
         endif

      endif

c
c     ideptyp = Crust 1.0
c
      if(ityp.eq.1 .or. ityp.eq.3) then 
         ierr = 0
         call get_moho_depth(depmoh,dlat,dlon,elev,dwa)

         if(ierr.eq.0) then

c no topography           if(elev.gt.0.d0) depmoh = depmoh + elev

           if(c1t.eq.'uc') defdep = depmoh * 0.25d0
           if(c1t.eq.'mc') defdep = depmoh * 0.50d0 
           if(c1t.eq.'lc') defdep = depmoh * 0.75d0
           if(c1t.eq.'mo') defdep = depmoh 

           if(dwa.gt.0.d0) defdep = defdep + dwa

           ldefd(2) = .true.
           idetyp = 2
           goto 900
         endif

      endif

c
c     ideptyp = FE Region
c
      if(ityp.eq.2 .or. ityp.eq.4) then

         if(.not.ldefd(3)) then

            file = file_check('grn_default_depth.ak135.dat')
            open(unit=33,file=trim(file))

            do 30 i = 1,1000
               read(33,*,end=31) ife, zfe
               fez(ife) = zfe
30          continue

31          close (33)

            ldefd(3) = .true.

         endif

         ierr = 0
         call HYPOSAT_GEO(sngl(dlat), sngl(dlon), I1, is, name, ierr)

         if (ierr.eq.0) then
            defdep = fez(is)
            idetyp = 3
            goto 900
         endif
      endif

899   continue
      defdep = defdepo
      idetyp = idetypo
      return
900   defdepo = defdep
      dlato   = dlat
      dlono   = dlon
      idetypo = idetyp
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subroutine version of a SCRIPS program called 'getCN1point'
c  to get the parameters of the model CRUST 1.0 as downloaded from
c
c  http://igppweb.ucsd.edu/~gabi/crust1.html
c
c     Here version to get only Moho depth for default  depth
c
c     NORSAR, Johannes Schweitzer, August 2020
c
c     calls: get_crust10
c
c     depmoh  - crustal thickness without water level or elevation 
c               above sea level
c     dwat    - water level
c     elev    - elevation above sea level
c
c     last changes 7 July 2022
c

      subroutine get_moho_depth(depmoh,elat,elon,eleva,dwat)

      save

      real*8 alpha1, depmoh, elat, elon, eleva, dwat

c
      include 'crust_10.h'
c

      real*8    dcolo,dcola,depth

      eleva = 0.d0
      dwat  = 0.d0

*-------------------
c     now look up coordinates

      dcola = 90.d0 - elat
      dcolo = 180.d0 + alpha1(elon)

      ilat1 = idint (dcola) + 1
      ilon1 = idint (dcolo) + 1

      if(ilat1.eq.ilato .and. ilon1.eq.ilono) go to 25

c
c     Reading from global crustal model Crust 1.0
c
      call get_crust10(ilat1,ilon1)

      ilato = ilat1
      ilono = ilon1

25    eleva  = topo(ilat1,ilon1)
      dwat   = awat(ilat1,ilon1)

      depth = 0.d0

      do 50 i=2,npm-1

      depth = depth + athi(i,ilat1,ilon1)

50    continue

      depmoh = depth - eleva

      return

      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Function to get only water depth out of Crust 1.0
c
c     NORSAR, Johannes Schweitzer, July 2022
c
c     calls: get_crust10
c

      function wdepth(elat,elon)

      save

      real*8 wdepth,elat,elon,alpha1

      integer ilat1, ilon1

      include 'crust_10.h'
c
*-------------------
c     now look up coordinates

      wdepth = 0.d0

      ilat1 = idint (90.d0 - elat) + 1
      ilon1 = idint (180.d0 + alpha1(elon)) + 1

      if(ilat1.eq.ilato .and. ilon1.eq.ilono) go to 25
c
c     Reading from global crustal model Crust 1.0
c
      call get_crust10(ilat1,ilon1)

      ilato = ilat1
      ilono = ilon1

25    wdepth = awat(ilat1,ilon1)

      return

      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
