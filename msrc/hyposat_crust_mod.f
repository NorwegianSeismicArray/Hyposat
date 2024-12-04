c
c  subroutine version of a SCRIPS program called 'getCN1point'
c  to get the parameters of the model CRUST 1.0 as downloaded from
c
c  http://igppweb.ucsd.edu/~gabi/crust1.html
c
c  Because we do not need any densities to locate an event,
c  this parameter was thrown out from the original data and program
c  files.
c
c     NORSAR, Johannes Schweitzer, November 2017
c
c     Softsediment layers thinner than topography  corrected
c     September 2016
c
c  calls: get_crust10
c
c layer one and two flipped, after the read statement!
c layer 1: water
c layer 2: ice
c
c
c   latest changes and cleaning up August 2022
c

      subroutine get_mod_c10(itrue,inum0,typctl)

      save

      real*8 alpha1

      integer   typctl,itrue,inum0
c
c
c     typctl  - verbosity level
c
c     itrue   - switch
c               = 0 if model used without topography 
c               > 0 if 'true' model is used including topography 
c
c     inum0   - switch
c               = 1 if model used for regional travel times
c               = 2 if model used for crustal station corrections
c               = 3 if model used for crustal reflection point corrections
c
      include 'model.h'

      include 'crust_10.h'
c
      real*8    vp(maxla),vs(maxla),
     *          dz(maxla),dcolo,dcola

      real*8      elat2l, elon2l, elatcl, eloncl, eps, el1
      integer     inum0l, itruel

c     grid resolution of crustal model
c     CRUST1.0 has a resolution of 1 degree bwetween 
c     -179.5 and 179.5 long and -89.5 and 89.5 lat
c
      eps = 1.d-4

      if (   dabs(elatc-elatcl).le.eps .and. dabs(elat2-elat2l).le.eps 
     + .and. dabs(elonc-eloncl).le.eps .and. dabs(elon2-elon2l).le.eps
     + .and. inum0.eq.inum0l           .and. itrue.eq.itruel ) goto 998
      
      rmax = 1.5d0

      if(inum0.eq.1) inum = 2
      if(inum0.eq.2) inum = 1
      if(inum0.eq.3) inum = 1

*-------------------
c     now look up coordinates and get model

      do 50 k = 1,inum

         if(k.eq.1) then
            dcola = 90.d0 - elatc
            dcolo = 180.d0 + alpha1(elonc)
            ilat1 = idint (dcola) + 1
            ilon1 = idint (dcolo) + 1
c           print *,k,dcola,dcolo,elatc,elonc
         else if(k.eq.2) then
            dcola = 90.d0 - elat2
            dcolo = 180.d0 + alpha1(elon2)
            ilat1 = idint (dcola) + 1
            ilon1 = idint (dcolo) + 1
c           print *,k,dcola,dcolo,elat2,elon2
         endif

c
c     Reading from global crustal model Crust 1.0
c
 
         call get_crust10(ilat1,ilon1)

         if(k.eq.1) then
           elev = topo(ilat1,ilon1)
           zw   = awat(ilat1,ilon1)
           do 40 i = 1,npm
              vp(i) = avp(i,ilat1,ilon1)
              vs(i) = avs(i,ilat1,ilon1)
              dz(i) = athi(i,ilat1,ilon1)
40         continue
         else
           elev = (elev + topo(ilat1,ilon1)) / 2.d0
           zw   = (zw + awat(ilat1,ilon1)) / 2.d0
           do 45 i = 1,npm
              vp(i) = (vp(i) + avp(i,ilat1,ilon1)) / 2.d0
              vs(i) = (vs(i) + avs(i,ilat1,ilon1)) / 2.d0
              dz(i) = (dz(i) + athi(i,ilat1,ilon1)) / 2.d0
45         continue
         endif

50       continue

         if(zw.ge.1.d-3 .and. inum0.lt.3) dz(1) = 0.d0

         jmod = 1
         iconr = 0
         el1 = elev

         do 70 i=1,npm

         if(typctl.gt.8) then

            if (i.eq.1) then
                print *,' ' 
                print *,' 8-layer crustal model (thickness, vp,vs)'
            endif
            if(i.lt.npm) print 794, i,dz(i),vp(i),vs(i)
794         format (i2,3f10.4)

            if(i.eq.npm) then
              if(k.eq.1) then
                print *,'latitude, longitude, elevation: ',
     +            elatc,elonc,elev,inum,itrue
              else if(k.eq.2) then
                print *,'latitude, longitude, elevation: ',
     +            elat2,elon2,elev,inum,itrue
              endif
              print 793,'Mantle below Moho: ave. vp, vs:  ',
     +           vp(i),vs(i)
793           format(a,2x,2f11.4)
            endif
         endif


         if(dz(i).ge.1.d-3) then
          
           jmod1 = jmod +1
   
           if(jmod.eq.1) then
              z(jmod)    = 0.d0
              if(itrue.eq.0) then
                 el1 = dz(i) - el1
                 if(el1.lt.-1.d-3) then
                    el1 = -el1
                    go to 70
                 else
                    z(jmod1) = el1
                    el1 = 0.d0
                 endif
              else
                 z(jmod1)   = dz(i)
              endif
           else
              z(jmod)    = z(jmod-1)

              if(itrue.eq.0 .and. el1.ge.1.d-3) then
                 el1 = dz(i)-el1
                 if(el1.lt.-1.d-3) then
                    el1 = -el1
                    go to 70
                 else
                    z(jmod1) = z(jmod) + el1
                    el1 = 0.d0
                 endif
              else
                 z(jmod1)   = z(jmod)+dz(i)
              endif
           endif

           v0(1,jmod) = vp(i)
           v0(1,jmod1)= vp(i)

           v0(2,jmod) = vs(i)
           v0(2,jmod1)= vs(i)

           azo(jmod)  = ' '
           azo(jmod1) = ' '

           if(v0(1,jmod).ge.6.3d0 .and.iconr.eq.0) then
              if(jmod.gt.1) then
                 azo(jmod-1) = 'CONR'
                 iconr = 1
              endif
           endif

           if(i.eq.npm) then
              azo(jmod-1) = 'MOHO'
              jmod = jmod1
           else
              jmod = jmod1+1
           endif

         endif

70       continue

      zmax = z(jmod)

      if(typctl.gt.8) then
        print*,'CRUST 1.0: ',elatc,elonc,elat2,elon2,elev,zw
        do 90 i=1,jmod
           print*,i,z(i),v0(1,i),v0(2,i),azo(i)
90      continue
      endif

      inum0l = inum0
      itruel = itrue
      elat2l = elat2
      elon2l = elon2
      elatcl = elatc
      eloncl = elonc

998   return

      end
c
c  subroutine version of a modified SCRIPS program called 'getCNpoint'
c  to get the parameters of the crust as descriped in:
c  Mooney, Laske and Masters, Crust 5.1: a global
c  crustal model at 5x5 degrees, JGR, January 1998
c
C    here modified to read only the crusts of a standard Earth model
c
c     NORSAR, Johannes Schweitzer, 2017/2018
c
c

      subroutine get_mod_global(typctl,ierr)

      save
c
c     mctyp = maximum number allowed different crutsal models of
c             standadrd spherical Earth models

      parameter (mctyp=20)
      integer   typctl,ierr

      real*8 dz, fvel(mctyp,8),fvels(mctyp,8),fthi(mctyp,8)

c
c     typctl  - verbosity level
c
c     ierr    - ne.0 if any error occured
c

      include 'model.h'

      character file*512

      character*2 ctype(mctyp)
      character*512 file_check

      file = file_check('std_crusts.dat')
      open(65,file=trim(file))

c...  read in key for crust types and all models
c...............................
      read(65,'(a)')
      read(65,'(a)')
      read(65,'(a)')
      read(65,'(a)')
      if(typctl.gt.4) then
         print*,' ... reading global crustal model file ...'
      endif

      do 101 i=1,mctyp+1
         if(i.gt.mctyp) then
           print *,'Maximum number (',mctyp,') of standadrd spherical',
     +             ' Earth models reached! See ',trim(file)
           stop
         endif
         read(65,'(a)',end=102) ctype(i)
         read(65,*)(fvel(i,l),l=1,8)
         read(65,*)(fvels(i,l),l=1,8)
         read(65,*)(fthi(i,l),l=1,7)
101   continue

102   ityp = i - 1

      close(65)
*-------------------

      do 200 i = 1,ityp

         if (mtypg(1:2).eq.ctype(i)) then
             k = i
             go to 202
          endif

200   continue

      print *,'Could not find standard model type ',mtypg
      go to 999

202   continue

      elev = 0.d0
      iconr = 0
      imoho = 0
      jmod  = 1

      do 70 i=1,8
c
      dz = fthi(k,i)
      if(i.eq.8) dz = 20.d0

      if(dz.ge.1.d-3) then
          
         jmod1 = jmod + 1
         if(jmod.eq.1) then
            z(jmod)    = 0.d0
            z(jmod1)   = dz
         else
            z(jmod)    = z(jmod-1)
            z(jmod1)   = z(jmod)+dz
         endif

         v0(1,jmod) = fvel(k,i)
         v0(1,jmod1)= fvel(k,i)

         v0(2,jmod) = fvels(k,i)
         v0(2,jmod1)= fvels(k,i)

         azo(jmod)  = ' '
         azo(jmod1) = ' '

         if(v0(1,jmod).ge.6.3d0 .and.iconr.eq.0) then
            iconr = jmod-1
            azo(iconr) = 'CONR'
         endif

         if(i.eq.8) then
            imoho = jmod-1
            if(imoho.eq.iconr) iconr=0
            azo(imoho) = 'MOHO'
            jmod = jmod1
         else
            jmod = jmod1+1
         endif

         if(imoho.gt.0 .and. iconr.le.0) then
            do 50 j=1,i
            if(dabs(v0(1,j)-v0(1,imoho)).lt.1.d-2) then
               iconr = j-1
               azo(iconr) = 'CONR'
               go to 51
            endif
50          continue
51          continue
         endif

             
      endif
          
70    continue

      zmax = z(jmod)

c
      if(typctl.gt.8) then
        print *,'Standard Earth model : ',mtypg
        do 90 i=1,jmod
           print*,i,z(i),v0(1,i),v0(2,i),azo(i)
90      continue
      endif
      ierr = 0
      return

999   ierr = 99
      print *,'Something wrong with crust of standard model ',mtypg
      return

      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_mod_reg(ierr)

      save

      integer   ierr

c
c     ierr    - ne.0 if any error occured
c
      include 'model.h'
c

      character line*34

      if (imo.le.0 .or. imo.eq.3 .or. imo.eq.4) goto 9000

      if (mtyp.ne.'REG' .and. mtyp.ne.'RER') goto 9000
      if (mtyp.eq.'REG' .and. rmax.gt.0.d0 .and. jmod.gt.0) goto 9999

      OPEN (UNIT=15,FILE=trim(filloc),err=55)
C
C     Loop to get the models of P and S velocities
C
      read(15,*,err=55) rmax

      I=0
      jmod = 0

50    I=I+1
      IF(I.GT.maxla) THEN
         write(*,'('' Model contains too many layers (> '',i3,
     *           '')'')') maxla
         GO TO 9000
      ENDIF

      READ(15,'(A)',err=55,end=56) line

      if (line(31:34).ne.'    ' .and. line(31:34).ne.'MOHO' .and.
     *    line(31:34).ne.'CONR' ) go to 55

      READ(line,'(3F10.3,A4)',err=55,end=56)
     *     Z(I),V0(1,i),V0(2,i),azo(I)

      if(Z(i) + v0(1,i) + v0(2,i).eq.0.d0) go to 56

      if(v0(2,i).le.1.d-2) zw = z(i)

      go to 50

55    write(*,'('' Read ERROR for file: '',a)') filloc
      go to 9000

56    jmod=I-1
      zmax  = Z(jmod)
      elev = 0.d0
      mtyp = 'REG'
      ierr = 0

      go to 9999
                                                             
9000  ierr = 99 

9999  close (15)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Function get_mtyp(modeln0)

      character*3 get_mtyp
      character*40 uppcas
      character*20 modeln0
      character*40 modeln
  
      modeln = uppcas(modeln0)
C (1:20)

      get_mtyp = '_'

      if(index(modeln,'JB').gt.0) then
         get_mtyp = 'E1 '
         goto 9000
      else if(index(modeln,'PREM').gt.0) then
         get_mtyp = 'E2 '
         goto 9000
      else if(index(modeln,'IASP91A').gt.0) then
         get_mtyp = 'E4 '
         goto 9000
      else if(index(modeln,'IASP91').gt.0) then
         get_mtyp = 'E3 '
         goto 9000
      else if(index(modeln,'SP6').gt.0) then
         get_mtyp = 'E5 '
         goto 9000
      else if(index(modeln,'AK135').gt.0) then
         get_mtyp = 'E6 '
         goto 9000
      else if(index(modeln,'FESCAN').gt.0) then
         get_mtyp = 'E7 '
         goto 9000
      else if(index(modeln,'BAREY').gt.0) then
         get_mtyp = 'E8 '
         goto 9000
      else if(index(modeln,'BAREZ').gt.0) then
         get_mtyp = 'E9 '
         goto 9000
      else if(index(modeln,'BARENTS16').gt.0) then
         get_mtyp = 'EA '
         goto 9000
      else if(index(modeln,'BERGEN').gt.0) then
         get_mtyp = 'EB '
         goto 9000
      else if(index(modeln,'EK137').gt.0) then
         get_mtyp = 'EC '
         goto 9000
      endif

9000  return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_crust10(ilat,ilon)
c
c  to read file with the parameters of the model CRUST 1.0 as 
c  downloaded from
c
c  http://igppweb.ucsd.edu/~gabi/crust1.html
c
c     NORSAR, Johannes Schweitzer, August 2020
c
c     Here changes to read only the record containing the model with
c     the actual coordinates
c
c     7 July 2022
c

      character file*512

      character*512 file_check

      integer irec

      include 'crust_10.h'

      SAVE

      irec = (ilat-1)*nlo + ilon

      if(icrust10(irec).eq.1) goto 999

      file = file_check('crust1.vp')
      open(65,file=trim(file),status='old',access='direct',recl=55,
     +        form='formatted',err=998)
      read(65,'(9f6.2)',rec=irec,err=998)(avp(l,ilat,ilon),l=1,npm)
      close (65)

      file = file_check('crust1.vs')
      open(65,file=trim(file),status='old',access='direct',recl=55,
     +        form='formatted',err=998)
      read(65,'(9f6.2)',rec=irec,err=998)(avs(l,ilat,ilon),l=1,npm)
      close (65)

      file = file_check('crust1.bnds')
      open(65,file=trim(file),status='old',access='direct',recl=64,
     +        form='formatted',err=998)
      read(65,'(9f7.2)',rec=irec,err=998)(bnd(l,ilat,ilon),l=1,npm)
      close (65)

      do 103 l = 1,npm-1
         athi(l,ilat,ilon) = bnd(l,ilat,ilon) - bnd(l+1,ilat,ilon)
         if(dabs(athi(l,ilat,ilon)) .lt. 1.d-2) 
     +      athi(l,ilat,ilon) = 0.d0
         if(avp(l,ilat,ilon) .lt. 1.d-2) athi(l,ilat,ilon) = 0.d0
103   continue

      topo(ilat,ilon) = bnd(1,ilat,ilon)
      awat(ilat,ilon) = (bnd(1,ilat,ilon) - bnd(2,ilat,ilon))
      if(awat(ilat,ilon) .lt. 1.d-2) awat(ilat,ilon) = 0.d0
      athi(npm,ilat,ilon) = 20.d0

      icrust10(irec) = 1
      goto 999

998   print *,'Something wrong with CRUST 1.0 - input files'
      close(65)
      stop

999   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


