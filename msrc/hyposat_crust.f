C
      subroutine CRUST(tcrust,ddelta,PA0,PHTYP,zo,mid,cortyp,lpwp,
     +                 TYPCTL0,ierc)
c
c     CRUST is a subroutine to calculate travel-time differences 
c     for phases passing a standard crust and a loc/regional model
c     (either input or from Model CRUST 1.0)
c
C     JOHANNES SCHWEITZER
C
c     March 1999    
c
c     June 2004: subroutine EFAD included
c
c     February 2016: Common MODELG repaired
c
c     October 2017: Several adjustments added to calculate also 
c                   corrections for Regional Models in source region and 
c                   not only Crust 5.1
c
c     December 2017: CRUST 5.1 exchanged with CRUST 1.0
c
c
c     July/August 2020: changes to calculate correctly pwP or sP and other phases 
c                 in the case of a water layer at the reflection point;
c
c     June 2021: ddelta (change in epicentral distance) also as output
c
c     input:
c 
c             pa0     = ray parameter in [s/deg]
c
c             phtyp   = gives the phase type (P or S)
c
c             zo      = source depth
c
c             mid     = index of global crustal model to be used as
c                       reference
c
c             cortyp  = usage character for CRUST 1.0
c                     =  s  station corrections
c                     =  r  reflection point corretions
c                     =  rps converted reflection corretions
c
c
c             in COMMON MODEL Local Model:
c 
c             imo     <= 1 not used here
c                     =  2 CRUST 1.0 only used for travel-time 
c                           corrections at position elatc ,elonc 
c                           (station correction)
c                     =  3 not used here
c                     =  4 as (2) CRUST 1.0 used for travel-time 
c                          corrections
c                     =  5 local/regional model for source and near
c                          source reflections; CRUST 1.0 for 
c                          station corrections
c                     =  6 CRUST 1.0 only for reflection point corrections
c
c             elatc   = latitude  to get the right CRUST 1.0 model 
c                       position for the station
c
c             elonc   = longitude to get the right CRUST 1.0 model 
c                       position for the station
c
c             mtyp    =  crustal model type
c 
c             v0(1,i) =  P velocity in layer i
c             v0(2,i) =  S velocity in layer i
c
c             z(i)    =  depth of layer i
c
c             zmax    =  maximum model depth 
c
c             elev    =  topograhic elevation at modelled point 
c
c             jmod    =  number of layers
c
c             in COMMON MODELG parameter of standard global-model crust:
c 
c             v0g(nmod,1,i) =  P velocity in layer i
c             v0g(nmod,2,i) =  S velocity in layer i
c
c             zg(nmod,i)    =  depth of layer i
c
c             zmaxg(nmod)   =  maximum model depth 
c
c             elevg(nmod)   =  topograhic elevation at modelled point 
c
c             jmodg(nmod)   =  number of layers
c
c
c     output:
c
c             tcrust  = travel-time correction for requested phase
c                       with respect to a standard model
c
c             ddelta  = change in epicentral distance due to the
c                       structure change
c
c             lpwp    = logical switch if phase was travelling through 
c                       a water layer
c
c             typctl0 = verbosity level
c             in COMMON
c
c             vstat   = local velocity from Crust 1.0 at station for
c                       elevation correction of remaining topography
c                       (elevation - Crust 1.0 topo)
c
c             ierc    = error status
c
      IMPLICIT real*8 (A-H,O-Z)
c
      save

      real*8   pa0,zo,tcrust,ddelta

      INTEGER  typctl0,typctl, ierr, ierc

      logical  lpwp

      include 'model.h'

      dimension v0s(2,maxla),zs(maxla)

      DIMENSION h(maxla),del(2),time(2),V(maxla),G(maxla),V2(maxla)

      CHARACTER phtyp*1, cortyp*3

      include 'modelg.h'

      PI = 4.d0*DATAN(1.d0)
      PIM= PI/180.d0
      AA = 6371.d0*PIM

      typctl = typctl0
      pa     = pa0
      rvv    = pa / aa
      vv     = 1.d0 / rvv
      rv2    = q2(rvv)

      fa = 1.d0
      if(cortyp.eq.'r' .or. cortyp.eq.'rw') fa = 2.d0

      lpwp = .false.

      vstat = -999.d0

      ierc = 0

      if(imo.eq.6 .and. cortyp.eq.'s') go to 9999

      if (imo.eq.2 .or. imo.eq.4 .or. (imo.eq.5 .and. .not.locmod) .or.
     +    imo.eq.6) then
c
         if(cortyp.eq.'s') then
            inum  = 2
            itrue = 1
         else 
            inum = 3
            itrue = 1
         endif
c
         call get_mod_c10(itrue,inum,typctl)

         if(zw.ge.1.d-3) lpwp = .true.

         if(imo.eq.2 .or. imo.eq.5) mtyp = 'RER'

      else if(imo.eq.1 .or. (imo.eq.5 .and.locmod)) then

         if(mtyp.ne.'REG') then

            call get_mod_reg(ierr)

            if(ierr.ne.0) then
               ierc = ierr
               go to 9999
            endif
         endif

      else

         print *,'No regional/local model defined for crustal travel',
     +           ' time corrections'
         stop

      endif

c     print *,'CRUST 1',imo,elat,elon,elatc,elonc,mtyp,cortyp,
c    +        inum,itrue,zw,elev,lpwp

c
c     get the right reference crust
c

      zmaxs = zmaxg(mid)
      elevs = elevg(mid)
      jmods = jmodg(mid)

      do 50 i = 1,jmods
        v0s(1,i) = v0g(mid,1,i)
        v0s(2,i) = v0g(mid,2,i)
        zs(i)    = zg(mid,i)
50    conTinue

c
c     define model thickness
c
      zma   = zmax - elev
      zmas  = zmaxs - elevs

      zmini = dmin1(zmas,zma)

      zmin  = zmini + elev
      zmins = zmini + elevs

c     print *,' CRUST 2',mid,zmaxs,elevs,jmods,mtyp,mtype(mid)
c     print *, zmax,zmaxs,elev,elevs, zw
c     print *,zma,zmas,zmax,zmaxs,elev,elevs,zmini,zmin,zmins

c
c     select phase type
c
      if(phtyp.eq.'P') iph = 1
      if(phtyp.eq.'S') iph = 2

c
c     reset onset table
c
      ddelta  = 0.d0
      t1      = 0.d0
      tcrust  = 0.d0
      del(1)  = 0.d0
      del(2)  = 0.d0
      time(1) = 0.d0
      time(2) = 0.d0

      do 7000 k = 1,2

      if(k.eq.1) jmodk = jmod
      if(k.eq.2) jmodk = jmods

      i2 = 0
      iz = 0
      il = 0 

      istart = 1

      DO 500 I=1,jmodk

      i2 = i2 + 1

c
c     first the crust in the local/regional model
c
      if(k.eq.1) then

         if(dabs(zo-z(i)).lt.1.d-3) iz = i

         if(i.gt.1 .and. iz.eq.0) then
            if(zo.lt.z(i).and.zo.gt.z(i-1)) then
               vnew = V0(iph,I-1) + 
     +               (V0(iph,I)-V0(iph,I-1)) * ((z(i)-zo)/(z(i)-z(i-1)))
               call efad(zo,vnew,H(I2),V(I2))
               iz = i2
               i2 = i2 + 1
            endif
         endif

         if(dabs(zmin-z(i)).lt.1.d-3) il = i

         if(i.gt.1 .and. il.eq.0) then
            if(zmin.lt.z(i).and.zmin.gt.z(i-1)) then
               vnew = V0(iph,I-1) + 
     +               (V0(iph,I)-V0(iph,I-1))   * 
     +               ((z(i)-zmin)/(z(i)-z(i-1)))
               call efad(zmin,vnew,H(I2),V(I2))
               il = i2
               i2 = i2 + 1
            endif
         endif

         call efad(z(I),V0(iph,I),H(I2),V(I2))

         if(lpwp .and. z(i).le.zw) then

            if(cortyp.eq.'rps' .or. cortyp.eq.'r') istart = i+1

            if(cortyp.eq.'rw' .and. (phtyp.eq.'S')) istart = i+1
 
         endif

c
c     then the crust in the standard model
c
      else if(k.eq.2) then

         if(dabs(zo-zs(i)).lt.1.d-3) iz = i

         if(i.gt.1 .and. iz.eq.0) then
            if(zo.lt.zs(i).and.zo.gt.zs(i-1)) then
               vnew = V0s(iph,I-1) + 
     +               ((V0s(iph,I)-V0s(iph,I-1))  * 
     +              ((zs(i)-zo)/(zs(i)-zs(i-1))) )
               call efad(zo,vnew,H(I2),V(I2))
               iz = i2
               i2 = i2 + 1
            endif
         endif

         if(dabs(zmins-zs(i)).lt.1.d-3) il = i

         if(i.gt.1 .and. il.eq.0) then
            if(zmins.lt.zs(i).and.zmins.gt.zs(i-1)) then
               vnew = V0s(iph,I-1) + 
     +               (V0s(iph,I)-V0s(iph,I-1))   * 
     +               ((zs(i)-zmins)/(zs(i)-zs(i-1)))
               call efad(zmins,vnew,H(I2),V(I2))
               il = i2
               i2 = i2 + 1
            endif
         endif

         call efad(zs(I),V0s(iph,I),H(I2),V(I2))

      endif

500   continue

      m = i2 - 1

      DO 800 I=1,M

      I2=I+1
      V2(I)=V(I2)

      IF(dabs(V2(I)-V(I)) .le. 0.001d0) THEN
         V2(I)=1.0001d0*V(I)
         V(I2)=V2(I)
      ENDIF

      zdiff=H(I2)-H(I)
      IF(dabs(zdiff).le.0.0001d0)  then
          zdiff=0.0001d0
          H(i2)= H(i2) + zdiff
      endif

      G(I)=(V2(I)-V(I))/zdiff
  
c     print *,i,H(I),V(I),H(i2),V2(I)
c     print *,i,H(I),V(I)

800   continue

      if(cortyp.eq.'s' .and. k.eq.1) vstat = v(1)

      T=0.D0
      R=0.D0

      if(il.gt.0 .and. il.le.m) m = il-1
      if(iz.gt.0 .and. iz.le.m) m = iz-1
 
c     print *, 'CRUST 3: ',k,phtyp,zo,rvv,pa,vv,vstat,istart,m,il,iz

      if(istart.gt.m) then
c       print *,'Something wrong with crustal correction for ',phtyp,
c    +          cortyp, ' Z0 is above possible ray coverage'
        ierc = 88
        go to 9999
      endif

      DO  1100  KK=istart,M

      E=V(KK)
      G1=(1.d0 - E*E*RV2)
      if(g1.lt.0.d0) go to 1101
      P=DSQRT(G1)

      F=V2(KK)
      G2=(1.d0-F*F*RV2)
      if(g2.le.0.d0) then
         F = VV
         Q = 0.d0
      else
         Q=DSQRT(G2)
      endif

      O=1.d0/G(KK)

      R=R+fa*(P-Q)*O
      DT=DLOG(F*(1.D0+P)/(E*(1.D0+Q)))*O
      T=T+fa*DT

1100  CONTINUE

1101  del(k)  = r/(aa*rvv)
      time(k) = t

7000  continue

      ddelta = del(1) - del(2)
      t1 = ddelta * pa
      tcrust = time(1) - time(2) - t1

      if(typctl.ge.8) then
         print *,'crust-correction: ',pa,phtyp,' times ',tcrust,'(',
     +     time(1),time(2),t1,')',' Del ',ddelta,'(',del(1),del(2),')' 
      endif

9999  RETURN
      END

c
c     subroutine crustc
c
c     driver for subroutine crust to calculate the travel-time effect 
c     for reflections at the Earth's surface for 
c     different crustal velocity structures and 
c     topography and calculates station corrections
c     for all kind of body phases.
c
c     Reflections can be corrected for the following principle 
c     phases:
c
c                 pP, pwP, sP, sS, pS 
c                 PP, SS, PS, SP
c                 P'P', S'S', P'S', S'P'
c 
c
      subroutine crustc(tcrust,delt,phase_t0,rayp0,depth,tcrustw,
     +                  deltw,mind,ind,iw,typctl)
c
c     input:
c              phase_t0 phase type (either P or S)
c
c              rayp0    ray parameter of phase in [s/deg]
c
c              depth    source depth
c
c              ind      switch for phase type
c
c                       = 1 station corrections for all phases
c
c                       = 2 depth phases pP, sS, pwP, ...
c
c                       = 3 surphase reflections PP, SS, PwP, ...
c
c                       = 4 converted depth phases sP, pS
c
c                       = 5 converted surphase reflections  SP & PS
c
c              iw       = 0 no water layer taken in account
c     
c                       > 0 water layer may be taken in account
c
c              mind     = index of crustal reference model
c
c     output:
c              tcrust   = calculated traveltime correction
c
c              delt     = calculated distance correction
c
c              tcrustw  = calculated traveltime correction including
c                         water layer
c
c              deltw    = calculated distance correction including
c                         water layer
c
c     October 2017
c
c     some corrections spring/sommer 2018
c
c     changed to handle also pwP phases if water layers at reflection points
c     many changes August 2020
c
c     more changes to handle pwP-type phases July 2022
c
c
c     calls SUBROUTINE CRUST
c
c

      implicit real*8 (a-h,o-z)

      real*8 rayp0, depth, tcrust, delt

      integer*4 mind, ind, typctl, ierr

      character*1 phase_t0, phase_r, cortyp*3

      logical lpw

      real*8 crustc1, crustc2

      real*8 rayp, zo

      tcrust  = 0.d0
      tcrustw = 0.d0
      delt    = 0.d0
      deltw   = 0.d0

      ierr = 0

      rayp = rayp0

      if(ind.eq.1) then
c
c     station correction 
c

        cortyp = 's'

        phase_r = phase_t0

        zo      = 9999.d0
        call crust(tcrust,delt,rayp,phase_r,zo,mind,cortyp,lpw,
     +             typctl,ierr)

c       print*,'crustc (s)  ',ind,rayp,phase_r,zo,mind,cortyp,
c    +          tcrust,delt

      else if(ind.eq.2 .or. ind.eq.3) then

c
c       reflection point correction, no converted phases
c
c       depth phases & surface multiples
c

        cortyp = 'r'
        phase_r = phase_t0

        if(ind.eq.2) then
           zo      = depth
        else if(ind.eq.3) then
           zo = 9999.d0
        endif

        call crust(tcrust,delt,rayp,phase_r,zo,mind,cortyp,lpw,
     +             typctl,ierr)

c       print*,'crustc (r)  ',ind,rayp,phase_r,zo,mind,cortyp,
c    +          tcrust,delt,lpw

        if (phase_r.eq.'P' .and. lpw .and. iw.gt.0) then

           cortyp = 'rw'

           call crust(tcrustw,deltw,rayp,phase_r,zo,mind,
     +          cortyp,lpw,typctl,ierr)

c          print*,'crustc (rw) ',ind,rayp,phase_r,zo,mind,cortyp,
c    +          tcrustw,deltw,lpw
        endif

      else if(ind.eq.4 .or. ind.eq.5) then

c
c       reflection point correction, converted phases only
c
c       depth phases & surface multiples
c

        cortyp = 'rps'

        phase_r = phase_t0

        crustc1 = 0.d0
        crustc2 = 0.d0
        delt1   = 0.d0
        delt2   = 0.d0

        if(ind.eq.4) then
           zo      = depth
        else if(ind.eq.5) then
           zo = 9999.d0
        endif

        call crust(crustc1,delt1,rayp,phase_r,zo,mind,cortyp,lpw,
     +             typctl,ierr)

        if(phase_t0 .eq. 'P') phase_r = 'S'
        if(phase_t0 .eq. 'S') phase_r = 'P'

        zo      = 9999.d0
        call crust(crustc2,delt2,rayp,phase_r,zo,mind,cortyp,lpw,
     +             typctl,ierr)

        tcrust = crustc1 + crustc2
        delt   = delt1 + delt2

c       print*,'crustc (rps)',ind,rayp,phase_r,zo,mind,cortyp,
c    +          tcrust,crustc1,crustc2, delt1, delt2, delt

      else

        ierr = 999

      endif

      if(ierr.gt.0) then
         tcrust  = 0.d0
         tcrustw = 0.d0
         delt    = 0.d0
         deltw   = 0.d0
      endif
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
