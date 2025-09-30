      subroutine tauget_mod(zs,delta,n,phcd,ttc,dtdd,dtdh,dddp,modnam)
c
c     This routine calls IASP91-type tau-spline travel-time tables produced 
c     with the software known as libtau.f and libsun.f .
c
c     If this software is not available, it can be retrieved from 
c     anonymous ftp-server at the USGS, at IRIS, and from the RSES, ANU, 
c     Canberra.
c
c     latest changes 20 March 2017, JS, NORSAR
c
      save 

      include 'ttimes.h'

      real*4 zs, zso, delta

      integer n

      character*20 modnam, modnamo

      logical first
      common /bkin0/first,modnamo,zso

      if(n.lt.0) then
         zso     = -999.
         modnamo = ' '
         n = 0
      endif

      if (modnam.ne.modnamo .or. abs(zs-zso).gt.1.e-2) then

         in = 1 

         if (modnam.ne.modnamo) then
             call tabin(in,modnam)
             first = .true.
         endif

         call depset(zs)

         modnamo = modnam
         zso     = zs

      endif

      call trtm(delta,n,ttc,dtdd,dtdh,dddp,phcd)

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine tauget_ray(phase,phtyp,rayp0,modnam,depth,delray,ttray,
     +                      rayflag )
c
c     This routine calls IASP91-type tau-spline travel-time tables produced 
c     with the software known as libtau.f and libsun.f .
c
c     If this software is not available, it can be retrieved from 
c     anonymous ftp-server at the USGS, at IRIS, and from the RSES, ANU, 
c     Canberra.
c
c     latest changes 31 May 2021, JS, NORSAR
c
      save

      real*8 rayp0, delray, ttray, depth
      character*8 phase,phtyp*1, phase_type*1

      logical rayflag, rayok

      character*20 modnam,modnamo

      real*4  zs, zso, tcor, xcor
      logical first
      common /bkin0/first,modnamo,zso

      rayflag    = .false.
      rayok      = .false.
      delray     = 0.d0
      ttray      = 0.d0
      tcor       = 0.
      xcor       = 0.
c

      if(phtyp.eq.' ') then
         if(phase.eq.' ' ) then
            if(rayp0.le.20.d0) then
               phtyp = 'P'
               phase = 'PKPdf'
            else
               phtyp = 'S'
               phase = 'S'
            endif
         else
            phtyp = phase_type(phase)
         endif
      endif

      zs   = sngl(depth)

      if (modnam.ne.modnamo .or. abs(zs-zso).le.1.e-3) then

         in = 1

         if(modnam.ne.modnamo) then
            call tabin(in,modnam)
            first = .true.
         endif
         call depset(zs)

         modnamo = modnam
         zso  = zs
      endif

      rayp = sngl(rayp0)

      itest = 0

100   call oneray(phase,rayp,xcor,tcor,rayok)

c     print *,phase,rayp,xcor,tcor,rayok

      if(rayok) then

         delray  = dble(xcor)
         ttray   = dble(tcor)
         rayflag = .true.

         go to 900

      else

         if(phtyp .eq. 'P') then

            if(rayp0.lt.5.d0 .and. itest.eq.0) then
               phase = 'PKPdf'
               itest = 1
               go to 100
            endif
     
            if(itest.eq.1) then

               if(phase.eq.'PKPdf') then
                 phase = 'PKPdif'
                 go to 100
               endif
     
               if(phase.eq.'PKPdif') then
                  phase = 'PKPbc'
                  go to 100
               endif
     
               if(phase.eq.'PKPbc') then
                  phase = 'PKPab'
                  go to 100
               endif
     
               if(phase.eq.'PKPab') then
                  phase = 'Pdif'
                  go to 100
               endif

               if(phase.eq.'Pdif') then
                  phase = 'PKiKP'
                  go to 100
               endif

            endif
     
            if(rayp0.le.21.d0 .and. itest.ge.0) then
               phase = 'P'
               itest = -1
               go to 100
            endif

            if(itest.lt.0) then

               if(phase.eq.'P') then
                  phase = 'Pn'
                  go to 100
               endif

               if(phase.eq.'Pn') then
                  phase = 'Pb'
                  go to 100
               endif

               if(phase.eq.'Pb') then
                  phase = 'Pg'
                  go to 100
               endif

            endif

         else if(phtyp .eq. 'S') then

            if(rayp0.lt.9.d0 .and. itest.eq.0) then
               phase = 'SKSdf'
               itest = 1
               go to 100
            endif

            if(itest.eq.1) then

               if(phase.eq.'SKSdf') then
                  phase = 'SKSdif'
                  go to 100
               endif

               if(phase.eq.'SKSdif') then
                  phase = 'SKSac'
                  go to 100
               endif

               if(phase.eq.'SKSac') then
                  phase = 'Sdif'
                  go to 100
               endif

            endif

            if(itest.eq.1) then
               phase = 'S'
               itest = -1
               go to 100
            endif

            if(itest.lt.0) then

               if(phase.eq.'S') then
                  phase = 'Sn'
                  go to 100
               endif

               if(phase.eq.'Sn') then
                  phase = 'Sb'
                  go to 100
               endif

               if(phase.eq.'Sb') then
                  phase = 'Sg'
                  go to 100
               endif

            endif

         endif

      endif

900   continue

      return
      end



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine ellip(ecolatr,azi,del,zo,phas,p,ecor,ierr)
c
c****6789012345678901234567890123456789012345678901234567890123456789012
c
c     Subroutine ellip calls a routine to calculates the ellipticity 
c     correction for a given source-receiver combination for seismic 
c     phases of the AK135 tables (as far as availablbe). Approximations
c     are used for several not defined phases.
c
c     After Brian Kennett (pers. communication) does this set of 
c     ellipticity corrections also work fine with IASP91 tables. 
c     Therefore, we will only use this set of tables in HYPOSAT for all 
c     different velocity models.
c
c     Johannes Schweitzer, NORSAR, February 2010
c
c     input:  ecolatr       geocentric colatitude of the event in rad
c
c             azi           azimuth from event to station in deg
c
c             del           distance between event and station in deg
c
c             zo            event depth in km
c
c             phas          phase name
c
c             p             ray paramter of phase in sec/deg
c
c     output: ecor          ellipticity correction of this phase in sec
c
c             ierr          error status
c
c
c version:  25. October 1996,  johannes schweitzer
c
c           03 May 2021, changed to double precision
c

      integer ierr
      real*8  ecolatr,azi,p,ecor,del,zo
      real*8  ecolate,p1,delo,azi1,deg2rad
      character*8 phas, phas1
      logical errf

c
c     definition of several constants:
c
      deg2rad = datan(1.d0) / 45.d0
      ierr    = 0
      ecor    = 0.d0
      azi1    = azi
      delo    = del
      p1      = p
      ecolate = ecolatr

      phas1 = phas

c
c     Search for multiple core phases observed at distance 0 deg,
c     but not for .PKi... or .SKi... phases.
c
c     In this case, the phases travelled once around the Earth and 
c     ray parameter p1 (=0.!) is not an indication for this!
c
      if(delo.eq.0.d0 .and. 
     *   (phas1(3:3).ne.'i'.and.phas1(4:4).ne.'i') ) then
         if (phas1(1:2).eq.'PK'.or.phas1(1:2).eq.'SK') p1=-999.d0
         if (phas1(1:2).eq."P'".or.phas1(1:2).eq."S'") p1=-999.d0
         if (phas1(2:3).eq.'PK'.or.phas1(2:3).eq.'SK') p1=-999.d0
         if (phas1(2:3).eq."P'".or.phas1(2:3).eq."S'") p1=-999.d0
      endif

      if(p1.lt.0.d0) then
         delo = 360.d0 - delo
         azi1 = azi1 - 180.d0
         if(azi1.lt.0.d0) azi1 = 360.d0 + azi1
      endif

      azi1    = deg2rad*azi1

      index = 3
      call elpcor(phas1,delo,zo,ecolate,azi1,ecor,errf,index)

      if(errf) then
         ierr = 999
         ecor = 0.d0
      endif

      return
      end
C=======================================================================
        SUBROUTINE elpcor(phase,edist,edepth,ecolat,azim,tcor,abrt,ind)
c
c       SUBROUTINE ellip()
C                                                                         
C    Ellipticity correction for any given phase using
C    Dziewonski & Gilbert representation
C                                                   
C      The ellipticity corrections are found by linear interpolation       
C    in terms of values calculated for the ak135 model for a wide 
C    range of phases to match the output of the iasp software 
C
Cccj.s.     first call:  ellref(ecolat) 
Cccj.s.                        - to set up source dependent constants
C     2nd call  :  ellcor(phase,edist,depth,ecolat,azim,tcor,abrt) 
C                        - to calculate correction for a station
C
C    Parameters: 
C    character  
C          phase : a  string specifying the PHASE,   -e.g P, ScP etc.  
C                                                        
C    real*8
C          edist  :  epicentral distance to station (in degrees)     
C          edepth :  depth of event         
C          ecolat :  epicentral co-latitude of source (in radians) 
C          azim   :  azimuth from source to station (in radians)
C                                
C          tcor   :  time correction for path to allow for ellipticity
c
c          ind          steers subroutine:
c              <= 1     initial file reading
c              = 2      epicenter setting
c              > 2      ellipticity correction calculation
c
C 
C    logical 
C          abrt   :  a logical variable -usally set to .FALSE.  
C                    which is set to .TRUE. if a phase for      
C                    which no data is available is chosen       
c
c          lread  : set to true after first read
c
c    call:
c           function phase_alias
C                                                                         
C=======================================================================
C   B.L.N. Kennett RSES,ANU        May 1995, August 1996                 
C   (based on earlier routine by D.J. Brown)
C   with input from W. Spakman, Utrecht
C=======================================================================
c
c    Slightly changed version:
c           input of data (path via environment variable HYPOSAT_DATA)
c           calling name elpcor
c           no initial call
c
c    October 1996 J. Schweitzer, Bochum
c
c       environment variable name corrected: Jan 27, 1997
c
c    May 2021 JS:
c    changed to double precision
c    reading changed from 'direct access' readings to one time input
c
c
c     save sc0,sc1,sc2

      character *(*) phase

      integer numph, nd
      parameter (numph=57, nd=6)

c     integer Ne,ind,np(numph)
      integer ind,np(numph)

      character*8 phcod(numph)
      integer phind(numph),phspn(numph),phnch(numph)
      real*8 edist,edepth,ecolat,azim,
     ^       sc0,sc1,sc2,s3,tcor,
     ^       tau0, a0,b0,h0,s0,e0,f0,g0,
     ^       tau1, a1,b1,h1,s1,e1,f1,g1,
     ^       tau2, a2,b2,h2,s2,e2,f2,g2,
     ^       deldst
      real*8 dpth(nd),delta(numph,50), d1(numph),d2(numph)
      real*8 t0(numph,50,nd),t1(numph,50,nd),t2(numph,50,nd)
      logical abrt

      integer phase_alias

      save
c
c j.s.
c     
      character ic*512, file_check*512
      data phcod/
     & 'Pup   ','P     ','Pdif  ','PKPab ','PKPbc ','PKPdf ',
     & 'PKiKP ','pP    ','pPKPab','pPKPbc','pPKPdf','pPKiKP',
     & 'sP    ','sPKPab','sPKPbc','sPKPdf','sPKiKP','PcP   ',
     & 'ScP   ','SKPab ','SKPbc ','SKPdf ','SKiKP ','PKKPab',
     & 'PKKPbc','PKKPdf','SKKPab','SKKPbc','SKKPdf','PP    ',
     & "P'P'  ",'Sup   ','S     ','Sdif  ','SKSac ','SKSdf ',
     & 'pS    ','pSKSac','pSKSdf','sS    ','sSKSac','sSKSdf',
     & 'ScS   ','PcS   ','PKSab ','PKSbc ','PKSdf ','PKKSab',
     & 'PKKSbc','PKKSdf','SKKSac','SKKSdf','SS    ',"S'S'  ",
     & 'SP    ','PS    ','PnS   '/
      data phind/
     &        1,      14,      91,     136,     165,     178,
     &      235,     364,     433,     462,     475,     532,
     &      661,     742,     771,     784,     841,     970,
     &     1047,    1100,    1113,    1134,    1195,    1316,
     &     1337,    1382,    1507,    1516,    1573,    1702,
     &     1827,    1932,    1945,    2022,    2067,    2132,
     &     2197,    2234,    2295,    2356,    2425,    2490,
     &     2551,    2628,    2681,    2694,    2711,    2772,
     &     2781,    2838,    2967,    3140,    3273,    3398,
     &     3587,    3656,    3697/
      data phspn/
     &        3,      19,      11,       7,       3,      14,
     &       32,      17,       7,       3,      14,      32,
     &       20,       7,       3,      14,      32,      19,
     &       13,       3,       5,      15,      30,       5,
     &       11,      31,       2,      14,      32,      31,
     &       26,       3,      19,      11,      16,      16,
     &        9,      15,      15,      17,      16,      15,
     &       19,      13,       3,       4,      15,       2,
     &       14,      32,      43,      33,      31,      47,
     &       17,      10,       6/ 
      data phnch/
     &        3,       1,       4,       5,       5,       5,
     &        5,       2,       6,       6,       6,       6,
     &        2,       6,       6,       6,       6,       3,
     &        3,       5,       5,       5,       5,       6,
     &        6,       6,       6,       6,       6,       2,
     &        4,       3,       1,       4,       5,       5,
     &        2,       6,       6,       2,       6,       6,
     &        3,       3,       5,       5,       5,       6,
     &        6,       6,       6,       6,       2,       4,
     &        2,       2,       3/ 
      data dpth/ 0.d0, 100.d0, 200.d0, 300.d0, 500.d0, 700.d0 /

c...
c     In addition to the phase names listed above a number of phase 
c     aliases are available in the routine phase_alias, e.g. Pn --> 
c     P etc. The input phase code is first checked against the phcod 
C     array and next against the phase aliases.
c<sc>
c	           initial call to set up source dependent constants
cj.s. entry ellref(ecolat)
c                                            

      if (ind.le.1) then
c                                              acquire phase information
c
c     changes for Bochum Oct 23, 1996 J.S.
c
c          open ellipticity correction file saved in varibale 'ic'
c
       ic = file_check('elcordir.tbl')
       open(35,file=trim(ic),access='direct',form='formatted',recl=80) 

       do 6 j = 1,numph
          nr = phind(j)
c         print*, 'nrec:',nr

          read(35,61,rec=nr) phcod(j),np(j),d1(j),d2(j)
c         print*, 'phcode,np,d1,d2: ', j,phcod(j),np(j),d1(j),d2(j)
          nr = nr+1
          do 5 i=1,np(j)
            read(35,62,rec=nr) delta(j,i)
            nr = nr+1
            read(35,63,rec=nr) (t0(j,i,m),m=1,6)
            nr = nr+1
            read(35,63,rec=nr) (t1(j,i,m),m=1,6)
            nr = nr+1
            read(35,63,rec=nr) (t2(j,i,m),m=1,6)
            nr = nr+1
5         continue         
6      continue         

 61    format(a8,i10,2f10.0)
 62    format(f10.0)
 63    format(6f10.4)

c 
       close (35)
       return

      endif


      if (ind.eq.2) then

cj.s. s3 = sqrt(3.0)/2.0
         s3 = dsqrt(0.75d0)
         sc0 = 0.25d0*(1.d0+3.d0*dcos(2.d0*ecolat))
         sc1 = s3*dsin(2.d0*ecolat)
         sc2 = s3*dsin(ecolat)*dsin(ecolat)

c        print *,sc0,sc1,sc2,ecolat
         return

      endif

c<sc>
c<ec>                                           phase identification
cj.s. entry ellcor(phase,edist,edepth,ecolat,azim,tcor,abrt)
c      print *, 'phase,edist,edepth,ecolat,azim'
c      print *,  phase,edist,edepth,ecolat,azim
c     Nd = 6
c     NUMPH = 57

      deldst = 5.0d0
      abrt = .FALSE.
c                                       check on the length of phase
c     l=len(phase)
c     if(l.lt.8) then
c      stop 
c    >    'character variable `phase` should have at least length 8'
c     endif

c                                             select phase
      ip = -1
c
c j.s.      nc=min(lnblk(phase),8)
c     to reduce code, use only one function to count characters
c
      nc=min(len_trim(phase),8)
      do 10 i=1,NUMPH
c     print *,i,phase,nc,phcod(i),phnch(i)
        if(nc.ne.phnch(i)) goto 10
        if (phase(1:nc) .eq. phcod(i)(1:nc)) then
          ip = i
          go to 11
        endif
 10   continue
 11   continue

      if(ip.eq.-1) then
c                                             check phase aliases
        ip = phase_alias(phase,edist)
      endif
c                                              phase not found
c     print *, 'ip:',ip,phase
      if(ip.lt.0) then
c       print *, phase,'  is not available'
        abrt = .true.
        return
      endif

c     Ne = phspn(ip)
      if(np(ip).ne.phspn(ip)) then
        print*, 'ELPCOR: HELP! - index wrong for ellip. corrections'
        abrt = .true.
        return
      endif
c                                          special case of upgoing waves
*
c
c      nr = phind(ip)

       nr = ip
c                                  distance index
       idist = 1 + idint( (edist-d1(nr))/ deldst )
       if(edist.lt.d1(nr)) idist = 1
       if(edist.gt.d2(nr)) idist = np(nr)-1
c                                  depth index
       do 25 j = 1,Nd-1
         if ((dpth(j).le.edepth).and.(dpth(j+1).ge.edepth))then
            jdepth = j
            goto 26
         endif
 25    continue
 26    continue
c      print *, 'idist, jdepth;',idist,jdepth
c
*                      need to allow for zero entries (where phase
*                      description strongly depth dependent)
c tau0
         a0 = t0(nr,idist,jdepth)
         b0 = t0(nr,idist,jdepth+1)
         h0 = t0(nr,idist+1,jdepth+1)
         s0 = t0(nr,idist+1,jdepth)
         e0 = a0 + (s0-a0)*(edist-delta(nr,idist))/(delta(nr,idist+1)-
     ^        delta(nr,idist))
         f0 = b0 + (h0-b0)*(edist-delta(nr,idist))/(delta(nr,idist+1)-
     ^        delta(nr,idist))
         g0 = e0 + (f0-e0)*
     ^             (edepth-dpth(jdepth))/(dpth(jdepth+1)-dpth(jdepth))
         tau0 = g0
c tau1
         a1 = t1(nr,idist,jdepth)
         b1 = t1(nr,idist,jdepth+1)
         h1 = t1(nr,idist+1,jdepth+1)
         s1 = t1(nr,idist+1,jdepth)
         e1 = a1 + (s1-a1)*(edist-delta(nr,idist))/(delta(nr,idist+1)-
     ^        delta(nr,idist))
         f1 = b1 + (h1-b1)*(edist-delta(nr,idist))/(delta(nr,idist+1)-
     ^        delta(nr,idist))
         g1 = e1 + (f1-e1)*
     ^             (edepth-dpth(jdepth))/(dpth(jdepth+1)-dpth(jdepth))
         tau1 = g1
c tau2
         a2 = t2(nr,idist,jdepth)
         b2 = t2(nr,idist,jdepth+1)
         h2 = t2(nr,idist+1,jdepth+1)
         s2 = t2(nr,idist+1,jdepth)
         e2 = a2 + (s2-a2)*(edist-delta(nr,idist))/(delta(nr,idist+1)-
     ^        delta(nr,idist))
         f2 = b2 + (h2-b2)*(edist-delta(nr,idist))/(delta(nr,idist+1)-
     ^        delta(nr,idist))
         g2 = e2 + (f2-e2)*
     ^             (edepth-dpth(jdepth))/(dpth(jdepth+1)-dpth(jdepth))
         tau2 = g2
c
c        print *, 'tau0,tau1,tau2:',tau0,tau1,tau2
c j.s.   caz = cos(azim)
c j.s.   cbz = cos(2.0*azim)
c         print *, 'azim,caz,cbz',azim,caz,cbz    
c
         tcor= sc0*tau0 + sc1*dcos(azim)*tau1 + sc2*dcos(2.d0*azim)*tau2
c
      return
c<ec>
      end
      integer function phase_alias(phase,delta)

c     check for alternative phase names
c     to get ellipticity corrections
c
c     input phase, delta
c     output ip (index of phcod)

      character*(*) phase
      real*8        delta
      integer       ip

      ip = -1

      if(phase(1:3).eq.'Pg ') then
c       phase='P       '
        ip=2
      else if(phase(1:3).eq.'Sg ') then
c       phase='S       '
        ip=33
      else if(phase(1:3).eq.'Lg ') then
c       phase='S       '
        ip=33
      else if(phase(1:4).eq.'pPg ') then
c       phase='pP      '
        ip=8
      else if(phase(1:4).eq.'sPg ') then
c       phase='sP      '
        ip=13
      else if(phase(1:4).eq.'pSg ') then
c       phase='pS      '
        ip=37
      else if(phase(1:4).eq.'sSg ') then
c       phase='sS      '
        ip=40
c
      elseif(phase(1:3).eq.'Pb ') then
c       phase='P       '
        ip=2
      else if(phase(1:3).eq.'Sb ') then
c       phase='S       '
        ip=33
      else if(phase(1:4).eq.'pPb ') then
c       phase='pP      '
        ip=8
      else if(phase(1:4).eq.'sPb ') then
c       phase='sP      '
        ip=13
      else if(phase(1:4).eq.'pSb ') then
c       phase='pS      '
        ip=37
      else if(phase(1:4).eq.'sSb ') then
c       phase='sS      '
c
      elseif(phase(1:3).eq.'Pn ') then
c       phase='P       '
        ip=2
      else if(phase(1:3).eq.'Sn ') then
c       phase='S       '
        ip=33
      else if(phase(1:4).eq.'pPn ') then
c       phase='pP      '
        ip=8
      else if(phase(1:4).eq.'sPn ') then
c       phase='sP      '
        ip=13
      else if(phase(1:4).eq.'pSn ') then
c       phase='pS      '
        ip=37
      else if(phase(1:4).eq.'sSn ') then
c       phase='sS      '
        ip=40
      else if(phase(1:4).eq.'SPn ') then
c       phase='SP      '
        ip=55
      else if(phase(1:4).eq.'SPb ') then
c       phase='SP      '
        ip=55
      else if(phase(1:4).eq.'SPg ') then
c       phase='SP      '
        ip=55
      else if(phase(1:4).eq.'SnP ') then
c       phase='SP      '
        ip=55
      else if(phase(1:4).eq.'PSn ') then
c       phase='PS      '
        ip=56
      else if(phase(1:5).eq.'PgPg') then
c       phase='PP      '
        ip=30
      else if(phase(1:5).eq.'PbPb') then
c       phase='PP      '
        ip=30
      else if(phase(1:5).eq.'PnPn ') then
c       phase='PP      '
        ip=30
      else if(phase(1:5).eq.'SgSg ') then
c       phase='SS      '
        ip=53
      else if(phase(1:5).eq.'SbSb ') then
c       phase='SS      '
        ip=53
      else if(phase(1:5).eq.'SnSn ') then
c       phase='SS      '
        ip=53
      else if(phase(1:4).eq.'PbP ') then
c       phase='P       '
        ip=2
      else if(phase(1:4).eq.'PmP ') then
c       phase='P       '
        ip=2
      else if(phase(1:4).eq.'SbS ') then
c       phase='S       '
        ip=33
      else if(phase(1:4).eq.'SmS ') then
c       phase='S       '
        ip=33
      else if(phase(1:4).eq.'PbS ') then
c       phase='PS      '
        ip=56
      else if(phase(1:4).eq.'PmS ') then
c       phase='PS      '
        ip=56
      else if(phase(1:4).eq.'SbP ') then
c       phase='SP      '
        ip=55
      else if(phase(1:4).eq.'SmP ') then
c       phase='SP      '
        ip=55
c
c     approximations for depth pahses
c
      else if(phase(1:6).eq.'pPgPg ') then
c       phase='PP      '
        ip=30
      else if(phase(1:6).eq.'pPbPb ') then
c       phase='PP      '
        ip=30
      else if(phase(1:6).eq.'pPnPn ') then
c       phase='PP      '
        ip=30
      else if(phase(1:5).eq.'pPP  ') then
c       phase='PP      '
        ip=30
      else if(phase(1:6).eq.'sSgSg ') then
c       phase='SS      '
        ip=53
      else if(phase(1:6).eq.'sSbSb ') then
c       phase='SS      '
        ip=53
      else if(phase(1:6).eq.'sSnSn ') then
c       phase='SS      '
        ip=53
      else if(phase(1:5).eq.'sSS  ') then
c       phase='SS      '
        ip=53
      else if(phase(1:5).eq.'pPcP ') then
c       phase='PcP     '
        ip=18
      else if(phase(1:5).eq.'pPcP ') then
c       phase='PcP     '
        ip=18
      else if(phase(1:5).eq.'pPcP ') then
c       phase='PcP     '
        ip=18
      else if(phase(1:5).eq.'pPcP ') then
c       phase='PcP     '
        ip=18
      else if(phase(1:5).eq.'sScS ') then
c       phase='ScS     '
        ip=43
c                                       upgoing P, S
      else if(phase(1:2).eq.'p ') then
c       phase='Pup     '
        ip=1  
      else if(phase(1:2).eq.'s ') then
c       phase='Sup     '
        ip=32 
c                                        
      else if(delta.le.100.d0.and.phase.eq.'pPdif   ') then
c       phase='pP      '
        ip=8
      else if(delta.le.100.d0.and.phase.eq.'sPdif   ') then
c       phase='sP      '
        ip=13
      else if(delta.le.100.d0.and.phase.eq.'pSdif   ') then
c       phase='pS      '
        ip=37
      else if(delta.le.100.d0.and.phase.eq.'sSdif   ') then
c       phase='sS      '
        ip=40
      else if(delta.le.165.d0.and.phase.eq.'PKPdif  ') then
c       phase='PKPbc '
        ip=5
      else if(delta.le.165.d0.and.phase.eq.'pPKPdif ') then
c       phase='pPKPbc '
        ip=10
      else if(delta.le.165.d0.and.phase.eq.'sPKPdif ') then
c       phase='sPKPbc '
        ip=15
c                             
      else if(phase(1:6).eq."P'P'P'") then
c       phase="P'P'P'  "
        ip =-1
c                             
      else if(phase(1:4).eq."P'P'") then
c       phase="P'P'    "
        ip =31
      else if(phase(1:5).eq."pP'P'") then
c       phase="P'P'    "
        ip =31
      else if(phase(1:6).eq."S'S'S'") then
c       phase="S'S'S'  "
        ip =-1
      else if(phase(1:4).eq."S'S'") then
c       phase="S'S'    "
        ip =54
      else if(phase(1:5).eq."sS'S'") then
c       phase="S'S'    "
        ip =54
c                            diffractions (approx)
      else if(delta.gt.100.d0.and.phase.eq.'pPdif   ') then
c       phase='Pdif    '
        ip=3
      else if(delta.gt.100.d0.and.phase.eq.'sPdif   ') then
c       phase='Pdif    '
        ip=3
      else if(delta.gt.100.d0.and.phase.eq.'pSdif   ') then
c       phase='Sdif    '
        ip=34
      else if(delta.gt.100.d0.and.phase.eq.'sSdif   ') then
c       phase='Sdif     '
        ip=34
      endif

      phase_alias = ip
      return
      end
