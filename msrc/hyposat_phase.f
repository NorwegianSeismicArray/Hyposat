      function phase_type(phid0)
c
c           last corrections :  16 February 1997
c                               correcting range of characters to check
c
c                               November 3, 1997
c                               corrected for AB and AC-branches
c
c                               October 8, 2002
c                               corrected for P1 and S1 phase names
c                               and new IASPEI phase names like P4 ...
c
c                               27 August 2005
c
c                               07 September 2020
c                               some code changes
c                               multiple phases like PP & SSS added
c
c                               February 2023
c                               Surface waves added
c
      character phid0*8, phase_type*1, phid*8
      integer icph

      phid = phid0
      phase_type=' '

      icph = len_trim(phid)

      if(icph.le.1) go to 10

      if(ichar(phid(icph:icph)).gt.48 .and.
     +   ichar(phid(icph:icph)).lt.58   )   icph=icph-1

5     if(icph.le.1) go to 10

      if(icph.ge.4) then
         if(phid(icph-2:icph).eq.'dif') then
            icph=icph-3
            go to 5
         endif
         if(phid(icph-2:icph).eq.'pre'.or.phid(icph-2:icph).eq.
     +      'PRE') then
            icph=icph-3
            go to 5
         endif
      endif

      if(icph.ge.3) then

         if(phid(icph-1:icph).eq.'ab') then
            icph=icph-2
            go to 5
         endif
         if(phid(icph-1:icph).eq.'ac') then
            icph=icph-2
            go to 5
         endif
         if(phid(icph-1:icph).eq.'bc') then
            icph=icph-2
            go to 5
         endif
         if(phid(icph-1:icph).eq.'df') then
            icph=icph-2
            go to 5
         endif

      endif

      if(phid(icph:icph).eq.'n') then
         icph=icph-1
         if(icph.le.1) go to 10
      endif
      if(phid(icph:icph).eq.'g') then
         icph=icph-1
         if(icph.le.1) go to 10
      endif
      if(phid(icph:icph).eq.'b') then
         icph=icph-1
         if(icph.le.1) go to 10
      endif

      if(phid(icph:icph).eq."'") then
         icph=icph-1
         if(icph.le.1) go to 10
      endif
      if(phid(icph:icph).eq.'*') then
         icph=icph-1
      endif

10    if(icph.le.1) icph=1

      if(phid(icph:icph).eq.'P') phase_type='P'
      if(phid(icph:icph).eq.'S') phase_type='S'
      if(phid(1:3).eq.'Lg ')     phase_type='S'
      if(phid(1:3).eq.'Rg ')     phase_type='L'
      if(phid(1:3).eq.'LR ')     phase_type='L'
      if(phid(1:3).eq.'LQ ')     phase_type='L'
      if(phid(1:3).eq.'L  ')     phase_type='L'

      return
      end

c
c     subroutine check_phase (phid,imin,del,dis,dt,lsurf,lcon,lmoh)
c
c     phid = phase name (input & output)
c     imin = index for next check 
c            1 - phase name can be changed again
c            2 - no further phase name changes
c     del = epicentral distance in deg
c     dis = epicentral distance in km
c     dt  = traveltime residual
c
c     lcon = source deeper than Conrad discontinuity
c     lmoh = source deeper than Moho discontinuity
c     lsurf = phase is a surface wave
c
c     April 2021, Johannes Schweitzer, NORSAR
c
c     latest changes 28 June 2022 (depth phases added)
c
      subroutine check_phase (phid,imin,del,dis,dt,lsurf,lcon,lmoh)
      character*8  phid
      integer      imin
      real*8       del, dis, dt 
      logical      lsurf, lcon, lmoh

      character*8  phid2

c
c  depth phases
c
      if (phid(1:1).eq.'p') then
         if(phid(1:2).eq.'pw') then
            phid2 = phid
            phid = 's' // phid2(3:8) // ' '
         else
            phid(1:1) = 's'
         endif 
         imin = 2
         goto 100
      endif 

      if( phid(1:1).eq.'s') then
         phid(1:1) =  'p'
         imin = 2
         goto 100
      endif

c
c  regional phases
c
      if (phid(1:2).eq.'P ' .and. del.lt.13.d0) then
         phid = 'Pn'
         imin = 1
         goto 100
      endif

      if (phid.eq.'Pn') then
         if(dis.le.400.d0 .and. .not.lmoh) then
            phid = 'Pb'
            imin = 1
            goto 100
         else if(dis.gt.400.d0 .and. dt.gt.0.d0) then
            if(del.lt.13.d0) then
               phid = 'PnPn'
               imin = 1
            else
               phid = 'P '
               imin = 2
            endif
            goto 100
         endif
      endif

      if (phid.eq.'PnPn' .and. .not.lmoh) then
         phid = 'Pb'
         imin = 1
         goto 100
      endif

      if (phid.eq.'Pb' .and. .not.lcon) then
         phid = 'Pg'
         imin = 1
         goto 100
      endif

      if (phid.eq.'Pg') then
         if(dt.lt.0.d0) then
            if(dis.gt.150.d0) then
               phid = 'Pn'
            else if(.not.lmoh) then
               phid = 'Pb'
            endif
            imin = 2
            goto 100
         else if(dt.gt.0.d0) then
            if(dis.lt.150.d0) phid = 'Pn'
            imin = 2
            goto 100
         endif
      endif

      if (phid(1:2).eq.'S ' .and. del.lt.13.d0) then
         phid = 'Sn'
         imin = 1
         goto 100
      endif

      if (phid.eq.'Sn') then
         if(dis.le.350.d0 .and. .not.lmoh) then
            phid = 'Sb'
            imin = 1
            goto 100
         else if(dis.gt.350.d0 .and. dt.gt.0.d0) then
            if(del.lt.13.d0) then
               phid = 'SnSn'
               imin = 1
            else
               phid = 'S '
               imin = 2
            endif
            goto 100
         endif
      endif

      if (phid.eq.'SnSn' .and. .not.lmoh) then
         phid = 'Sb'
         imin = 1
         goto 100
      endif

      if (phid.eq.'Sb' .and. .not.lcon) then
         phid = 'Sg'
         imin = 1
         goto 100
      endif

      if(phid.eq.'Lg' .and. dt.lt.0.d0) then
         phid = 'Sn'
         imin = 1
         lsurf = .false.
         goto 100
      endif

      if (phid.eq.'Sg') then
         if(dt.lt.0.d0) then
            if(dis.gt.150.d0) then
               phid = 'Sn'
            else
               phid = 'Sb'
            endif
            imin = 2
            goto 100
         else  if(dt.gt.0.d0) then
            if(dis.lt.150.d0) then
               phid = 'Sn'
            else
               phid = 'Lg'
               lsurf = .true.
            endif
            imin = 2
            goto 100
         endif
      endif

c
c core phases
c
      if(del.gt.113.d0 .and. phid(1:3).eq.'Pdi' .and. dt.gt.0.d0) then
         phid ='PKPdf'
         imin = 1
         goto 100
      endif

      if (phid.eq.'PKPbc' .and. del.gt.143.d0) then
         if(dt.gt.0.d0) then
            phid = 'PKPab'
            imin = 2
            goto 100
         else if(dt.lt.0.d0) then
            phid = 'PKPdf'
            imin = 1 
            goto 100
         endif
      endif

      if (phid.eq.'PKPab' .and. del.le.156.d0) then
         if(dt.lt.0.d0) then
            phid = 'PKPbc'
            imin = 1
            goto 100
         endif
      else if(phid.eq.'PKPab' .and. del.gt.156.d0 .and. dt.lt.0.d0) then
         phid = 'PKPdf'
         imin = 2
         goto 100
      endif

      if (phid.eq.'PKPdf' .and. dt.lt.0.d0 ) then
         if(del.le.103.d0) then
            phid = 'P'
            imin = 1
            goto 100
         else if(del.le.143.d0) then
            phid = 'Pdif'
            imin = 2
            goto 100
         else if(del.le.150.d0) then
            phid = 'PKPbc'
            imin = 2
            goto 100
         endif
      else if(phid.eq.'PKPdf' .and. dt.gt.0.d0 ) then
         if(del.le.143.d0 .and. del.gt.113.d0) then
               phid = 'PKiKP'
               imin = 2
               goto 100
         else if (del.gt.143.d0 .and. del.le.156.d0) then
            phid = 'PKPbc'
            imin = 2
            goto 100
         else if (del.gt.156.d0) then
            phid = 'PKPab'
            imin = 2
            goto 100
         endif
      endif

      imin = -99

100   continue
      return
      end

c
c     subroutine testphase (phid0,icha,dis)
c
c     some changes added for surface-reflections
c
c     3 August 1997, Johannes Schweitzer, NORSAR
c
c     13 February 2002: PKPab + PKPdif added
c
c     corrections 04 September 2005
c
c     2 February 2018: P'P'P' + S'S'S' added
c
c     October 2020 & April 2021 some changes in logics
c  
      subroutine testphase (phid0,icha,dis)
      real*8 dis
      integer icha
      character phid*8,phid0*8,s*1, phidd*8
      logical flags

      flags = .false.
      phid = phid0
      s    = ' '

      iw = index(phid0,'w')
      if(iw.gt.1) then
         phid  = phid0(1:iw-1) // phid0(iw+1:8) // ' '
      endif

      if(phid(1:1).eq.'p' .or. phid(1:1).eq.'s') then
         phidd = phid
         flags = .true.
         s    = phidd(1:1)
         phid = phidd(2:8) // ' '
      endif

      if(phid(1:1).ne.'P' ) go to 20

      if (dis.gt.113.d0) then
         if(icha.ge.7) go to 50
         if(phid.eq.'Pg' .or. phid.eq.'Pb' .or. phid.eq.'Pn') then
            phid='P'
            goto 100
         endif
         if(phid(1:4).eq.'Pdif' .or. phid.eq.'P') then
            phid='PKPdf'
            goto 100
         endif
         if(phid(1:4).eq.'PKP ') then
            phid='PKPdf'
            goto 100
         endif
         if(phid(1:5).eq.'PKPdf' .and. dis.lt.160.d0) then
            phid='PKiKP'
            goto 100
         endif
         if(dis.gt.143.d0) then
            if(dis.lt.160.d0) then
               if(phid(1:6).eq.'PKiKP' .or. phid(1:5).eq.'PKPdf') then
                  phid='PKPbc'
                  goto 100
               endif
               if(phid(1:5).eq.'PKPbc') then
                  phid='PKPab'
                  goto 100
               endif
            endif
            if(dis.gt.155.d0) then
               if(phid(1:6).eq.'PKiKP' .or. phid(1:5).eq.'PKPdf') then
                  phid='PKPdif'
                  goto 100
               endif
               if(phid(1:5).eq.'PKPbc') then
                  phid='PKPdif'
                  goto 100
               endif
               if(phid(1:5).eq.'PKPdif') then
                  phid='PKPab'
                  goto 100
               endif
            endif
            if(phid(1:5).eq.'PKPab') then
               phid='Pdif'
               goto 100
            endif
         endif
         go to 50
      endif

      if(icha.ge.7) go to 50

      if(phid(1:4).eq.'Pdif' .and. dis.lt.105.d0) then
         phid='P'
         goto 100
      endif

      if(dis.lt.3.d0) then
         if(phid.eq.'PmP') then
            phid='Pn'
            goto 100
         endif
         if(phid.eq.'PbP') then
            phid='Pb'
            goto 100
         endif
      else
         if(phid.eq.'PmP' .or. phid.eq.'PbP') then
            phid='Pg'
            goto 100
         endif
      endif

      if(phid.eq.'P') then
         if(dis.lt.25.d0) then
            phid='Pn'
            goto 100
         else
            goto 50
         endif
      endif

      if(phid.eq.'Pn') then
         phid='Pb'
         goto 100
      endif
      if(phid.eq.'Pb') then
         phid='Pg'
         goto 100
      endif
      if(phid.eq.'Pg') then
         phid='P'
         goto 100
      endif

      if(phid(1:5).eq."P'P' ") then
         phid="P'P'ab"
         goto 100
      endif
      if(phid(1:6).eq."P'P'ab") then
         phid="P'P'bc"
         goto 100
      endif
      if(phid(1:6).eq."P'P'bc") then
         phid="P'P'df"
         goto 100
      endif
      if(phid(1:7).eq."P'P'P' ") then
         phid="P'P'P'ab"
         goto 100
      endif
      if(phid(1:8).eq."P'P'P'ab") then
         phid="P'P'P'bc"
         goto 100
      endif
      if(phid(1:8).eq."P'P'P'bc") then
         phid="P'P'P'df"
         goto 100
      endif
      if(phid(1:6).eq.'PKKP  ') then
         phid='PKKPab'
         goto 100
      endif
      if(phid(1:6).eq.'PKKPab') then
         phid='PKKPbc'
         goto 100
      endif
      if(phid(1:6).eq.'PKKPbc') then
         phid='PKKPdf'
         goto 100
      endif

      if(phid.eq.'PP') then
         phid='PnPn'
         goto 100
      endif

      if(phid.eq.'PnPn') then
         phid='PbPb'
         goto 100
      endif

      if(phid.eq.'PbPb') then
         phid='PgPg'
         goto 100
      endif

      if(phid.eq.'PgPg') then
         phid='PP'
         goto 100
      endif

      if(phid.eq.'PPP') then
         phid='PnPnPn'
         goto 100
      endif

      if(phid.eq.'PnPnPn') then
         phid='PbPbPb'
         goto 100
      endif

      if(phid.eq.'PbPbPb') then
         phid='PgPgPg'
         goto 100
      endif

      if(phid.eq.'PgPgPg') then
         phid='PPP'
         goto 100
      endif

      if(phid.eq.'PPPP') then
         phid='PPP'
         goto 100
      endif

20    if(icha.ge.10) go to 30

      if(phid.eq.'SKS') then
         phid='SKSac'
         goto 100
      endif
      if(phid.eq.'SKSdf') then
         phid='SKSac'
         goto 100
      endif
      if(phid.eq.'SKSac') then
         if(dis.lt.90.d0) then
            phid='S'
            goto 100
         else
            phid='SKKSac'
            goto 100
         endif
      endif
      if(phid(1:4).eq.'Sdif' .and. dis.lt.105.d0) then
         phid='S'
         goto 100
      endif
      if(phid.eq.'S') then
         if(dis.lt.25.d0) then
            phid='Sn'
            goto 100
         else  if(dis.gt.50.d0) then
            phid='SKSac'
            goto 100
         else
            goto 50
         endif
      endif
      if(phid.eq.'SmS') then
         phid='Sn'
         goto 100
      endif
      if(phid.eq.'Sn') then
         phid='Sb'
         goto 100
      endif
      if(phid.eq.'SbS') then
         phid='Sb'
         goto 100
      endif
      if(phid.eq.'Sb') then
         phid='Sg'
         goto 100
      endif

      if(phid.eq.'Rg') then
         phid='Sg'
         goto 100
      endif
      if(phid.eq.'Lg') then
         phid='Sg'
         goto 100
      endif
      if(phid(1:2).eq.'Sg') then
         if(dis.gt.50.d0) then
            phid='SKSdf'
            goto 100
         else
            phid='S'
            goto 100
         endif
      endif

      if(phid.eq."S'S'") then
         phid="S'S'ac"
         goto 100
      endif
      if(phid.eq."S'S'ac") then
         phid="S'S'df"
         goto 100
      endif

      if(phid.eq."S'S'S'") then
         phid="S'S'S'ac"
         goto 100
      endif
      if(phid.eq."S'S'S'ac") then
         phid="S'S'S'df"
         goto 100
      endif

      if(phid.eq.'SKKS') then
         phid='SKKSac'
         goto 100
      endif
      if(phid.eq.'SKKSac') then
         phid='SKKSdf'
         goto 100
      endif

      if(phid.eq.'SS') then
         phid='SnSn'
         goto 100
      endif
      if(phid.eq.'SnSn') then
         phid='SbSb'
         goto 100
      endif
      if(phid.eq.'SbSb') then
         phid='SgSg'
         goto 100
      endif
      if(phid.eq.'SgSg') then
         phid='SS'
         goto 100
      endif

      if(phid.eq.'SSS') then
         phid='SnSnSn'
         goto 100
      endif
      if(phid.eq.'SnSnSn') then
         phid='SbSbSb'
         goto 100
      endif
      if(phid.eq.'SbSbSb') then
         phid='SgSgSg'
         goto 100
      endif
      if(phid.eq.'SgSgSg') then
         phid='SSS'
         goto 100
      endif

      if(phid.eq.'SSSS') then
         phid='SSS'
         goto 100
      endif

30    if(icha.ge.3 .and. phid(1:1).eq.'L') go to 50

      if(phid.eq.'L') then
         phid='LQ'
         goto 100
      endif
      if(phid.eq.'LQ') then
         phid='LR'
         goto 100
      endif
      if(phid.eq.'LR') then
         phid='LQ'
         goto 100
      endif

50    icha = 999
      return

100   icha = icha+1
      if(flags) then
         phid0= s // phid(1:7)
      else
         phid0 = phid
      endif

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function phasw(phd)

      character*8 phasw,phd,phd1

      phd1 = phd

      if(phd(1:2).eq.'pP') then
         phd1(2:2) ='w'
         phd1(3:8) = phd(2:7)
      endif
      if(phd(1:2).eq.phd(3:4)) then
         phd1(3:3) = 'w'
         phd1(4:8) = phd(3:7)
      endif
      if(phd(1:3).eq.phd(4:8)) then
         phd1(4:4) = 'w'
         phd1(5:8) = phd(4:7)
      endif
      if(phd(1:4).eq.phd(5:8)) then
         phd1(5:5) = 'w'
         phd1(6:8) = phd(5:7)
      endif

      phasw = phd1

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mult_ons(nobs,nobs0,iev,phase,tt,tts,tt2,azi,azis,
     +           p,ps,touse,amp,per,dtphmax,mread,arid,comment)

      implicit real*8 (a-h,o-z)
      implicit integer (i-n)
      character*(*) touse(*), phase(*), arid(*), comment(*)

      character*8 ph0*8, tu0*9

      real*8 dmerg1, dmerg2

      dimension iev(*),tt(*),tts(*),tt2(*),azi(*),azis(*),p(*),ps(*),
     +          amp(*),per(*)

      logical first

c
c     last changes 26 October 2016
c

      nobs0 = nobs

      do 9000 i = 1, nobs-1

         if (touse(i)(1:1) .eq. 'm') go to 8205

         ind = 1

         ph0 = phase(i)
         t0 = tt(i)
         ts0 = tts(i)
         t20 = tt2(i)
         tu0 = touse(i)
         p0 = p(i)
         ps0 = ps(i)
         indp = 0
         if(p0.gt.0.d0) indp = 1

         do 8200 j = i+1,nobs 

         if(iev(i).ne.iev(j)) go to 8200

         first = .false.


         if( (ph0(1:1).eq.'P'.and.phase(j).eq.'P1') .or.
     +       (ph0(1:1).eq.'S'.and.phase(j).eq.'S1') .or.
     +       (phase(j)(1:1).eq.'P'.and.ph0.eq.'P1') .or.
     +       (phase(j)(1:1).eq.'S'.and.ph0.eq.'S1')  ) first = .true.

         if( ph0.ne.phase(j) .and.  .not.first ) go to 8200

         dt0 = dabs(t0-tt(j))

         if(dt0.le.dtphmax) then 

               touse(j)(1:1) = 'm'
               if(touse(j)(4:4).eq.'D') touse(j)(4:4) = 'm'
               touse(j)(5:5) = ' '

               if(dt0.ge.0.01d0) then
                    
                  t0 = dmerg1(ind,t0,tt(j))

                  ts0 = dmerg2(tts(j),ts0,dt0)
                  t20 = dmerg2(tt2(j),t20,dt0)

                  touse(i)(1:1) = 'm'
                  if(touse(i)(4:4).eq.'D') touse(i)(4:4) = 'm'
                  touse(i)(5:5) = ' '

                  ind = ind + 1

               endif

               if(p(j).lt.0.d0 .or. touse(j)(3:3).eq.'m') go to 8200

               dp = dabs(p0-p(j))

               if(dp.le.0.0001d0) then 
                  if(ind.gt.1) touse(i)(3:3) = 'm'
                  go to 8150
               endif

               if(p0.lt.0.0001d0) then
                  p0 = p(j)
                  ps0 = ps(j)
                  indp = 1
                  if(ind.gt.1) then
                     touse(i)(3:3) = 'm'
                  else
                     p(i) = p0
                     ps(i) = ps(j)
                     touse(i)(3:3) = touse(j)(3:3)
                  endif
               else
                  p0 = dmerg1(indp,p0,p(j))
                  ps0 = dmerg2(ps0,ps(j),dp)
                  indp = indp + 1
                  touse(i)(3:3) = 'm'
               endif

8150           if(touse(j)(3:3).eq.'S') tu0(3:3) = 'S'
               touse(j)(3:3) = 'm'

               if (first .and. ph0(1:1).eq.'P') ph0= 'P1'
               if (first .and. ph0(1:1).eq.'S') ph0= 'S1'

         endif

8200     continue


         if (ind.gt.1) then
            nobs0 = nobs0 + 1

            if (nobs0 .le. mread) then
               iev(nobs0) = iev(i)
               phase(nobs0) = ph0
               tt(nobs0) = t0
               tts(nobs0) = ts0
               tt2(nobs0) = t20
               azi(nobs0) = -999.d0
               azis(nobs0) = -999.d0
               p(nobs0) = p0
               ps(nobs0) = ps0
               touse(nobs0) = tu0
               touse(nobs0)(2:2) = ' '
               touse(nobs0)(9:9) = '*'
               amp(nobs0) = -999.d0
               per(nobs0) =  -999.d0
               touse(nobs0)(6:6) = ' '
               arid(nobs0) = '  merged'
               comment(nobs0) = ' '
c              if(i.eq.1) then
c                print*,nobs, nobs0, iold
c                print*, iev(nobs0), phase(nobs0), tt(nobs0),
c    +           tts(nobs0), ind,indp
c              endif
            else
               print *, ' Merging of onsets extend maximum number '
     +              ,'of allowed onsets!'
            endif
         endif

8205     if (touse(i)(2:2).eq.'m' .or. azi(i).lt.0.d0) go to 9000

         do 8300 j = i+1,nobs

            if(iev(i).eq.iev(j)) then

               if(azi(j).lt.0.d0 .or. touse(j)(2:2).eq.'m') go to 8300

               if(dabs(azi(i)-azi(j)).le.0.1d0) then
                  if(touse(j)(2:2).eq.'A') touse(j)(2:2) = 'm'
               endif
            endif

8300     continue

9000  continue

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function dmerg1(n,v1,v2)
      implicit real*8 (a-h,o-z)
      implicit integer (i-n)

      real*8 dmerg1

      dmerg1 = ( v1 * dble(n) + v2 ) / dble(n + 1)

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function dmerg2(v1,v2,v3)
      implicit real*8 (a-h,o-z)
      implicit integer (i-n)

      real*8 dmerg2, dpythag

      dmerg0 = dpythag(v1,v2)

      dmerg2 = dpythag(dmerg0,v3)

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
