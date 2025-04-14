CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      function alpha1(a)
c  all angels are between -180 and + 180 degrees
      real*8 a,alpha1
      if(a.gt. 180.d0)  then
         alpha1 = a  - 360.d0
      else if(a.lt.-180.d0) then
         alpha1 = a  + 360.d0
      else
         alpha1 = a
      endif
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      function alpha2(a)
c  all angels are between 0 and 360 degrees
      real*8 a,alpha2
      if(a.gt. 360.d0)  then
         alpha2 = a  - 360.d0
      else if(a.lt.0.d0) then
         alpha2 = a  + 360.d0
      else
         alpha2 = a
      endif
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine findrange(amin,amax,a,n1,n2,ind)
c
c       fixed for longitudes: Nov 5, 1997 JS
c
      implicit real*8 (a-h,o-z)
      dimension a(*)
      integer ind,n1,n2

      amin = 9.d99
      amax = -9.d99

      if(ind.eq.1) then

       do 10 i=n1,n2,-1

         if(a(i).lt.amin) amin = a(i)
         if(a(i).gt.amax) amax = a(i)

10       continue

      else if (ind.eq.2) then
c
c       We have to handle the longitude values especially!
c

         deg2rad = datan(1.d0)/45.d0
       bmin = 9.d99
       bmax = -9.d99
       cmin = 9.d99
       cmax = -9.d99

       do 20 i=n1,n2,-1

         p1 = deg2rad*a(i)
         p2 = dcos(p1)
         p3 = dsin(p1)

         if(p2.lt.bmin) bmin = p2
         if(p2.gt.bmax) bmax = p2

         if(p3.lt.cmin) cmin = p3
         if(p3.gt.cmax) cmax = p3

20       continue

         amin = alpha1(datan2(cmin,bmax)/deg2rad)
         amax = alpha1(datan2(cmax,bmin)/deg2rad)

      endif
            
        return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
      function q2(a)
      real*8 a,q2
      q2 = a*a
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      function dmean(a,n1,n2)
      real*8 a(*),dmean, b
      integer n1,n2
      if(n1.ne.n2) then
         b = 0.d0
         do 10 i = n1,n2,-1
10       b = b + a(i)
         dmean = b /dble(n1-n2+1)
      else
c dmean = 0.d0
       dmean =  a(n1)
      endif
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      function ddmax(a,n1,n2)
      real*8 a(*),ddmax
      integer n1,n2
      ddmax = -9.d99
      do 10 i = n1,n2,-1
      if(a(i).gt.ddmax) ddmax = a(i)
10    continue
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      FUNCTION UPPCAS(WORD)
      character*(*) word
      character*40 uppcas,tword
      integer lent

      lent = len(word) 
      if(lent.gt.40) then
         print *,' UPPCAS input too long ',word
         STOP
      endif
      uppcas = ' '
      tword  = ' '
      tword  = word(1:lent)
      do 100 i=1,lent
      ii= ichar(tword(i:i))
      if(ii.ge.97 .and. ii.le.122) then
         ii=ii-32
         tword(i:i)=char(ii)
      endif
100   continue
      uppcas(1:lent) = tword(1:lent)
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION LOWCAS(WORD)
      character*(*) word
      character*40 lowcas,tword
      integer lent

      lent = len(word)
      if(lent.gt.40) then
         print *,' LOWCAS input too long ',word
         STOP
      endif
      lowcas = ' '
      tword  = ' '
      tword  = word(1:lent)
      do 100 i=1,lent
      ii= ichar(tword(i:i))
      if(ii.ge.65 .and. ii.le.90) then
         ii=ii+32
         tword(i:i)=char(ii)
      endif
100   continue
      lowcas(1:lent) = tword(1:lent)
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c     FETOH2 is a HYPOSAT specific driver for FETOH
c
c     To avoid rounding problems when conversion of seconds into
c     integer full seconds and milliseconds for outputs
c
c     calls: fetoh.c
c       
c     J. Schweitzer, NORSAR
c
c     2 January 2021
c
c
      subroutine fetoh2(time,ydoy,year,month,mon,day,dayoy,hour,
     +                   minute,second)
      real*8 time
      integer ydoy,year,month,day,dayoy,hour,minute
      real*4  second
      character*4 mon

      mon = '    '
      ydoy = 0
      year = 0
      month = 0
      day = 0
      dayoy = 0
      hour = 0
      minute = 0

      second = 0.
      
      call  fetoh(time,ydoy,year,month,mon,day,dayoy,hour,
     +                   minute,second)

      isec = nint(second*1000)

      if( isec.gt.59999) then

        time = time + 0.0005d0
        
        mon = '    '
        ydoy = 0
        year = 0
        month = 0
        day = 0
        dayoy = 0
        hour = 0
        minute = 0

        second = 0.
        call  fetoh(time,ydoy,year,month,mon,day,dayoy,hour,
     +                   minute,second)

      endif

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c     zo2to is a subroutine to adapt the source time whenever 
c     a change in depth is 'quite big' (more than 10 km).
c
c     This is not exact but just an approximation to use a better 
c     source time for the next iteration after changing the depth!!!!
c
c     calls function dpythag
c
c     Johannes Schweitzer, June 2021, NORSAR
c
      subroutine zo2to(dz,to,vto)

      real*8 dz, to, vto, dtz

      real*8 dpythag

      if(dabs(dz).gt.10.d0) then
         dtz   = dz / 8.0d0
         to    = to + dtz
         vto   = dpythag(vto,dtz)
c        print *,'T0 corrected for ',dz,' km by ',dtz,' s  to ',to,
c    +           ' s +/- ',vto,' s'
      endif

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c     principle is a subroutine to calculate the unit vectors of the 
c     two half-axes of the epicenter uncertainty from the GMI-results
c
c     input:
c
c     a - matrix v (either 3x3 or 4x4) containing the eigenvectors of
c         the GMI result
c
c     m - number of eigenvectors (either 3 or 4)
c
c     assumption: 
c
c     eigenvector 1: source time
c     eigenvector 2: latitude
c     eigenvector 3: longitude
c     eigenvector 4: depth
c
c     These eigenvectors have all components in all 3 (4) dimensions.
c
c     output:
c
c     principle rotates (in several steps) matrix v so that EV1 
c     (and EW4) have no longer components in lat and lon directions.
c
c     x1 - unit vector of first half axes of uncertainty ellipse of 
c          the epicenter
c     x2 - unit vector of second half axes of uncertainty ellipse of 
c          the epicenter
c     
      subroutine principle(a,x1,x2,m)

      real*8 a(4,4)
      real*8 x1(2),x2(2)
      integer m

      real*8 b(4,4),c(4,4),d(4,4),e(4,4),phi,cp,sp
      integer i,j

      if(m.eq.4) then
c
c     rotating depth components into time/lat/lon 
c

c
c     1st rotation, not changing time-latitude plane
c
        phi = atan2(a(4,3),-a(4,4))
        cp = dcos(phi)
        sp = dsin(phi)

        b(1,1) = 1.d0
        b(1,2) = 0.d0
        b(1,3) = 0.d0
        b(1,4) = 0.d0

        b(2,1) = 0.d0
        b(2,2) = 1.d0
        b(2,3) = 0.d0
        b(2,4) = 0.d0

        b(3,1) = 0.d0
        b(3,2) = 0.d0
        b(3,3) = cp
        b(3,4) = -sp

        b(4,1) = 0.d0
        b(4,2) = 0.d0
        b(4,3) = sp
        b(4,4) = cp

        call matmul(a,b,c,m) 

c
c     2nd rotation not changing time-longitude plane
c
        phi = atan2(c(4,2),-c(4,4))
        cp = dcos(phi)
        sp = dsin(phi)

        b(1,1) = 1.d0
        b(1,2) = 0.d0
        b(1,3) = 0.d0
        b(1,4) = 0.d0

        b(2,1) = 0.d0
        b(2,2) = cp
        b(2,3) = 0.d0
        b(2,4) = -sp

        b(3,1) = 0.d0
        b(3,2) = 0.d0
        b(3,3) = 1.d0
        b(3,4) = 0.d0

        b(4,1) = 0.d0
        b(4,2) = sp
        b(4,3) = 0.d0
        b(4,4) = cp

        call matmul(c,b,d,m) 

c
c     3rd rotation not changing latitude-longitude plane
c
        phi = atan2(d(4,1),-d(4,4))
        cp = dcos(phi)
        sp = dsin(phi)

        b(1,1) = cp
        b(1,2) = 0.d0
        b(1,3) = 0.d0
        b(1,4) = -sp

        b(2,1) = 0.d0
        b(2,2) = 1.d0
        b(2,3) = 0.d0
        b(2,4) = 0.d0

        b(3,1) = 0.d0
        b(3,2) = 0.d0
        b(3,3) = 1.d0
        b(3,4) = 0.d0

        b(4,1) = sp
        b(4,2) = 0.d0
        b(4,2) = 0.d0
        b(4,4) = cp

        call matmul(d,b,e,m) 

      else

        do 80 i=1,m
           do 70 j=1,m
             e(i,j) = a(i,j)
70         continue
80      continue

      endif

c
c     rotating time components into lat/lon 
c

c
c     1st (or 4th) rotation keeping lon fixed
c
      phi = atan2(e(1,1),-e(1,2))
      cp = dcos(phi)
      sp = dsin(phi)

      b(1,1) = -sp
      b(1,2) = cp
      b(1,3) = 0.d0

      b(2,1) = cp
      b(2,2) = sp
      b(2,3) = 0.d0

      b(3,1) = 0.d0
      b(3,2) = 0.d0
      b(3,3) = 1.d0

      call matmul(e,b,c,m) 

c
c     2nd (or 5th) rotation keeping lat fixed
c
      phi = atan2(c(1,1),-c(1,3))
      cp = dcos(phi)
      sp = dsin(phi)

      b(1,1) = -sp
      b(1,2) = 0.d0
      b(1,3) = cp

      b(2,1) = 0.d0
      b(2,2) = 1.d0
      b(2,3) = 0.d0

      b(3,1) = cp
      b(3,2) = 0.d0
      b(3,3) = sp

      call matmul(c,b,d,m) 

c
c     the two principle axes of the uncertainty ellipse
c     in the lat/lon plane are:
c
      x1(1) = d(2,2)
      x1(2) = d(3,2)

      x2(1) = d(2,3)
      x2(2) = d(3,3)

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c     subroutine matmul
c
c     multiplication of an 'N x N' matrix with another 'N x N' matrix
c
      subroutine matmul(a,b,c,n)

      real*8 a,b,c
      dimension a(4,4),b(4,4),c(4,4)
      integer n

      real*8 s
      integer i,j,k

      do 200 i=1,n
        do 100 j=1,n
          s=0.d0
          do 50 k=1,n
            s = s + a(i,k)*b(k,j)
 50        continue   
          c(i,j) = s
 100     continue
 200   continue
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      function write_isf_error(typctl)

      integer write_isf_error

      integer typctl

      include 'isf_head.h'

      write_isf_error = 0
      if(typctl.gt.0) then
         print *, isf_error
      endif

      return

      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      function rdig(a,i)

c     function to round the real number a to i decimal digits to avoid
c     numbers like  -0.00 when printing out.

      real*8 a, b, rdig
      integer i, j

      rdig = a

      if(a.lt.0.d0) then

        j = -i-1
        b = -5.d0 * 10.d0**j

        if(a.gt.b) rdig = 0.d0

c       print *,i,a,rdig,b

      endif

      return

      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
