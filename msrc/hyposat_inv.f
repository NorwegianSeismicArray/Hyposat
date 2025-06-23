      subroutine hyposat_gmi(n,m,kq,ierr,typctl)

C
C
C     hyposat_gmi developed as a subroutine to do
C     generalized matrix inversion with single value decomposition
C     
C
C      23. July    1998       JOHANNES SCHWEITZER
c
c                    17. October 2000 vector of uncertainty ellipse included
c
c                    10. October 2002 special case for n = m fixed
c
c                    2022 / 2023 uncertainty ellipse recalcuated and
c                    changed & code more streamlined
c
c        last changes :  3. March  2023       JOHANNES SCHWEITZER
c
c
c     input:
c
c     a         matrix of partial derivatives
c
c     dat       data vector
c
c     dats      vector of 'a priori' standard deviations of data
c
c     rs        vector with 'a priori' standard deviations of model
c               parameters
c
c     n         number of data
c
c     m         number of model parameters
c
c     typctl      verbosity level
c
c         = 9, 10  the matrix of partial derivatives before and
c               after weighting is printed
c
c
c         special output in an output file 'hyposat_gmi.out'
c
c         = 11  the resolution matrix is printed in the output file
c
c         = 12  the covariance matrix is printed in the output file
c
c         = 13  the correlation matrix is printed in the output file
c
c        => 13  all three are printed in the output file
c
c        => 20  additionally the diagonal elements of the 
c               information-density matrix is printed
c
c        => 30  additionally the whole information-density matrix is 
c               printed
c
c     output:
c
c     r        solution vector of model parameters
c
c     var       'a posteriori' standard deviations of model parameters
c
c     res       vector of calculated data
c
c     ierr      if svd failed ierr = maximum of svd-iterations
c
c     a         contains the correlation matrix
c
c     dinf      diagonal elements of the information-density matrix
c
c     dinfm     maximum value of all elements of dinf
c
c     ax1/ax2   epicenter uncertainty ellipse half-axes
c
c     Several other results are calculated internaly, but not (yet) given
c     back as output. See code and change respectively!
c
      IMPLICIT real*8 (A-H,O-Z)

      include 'gmi.h'

      include 'gm2.h' 

      DIMENSION AT(NN,NN),EWMOD(MM),rm(mm,mm), d2(nn)

      integer ierr, typctl, unit

      real*8 q2

      ierr = 0

      quot = 1.d-5

      if(typctl.gt.10) then
       unit = 35
       open (unit,file='hyposat_gmi.out')
      endif

C
C 'a priori' data uncertainties, 
C  and 'a priori' model uncertainties. 
C
      DO 11 J=1,M

      DO 10 I=1,N

      B(I,J)= rs(j)*A(I,J)/dats(I)

c     if(typctl.gt.8 .and. typctl.lt.11) then
c      print *,i,j,b(i,j),rs(J),dats(I),dat(i)
c     endif

10    continue
11    continue

C
C single value decomposition (SVD)
C

c     print *,'The b-matrix before SVD:'
c     print *,'-----------------------'
c     do i=1,n,1
c      print *,(b(i,j),j=1,m)
c     enddo
c     print *,'-----------------------'
c     print *,' '

      CALL SVDCMP(n,m,ierr)

c     print *,'The b-matrix after SVD:'
c     print *,'-----------------------'
c     do i=1,n,1
c      print *,(b(i,j),j=1,m)
c     enddo
c     print *,'-----------------------'
c     print *,' '

      if(ierr.ge.60) then
       print *,'SVD: too many iterations ( ',ierr,' )'
       return
      endif

C
C modifying the singular values
C
      kq = m

      DO 50 K=1,m

      IF (W(K).LT.QUOT) then
        W(K)    = 1.d0
        if (k.eq.kq) kq = kq-1
          EWMOD(K)=0.d0
        else
          EWMOD(K)=1.d0/W(K)
        endif

50    CONTINUE

C
C calucating the generalised inverse of A , stored in array AT
c (and zero setting of field RES and calculating weighted data D2)
C
      DO 112 I=1,N

        res(i) = 0.d0
        d2(i) = DAT(i)/dats(i)

        DO 111 J=1,M

          AT(J,I)=0.d0

          DO 110 K=1,KQ
            AT(J,I)=AT(J,I)+V(J,K)*EWMOD(K)*b(I,K)
110       CONTINUE
111     CONTINUE
112   CONTINUE

C
C weighting with 'a priory' data uncertainties
C and       with 'a priory' model uncertainties
C and calculating the solution vector
C
C
      DO 131 J=1,M

        r(j)   = 0.d0

        DO 130 I=1,N
          r(J)=r(J)+AT(J,I)*d2(i)*rs(J)
130     continue
131   continue

C
C calculating the resolution matrix
C
      DO 146 J2=1,M
        DO 145 J1=1,M

        rm(J1,J2)=0.d0

        DO 140 K=1,m
          rm(J1,J2)=rm(J1,J2)+V(J1,K)*V(J2,K)
140     CONTINUE

        rm(J1,J2)=rs(J1)*rm(J1,J2)/rs(J2)

145     CONTINUE
146   CONTINUE

      if (typctl.eq.11 .or. typctl.gt.13) then

        write(unit,'(/'' The Resolution Matrix:''/)')
        do 150 j1=1,m
          write(unit,'(5f12.9)') (rm(j1,j2),j2=1,m)
150     continue

      endif

C
C residual variances (mean value stored in rv)
C

      rv = 1.d0

      if(n.ge.m) then

        DO 161 J=1,M
          DO 160 I=1,N
            res(I)=res(I)+A(I,J)*r(J)
160       CONTINUE
161     CONTINUE

        RV0=0.d0

        DO 165 I=1,N
          EPS=(res(I)-DAT(I))/dats(I)
          rv0=RV0+q2(EPS)
165     CONTINUE

        if(rv0.gt.1.d-8) then
          if(n-m.gt.m) then
             RV=RV0/DBLE(N-M)
          else
             RV=RV0/DBLE(N)
          endif
        endif

      endif

c      write(*,'(/'' The Matrix V:''/)')
c      do 169 j1=1,m
c         write(*,'(4f12.8,2x,2f13.8)') 
c    +         (v(j1,j2),j2=1,m),W(j1),EWMOD(j1)
c169      continue
 

C
C covariance matrix (temporarly stored in AT)
C
      DO 175 J2=1,M
        DO 174 J1=1,j2

        sum = 0.d0

        DO 170 K=1,m
          sum = sum + rs(j1)*V(J1,K)*q2(EWMOD(K))*V(J2,K)*rs(j2)
170     continue

        AT(J1,J2)= sum*rv
        if(j1.ne.j2) AT(J2,J1)= AT(J1,J2)

174     CONTINUE
175   CONTINUE

      if (typctl.eq.12 .or. typctl.gt.13) then

        write(unit,'(/'' The Covariance Matrix:''/)')
        do 176 j1=1,m
          write(unit,'(6f12.8)') 
     +         (AT(j1,j2),j2=1,m)
176     continue

      endif

C
C vector with standard deviations of parameters modelled
C
      DO 180 J=1,M
        var(j)=dsqrt(AT(j,J))
180   continue

C
C correlation matrix  ( stored in A )
C
      if (typctl.ge.13) then
        DO 191 J2=1,M
          DO 190 J1=1,M
          a(j1,j2) = 0.d0
          if(var(J1).gt.1.d-8 .and. var(J2).gt.1.d-8) 
     +        A(J1,J2)=AT(J1,J2)/(var(J1)*var(J2))
190     continue
191     continue

        write(unit,'(/'' The Correlation Matrix:''/)')
        do 195 j1=1,m
          write(unit,'(5f12.8)') (a(j1,j2),j2=1,m)
195     continue
      endif

c
c     Now we have to calculate the vectors of the principal axes 
c     of the uncertainty ellipse of the epicenter (lat/lon). 
c
 
      if (iellip .and. kq.ge.3) then

        call principle(v,ax1,ax2,kq)

        f1 = ewmod(2)*rs(2)*dsqrt(rv)
        f2 = ewmod(3)*rs(3)*dsqrt(rv)

        AX1(1) = ax1(1)*f1
        AX1(2) = ax1(2)*f1

        AX2(1) = ax2(1)*f2
        AX2(2) = ax2(2)*f2

        go to 199

      endif

      iellip = .false.
C
C calculating the information density matrix (stored in AT )
C
199   continue

      DO 211 I2=1,N
        DO 210 I1=1,N
          AT(I1,I2)=0.d0
          DO 200 K=1,m
            AT(I1,I2)=AT(I1,I2)+b(I1,K)*b(I2,K)
200       CONTINUE
          AT(I1,I2)=dats(I1)*AT(I1,I2)/dats(I2)
210     CONTINUE
211   CONTINUE

      dinfm = 0.d0
      do 214 j1=1,n
        dinf(j1)=AT(j1,j1)
        if(dinfm.lt.dinf(j1)) dinfm=dinf(j1)
214   continue

      if (typctl.ge.20) then

        if (typctl.ge.30) then

          write(unit,'(/'' The Information-Density Matrix:''/)')
          do 215 j1=1,n
            write(unit,'(10f12.8)') (at(j1,j2),j2=1,n)
215       continue
        else
          write(unit,'(/'' The Diagonal Elements of the '',
     +                  ''Information-Density Matrix:''/)')
          do 216 j1=1,n
            write(unit,'(f12.8)') dinf(j1)
216       continue

        endif
      endif
C
      if(typctl.gt.10) then
        close (unit)
      endif

      return
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DLSQ(N,M) 
C 
C       method: least squares fit
c
c       here double precision (real*8) version
C 
C       linear equation sytem: GGL*AAL=DDL 
C                   (n equations for m unknowns, n>=M)
C       kernel matrix GGL (n times m), model AAL (m times 1),
C       data DDL (n times 1)
C       VVL = STANDARD DEVIATIONS (sqrt of variances) of AAL
C       RRL = RESIDUALS = GGL*AAL-DDL
C 
C       code from Gerhard Mueller, Inst. of Met. and Geophysics,
C                               University Frankfurt/Main
C 
C       NNL and MML must have the actual values from the calling routine.
C 
      IMPLICIT real*8 (A-H,O-Z)   

      include 'lsq.h'

      DIMENSION GT(MML,NNL),Q(MML,MML),QI(MML,MML),AL(MML,MML),
     *      ALI(MML,MML),ALIT(MML,MML),GTD(MML),DTH(NNL)

C 
C       MATRIX Q UND VEKTOR GTD   
C 
      if(m.gt.mml .or. n.gt.nnl) then
        print *,' Dimensions in DLSQ-routine are too small!!'
        stop
      endif

      lsqerr = 0

      DO  51  I=1,M 
        vvl(i) = 0.d0
        DO  50  J=1,N 
        GT(I,J)=GGL(J,I)
50      continue
51      continue

      DO  101  I=1,M
      DO  100  J=1,M
      al(i,j) = 0.d0
      CC=0.d0
      DO  80  K=1,N 
      rrl (j) = 0.d0
      CC=CC+GT(I,K)*GGL(K,J)  
80    continue
      Q(I,J)=CC 
100   continue
101   continue

      DO  200  I=1,M
      CC=0.d0
      DO  150  K=1,N
        CC=CC+GT(I,K)*DDL(K)
150     continue
      GTD(I)=CC 
200   continue
C 
C       MATRIX AL 
C 
      DO  2000  I=1,M   
      S=0.d0
      IF(I.EQ.1)  GO TO 700 
      DO  500  K=1,I-1  
        S=S+q2(AL(I,K))
500     continue
700     AL(I,I)=DSQRT(dabs(Q(I,I)-S))
      IF(I.EQ.M)  GO TO 2000
      DO  1001  J=I+1,M 
      S=0.d0
      IF(I.EQ.1)  GO TO 1000
      DO  800  K=1,I-1  
      S=S+AL(I,K)*AL(J,K)   
800   continue

      if(al(i,i).le.0.d0) then
         lsqerr = 1
         print *,'LSQ-ERROR: ',lsqerr, al(i,i)
         go to 8000
      endif
1000  AL(J,I)=(Q(I,J)-S)/AL(I,I)
1001  CONTINUE  
2000  CONTINUE  
C 
C       MATRIX ALI
C 
      DO  4000  I=1,M   
      if(al(i,i).le.0.d0) then
         lsqerr = 2
         print *,'LSQ-ERROR: ',lsqerr, al(i,i)
         go to 8000
      endif
      ALI(I,I)=1.d0/AL(I,I)   
      IF(I.EQ.1)  GO TO 3200
      DO  3000  J=1,I-1 
      S=0.d0
      DO  2400  K=J,I-1 
        S=S+AL(I,K)*ALI(K,J)  
2400    continue
        ALI(I,J)=-S/AL(I,I)   
3000    continue
3200    DO  3500  J=I+1,M 
        ALI(I,J)=0.d0 
3500    continue
4000  CONTINUE  
C 
C       MATRIX QI 
C 
      DO  4501  I=1,M   
      DO  4500  J=1,M   
        ALIT(I,J)=ALI(J,I)
4500  CONTINUE  
4501  CONTINUE  
      DO  4801  I=1,M   
      DO  4800  J=1,M   
      CC=0.d0
      DO  4600  K=1,M   
        CC=CC+ALIT(I,K)*ALI(K,J)  
4600    continue
        QI(I,J)=CC
4800  CONTINUE  
4801  CONTINUE  
C 
C       MODEL A  
C 
      DO  5000  I=1,M   
      CC=0.d0
      DO  4900  K=1,M   
        CC=CC+QI(I,K)*GTD(K)  
4900    continue
        AAL(I)=CC   
5000    continue
C 
C       RESIDUALS AND MODEL STANDARD DEVIATIONS  
C 
      DO  5500  I=1,N   
      CC=0.d0
      DO  5300  K=1,M   
        CC=CC+GGL(I,K)*AAL(K) 
5300    continue
        DTH(I)=CC 
5500    continue
      S=0.d0
      DO  6000  I=1,N   
      RRL(I)=DDL(I)-DTH(I)
        S=S+q2(RRL(I))
6000    continue
      if(n.gt.m) then
      E=S/(N-M) 
        DO  7000  I=1,M   
          VVL(I)=DSQRT(QI(I,I)*E)
7000      continue
      endif
8000    RETURN
      END   
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
