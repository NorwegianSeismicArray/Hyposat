c
c    include file model.h
c

c     common block with information about the local/regional model
c     (used also for CRUST 1.0)
c
c     maxla = maximum number of layers for a local/regional velocity
c             model
c

      parameter (maxla = 101)

      DIMENSION V0(2,maxla),z(maxla),rzv(2)

      character mtyp*3, mtypg*3, filloc*512, azo(maxla)*4

      integer   jmod,imo

      logical   locgeo,locsta,locmod

      COMMON /MODEL/  v0,z,rzv,elatc,elonc,rmax,zmax,elat2,elon2,
     +                vstat,zw,elev,jmod,imo,locgeo,locsta,
     +                locmod,mtyp,mtypg,filloc,azo

      real*8 v0,z,rzv,elatc,elonc,zmax,elat2,elon2,rmax,zw,elev,
     +       vstat

