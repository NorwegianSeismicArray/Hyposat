c
c     include file modelg.h
c
c     common block with information about the crustal structure of
c     the standard models used in location
c
c     maxla = defined in model.h
c
      parameter (nmod=4)

      dimension v0g(nmod,2,maxla),zg(nmod,maxla),elevg(nmod),
     +          zmaxg(nmod),zconr(nmod),zmoho(nmod)

      character mtype(nmod)*3

      integer jmodg(nmod)

      COMMON /MODELG/ v0g,zg,elevg,zmaxg,zconr,zmoho,jmodg,mtype

