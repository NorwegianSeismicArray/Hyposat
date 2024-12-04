c
c     include file for CRUST 1.0 Model
c
c     status: 11 July 2022
c 

      integer npm, nlo, nla

      parameter(npm=9,nlo=360,nla=180,irc=nlo*nla)

      real*8  avp(npm,nla,nlo), avs(npm,nla,nlo),athi(npm,nla,nlo),
     +        topo(nla,nlo),bnd(npm,nla,nlo),awat(nla,nlo)

      integer icrust10(irc), ilato, ilono

      common /CRUST10/ avp,avs,athi,bnd,topo,awat,icrust10,ilato,ilono

