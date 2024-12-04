cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine isf_out_line(unit,line,com)
      integer unit
      character line*(*),com*(*)

      character uppcas*40,chgcas*40

      character sta*5,arrid*8,phas1*8,phas2*8,magtype*2
      character timedef*1,azimdef*1,slowdef*1,sp_fm*1,detchar*1,
     +          magind*1,picktype*1,dumph*8
      real*4 dist,esaz,timeres,azim,azimres,slow,slowres,snr,amp,per,mag
      integer hh,mi,ss,msec
      integer write_phase, itest

      integer ISF_NULL
      parameter (ISF_NULL=9999999)

      chgcas = uppcas(line(1:5))
      sta = chgcas(1:5)
      read (line(6:13),'(f8.3)') dist
      read (line(14:20),'(f7.2)') esaz
      read (line(22:29),'(a8)') phas1
      read (line(30:37),'(a8)') phas2

      if(phas1.eq.'        ')           phas1='x '

      if(phas2.ne.'        ') then
         if(index(phas1,'x').gt.0 .or.
     +     (phas1.ne.phas2 .and. index(phas1,'1').eq.0 .and.
     +      line(88:88).eq.' ') ) then
           dumph = phas2
           lc = len_trim(dumph)
           if(lc.gt.6) lc = 6
           phas1 = '        '
           phas1(1:1) = '('
           phas1(2:lc+1) = dumph(1:lc)
           phas1(lc+2:lc+2) = ')'
         else
           phas1=phas2
         endif
      endif

      read (line(39:40),'(i2)') hh
      read (line(42:43),'(i2)') mi
      read (line(45:46),'(i2)') ss
      read (line(48:50),'(i3)') msec
      timeres = ISF_NULL
      if(line(51:58).ne.'        ') read (line(51:58),'(f8.3)') timeres
      azim = ISF_NULL
      if(line(60:65).ne.'      ') read (line(60:65),'(f6.2)') azim
      azimres = ISF_NULL
      if(line(67:73).ne.'       ') then
         read (line(67:73),'(f7.2)') azimres
      else
         azim = ISF_NULL
      endif

      slow = ISF_NULL
      if(line(74:79).ne.'      ') read (line(74:79),'(f6.2)') slow
      slowres = ISF_NULL
      if(line(81:86).ne.'      ') then
         read (line(81:86),'(f6.2)') slowres
      else
         slow = ISF_NULL
      endif

      timedef = '_'
      chgcas = uppcas(line(88:90))
      if(chgcas(1:1).eq.'T') timedef = 'T'
      if(line(88:88).eq.'m') timedef = 'x'
      azimdef = '_'
      if(chgcas(2:2).eq.'A') azimdef = 'A'
      if(line(89:89).eq.'m') azimdef = 'x'
      slowdef = '_'
      if(chgcas(3:3).eq.'S') slowdef = 'S'
      if(line(90:90).eq.'m') slowdef = 'x'
      snr = ISF_NULL
      if(line(94:101).ne.'        ') read (line(95:101),'(f7.2)') snr
      amp = ISF_NULL
      if(line(102:114).ne.'             ')
     +    read (line(103:114),'(f12.2)') amp
      per = ISF_NULL
      if(line(115:121).ne.'       ') read (line(116:121),'(f6.3)') per
      magtype = '  '
      mag = ISF_NULL
      if(line(122:129).ne.'        ') then
         read (line(123:126),'(f4.2)') mag
         magtype  = line(128:129)
      endif

      picktype = '_'
      sp_fm    = '_'
      detchar  = line(131:131)

      magind   = ' '

      arrid = '       _'
      if(line(140:147).ne.'        ') arrid = line(140:147)

      lc = len_trim(arrid)

      if(lc.lt.8) then
         arrid = ' '
         arrid((8-lc+1):8) = line(140:139+lc)
      endif

      itest = write_phase(unit,sta,dist,esaz,phas1,hh,mi,
     + ss,msec,timeres,azim,azimres,slow,slowres,timedef,azimdef,
     + slowdef,snr,amp,per,picktype,sp_fm,detchar,magtype,magind,
     + mag,arrid,com)

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine json_out_line(line,com,time,json_rc)
c
c     time - onset time
c     line - line with observation information
c     com  - onset id(s)
c     jrc  - json error code
c
      character line*(*), com*(*)
      character phas1*8, phas2*8, used*5
      integer  unit, json_rc
      real*8    time, val

c
      call json_start_dict_group("observation", json_rc)
      call json_add_string("stat", line(1:5), json_rc)

      read (line(6:13),'(f8.3)') val
      call json_add_double("delta", val, json_rc)

      read (line(14:20),'(f7.2)') val
      call json_add_double("azi", val, json_rc)

      read (line(22:29),'(a8)') phas1
      read (line(30:37),'(a8)') phas2

      call json_add_string("phase", phas1, json_rc)
      if(phas1.ne.phas2 .and. phas2.ne.'        ' ) then
        call json_add_string("phase_used", phas2, json_rc)
      endif

      call json_add_double("time", time, json_rc)

      if(line(52:58).ne.'       ') then
         read (line(52:58),'(f7.2)') val
         call json_add_double("time_res", val, json_rc)
      endif

      if(line(60:65).ne.'      ') then
        read (line(60:65),'(f6.2)') val
        call json_add_double("baz", val, json_rc)

        if(line(67:73).ne.'       ') then
          read (line(67:73),'(f7.2)') val
          call json_add_double("baz_res", val, json_rc)
        endif
      endif

      if(line(74:79).ne.'      ') then
        read (line(74:79),'(f6.2)') val
        call json_add_double("ray_param", val, json_rc)

        if(line(67:73).ne.'       ') then
          read (line(67:73),'(f7.2)') val
          call json_add_double("ray_param_res", val, json_rc)
        endif
      endif

      used = line(88:92)
      do 10 i=1,len(used)
10    if(used(i:i).eq.' ') used(i:i)='_'
      call json_add_string("used", used, json_rc)

      if(line(94:101).ne.'        ') then
         read (line(95:101),'(f7.2)') val
         call json_add_double("snr", val, json_rc)
      endif

      if(line(102:114).ne.'             ') then
        read (line(103:114),'(f12.2)') val
        call json_add_double("amplitude", val, json_rc)
      endif

      if(line(115:121).ne.'       ') then
        read (line(116:121),'(f6.3)') val
        call json_add_double("period", val, json_rc)
      endif

      if(line(122:129).ne.'        ') then
        read (line(123:126),'(f4.2)') val
        call json_add_double("magnitude", val, json_rc)
        call json_add_string("magnitude_type", line(128:129), json_rc)
      endif

      if(line(133:138).ne.'        ') then
         read(line(133:138),'(f6.2)') val
         call json_add_double("emergence_angle", val,json_rc)
      endif

      if(line(140:147).ne.'        ') then
         call json_add_string("arid", line(140:147), json_rc)
      endif

      if(len_trim(com).gt.0) then
         call json_add_string("additional arid", com, json_rc)
      endif

      call json_end_group(json_rc)

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
