
c     Parses a line asuming it to be a data type line.
c     Format is:  DATA_TYPE data_type:subtype data_format:subformat
c     Only data_type is required.
c     If there is extra text it is ignored. 
c     No checks are made as to whether data_type is valid or not.

c     Returns 0 if the line is a properly formatted data type line 
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_data_type (line,data_type,subtype,
     +                                          data_format,subformat)

      character line*(*),data_type*(*),subtype*(*),data_format*(*)
      character subformat*(*)
      include 'isf_head.h'
      integer start,end,mid

      if (line(1:10) .ne. 'DATA_TYPE ') then
          isf_error = 'not a data_type line: '//line
          read_data_type = 20
          return
      end if

c     Initialise strings - some of which may not be set.
      data_type   = ' '
      subtype     = ' '
      data_format = ' '
      subformat   = ' '

      start=11
      do while (line(start:start) .eq. ' ' .and. start .le. len(line))
          start=start+1
      end do

      end = index(line(start:),' ')
      if (end .ne. 0) then
          end = start+end
          mid = index(line(start:end),':')

          if (mid .ne. 0) then
              mid=start+mid
              data_type = line(start:mid-2)
              subtype = line(mid:end-2)
          else
              data_type = line (start:end-2)
          end if
          start = end
      else
          data_type = line(start:)
      end if

      do while (line(start:start) .eq. ' ' .and. start .le. len(line))
          start=start+1
      end do

      end = index(line(start:),' ')
      if (end .ne. 0) then
          end = start+end
          mid = index(line(start:end),':')
          if (mid .ne. 0) then
              mid=start+mid
              data_format = line(start:mid-2)
              subformat = line(mid:end-2)
          else
              data_format = line (start:end-2)
          end if
      else
          data_format = line(start:)
      end if

      read_data_type = 0
      return

      end

c     Data type BULLETIN
c     ==================


c     Parses a line asuming it to be an event title line.
c     Requires event ID to be present but allows lines with no region.

c     Returns 0 if the line is a properly formatted event ID line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_event_id(line, evid, region)
      include 'isf_head.h'
      integer partline,check_whole

      character line*(*), evid*(*), region*(*)
      character substr*(ISF_LINE_LEN)
      integer i,i2

c     Chars 1-5: must be the word 'Event'. Char 6: must be a space. 
      if ( (line(1:6) .ne. 'EVENT ') .and. (line(1:6) .ne. 'Event ') ) 
     +then
          isf_error = 'not an event title line: '//line
          read_event_id = 20
          return
      end if

c     Char 17 or Char 16 or Char 15: must be a space
      if (line(17:17) .ne. ' ') then
        if (line(16:16) .ne. ' ') then
          if (line(15:15) .ne. ' ') then
             isf_error = 'bad format, check char 15-17: '//line
             read_event_id = 20
             return
          else
             i2 = 8
          end if
        else
          i2 = 9
        end if
      else
        i2 = 10
      end if

c     Chars 7-15: event ID
      if (partline(evid,line,7,i2) .eq. 0) then
          isf_error = 'missing evid: '//line
          read_event_id = 20
          return
      end if

      if ( check_whole(evid) .eq. 1 ) then
          isf_error = 'bad evid: '//line
          read_event_id = 20
          return
      end if

c     Not quite right but lots of people hit CR after evid
      if (len(line) .lt. 15) then
          read_event_id = 0
          return
      end if

c     Chars 16/17/18-80: geographic region if there
      if(line(17:17) .eq. ' ') then
         i=partline(region,line,18,63)
      else if(line(16:16) .eq. ' ') then
         i=partline(region,line,17,64)
      else if (line(15:15) .eq. ' ') then
         i=partline(region,line,16,65)
      end if

c     Check for extra characters after char 80.
      if (partline(substr,line,80,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_event_id = 20
          return
      end if

      read_event_id = 0
      return
      end

c     Tests a line to discover if it is an origin header line.

c     Returns 0 if the line is a properly formatted origin header line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_origin_head(line)

      character line*(*)
      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(136)
      integer headlen 
      data    headlen /136/

      head = '   Date       Time        Err   RMS Latitude Longitude' //  
     +       '  Smaj  Smin  Az Depth   Err Ndef Nsta Gap  mdist'      //
     +       '  Mdist Qual   Author      OrigID'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not an origin header: '//line
          read_origin_head = 20
          return
      end if

c     Check for extra characters after char 136.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
             write (*,'(''read_origin_head: '',a)') isf_error
          read_origin_head = 20
          return
      end if

      read_origin_head = 0
      return
      end


c     Parses a line asuming it to be an origin line.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted origin line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_origin(line,yyyy,mm,dd,hh,mi,ss,msec,
     + timfix,stime,sdobs,lat,lon,epifix,smaj,smin,strike,depth,
     + depfix,sdepth,ndef,nsta,gap,mindist,maxdist,antype,loctype,
     + etype,author,origid)

      character line*(*), author*(*), origid*(*), etype*(*)
      character*1 timfix, epifix, depfix, antype, loctype
      integer yyyy, mm, dd, hh, mi, ss, msec
      integer strike, ndef, nsta, gap
      real*4 stime, sdobs, lat, lon, smaj, smin, depth, sdepth
      real*4 mindist, maxdist

      include 'isf_head.h'
      integer partline, check_int, atoi, isdigit
      integer check_real,check_whole
      real*4 ator

      character substr*(ISF_LINE_LEN)

c     Chars 1-4: year.
      if (partline(substr,line,1,4) .eq. 0) then
          isf_error = 'missing year: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
          isf_error = 'bad year: '//line
          read_origin = 20
          return
      end if
      yyyy = atoi(substr)

c     Char 5: '/' character.
      if (line(5:5) .ne. '/') then
          isf_error = 'bad date: '//line
          read_origin = 20
          return
      end if

c     Chars  6-7: month.
      if (partline(substr,line,6,2) .eq. 0) then
          isf_error = 'missing month: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
        isf_error = 'bad month: '//line
          read_origin = 20
          return
      end if
      mm = atoi(substr)

c     Char 8: '/' character.
      if (line(8:8) .ne. '/') then
          isf_error = 'bad date: '//line
         read_origin = 20
          return
      end if

c     Chars  9-10: day.
      if (partline(substr,line,9,2) .eq. 0) then
          isf_error = 'missing day: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
          isf_error = 'bad day: '//line
          read_origin = 20
          return
      end if
      dd = atoi(substr)

c     Char 11: space.
      if (line(11:11) .ne. ' ') then
          isf_error = 'bad date: '//line
          read_origin = 20
          return
      end if

c     Chars  12,13: hour.
      if (partline(substr,line,12,2) .eq. 0) then
          isf_error = 'missing hour: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
        isf_error = 'bad hour: '//line
          read_origin = 20
          return
      end if
      hh = atoi(substr)

c     Char 14:  ':' character.
      if (line(14:14) .ne. ':') then
          isf_error = 'bad date: '//line
          read_origin = 20
          return
      end if

c     Chars 15,16: minute.
      if (partline(substr,line,15,2) .eq. 0) then
          isf_error = 'missing minute: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
        isf_error = 'bad minute: '//line
          read_origin = 20
          return
      end if
      mi = atoi(substr)

c     Char 17:  ':' character.
      if (line(17:17) .ne. ':') then
          isf_error = 'bad date: '//line
          read_origin = 20
          return
      end if

c     Chars 18,19: integral second.
      if (partline(substr,line,18,2) .eq. 0) then
          isf_error = 'missing second: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
        isf_error = 'bad second: '//line
          read_origin = 20
          return
      end if
      ss = atoi(substr)

c     Char 20-22: msec or spaces.
c     Allow decimal point without any numbers after it.
      if (partline(substr,line,20,3) .ne. 0) then
c     	Char 20: '.' character.
          if (line(20:20) .ne. '.') then
              read_origin = 20
              return
          end if

c     	Chars 21,22: 10s of msec.
          if (isdigit(line(21:21)) .eq. 0) then
              isf_error = 'bad date: '//line
              read_origin = 20
              return
          end if
          msec = (ichar(line(21:21)) - ichar('0'))*100

          if (isdigit(line(22:22)) .ne. 0) then
              msec = msec + (ichar(line(22:22)) - ichar('0'))*10
          else if (line(22:22) .ne. ' ' ) then
              isf_error = 'bad msec: '//line
              read_origin = 20
              return
          end if
      else
c     	Char 20: '.' character or space.
          if (line(20:20) .ne. '.' .and. line(20:20) .ne. ' ') then
              isf_error = 'bad date: '//line
              read_origin = 20
              return
          end if
          msec = ISF_NULL
      end if

c     Char 23: timfix - either f or space.
      if (line(23:23) .eq. 'f' .or. line(23:23) .eq. ' ') then
          timfix = line(23:23)
      else
          isf_error = 'bad timfix: '//line
          read_origin = 20
          return
      end if

c     Char 24: space.
      if (line(24:24) .ne. ' ') then
          isf_error = 'bad format, char 24: '//line
          read_origin = 20
          return
      end if

c     Chars 25-29: origin time error - real if anything.
      if (partline(substr,line,25,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad stime: '//line
              read_origin = 20
              return
          end if
          stime = ator(substr)
      else
          stime =ISF_NULL
      end if

c     Char 30: space.
      if (line(30:30) .ne. ' ') then
          isf_error = 'bad format, char 30: '//line
          read_origin = 20
          return
      end if

c     Chars 31-35: rms (sdobs) - real if anything.
      if (partline(substr,line,31,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad sdobs: '//line
              read_origin = 20
              return
          end if
          sdobs = ator(substr)
      else
          sdobs =ISF_NULL
      end if

c     Char 36: space.
      if (line(36:36) .ne. ' ') then
          isf_error = 'bad format, char 36: '//line
          read_origin = 20
          return
      end if

c      Chars 37-44: lattitude - must be there.
      if (partline(substr,line,37,8) .eq. 0) then
          isf_error = 'missing lattitude: '//line
          read_origin = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
           isf_error = 'bad lattitude: '//line
          read_origin = 20
          return
      end if
      lat = ator(substr)

c     Char 45: space.
      if (line(45:45) .ne. ' ') then
          isf_error = 'bad format, char 45: '//line
          read_origin = 20
          return
      end if

c      Chars 46-54: longitude - must be there.
      if (partline(substr,line,46,9) .eq. 0) then
          isf_error = 'missing longitude: '//line
          read_origin = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
           isf_error = 'bad longitude: '//line
          read_origin = 20
          return
      end if
      lon = ator(substr)

c     Char 55: epifix - either f or space.
      if (line(55:55) .eq. 'f' .or. line(55:55) .eq. ' ') then
          epifix = line(55:55)
      else
          isf_error = 'bad epifix: '//line
          read_origin = 20
          return
      end if

c     Chars 56-60: semi-major axis of error ellipse - real if there.
c     This is departure from format but smaj < smin is daft.
      if (partline(substr,line,56,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad smaj: '//line
              read_origin = 20
              return
          end if
          smaj = ator(substr)
      else
          smaj =ISF_NULL
      end if

c     Char 61: space.
      if (line(61:61) .ne. ' ') then
          isf_error = 'bad format, char 61: '//line
          read_origin = 20
          return
      end if

c     Chars 62-66: semi-minor axis of error ellipse - real if there.
      if (partline(substr,line,62,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad smin: '//line
              read_origin = 20
              return
          end if
          smin = ator(substr)
      else
          smin =ISF_NULL
      end if

c     Char 67: space.
      if (line(67:67) .ne. ' ') then
          isf_error = 'bad format, char 67: '//line
          read_origin = 20
          return
      end if

c     Chars 68-70: strike - integer if there.
c     Strike can be -1, when its a flag to signify that smaj,smin
c     are really slat,slon. 
      if (partline(substr,line,68,3) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_error = 'bad strike: '//line
              read_origin = 20
              return
          end if
          strike = atoi(substr)
      else
          strike =ISF_NULL
      end if

c     Char 71: space.
      if (line(71:71) .ne. ' ') then
          isf_error = 'bad format, char 71: '//line
          read_origin = 20
          return
      end if

c     Chars 72-76: depth - real if there.
      if (partline(substr,line,72,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad depth: '//line
              read_origin = 20
              return
          end if
          depth = ator(substr)
      else
          depth =ISF_NULL
      end if

c     Char 77: depfix - either d,f, or space.
      if (line(77:77) .eq. 'f' .or. line(77:77) .eq. ' ' .or. 
     +    line(77:77) .eq. 'd') then
          depfix = line(77:77)
      else
          isf_error = 'bad depfix: '//line
          read_origin = 20
          return
      end if

c     Char 78: space.
      if (line(78:78) .ne. ' ') then
          isf_error = 'bad format, char 78: '//line
          read_origin = 20
          return
      end if

c     Chars 79-82: depth error - real if there.
c
C JS 11 Dec 2018
C
c     if (partline(substr,line,79,4) .ne. 0) then
      if (line(79:82) .ne. '    ') then
          if (partline(substr,line,79,4) .ne. 0) then
             if (check_real(substr) .eq. 1) then
                 isf_error = 'bad sdepth: '//line
                 read_origin = 20
                 return
             else
                 sdepth = ator(substr)
             end if
          else
                 sdepth =ISF_NULL
          end if
      else
          sdepth =ISF_NULL
      end if

c     Char 83: space.
      if (line(83:83) .ne. ' ') then
          isf_error = 'bad format, char 83: '//line
          read_origin = 20
          return
      end if

c     Chars 84-87: ndef - integer if there.
      if (partline(substr,line,84,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_error = 'bad ndef: '//line
              read_origin = 20
              return
          end if
          ndef = atoi(substr)
      else
          ndef =ISF_NULL
      end if

c     Char 88: space.
      if (line(88:88) .ne. ' ') then
          isf_error = 'bad format, char 88: '//line
          read_origin = 20
          return
      end if

c     Chars 89-92: nsta - integer if there.
      if (partline(substr,line,89,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_error = 'bad nsta: '//line
              read_origin = 20
              return
          end if
          nsta = atoi(substr)
      else
          nsta =ISF_NULL
      end if

c     Char 93: space.
      if (line(93:93) .ne. ' ') then
          isf_error = 'bad format, char 93: '//line
          read_origin = 20
          return
      end if

c     Chars 94-96: gap - integer if there.
      if (partline(substr,line,94,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_error = 'bad gap: '//line
              read_origin = 20
              return
          end if
          gap = atoi(substr)
      else
          gap =ISF_NULL
      end if

c     Char 97: space.
      if (line(97:97) .ne. ' ') then
          isf_error = 'bad format, char 97: '//line
          read_origin = 20
          return
      end if

c     Chars 98-103: minimum distance - real if there.
      if (partline(substr,line,98,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mindist: '//line
              read_origin = 20
              return
          end if
          mindist = ator(substr)
      else
          mindist =ISF_NULL
      end if

c     Char 104: space.
      if (line(104:104) .ne. ' ') then
          isf_error = 'bad format, char 104: '//line
          read_origin = 20
          return
      end if

c     Chars 105-110: maximum distance - real if there.
      if (partline(substr,line,105,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad maxdist: '//line
              read_origin = 20
              return
          end if
          maxdist = ator(substr)
      else
          maxdist =ISF_NULL
      end if

c     Char 111: space.
      if (line(111:111) .ne. ' ') then
          isf_error = 'bad format, char 111: '//line
          read_origin = 20
          return
      end if

c     Char 112: analysis type - either space, a, m, or g.
      if (line(112:112) .eq. 'a' .or. line(112:112) .eq. 'm' .or.
     +    line(112:112) .eq. 'g' .or. line(112:112) .eq. ' ') then

          antype = line(112:112)
      else
          isf_error = 'bad antype: '//line
          read_origin = 20
          return
      end if

c     Char 113: space.
      if (line(113:113) .ne. ' ') then
          isf_error = 'bad format, char 113: '//line
          read_origin = 20
          return
      end if

c     Char 114: location method - either space, i, p, g, or o.
      if (line(114:114) .eq. 'i' .or. line(114:114) .eq. 'p' .or.
     +    line(114:114) .eq. 'g' .or. line(114:114) .eq. 'o' .or.
     +    line(114:114) .eq. ' ') then

          loctype = line(114:114)
      else
          isf_error = 'bad loctype: '//line
          read_origin = 20
          return
      end if

c     Char 115: space.
      if (line(115:115) .ne. ' ') then
          isf_error = 'bad format, char 115: '//line
          read_origin = 20
          return
      end if

c     Chars 116-117: event type, any characters allowed but must be there
      if (partline(etype,line,116,2) .eq. 0) then
          etype = ' '
      else if( len(etype) .ne. 2) then
          isf_error = 'bad etype: '//line
          read_origin = 20
          return
      end if

c     Char 118: space.
      if (line(118:118) .ne. ' ' ) then
          isf_error = 'bad format, char 118: '//line
          read_origin = 20
          return
      end if

c     Chars 119-127: author, any characters allowed but must be there.
      if (partline(author,line,119,9) .eq. 0) then
          isf_error = 'missing author: '//line
          read_origin = 20
          return
      end if

      if ( check_whole(author) .eq. 1 ) then
          isf_error = 'bad author: '//line
          read_origin = 20
          return
      end if

c     Char 128: space.
      if (line(128:128) .ne. ' ' ) then
          isf_error = 'bad format, char 128: '//line
          read_origin = 20
          return
      end if

c     Chars 129-136: origin ID, any characters allowed but must be there.
      if (partline(origid,line,129,8) .eq. 0) then
          isf_error = 'missing origid: '//line
          read_origin = 20
          return
      end if

      if ( check_whole(origid) .eq. 1 ) then
          isf_error = 'bad origid: '//line
          read_origin = 20
          return
      end if

c     Check for extra characters after char 136.
      if (partline(substr,line,137,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_origin = 20
          return
      end if

      read_origin = 0
      return
      end


c     Parses a line to test whether it is a prime origin label.

c     Returns 0 if the line is a properly formatted prime origin line.
c     Returns 20 and writes a diagnostic to isf_error if not.

      integer function read_origin_prime(line)

      character line*(*)

      include 'isf_head.h'
      integer partline

      character substr*(ISF_LINE_LEN)

      if (line(1:9) .ne. ' (#PRIME)') then
          isf_error = 'not a prime comment: '//line
          read_origin_prime = 20
          return
      end if

      if (partline(substr,line,10,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_origin_prime = 20
          return
      end if

      read_origin_prime = 0
      return
      end


c     Parses a line to test whether it is a centroid origin label.

c     Returns 0 if the line is a properly formatted centroid origin line.
c     Returns 20 and writes a diagnostic to isf_error if not.

      integer function read_origin_centroid(line)

      character line*(*)

      include 'isf_head.h'
      integer partline

      character substr*(ISF_LINE_LEN)

      if (line(1:12) .ne. ' (#CENTROID)') then
          isf_error = 'not a centroid comment: '//line
          read_origin_centroid = 20
          return
      end if

      if (partline(substr,line,13,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_origin_centroid = 20
          return
      end if

      read_origin_centroid = 0
      return
      end


c     Parses a line assuming it to be an origin parameter formatted comment.
c     Accepts any number of parameter=value pairs as long as the line is
c     short enough.

c     Returns 0 if the line is a properly formatted origin paramter line.
c     Returns 20 and writes a diagnostic to isf_error if not.

      integer function read_origin_param(line,param,value,error,
     +                                                     numparam)

      character line*(*)
      character param(*)*(*),value(*)*(*),error(*)*(*)
      integer numparam
      include 'isf_head.h'
      integer i,start,end,break,mid
      character substr*(ISF_LINE_LEN)

      if (line(1:9) .ne. ' (#PARAM ') then
          isf_error = 'not an origin parameter line: '//line
          read_origin_param = 20
          return
      end if

      start=10
      do while (line(start:start) .eq. ' ')
          start=start+1
      end do

      end=len(line)
      do while (line(end:end) .eq. ' ' .or. line(end:end) .eq. ')')
          if (line(end:end) .eq. ')') then
              line(end:end) = ' '
          end if
          end=end-1
      end do

      if (end .gt. ISF_COMM_LEN+10) then
          isf_error = 'line too long: '//line
          read_origin_param = 20
          return
      end if

c     Go through the rest of the line one character at a time, separating 
c     words on ' ' to get param=value pairs and on '=' to get the
c     individual parameters and vales.
      numparam=0
      break = index(line(start:),' ')

      do while (break .ne. 0 .and. start .le. end)
          break = break + start
          mid = index(line(start:break),'=')

          if (mid .eq. 0) then
              isf_error = 'param without value: '//line
              read_origin_param = 20
              return
          end if

          mid = mid + start
          numparam = numparam+1
          param(numparam) = line(start:mid-2)
          value(numparam) = line(mid:break-2)
          start = break
          break = index(line(start:),' ')
      end do

c     For each resulting value check whether includes an error part.
      do i=1,numparam
          mid = index(value(i),'+')
          if (mid .ne. 0) then
              substr = value(i)
              value(i) = substr(1:mid-1)
              error(i) = substr(mid+1:)
          else
              error(i) = ' '
          end if
      end do

      read_origin_param = 0
      return
      end


c     Tests a line to discover if it is a first moment tensor header comment.

c     Returns 0 if the line is a first moment tensor header line.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_momten_head_1(line)

      character line*(*)
      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(88)
      integer headlen 
      data    headlen /88/

      head = ' (#MOMTENS sc    M0 fCLVD    MRR    MTT    MPP    MRT' //
     +       '    MTP    MPR NST1 NST2 Author   )'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not a first moment tensor header: 
     +'//line
          read_momten_head_1 = 20
          return
      end if

c     Check for extra characters after char 88.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_momten_head_1 = 20
          return
      end if

      read_momten_head_1 = 0
      return
      end


c     Tests a line to discover if it is a second moment tensor header comment.

c     Returns 0 if the line is a second moment tensor header line.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_momten_head_2(line)

      character line*(*)
      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(88)
      integer headlen 
      data    headlen /88/

      head = ' (#             eM0 eCLVD    eRR    eTT    ePP    eRT' //
     +       '    eTP    ePR NCO1 NCO2 Duration )'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not a second moment tensor header:' // 
     +                          line
          read_momten_head_2 = 20
          return
      end if

c     Check for extra characters after char 88.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_momten_head_2 = 20
          return
      end if

      read_momten_head_2 = 0
      return
      end


c     Parses a line asuming it to be a first moment tensor data comment.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted first moment tensor data line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_momten_line_1(line,scale_factor,
     + scalar_moment,fclvd,mrr,mtt,mpp,mrt,mtp,mpr,nsta1,nsta2,
     + author)

      character line*(*), author*(*)
      integer scale_factor,nsta1,nsta2
      real*4 scalar_moment,fclvd,mrr,mtt,mpp,mrt,mtp,mpr
      include 'isf_head.h'
      integer partline,check_int,check_real,atoi,check_whole
      real*4 ator
      character substr*(ISF_LINE_LEN)


c     Chars 1-11: should be the string  ' (#        '
      if (line(1:11) .ne. ' (#        ') then
          isf_error = 'not a first moment tensor data line: 
     +'//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 12,13: scale factor - integer */
      if (partline(substr,line,12,2) .eq. 0) then
          isf_error = 'missing scale factor: '//line
          read_momten_line_1 = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
           isf_error = 'bad scale factor: '//line
          read_momten_line_1 = 20
          return
      end if
      scale_factor = atoi(substr)

c     Char 14: space.
      if (line(14:14) .ne. ' ' ) then
          isf_error = 'bad format, char 14: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 15-19: scalar seismic moment - must be real.
      if (partline(substr,line,15,5) .eq. 0) then
          isf_error = 'missing moment: '//line
          read_momten_line_1 = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
           isf_error = 'bad moment: '//line
          read_momten_line_1 = 20
          return
      end if
      scalar_moment = ator(substr)

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_error = 'bad format, char 20: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 21-25: fCLVD, real if anything.
      if (partline(substr,line,21,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad fclvd: '//line
              read_momten_line_1 = 20
              return
          end if
          fclvd = ator(substr)
      else
          fclvd =ISF_NULL
      end if

c     Char 26: space.
      if (line(26:26) .ne. ' ' ) then
          isf_error = 'bad format, char 26: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 27-32: radial-radial element, real if anything.
      if (partline(substr,line,27,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mrr: '//line
              read_momten_line_1 = 20
              return
          end if
          mrr = ator(substr)
      else
          mrr =ISF_NULL
      end if

c     Char 33: space.
      if (line(33:33) .ne. ' ' ) then
          isf_error = 'bad format, char 33: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 34-39: theta-theta element, real if anything.
      if (partline(substr,line,34,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mtt: '//line
              read_momten_line_1 = 20
              return
          end if
          mtt = ator(substr)
      else
          mtt =ISF_NULL
      end if

c     Char 40: space.
      if (line(40:40) .ne. ' ' ) then
          isf_error = 'bad format, char 40: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 41-46: phi-phi element, real if anything.
      if (partline(substr,line,41,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mpp: '//line
              read_momten_line_1 = 20
              return
          end if
          mpp = ator(substr)
      else
          mpp =ISF_NULL
      end if

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_error = 'bad format, char 47: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 48-53: radial-theta element, real if anything.
      if (partline(substr,line,48,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mrt: '//line
              read_momten_line_1 = 20
              return
          end if
          mrt = ator(substr)
      else
          mrt =ISF_NULL
      end if

c     Char 54: space.
      if (line(54:54) .ne. ' ' ) then
          isf_error = 'bad format, char 54: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 55-60: theta-phi element, real if anything.
      if (partline(substr,line,55,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mtp: '//line
              read_momten_line_1 = 20
              return
          end if
          mtp = ator(substr)
      else
          mtp =ISF_NULL
      end if

c     Char 61: space.
      if (line(61:61) .ne. ' ' ) then
          isf_error = 'bad format, char 61: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 62-67: phi-radial element, real if anything.
      if (partline(substr,line,62,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mpr: '//line
              read_momten_line_1 = 20
              return
          end if
          mpr = ator(substr)
      else
          mpr =ISF_NULL
      end if

c     Char 68: space.
      if (line(68:68) .ne. ' ' ) then
          isf_error = 'bad format, char 68: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 69-72: nsta1, int if anything.
      if (partline(substr,line,69,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_error = 'bad nsta1: '//line
              read_momten_line_1 = 20
              return
          end if
          nsta1 = atoi(substr)
      else
          nsta1 =ISF_NULL
      end if

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_error = 'bad format, char 73: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 74-77: nsta2, int if anything.
      if (partline(substr,line,74,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_error = 'bad nsta2: '//line
              read_momten_line_1 = 20
              return
          end if
          nsta2 = atoi(substr)
      else
          nsta2 =ISF_NULL
      end if

c     Char 78: space.
      if (line(78:78) .ne. ' ' ) then
          isf_error = 'bad format, char 78: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 79-87: author, any characters allowed but must be there.
      if (partline(author,line,79,9) .eq. 0) then
          isf_error = 'missing author: '//line
          read_momten_line_1 = 20
          return
      end if

      if ( check_whole(author) .eq. 1 ) then
          isf_error = 'bad author: '//line
          read_momten_line_1 = 20
          return
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,88,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_momten_line_1 = 20
          return
      end if

      read_momten_line_1 = 0
      return
      end

c     Parses a line asuming it to be a second moment tensor data comment.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if a properly formatted second moment tensor data line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_momten_line_2(line,scalar_moment_unc,
     + fclvd_unc,mrr_unc,mtt_unc,mpp_unc,mrt_unc,mtp_unc,mpr_unc,
     + ncomp1,ncomp2,duration)

      character line*(*)
      integer ncomp1,ncomp2
      real*4 scalar_moment_unc,fclvd_unc,mrr_unc,mtt_unc,mpp_unc
      real*4 mrt_unc,mtp_unc,mpr_unc,duration
      include 'isf_head.h'
      integer partline,check_int,check_real,atoi
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-14: should be the string  ' (#           '
      if (line(1:14) .ne. ' (#           ') then
          isf_error = 'not a second moment tensor data line: 
     +'//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 15-19:  uncertainty in scalar seismic moment - real if there.
      if (partline(substr,line,15,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad scalar_moment_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          scalar_moment_unc = ator(substr)
      else
          scalar_moment_unc = ISF_NULL
      end if

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_error = 'bad format, char 20: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 21-25: uncertainty in fCLVD, real if anything.
      if (partline(substr,line,21,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad fclvd_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          fclvd_unc = ator(substr)
      else
          fclvd_unc =ISF_NULL
      end if

c     Char 26: space.
      if (line(26:26) .ne. ' ' ) then
          isf_error = 'bad format, char 26: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 27-32: uncertainty in radial-radial element, real if anything.
      if (partline(substr,line,27,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mrr_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mrr_unc = ator(substr)
      else
          mrr_unc =ISF_NULL
      end if

c     Char 33: space.
      if (line(33:33) .ne. ' ' ) then
          isf_error = 'bad format, char 33: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 34-39: uncertainty in theta-theta element, real if anything.
      if (partline(substr,line,34,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mtt_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mtt_unc = ator(substr)
      else
          mtt_unc =ISF_NULL
      end if

c     Char 40: space.
      if (line(40:40) .ne. ' ' ) then
          isf_error = 'bad format, char 40: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 41-46: uncertainty in phi-phi element, real if anything.
      if (partline(substr,line,41,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mpp_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mpp_unc = ator(substr)
      else
          mpp_unc =ISF_NULL
      end if

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_error = 'bad format, char 47: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 48-53: uncertainty in radial-theta element, real if anything.
      if (partline(substr,line,48,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mrt_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mrt_unc = ator(substr)
      else
          mrt_unc =ISF_NULL
      end if

c     Char 54: space.
      if (line(54:54) .ne. ' ' ) then
          isf_error = 'bad format, char 54: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 55-60: uncertainty in theta-phi element, real if anything.
      if (partline(substr,line,55,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mtp_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mtp_unc = ator(substr)
      else
          mtp_unc =ISF_NULL
      end if

c     Char 61: space.
      if (line(61:61) .ne. ' ' ) then
          isf_error = 'bad format, char 61: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 62-67: uncertainty in phi-radial element, real if anything.
      if (partline(substr,line,62,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad mpr_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mpr_unc = ator(substr)
      else
          mpr_unc =ISF_NULL
      end if

c     Char 68: space.
      if (line(68:68) .ne. ' ' ) then
          isf_error = 'bad format, char 68: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 69-72: ncomp1, int if anything.
      if (partline(substr,line,69,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_error = 'bad ncomp1: '//line
              read_momten_line_2 = 20
              return
          end if
          ncomp1 = atoi(substr)
      else
          ncomp1 =ISF_NULL
      end if

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_error = 'bad format, char 73: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 74-77: ncomp2, int if anything.
      if (partline(substr,line,74,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_error = 'bad ncomp2: '//line
              read_momten_line_2 = 20
              return
          end if
          ncomp2 = atoi(substr)
      else
          ncomp2 =ISF_NULL
      end if

c     Char 78: space.
      if (line(78:78) .ne. ' ' ) then
          isf_error = 'bad format, char 78: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 79-86:  duration, real if anything.
      if (partline(substr,line,79,8) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_error = 'bad duration: '//line
              read_momten_line_2 = 20
              return
          end if
          duration = ator(substr)
      else
          duration =ISF_NULL
      end if

c     Check for extra characters - not including close bracket.  */
      if (partline(substr,line,87,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_momten_line_2 = 20
          return
      end if

      read_momten_line_2 = 0
      return
      end


c     Tests a line to discover if it is a fault plane header comment.

c     Returns 0 if the line is a fault plane header.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_fault_plane_head(line)

      character line*(*)
      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(64)
      integer headlen 
      data    headlen /64/

      head = ' (#FAULT_PLANE Typ Strike   Dip    Rake  NP  NS Plane' //
     +       ' Author   )'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not a fault plane header: '//line
          read_fault_plane_head = 20
          return
      end if

c     Check for extra characters after char 64.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_fault_plane_head = 20
          return
      end if

      read_fault_plane_head = 0
      return
      end


c     Parses a line asuming it to be a fault plane data comment.
c     Could be first or second plane, the only difference is whether
c     author field is expected or not.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted fault plane data line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_fault_plane(line,f_type,strike,dip,
     +                                    rake,np,ns,f_plane,author)

      character line*(*)
      character f_plane*(*), f_type*(*), author*(*)
      integer np,ns
      real*4 strike,dip,rake

      include 'isf_head.h'
      integer partline,check_int,check_real,check_whole,atoi
      real*4 ator
      character substr*(ISF_LINE_LEN)
      integer line_num

c     Chars 1-3: the strings  ' (#' or ' (+',
c     depending on whether this is the first or second plane given.
c     Chars 4-15: spaces.
      if (line(1:15) .eq. ' (#            ') then
          line_num = 1
      else if (line(1:15) .eq. ' (+            ') then
          line_num = 2
      else
          isf_error = 'not a fault plane line: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 16-18: fault plane solution type.
      if (partline(f_type,line,16,3) .ne. 0) then
          if (f_type .ne. 'FM' .and. f_type .ne. 'BB' 
     +                         .and. f_type .ne. 'BDC') then
              isf_error = 'bad f_type: '//line
              read_fault_plane = 20
              return
          end if
      else
          f_type = ' '
      end if

c     Char 19: space.
      if (line(19:19) .ne. ' ' ) then
          isf_error = 'bad format, char 19: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 20-25: strike, must be real.
      if (partline(substr,line,20,6) .eq. 0) then
          isf_error = 'missing strike: '//line
          read_fault_plane = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
           isf_error = 'bad strike: '//line
          read_fault_plane = 20
          return
      end if
      strike = ator(substr)

c     Char 26: space.
      if (line(26:26) .ne. ' ' ) then
          isf_error = 'bad format, char 26: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 27-31: dip, must be real.
      if (partline(substr,line,27,5) .eq. 0) then
          isf_error = 'missing dip: '//line
          read_fault_plane = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
           isf_error = 'bad dip: '//line
          read_fault_plane = 20
          return
      end if
      dip = ator(substr)

c     Char 32: space.
      if (line(32:32) .ne. ' ' ) then
          isf_error = 'bad format, char 32: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 33-39: rake, real - need not be there if both planes given.
      if (partline(substr,line,33,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad rake: '//line
              read_fault_plane = 20
              return
          end if
          rake = ator(substr)
      else
          rake = ISF_NULL
      end if

c     Char 40: space.
      if (line(40:40) .ne. ' ' ) then
          isf_error = 'bad format, char 40: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 41-43: np, int if there.
      if (partline(substr,line,41,3) .ne. 0) then
          if (check_int(substr) .eq. 1) then
               isf_error = 'bad np: '//line
              read_fault_plane = 20
              return
          end if
          np = atoi(substr)
      else
          np = ISF_NULL
      end if

c     Char 44: space.
      if (line(44:44) .ne. ' ' ) then
          isf_error = 'bad format, char 44: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 45-47: ns, int if there.
      if (partline(substr,line,45,3) .ne. 0) then
          if (check_int(substr) .eq. 1) then
               isf_error = 'bad np: '//line
              read_fault_plane = 20
              return
          end if
          ns = atoi(substr)
      else
          ns = ISF_NULL
      end if

c     Char 48: space.
      if (line(48:48) .ne. ' ' ) then
          isf_error = 'bad format, char 48: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 49-53: plane identification.
      if (partline(f_plane,line,49,5) .ne. 0) then
          if (f_plane .ne. 'FAULT' .and. f_plane .ne. 'AUXIL' ) then
              isf_error = 'bad f_plane: '//line
              read_fault_plane = 20
              return
          end if
      else
          f_plane = ' '
      end if

c     Chars  54-63: First plane has author, don't read for second plane.
      if (line_num .eq. 1) then

c     	Char 54: space.
          if (line(54:54) .ne. ' ' ) then
              isf_error = 'bad format, char 54: '//line
              read_fault_plane = 20
              return
          end if

c     	Chars 55-63: author, any characters allowed but must be there.
          if (partline(author,line,55,9) .eq. 0) then
              isf_error = 'missing author: '//line
              read_fault_plane = 20
              return
          end if

          if ( check_whole(author) .eq. 1 ) then
              isf_error = 'bad author: '//line
              read_fault_plane = 20
              return
          end if
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,88,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_fault_plane = 20
          return
      end if

      read_fault_plane = 0
      return
      end


c     Tests a line to discover if it is a principal axes header comment.

c     Returns 0 if the line is a principal axes header.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_axes_head(line)

      character line*(*)
      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(83)
      integer headlen 
      data    headlen /83/

      head = ' (#PRINAX sc  T_val T_azim  T_pl  B_val B_azim  B_pl' //
     +       '  P_val P_azim  P_pl Author   )'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not a principal axes header: '//line
          read_axes_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_axes_head = 20
          return
      end if

      read_axes_head = 0
      return
      end

c     Tests a line to discover if it is a principal axes error header comment.
c     This line may or may not be present regardless of whether there is an
c     error data line or not.

c     Returns 0 if the line is a principal axes error header.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_axes_err_head(line)

      character line*(*)
      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(83)
      integer headlen 
      data    headlen /83/

      head = ' (+             eTv    eTa   eTp    eBv    eBa   eBp' //
     +       '    ePv    ePa   ePp fCLVD    )'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not a principal axes header: '//line
          read_axes_err_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_axes_err_head = 20
          return
      end if

      read_axes_err_head = 0
      return
      end


c     Parses a line asuming it to be a principal axes data comment.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted principal axes data line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_axes(line,scale_factor,t_val,t_azim,t_pl,
     +b_val,b_azim,b_pl,p_val,p_azim,p_pl,author)

      character line*(*), author*(*)
      integer scale_factor
      real*4 t_val,t_azim,t_pl,b_val,b_azim,b_pl,p_val,p_azim,p_pl

      include 'isf_head.h'
      integer partline,check_int,check_real,check_whole,atoi
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: should be the string  ' (#       '
      if (line(1:10) .ne. ' (#       ') then
          isf_error = 'not an axes line: '//line
          read_axes = 20
          return
      end if

c     Chars 11,12: scale factor - integer if there.
      if (partline(substr,line,11,2) .ne. 0) then
          if (check_int(substr) .eq. 1) then
               isf_error = 'bad scale factor: '//line
              read_axes = 20
              return
          end if
          scale_factor = atoi(substr)
      else
          scale_factor = ISF_NULL
      end if

c     Char 13: space.
      if (line(13:13) .ne. ' ' ) then
          isf_error = 'bad format, char 13: '//line
          read_axes = 20
          return
      end if

c     Chars 14-19: t value - real if there.
      if (partline(substr,line,14,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad t_val: '//line
              read_axes = 20
              return
          end if
          t_val = ator(substr)
      else
          t_val = ISF_NULL
      end if

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_error = 'bad format, char 20: '//line
          read_axes = 20
          return
      end if

c     Chars 21-26: t azim, must be real.
      if (partline(substr,line,21,6) .eq. 0) then
          isf_error = 'missing t_azim: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_error = 'bad t_azim: '//line
          read_axes = 20
          return
      end if
      t_azim = ator(substr)

c     Char 27: space.
      if (line(27:27) .ne. ' ' ) then
          isf_error = 'bad format, char 27: '//line
          read_axes = 20
          return
      end if

c     Chars 28-32: t plunge, must be real.
      if (partline(substr,line,28,6) .eq. 0) then
          isf_error = 'missing t_pl: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_error = 'bad t_pl: '//line
          read_axes = 20
          return
      end if
      t_pl = ator(substr)

c     Char 33: space.
      if (line(33:33) .ne. ' ' ) then
          isf_error = 'bad format, char 33: '//line
          read_axes = 20
          return
      end if

c     Chars 34-39: b value - real if there.
      if (partline(substr,line,34,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad b_val: '//line
              read_axes = 20
              return
          end if
          b_val = ator(substr)
      else
          b_val = ISF_NULL
      end if

c     Char 40: space.
      if (line(40:40) .ne. ' ' ) then
          isf_error = 'bad format, char 40: '//line
          read_axes = 20
          return
      end if

c     Chars 41-46: b azim, must be real.
      if (partline(substr,line,41,6) .eq. 0) then
          isf_error = 'missing b_azim: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_error = 'bad b_azim: '//line
          read_axes = 20
          return
      end if
      b_azim = ator(substr)

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_error = 'bad format, char 47: '//line
          read_axes = 20
          return
      end if

c     Chars 48-52: b plunge, must be real.
      if (partline(substr,line,48,5) .eq. 0) then
          isf_error = 'missing b_pl: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_error = 'bad b_pl: '//line
          read_axes = 20
          return
      end if
      b_pl = ator(substr)

c     Char 53: space.
      if (line(53:53) .ne. ' ' ) then
          isf_error = 'bad format, char 53: '//line
          read_axes = 20
          return
      end if

c     Chars 54-59: p value - real if there.
      if (partline(substr,line,54,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad p_val: '//line
              read_axes = 20
              return
          end if
          p_val = ator(substr)
      else
          p_val = ISF_NULL
      end if

c     Char 60: space.
      if (line(60:60) .ne. ' ' ) then
          isf_error = 'bad format, char 60: '//line
          read_axes = 20
          return
      end if

c     Chars 61-66: p azim, must be real.
      if (partline(substr,line,61,6) .eq. 0) then
          isf_error = 'missing p_azim: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_error = 'bad p_azim: '//line
          read_axes = 20
          return
      end if
      p_azim = ator(substr)

c     Char 67: space.
      if (line(67:67) .ne. ' ' ) then
          isf_error = 'bad format, char 67: '//line
          read_axes = 20
          return
      end if

c     Chars 68-72: p plunge, must be real.
      if (partline(substr,line,68,6) .eq. 0) then
          isf_error = 'missing p_pl: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_error = 'bad p_pl: '//line
          read_axes = 20
          return
      end if
      p_pl = ator(substr)

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_error = 'bad format, char 73: '//line
          read_axes = 20
          return
      end if

c     Chars 74-82: author, any characters allowed but must be there.
      if (partline(author,line,74,9) .eq. 0) then
          isf_error = 'missing author: '//line
          read_axes = 20
          return
      end if

      if ( check_whole(author) .eq. 1 ) then
          isf_error = 'bad author: '//line
          read_axes = 20
          return
      end if

c     Check for extra characters - not including close bracket.
      if (partline(substr,line,83,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_axes = 20
          return
      end if

      read_axes = 0
      return
      end


c     Parses a line asuming it to be a principal axes error comment.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted principal axes error line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_axes_err(line,t_val_unc,t_azim_unc,
     + t_pl_unc,b_val_unc,b_azim_unc,b_pl_unc,p_val_unc,p_azim_unc,
     + p_pl_unc,fclvd)

      character line*(*)
      real*4 t_val_unc,t_azim_unc,t_pl_unc,b_val_unc,b_azim_unc,b_pl_unc
      real*4 p_val_unc,p_azim_unc,p_pl_unc,fclvd

      include 'isf_head.h'
      integer partline,check_real
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-14: should be the string  ' (+           '.
      if (line(1:14) .ne. ' (+           ') then
          isf_error = 'not an axes error line: '//line
          read_axes_err = 20
          return
      end if

c     Chars 15-19: t value uncertainty.
      if (partline(substr,line,15,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad t_val_unc: '//line
              read_axes_err = 20
              return
          end if
          t_val_unc = ator(substr)
      else
          t_val_unc = ISF_NULL
      end if

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_error = 'bad format, char 20: '//line
          read_axes_err = 20
          return
      end if

c     Chars 21-26: t azim uncertainty.
      if (partline(substr,line,21,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad t_azim_unc: '//line
              read_axes_err = 20
              return
          end if
          t_azim_unc = ator(substr)
      else
          t_azim_unc = ISF_NULL
      end if

c     Char 27: space.
      if (line(27:27) .ne. ' ' ) then
          isf_error = 'bad format, char 27: '//line
          read_axes_err = 20
          return
      end if

c     Chars 28-32: t plunge uncertainty.
      if (partline(substr,line,28,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad t_pl_unc: '//line
              read_axes_err = 20
              return
          end if
          t_pl_unc = ator(substr)
      else
          t_pl_unc = ISF_NULL
      end if

c     Char 33,34: must be a spaces.
      if (line(33:34) .ne. '  ' ) then
          isf_error = 'bad format, char 33,34: '//line
          read_axes_err = 20
          return
      end if

c     Chars 35-39: b value uncertainty.
      if (partline(substr,line,35,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad b_val_unc: '//line
              read_axes_err = 20
              return
          end if
          b_val_unc = ator(substr)
      else
          b_val_unc = ISF_NULL
      end if

c     Char 40: space.
      if (line(40:40) .ne. ' ' ) then
          isf_error = 'bad format, char 40: '//line
          read_axes_err = 20
          return
      end if

c     Chars 41-46: b azim uncertainty.
      if (partline(substr,line,41,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad b_azim_unc: '//line
              read_axes_err = 20
              return
          end if
          b_azim_unc = ator(substr)
      else
          b_azim_unc = ISF_NULL
      end if

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_error = 'bad format, char 47: '//line
          read_axes_err = 20
          return
      end if

c     Chars 48-52: b plunge uncertainty.
      if (partline(substr,line,48,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad b_pl_unc: '//line
              read_axes_err = 20
              return
          end if
          b_pl_unc = ator(substr)
      else
          b_pl_unc = ISF_NULL
      end if

c     Char 53,54: must be a spaces.
      if (line(53:54) .ne. '  ' ) then
          isf_error = 'bad format, char 53,54: '//line
          read_axes_err = 20
          return
      end if

c     Chars 55-59: p value uncertainty.
      if (partline(substr,line,55,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad p_val_unc: '//line
              read_axes_err = 20
              return
          end if
          p_val_unc = ator(substr)
      else
          p_val_unc = ISF_NULL
      end if

c     Char 60: space.
      if (line(60:60) .ne. ' ' ) then
          isf_error = 'bad format, char 60: '//line
          read_axes_err = 20
          return
      end if

c     Chars 61-66: p azim uncertainty.
      if (partline(substr,line,61,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad p_azim_unc: '//line
              read_axes_err = 20
              return
          end if
          p_azim_unc = ator(substr)
      else
          p_azim_unc = ISF_NULL
      end if

c     Char 67: space.
      if (line(67:67) .ne. ' ' ) then
          isf_error = 'bad format, char 67: '//line
          read_axes_err = 20
          return
      end if

c     Chars 68-72: p plunge uncertainty.
      if (partline(substr,line,68,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad p_pl_unc: '//line
              read_axes_err = 20
              return
          end if
          p_pl_unc = ator(substr)
      else
          p_pl_unc = ISF_NULL
      end if

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_error = 'bad format, char 73: '//line
          read_axes_err = 20
          return
      end if

c     Chars 74-78: fclvd.
      if (partline(substr,line,74,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad fclvd: '//line
              read_axes_err = 20
              return
          end if
          fclvd = ator(substr)
      else
          fclvd = ISF_NULL
      end if

c     Check for extra characters - not including close bracket.
      if (partline(substr,line,79,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_axes_err = 20
          return
      end if

      read_axes_err = 0
      return
      end

c     Tests a line to discover if it is a magnitude block header line.

c     Returns 0 if the line is a magnitude block header.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_netmag_head(line)

      character line*(*)
      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(38)
      integer headlen 
      data    headlen /38/

      head = 'Magnitude  Err Nsta Author      OrigID'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not a magnitude header: '//line
          read_netmag_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_netmag_head = 20
          return
      end if

      read_netmag_head = 0
      return
      end

c     Parses a line assuming that it is a magnitude sub-block data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.
                                      
c     Returns 0 if the line is a properly formatted magnitude line,
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_netmag(line,magtype,magind,mag,
     +                               magerr,nsta,author,origid)

      character line*(*), magtype*(*), author*(*), origid*(*)
      character magind
      real*4 mag,magerr
      integer nsta

      include 'isf_head.h'
      integer partline,check_real,check_int,check_whole,atoi
      real*4 ator
      character substr*(ISF_LINE_LEN)


c     Chars 1-5: magnitude type, any characters allowed but must be there.
      if (partline(magtype,line,1,5) .eq. 0) then
          isf_error = 'missing magtype: '//line
          read_netmag = 20
          return
      end if

      if ( check_whole(magtype) .eq. 1 ) then
          isf_error = 'bad magtype: '//line
          read_netmag = 20
          return
      end if

c     Char 6: less than or greater than indicator or space only.
      if (line(6:6) .ne. ' ' .and. line(6:6) .ne. '<' .and. line(6:6) 
     +.ne. '>') then
          isf_error = 'bad magind: '//line
          read_netmag = 20
          return
      end if
      magind = line(6:6)

c     Chars 7-10: magnitude, must be real.
      if (partline(substr,line,7,4) .eq. 0) then
          isf_error = 'missing magnitude: '//line
          read_netmag = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_error = 'bad magnitude: '//line
          read_netmag = 20
          return
      end if
      mag = ator(substr)

c     Char 11: space.
      if (line(11:11) .ne. ' ' ) then
          isf_error = 'bad format, char 11: '//line
          read_netmag = 20
          return
      end if

c     Chars 12-14: magnitude error, real if anything.
      if (partline(substr,line,12,3) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad magnitude error: '//line
              read_netmag = 20
              return
          end if
          magerr = ator(substr)
      else
          magerr = ISF_NULL
      end if

c     Char 15: space.
      if (line(15:15) .ne. ' ' ) then
          isf_error = 'bad format, char 15: '//line
          read_netmag = 20
          return
      end if

c     Chars 16-19: number of stations, integer if anything.
      if (partline(substr,line,16,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
              isf_error = 'bad nsta: '//line
              read_netmag = 20
              return
          end if
          nsta = atoi(substr)
      else
          nsta = ISF_NULL
      end if

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_error = 'bad format, char 20: '//line
          read_netmag = 20
          return
      end if

c     Chars 21-29: author, any characters allowed but must be there.
      if (partline(author,line,21,9) .eq. 0) then
          isf_error = 'missing author: '//line
          read_netmag = 20
          return
      end if

      if ( check_whole(author) .eq. 1 ) then
          isf_error = 'bad author: '//line
          read_netmag = 20
          return
      end if

c     Char 30: space.
      if (line(30:30) .ne. ' ' ) then
          isf_error = 'bad format, char 30: '//line
          read_netmag = 20
          return
      end if

c     Chars 31-38: origin ID, any characters allowed but must be there.
      if (partline(origid,line,31,8) .eq. 0) then
          isf_error = 'missing origid: '//line
          read_netmag = 20
          return
      end if

      if ( check_whole(origid) .eq. 1 ) then
          isf_error = 'bad origid: '//line
          read_netmag = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,39,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_netmag = 20
          return
      end if

      read_netmag = 0
      return
      end

c     The stations contributing to a given netmag can follow in one or 
c     more formated comment lines and are separated by spaces.
c     The array of station code strings sta has size n, if the subroutine
c     is being run on a second or subsequent line then n will already be 
c     set and will be updated by this routine as more codes are added.

      integer function read_netmag_sta(line,sta,n)

      character line*(*), sta(*)*(*)
      integer n,isupper

      include 'isf_head.h'
      integer start,break,end

c     If it is the first then initialise array of station codes.
c     If not it must be a follow on line or something is wrong.
      if (line(1:12) .eq. ' (#STATIONS ') then
          n = 0
      else if (line(1:12) .eq. ' (+         ') then
          if (n .gt. ISF_NUM_STA) then
              isf_error = 'too many stations: '//line
              read_netmag_sta = 20
              return
          end if
      else
          isf_error = 'bad station list format: '//line
          read_netmag_sta = 20
          return
      end if

c     Don't read close bracket, if there is one there.
      start = 13
      end = len(line)
      do while ((line(end:end) .eq. ' ' .or. line(end:end) .eq. ')' ) 
     +            .and. end .gt. start)
          end = end - 1
      end do

      do while (line(start:start) .eq. ' ' .and. start .lt. end)
          start = start + 1
      end do

      if ( end .gt. ISF_LINE_LEN ) then 
          isf_error = 'line too long: '//line
          read_netmag_sta = 20
          return
      end if

c     Fill array of station codes.
      break = index(line(start:),' ')
      do while (break .ne. 0 .and. start .le. end)
          break = break + start

          if ( break - start - 1 .gt. ISF_NET_LEN + ISF_STA_LEN ) then
              isf_error = 'station code too long: '//line
              read_netmag_sta = 20
              return
          end if

          n = n + 1
          sta(n) = line(start:break-2)

c     	Check that this looks like a station code.
          if (isupper(sta(n)(1:1)) .eq. 0) then
              isf_error = 'illegal station: '//sta(n)
              read_netmag_sta = 20
              return
          end if

          start = break
          break = index(line(start:),' ')
      end do

      read_netmag_sta = 0
      return
      end

c     Parses a line assuming it to be an netmag basis formatted comment.

c     Returns 0 if the line is a properly formatted netmag basis line.
c     Returns 20 and writes a diagnostic to isf_error if not.

      integer function read_netmag_basis(line,param,value)

      character line*(*), param*(*), value*(*)

      include 'isf_head.h'
      integer check_whole
      integer start,break,end

c     Chars 1-8: should be the string ' (#BASIS '.
      if (line(1:9) .ne. ' (#BASIS ') then
          isf_error = 'not a netmag basis line: '//line
          read_netmag_basis = 20
          return
      end if

c     Don't read close bracket, if there is one there.
      start = 10
      end = len(line)
      do while ((line(end:end) .eq. ' ' .or. line(end:end) .eq. ')' ) 
     +            .and. end .gt. start)
          end = end - 1
      end do

      do while (line(start:start) .eq. ' ' .and. start .lt. end)
          start = start + 1
      end do

      if ( end .gt. ISF_LINE_LEN ) then 
          isf_error = 'line too long: '//line
          read_netmag_basis = 20
          return
      end if

      break = index(line(start:),'=')
      break = start+break
      param = line(start:break-2)
      value = line(break:end)

c     value is everything after = so make sure not too much
      if ( check_whole(value) .eq. 1 ) then
          isf_error = 'bad value: '//line
          read_netmag_basis = 20
          return
      end if

      read_netmag_basis = 0
      return
      end

c     Tests a line to discover if it is a effects block header line.

c     Returns 0 if the line is a effects block header.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_effects_head(line)

      character line*(*)
      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(69)
      integer headlen 
      data    headlen /69/

      head = 'Effects              Loctyp Location' //
     +       '           Intensity Scale Author'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not an effects header: '//line
          read_effects_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_effects_head = 20
          return
      end if

      read_effects_head = 0
      return
      end

c     Parses a line assuming that it is an effects block data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.
                                      
c     Returns 0 if the line is a properly formatted effects line,
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_effects(line,heard,felt,damage,casualties,
     + uplift,subsidence,fault,tsunami,seiche,volcano,acoustic,
     + gravity,t_wave,liquification,geyser,landslide,sandblow,cracks,
     + lights,odours,loctype,lat,lon,dist,azim,country,postcode,net,
     + sta,intensity1,modifier,intensity2,scale,author)

      character line*(*),author*(*),loctype*(*)
      character scale*(*),country*(*),postcode*(*),net*(*),sta*(*)
      character heard,felt,damage,casualties,uplift,subsidence
      character fault,tsunami,seiche,volcano
      character acoustic,gravity,t_wave,liquification,geyser
      character landslide,sandblow,cracks,lights,odours
      character modifier
      real*4 lat,lon,dist,azim,intensity1,intensity2

      include 'isf_head.h'
      integer partline,check_real,check_whole
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Char 1: heard flag.
      if (line(1:1) .ne. '_' .and. line(1:1) .ne. 'H' ) then
          isf_error = 'bad heard flag: '//line
          read_effects = 20
          return
      end if
      heard = line(1:1)

c     Char 2: felt flag.
      if (line(2:2) .ne. '_' .and. line(2:2) .ne. 'F' ) then
          isf_error = 'bad felt flag: '//line
          read_effects = 20
          return
      end if
      felt = line(2:2)

c     Char 3: damage flag.
      if (line(3:3) .ne. '_' .and. line(3:3) .ne. 'D' ) then
          isf_error = 'bad damage flag: '//line
          read_effects = 20
          return
      end if
      damage = line(3:3)

c     Char 4: casualties flag.
      if (line(4:4) .ne. '_' .and. line(4:4) .ne. 'C' ) then
          isf_error = 'bad casualties flag: '//line
          read_effects = 20
          return
      end if
      casualties = line(4:4)

c     Char 5: uplift flag.
      if (line(5:5) .ne. '_' .and. line(5:5) .ne. 'U' ) then
          isf_error = 'bad uplift flag: '//line
          read_effects = 20
          return
      end if
      uplift = line(5:5)

c     Char 6: subsidence flag.
      if (line(6:6) .ne. '_' .and. line(6:6) .ne. 'S' ) then
          isf_error = 'bad subsidence flag: '//line
          read_effects = 20
          return
      end if
      subsidence = line(6:6)

c     Char 7: surface faulting flag.
      if (line(7:7) .ne. '_' .and. line(7:7) .ne. 'F' ) then
          isf_error = 'bad surface faulting flag: '//line
          read_effects = 20
          return
      end if
      fault = line(7:7)

c     Char 8: tsunami flag.
      if (line(8:8) .ne. '_' .and. line(8:8) .ne. 'T' .and.  
     +                                    line(8:8) .ne. 'Q') then
          isf_error = 'bad tsunami flag: '//line
          read_effects = 20
          return
      end if
      tsunami = line(8:8)

c     Char 9: seiche flag.
      if (line(9:9) .ne. '_' .and. line(9:9) .ne. 'S' .and.  
     +                                    line(9:9) .ne. 'Q') then
          isf_error = 'bad seiche flag: '//line
          read_effects = 20
          return
      end if
      seiche = line(9:9)

c     Char 10: volcano flag.
      if (line(10:10) .ne. '_' .and. line(10:10) .ne. 'V' ) then
          isf_error = 'bad volcano flag: '//line
          read_effects = 20
          return
      end if
      volcano = line(10:10)

c     Char 11: acoustic flag.
      if (line(11:11) .ne. '_' .and. line(11:11) .ne. 'A' ) then
          isf_error = 'bad acoustic flag: '//line
          read_effects = 20
          return
      end if
      acoustic = line(11:11)

c     Char 12: gravity flag.
      if (line(12:12) .ne. '_' .and. line(12:12) .ne. 'G' ) then
          isf_error = 'bad gravity flag: '//line
          read_effects = 20
          return
      end if
      gravity = line(12:12)

c     Char 13: t_wave flag.
      if (line(13:13) .ne. '_' .and. line(13:13) .ne. 'T' ) then
          isf_error = 'bad t_wave flag: '//line
          read_effects = 20
          return
      end if
      t_wave = line(13:13)

c     Char 14: liquification flag.
      if (line(14:14) .ne. '_' .and. line(14:14) .ne. 'L' ) then
          isf_error = 'bad liquification flag: '//line
          read_effects = 20
          return
      end if
      liquification = line(14:14)

c     Char 15: geyser flag.
      if (line(15:15) .ne. '_' .and. line(15:15) .ne. 'G' ) then
          isf_error = 'bad geyser flag: '//line
          read_effects = 20
          return
      end if
      geyser = line(15:15)

c     Char 16: landslide flag.
      if (line(16:16) .ne. '_' .and. line(16:16) .ne. 'S' ) then
          isf_error = 'bad landslide flag: '//line
          read_effects = 20
          return
      end if
      landslide = line(16:16)

c     Char 17: sandblow flag.
      if (line(17:17) .ne. '_' .and. line(17:17) .ne. 'B' ) then
          isf_error = 'bad sandblow flag: '//line
          read_effects = 20
          return
      end if
      sandblow = line(17:17)

c     Char 18: cracks flag.
      if (line(18:18) .ne. '_' .and. line(18:18) .ne. 'C' ) then
          isf_error = 'bad cracks flag: '//line
          read_effects = 20
          return
      end if
      cracks = line(18:18)

c     Char 19: lights flag.
      if (line(19:19) .ne. '_' .and. line(19:19) .ne. 'V' ) then
          isf_error = 'bad lights flag: '//line
          read_effects = 20
          return
      end if
      lights = line(19:19)

c     Char 20: odours flag.
      if (line(20:20) .ne. '_' .and. line(20:20) .ne. 'V' ) then
          isf_error = 'bad odours flag: '//line
          read_effects = 20
          return
      end if
      odours = line(20:20)

c     Char 21: space.
      if (line(21:21) .ne. ' ' ) then
          isf_error = 'bad format, char 21: '//line
          read_effects = 20
          return
      end if

c     Chars 22-27: loctype. Checked below to see if sensible.
      if (partline(loctype,line,22,6) .eq. 0) then
          isf_error = 'missing loctype: '//line
          read_effects = 20
          return
      end if

c     Char 28: space.
      if (line(28:28) .ne. ' ' ) then
          isf_error = 'bad format, char 28: '//line
          read_effects = 20
          return
      end if

c     Chars 29-46: depend on loctype.
      if (loctype .eq. 'Summar') then

c     	Chars 29-46 should be blank.
          if (partline(substr,line,29,18) .ne. 0) then
              isf_error = 'bad summar format: '//line
              read_effects = 20
              return
          end if

      elseif (loctype .eq. 'LatLon') then

c     	Chars 29-36: lattitude - must be real.
          if (partline(substr,line,29,8) .eq. 0) then
              isf_error = 'missing lattitude: '//line
              read_effects = 20
              return
          end if

          if (check_real(substr) .eq. 1) then
              isf_error = 'bad lattitude: '//line
              read_effects = 20
              return
          end if
          lat = ator(substr)

c     	Char 37: space.
          if (line(37:37) .ne. ' ' ) then
              isf_error = 'bad format, char 37: '//line
              read_effects = 20
              return
          end if

c     	Chars 38-46: longitude - must be real.
          if (partline(substr,line,38,9) .eq. 0) then
              isf_error = 'missing longitude: '//line
              read_effects = 20
              return
          end if

          if (check_real(substr) .eq. 1) then
              isf_error = 'bad longitude: '//line
              read_effects = 20
              return
          end if
          lon = ator(substr)

      elseif (loctype .eq. 'DistAz') then

c     	Chars 29-36: distance - must be real.
          if (partline(substr,line,29,8) .eq. 0) then
              isf_error = 'missing distance: '//line
              read_effects = 20
              return
          end if

          if (check_real(substr) .eq. 1) then
              isf_error = 'bad distance: '//line
              read_effects = 20
              return
          end if
          dist = ator(substr)

c     	Char 37: space.
          if (line(37:37) .ne. ' ' ) then
              isf_error = 'bad format, char 37: '//line
              read_effects = 20
              return
          end if

c     	Chars 38-42: azimuth.
          if (partline(substr,line,38,5) .eq. 0) then
              isf_error = 'missing azimuth: '//line
              read_effects = 20
              return
          end if

          if (check_real(substr) .eq. 1) then
              isf_error = 'bad azimuth: '//line
              read_effects = 20
              return
          end if
          azim = ator(substr)

c     	Chars 43-46 should be blank.
          if (partline(substr,line,43,4) .ne. 0) then
              isf_error = 'bad DistAz format: '//line
              read_effects = 20
              return
          end if

      elseif (loctype .eq. 'CoPost') then

c     	Chars 29-31: country code.
          if (partline(country,line,29,3) .eq. 0) then
              isf_error = 'missing country: '//line
              read_effects = 20
              return
          end if

c     	Char 32: space.
          if (line(32:32) .ne. ' ' ) then
              isf_error = 'bad format, char 32: '//line
              read_effects = 20
              return
          end if

c     	Chars 33-42: post code.
          if (partline(postcode,line,33,10) .eq. 0) then
              isf_error = 'missing post code: '//line
              read_effects = 20
              return
          end if

c     	Chars 43-46 should be blank.
          if (partline(substr,line,43,4) .ne. 0) then
              isf_error = 'bad CoPost format: '//line
              read_effects = 20
              return
          end if

      elseif (loctype .eq. 'StaNet') then

c     	Chars 29-37: network code.
          if (partline(net,line,29,9) .eq. 0) then
              isf_error = 'missing network: '//line
              read_effects = 20
              return
          end if

c     	Char 38: space.
          if (line(38:38) .ne. ' ' ) then
              isf_error = 'bad format, char 38: '//line
              read_effects = 20
              return
          end if

c     	Chars 39-43: station code.
          if (partline(sta,line,39,5) .eq. 0) then
              isf_error = 'missing station code: '//line
              read_effects = 20
              return
          end if

c     	Chars 44-46 should be blank.
          if (partline(substr,line,43,3) .ne. 0) then
              isf_error = 'bad StaNet format: '//line
              read_effects = 20
              return
          end if
      else
          isf_error = 'unknown loctype: '//line
          read_effects = 20
          return
      end if

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_error = 'bad format, char 47: '//line
          read_effects = 20
          return
      end if

c     48-51: first intensity.
c     If first intensity null then don't allow second one or scale.
      if (partline(substr,line,48,4) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad intensity: '//line
              read_effects = 20
              return
          end if
          intensity1 = ator(substr)

c     	Char 52: intensity modifier.
          if (line(52:52) .ne. ' ' .and. line(52:52) .ne. '-' .and.
     +    line(52:52) .ne. '+' ) then
              isf_error = 'bad intensity modifier: '//line
              read_effects = 20
              return
          end if
          modifier = line(52:52)

c     	Chars 53-56: second intensity, only allowed if modifier is '-'.
          if (modifier .eq. '-') then

              if (partline(substr,line,53,4) .eq. 0) then
                  isf_error = 'missing intensity 2: '//line
                  read_effects = 20
                  return
              end if

              if (check_real(substr) .eq. 1) then
                  isf_error = 'bad intensity 2: '//line
                  read_effects = 20
                  return
              end if
              intensity2 = ator(substr)
          else 
              if (partline(substr,line,53,4) .ne. 0) then
                  isf_error = 'bad intensity format: '//line
                  read_effects = 20
                  return
              end if
              intensity2 = ISF_NULL
          end if

c     	Char 57: space.
          if (line(57:57) .ne. ' ' ) then
              isf_error = 'bad format, char 57: '//line
              read_effects = 20
              return
          end if

c     	Chars 58-62: intensity scale.
          if (partline(scale,line,57,5) .ne. 0) then
              if ( check_whole(scale) .eq. 1 ) then
                  isf_error = 'bad intensity scale: '//line
                  read_effects = 20
                  return
              end if
          else
              scale = ' '
          end if
      else
          if (partline(substr,line,52,11) .ne. 0) then
              isf_error = 'bad intensity format: '//line
              read_effects = 20
              return
          end if
          intensity1 = ISF_NULL
          modifier = ' '
          intensity2 = ISF_NULL
          scale = ' '
      end if

c     Char 63: space.
      if (line(63:63) .ne. ' ' ) then
          isf_error = 'bad format, char 63: '//line
          read_effects = 20
          return
      end if

c     Chars 64-72: author, any characters allowed but must be there.
      if (partline(author,line,64,9) .eq. 0) then
          isf_error = 'missing author: '//line
          read_effects = 20
          return
      end if

      if ( check_whole(author) .eq. 1 ) then
          isf_error = 'bad author: '//line
          read_effects = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,73,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_effects = 20
          return
      end if

      read_effects = 0
      return
      end


c     Tests a line to discover if it is a phase block header line.

c     Returns 0 if the line is a phase block header.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_phase_head(line)

      character line*(*)

      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(122)
      integer headlen 
      data    headlen /122/

      head = 'Sta     Dist  EvAz Phase        Time      TRes  Azim' //
     +       ' AzRes   Slow   SRes Def   SNR       Amp   Per Qual'  //
     +       ' Magnitude    ArrID'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not a phase header: '//line
          read_phase_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_phase_head = 20
          return
      end if

      read_phase_head = 0
      return
      end


c     Parses a line assuming that it is a phase block data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.
                                      
c     Returns 0 if the line is a properly formatted phase line,
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_phase(line,sta,dist,esaz,phase,hh,mi,ss,
     + msec,timeres,azim,azimres,slow,slowres,timedef,azimdef,slowdef,
     + snr,amp,per,picktype,sp_fm,detchar,magtype,magind,mag,arrid)

      character line*(*),sta*(*),arrid*(*),phase*(*),magtype*(*)
      character timedef,azimdef,slowdef,sp_fm,detchar,magind,picktype
      real*4 dist,esaz,timeres,azim,azimres,slow,slowres,snr,amp,per,mag
      integer hh,mi,ss,msec

      include 'isf_head.h'
      integer partline,check_real,check_int,check_whole,isdigit,atoi
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-5: station code.
      if (partline(sta,line,1,5) .eq. 0) then
          isf_error = 'missing sta: '//line
          read_phase = 20
          return
      end if

      if ( check_whole(sta) .eq. 1 ) then
          isf_error = 'bad sta: '//line
          read_phase = 20
          return
      end if

c     Char 6: space.
      if (line(6:6) .ne. ' ' ) then
          isf_error = 'bad format, char 6: '//line
          read_phase = 20
          return
      end if

c     Chars 7-12: distance, real if there.
      if (partline(substr,line,7,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad dist: '//line
              read_phase = 20
              return
          end if
          dist = ator(substr)
      else
          dist = ISF_NULL
      end if

c     Char 13: space.
      if (line(13:13) .ne. ' ' ) then
          isf_error = 'bad format, char 13: '//line
          read_phase = 20
          return
      end if

c     Chars 14-18: event to sta azimuth, real if there.
      if (partline(substr,line,14,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad esaz: '//line
              read_phase = 20
              return
          end if
          esaz = ator(substr)
      else
          esaz = ISF_NULL
      end if

c     Chars 20-27: phase code - can be null.
      if (partline(phase,line,20,8) .ne. 0) then
          if ( check_whole(phase) .eq. 1 ) then
              isf_error = 'bad phase: '//line
              read_phase = 20
              return
          end if
      else
          phase=' '
      end if

c     Char 28: space.
      if (line(28:28) .ne. ' ' ) then
          isf_error = 'bad format, char 28: '//line
          read_phase = 20
          return
      end if

c     Chars 29-40: time - can be null.
      if (partline(substr,line,29,12) .ne. 0) then
c     	Chars 29,30: hour.
          if (partline(substr,line,29,2) .eq. 0) then
              isf_error = 'missing hour: '//line
              read_phase = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad hour: '//line
              read_phase = 20
              return
          end if
          hh = atoi(substr)

c     	Char 31: ':' character.
          if (line(31:31) .ne. ':' ) then
              isf_error = 'bad date: '//line
              read_phase = 20
              return
          end if

c     	Chars 32,33: minute.
          if (partline(substr,line,32,2) .eq. 0) then
              isf_error = 'missing minute: '//line
              read_phase = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad minute: '//line
              read_phase = 20
              return
          end if
          mi = atoi(substr)

c     	Char 34: ':' character.
          if (line(34:34) .ne. ':' ) then
              isf_error = 'bad date: '//line
              read_phase = 20
              return
          end if
      
c     	Chars 35,36: integral second.
          if (partline(substr,line,35,2) .eq. 0) then
              isf_error = 'missing second: '//line
              read_phase = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad second: '//line
              read_phase = 20
              return
          end if
          ss = atoi(substr)

c     	Char 37-40: msec or spaces.
c     	Allow decimal place without any numbers after it.
          if (partline(substr,line,38,3) .ne. 0) then

c     		Char 37: '.' character.
              if (line(37:37) .ne. '.' ) then
                  isf_error = 'bad date: '//line
                  read_phase = 20
                  return
              end if

c     		Chars 38-40: msec.
              if (isdigit(line(38:38)) .eq. 0) then
                  isf_error = 'bad msec: '//line
                  read_phase = 20
                  return
              end if
              msec = (ichar(line(38:38)) - ichar('0'))*100

              if (isdigit(line(39:39)) .ne. 0) then
                  msec = msec + (ichar(line(39:39)) - ichar('0'))*10
              else if (line(39:39) .ne. ' ' .or. line(40:40) .ne. ' ') 
     +        then
                  isf_error = 'bad msec: '//line
                  read_phase = 20
                  return
              end if

              if (isdigit(line(40:40)) .ne. 0) then
                  msec = msec + (ichar(line(40:40)) - ichar('0'))
              else if (line(40:40) .ne. ' ') then
                  isf_error = 'bad msec: '//line
                  read_phase = 20
                  return
              end if
          else
c     		Char 37: '.' character or space.
              if (line(37:37) .ne. ' ' .and. line(37:37) .ne. '.' ) 
     +        then
                  isf_error = 'bad date: '//line
                  read_phase = 20
                  return
              end if
              msec = ISF_NULL
          end if
      else
          hh = ISF_NULL
          mi = ISF_NULL
          ss = ISF_NULL
          msec = ISF_NULL
      end if

c     Char 41: space.
      if (line(41:41) .ne. ' ' ) then
          isf_error = 'bad format, char 41: '//line
          read_phase = 20
          return
      end if

c     Chars 42-46: time residual, real if there.
      if (partline(substr,line,42,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad timeres: '//line
              read_phase = 20
              return
          end if
          timeres = ator(substr)
      else
          timeres = ISF_NULL
      end if

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_error = 'bad format, char 47: '//line
          read_phase = 20
          return
      end if

c     Chars 48-52: observed azimuth, real if there.
      if (partline(substr,line,48,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad azim: '//line
              read_phase = 20
              return
          end if
          azim = ator(substr)
      else
          azim = ISF_NULL
      end if

c     Char 53: space.
      if (line(53:53) .ne. ' ' ) then
          isf_error = 'bad format, char 53: '//line
          read_phase = 20
          return
      end if

c     Chars 54-58: azimuth residual, real if there.
      if (partline(substr,line,54,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad azimres: '//line
              read_phase = 20
              return
          end if
          azimres = ator(substr)
      else
          azimres = ISF_NULL
      end if

c     Char 59: space.
      if (line(59:59) .ne. ' ' ) then
          isf_error = 'bad format, char 59: '//line
          read_phase = 20
          return
      end if

c     Chars 60-65: slowness, real if there.
      if (partline(substr,line,60,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad slow: '//line
              read_phase = 20
              return
          end if
          slow = ator(substr)
      else
          slow = ISF_NULL
      end if

c     Char 66: space.
      if (line(66:66) .ne. ' ' ) then
          isf_error = 'bad format, char 66: '//line
          read_phase = 20
          return
      end if

c     Chars 67-72: slowness residual, real if there.
      if (partline(substr,line,67,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad slowres: '//line
              read_phase = 20
              return
          end if
          slowres = ator(substr)
      else
          slowres = ISF_NULL
      end if

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_error = 'bad format, char 73: '//line
          read_phase = 20
          return
      end if

c     Char 74: time defining flag.
      if (line(74:74) .eq. '_' .or. line(74:74) .eq. 'T' ) then
          timedef=line(74:74)
      else if (line(74:74) .eq. ' ') then
          timedef='_'
      else
          isf_error = 'bad timedef flag: '//line
          read_phase = 20
          return
      end if

c     Char 75: azimuth defining flag.
      if (line(75:75) .eq. '_' .or. line(75:75) .eq. 'A' ) then
          azimdef=line(75:75)
      else if (line(75:75) .eq. ' ') then
          azimdef='_'
      else
          isf_error = 'bad azimdef flag: '//line
          read_phase = 20
          return
      end if

c     Char 76: slowness defining flag.
      if (line(76:76) .eq. '_' .or. line(76:76) .eq. 'S' ) then
          slowdef=line(76:76)
      else if (line(76:76) .eq. ' ') then
          slowdef='_'
      else
          isf_error = 'bad slowdef flag: '//line
          read_phase = 20
          return
      end if

c     Char 77: space.
      if (line(77:77) .ne. ' ' ) then
          isf_error = 'bad format, char 77: '//line
          read_phase = 20
          return
      end if

c     Chars 78-82: signal-to noise, real if there.
      if (partline(substr,line,78,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad snr: '//line
              read_phase = 20
              return
          end if
          snr = ator(substr)
      else
          snr = ISF_NULL
      end if

c     Char 83: space.
      if (line(83:83) .ne. ' ' ) then
          isf_error = 'bad format, char 83: '//line
          read_phase = 20
          return
      end if

c     Chars 84-92: amplitude, real if there.
      if (partline(substr,line,84,9) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad amp: '//line
              read_phase = 20
              return
          end if
          amp = ator(substr)
      else
          amp = ISF_NULL
      end if

c     Char 93: space.
      if (line(93:93) .ne. ' ' ) then
          isf_error = 'bad format, char 93: '//line
          read_phase = 20
          return
      end if

c     Chars 94-98: period, real if there.
      if (partline(substr,line,94,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad per: '//line
              read_phase = 20
              return
          end if
          per = ator(substr)
      else
          per = ISF_NULL
      end if

c     Char 99: space.
      if (line(99:99) .ne. ' ' ) then
          isf_error = 'bad format, char 99: '//line
          read_phase = 20
          return
      end if

c     Char 100: picktype.
      if (line(100:100) .eq. 'a' .or. line(100:100) .eq. 'm' .or.
     +    line(100:100) .eq. '_') then

          picktype=line(100:100)
      else if (line(100:100) .eq. ' ') then
          picktype='_'
      else
          isf_error = 'bad picktype: '//line
          read_phase = 20
          return
      end if

c     Char 101: sp_fm.
      if (line(101:101) .eq. 'c' .or. line(101:101) .eq. 'd' .or.
     +    line(101:101) .eq. '_') then

          sp_fm=line(101:101)
      else if (line(101:101) .eq. ' ') then
          sp_fm='_'
      else
          isf_error = 'bad sp_fm: '//line
          read_phase = 20
          return
      end if

c     Char 102: detchar.
      if (line(102:102) .eq. 'i' .or. line(102:102) .eq. 'e' .or.
     +    line(102:102) .eq. 'q' .or. line(102:102) .eq. '_') then

          detchar=line(102:102)
      else if (line(102:102) .eq. ' ') then
          detchar='_'
      else
          isf_error = 'bad detchar: '//line
          read_phase = 20
          return
      end if

c     Char 103: space.
      if (line(103:103) .ne. ' ' ) then
          isf_error = 'bad format, char 103: '//line
          read_phase = 20
          return
      end if

c     Chars 104-108: magnitude type.
      if (partline(magtype,line,104,5) .ne. 0) then
          if ( check_whole(magtype) .eq. 1 ) then
              isf_error = 'bad magtype: '//line
              read_phase = 20
              return
          end if
      else
          magtype=' '
      end if

c     Char 109: less than or greater than indicator or space only.
      if (line(109:109) .eq. ' ' .or. line(109:109) .eq. '>' .or.
     +    line(109:109) .eq. '<' ) then

          magind=line(109:109)
      else
          isf_error = 'bad magind: '//line
          read_phase = 20
          return
      end if

c     Chars 110-113: magnitude, real if there.
      if (partline(substr,line,110,4) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad mag: '//line
              read_phase = 20
              return
          end if
          mag = ator(substr)
      else
          mag = ISF_NULL
      end if

c     Char 114: space.
      if (line(114:114) .ne. ' ' ) then
          isf_error = 'bad format, char 114: '//line
          read_phase = 20
          return
      end if

c     Chars 115-122: arrival ID, any characters allowed but must be there.
      if (partline(arrid,line,115,9) .eq. 0) then
          isf_error = 'missing arrid: '//line
          read_phase = 20
          return
      end if

      if ( check_whole(arrid) .eq. 1 ) then
          isf_error = 'bad arrid: '//line
          read_phase = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,123,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_phase = 20
          return
      end if

      read_phase = 0
      return
      end


c     Parses a line assuming it to be a phase origid line.

c     Returns 0 if the line is a phase orig line.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_phase_origid(line,origid)

      character line*(*), origid*(*)

      include 'isf_head.h'
      integer partline,check_whole
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: comment start string and space.
      if (line(1:10) .ne. ' (#OrigID ') then
          isf_error = 'not a phase origin line: '//line
          read_phase_origid = 20
          return
      end if

c     Chars 11-18: origin ID.
      if (partline(origid,line,11,8) .eq. 0) then
          isf_error = 'missing origid: '//line
          read_phase_origid = 20
          return
      end if

      if ( check_whole(origid) .eq. 1 ) then
          isf_error = 'bad origid: '//line
          read_phase_origid = 20
          return
      end if

c     Check for extra characters - not including close bracket.
      if (partline(substr,line,19,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_phase_origid = 20
          return
      end if

      read_phase_origid = 0
      return
      end


c     Tests a line to discover if it is a phase info block header line.

c     Returns 0 if the line is a phase info block header.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_phase_info_head(line)

      character line*(*)

      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(125)
      integer headlen 
      data    headlen /124/

      head = 'Net      Chan F Low_F HighF AuthPhas    Date     eTime' //
     +       ' wTime eAzim wAzim  eSlow wSlow      eAmp  ePer eMag'   //     
     +       ' Author      ArrID'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not a phase info header: '//line
          read_phase_info_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_phase_info_head = 20
          return
      end if

      read_phase_info_head = 0
      return
      end


c     Parses a line assuming that it is a phase info block data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.
                                      
c     Returns 0 if the line is a properly formatted phase info line,
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_phase_info(line,net,chan,filter,filter_min,
     +                filter_max,phase,yyyy,mm,dd,time_unc,time_weight, 
     +                azim_unc,azim_weight,slow_unc,slow_weight,
     +                amp_unc,per_unc,mag_unc,author,arrid)

      character line*(*),net*(*),chan*(*),author*(*),arrid*(*)
      character phase*(*),filter
      real*4 filter_min,filter_max,time_unc,time_weight,azim_unc
      real*4 azim_weight,slow_unc,slow_weight,amp_unc,per_unc,mag_unc
      integer yyyy,mm,dd

      include 'isf_head.h'
      integer partline,check_real,check_int,check_whole,atoi
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-9: network code.
      if (partline(net,line,1,9) .ne. 0) then
          if ( check_whole(net) .eq. 1 ) then
              isf_error = 'bad net: '//line
              read_phase_info = 20
              return
          end if
      else
          net = ' '
      end if

c     Char 10: space.
      if (line(10:10) .ne. ' ' ) then
          isf_error = 'bad format, char 10: '//line
          read_phase_info = 20
          return
      end if

c     Chars 11-13: channel.
      if (partline(chan,line,11,3) .ne. 0) then
          if ( check_whole(chan) .eq. 1 ) then
              isf_error = 'bad chan: '//line
              read_phase_info = 20
              return
          end if
      else
          chan = ' '
      end if

c     Char 14: space.
      if (line(14:14) .ne. ' ' ) then
          isf_error = 'bad format, char 14: '//line
          read_phase_info = 20
          return
      end if

c     Char 15: filter.
      if (line(15:15) .eq. '0' .or. line(15:15) .eq. 'C' 
     +                         .or. line(15:15) .eq. ' ') then
          filter=line(15:15)
      else
          isf_error = 'bad filter: '//line
          read_phase_info = 20
          return
      end if

c     Char 16: space.
      if (line(16:16) .ne. ' ' ) then
          isf_error = 'bad format, char 16: '//line
          read_phase_info = 20
          return
      end if

c     Chars 17-21: minimum filter frequency, real if there.
      if (partline(substr,line,17,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad filter_min: '//line
              read_phase_info = 20
              return
          end if
          filter_min = ator(substr)
      else
          filter_min = ISF_NULL
      end if

c     Char 22: space.
      if (line(22:22) .ne. ' ' ) then
          isf_error = 'bad format, char 22: '//line
          read_phase_info = 20
          return
      end if

c     Chars 23-27: maximum filter frequency, real if there.
      if (partline(substr,line,23,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad filter_max: '//line
              read_phase_info = 20
              return
          end if
          filter_max = ator(substr)
      else
          filter_max = ISF_NULL
      end if

c     Char 28: space.
      if (line(28:28) .ne. ' ' ) then
          isf_error = 'bad format, char 28: '//line
          read_phase_info = 20
          return
      end if

c     Chars 29-36: author's phase.
      if (partline(phase,line,29,8) .ne. 0) then
          if ( check_whole(phase) .eq. 1 ) then
              isf_error = 'bad phase: '//line
              read_phase_info = 20
              return
          end if
      else
          phase=' '
      end if

c     Char 37: space.
      if (line(37:37) .ne. ' ' ) then
          isf_error = 'bad format, char 37: '//line
          read_phase_info = 20
          return
      end if

c     Chars 38-47: arrival date.
      if (partline(substr,line,38,10) .ne. 0) then

c     	38-41: year.
          if (partline(substr,line,38,4) .eq. 0) then
              isf_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if
          yyyy = atoi(substr)

c     	Char 42: '/' character.
          if (line(42:42) .ne. '/' ) then
              isf_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if

c     	Chars 43,44: month.
          if (partline(substr,line,43,2) .eq. 0) then
              isf_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if
          mm = atoi(substr)

c     	Char 45: '/' character.
          if (line(45:45) .ne. '/' ) then
              isf_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if

c     	Chars 46,47: day.
          if (partline(substr,line,46,2) .eq. 0) then
              isf_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if
          dd = atoi(substr)

      else
          yyyy = ISF_NULL
          mm = ISF_NULL
          dd = ISF_NULL
      end if

c     Char 48: space.
      if (line(48:48) .ne. ' ' ) then
          isf_error = 'bad format, char 48: '//line
          read_phase_info = 20
          return
      end if

c     Chars  49-54: uncertainty in arrival time.
      if (partline(substr,line,49,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad time_unc: '//line
              read_phase_info = 20
              return
          end if
          time_unc = ator(substr)
      else
          time_unc = ISF_NULL
      end if

c     Char 55: space.
      if (line(55:55) .ne. ' ' ) then
          isf_error = 'bad format, char 55: '//line
          read_phase_info = 20
          return
      end if

c     Chars 56-60: time weight.
      if (partline(substr,line,56,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad time_weight: '//line
              read_phase_info = 20
              return
          end if
          time_weight = ator(substr)
      else
          time_weight = ISF_NULL
      end if

c     Char 61: space.
      if (line(61:61) .ne. ' ' ) then
          isf_error = 'bad format, char 61: '//line
          read_phase_info = 20
          return
      end if

c     Chars 62-66: azimuth uncertainty.
      if (partline(substr,line,61,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad azim_unc: '//line
              read_phase_info = 20
              return
          end if
          azim_unc = ator(substr)
      else
          azim_unc = ISF_NULL
      end if

c     Char 67: space.
      if (line(67:67) .ne. ' ' ) then
          isf_error = 'bad format, char 67: '//line
          read_phase_info = 20
          return
      end if

c     Chars 68-72: azimuth weight.
      if (partline(substr,line,68,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad azim_weight: '//line
              read_phase_info = 20
              return
          end if
          azim_weight = ator(substr)
      else
          azim_weight = ISF_NULL
      end if

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_error = 'bad format, char 73: '//line
          read_phase_info = 20
          return
      end if

c     Chars 74-79: slowness uncertainty.
      if (partline(substr,line,73,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad slow_unc: '//line
              read_phase_info = 20
              return
          end if
          slow_unc = ator(substr)
      else
          slow_unc = ISF_NULL
      end if

c     Char 80: space.
      if (line(80:80) .ne. ' ' ) then
          isf_error = 'bad format, char 80: '//line
          read_phase_info = 20
          return
      end if

c     Chars 81-85: slowness weight.
      if (partline(substr,line,81,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad slow_weight: '//line
              read_phase_info = 20
              return
          end if
          slow_weight = ator(substr)
      else
          slow_weight = ISF_NULL
      end if

c     Char 86: space.
      if (line(86:86) .ne. ' ' ) then
          isf_error = 'bad format, char 86: '//line
          read_phase_info = 20
          return
      end if

c     Chars 87-95: amplitude unceratinty.
      if (partline(substr,line,87,9) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad amp_unc: '//line
              read_phase_info = 20
              return
          end if
          amp_unc = ator(substr)
      else
          amp_unc = ISF_NULL
      end if

c     Char 96: space.
      if (line(96:96) .ne. ' ' ) then
          isf_error = 'bad format, char 96: '//line
          read_phase_info = 20
          return
      end if

c     Chars 97-101: period uncertainty.
      if (partline(substr,line,97,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad per_unc: '//line
              read_phase_info = 20
              return
          end if
          per_unc = ator(substr)
      else
          per_unc = ISF_NULL
      end if

c     Char 102: space.
      if (line(102:102) .ne. ' ' ) then
          isf_error = 'bad format, char 102: '//line
          read_phase_info = 20
          return
      end if


c     Chars 103-105: uncertainty in station magnitude.
      if (partline(substr,line,103,3) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad mag_unc: '//line
              read_phase_info = 20
              return
          end if
          mag_unc = ator(substr)
      else
          mag_unc = ISF_NULL
      end if

c     Char 106: space.
      if (line(106:106) .ne. ' ' ) then
          isf_error = 'bad format, char 106: '//line
          read_phase_info = 20
          return
      end if

c     Chars 107-115: author.
      if (partline(author,line,107,9) .eq. 1) then
          if ( check_whole(author) .eq. 1 ) then
              isf_error = 'bad author: '//line
              read_phase_info = 20
              return
          end if
      else
          author = ' '
      end if

c     Char 116: space.
      if (line(116:116) .ne. ' ' ) then
          isf_error = 'bad format, char 116: '//line
          read_phase_info = 20
          return
      end if

c     Chars 117-124: arrival ID, any characters allowed but must be there.
      if (partline(arrid,line,117,9) .eq. 0) then
          isf_error = 'missing arrid: '//line
          read_phase_info = 20
          return
      end if

      if ( check_whole(arrid) .eq. 1 ) then
          isf_error = 'bad arrid: '//line
          read_phase_info = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,125,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_phase_info = 20
          return
      end if

      read_phase_info = 0
      return
      end


c     Parses a line assuming it to be an additional phase measurement line.
c     Accepts any number of parameter=value pairs as long as the line is
c     short enough.

c     Returns 0 if the line is a properly formatted phase measurement line.
c     Returns 20 and writes a diagnostic to isf_error if not.

      integer function read_phase_measure(line,param,value,error,
     +                                                      numparam)

      character line*(*)
      character param(*)*(*),value(*)*(*),error(*)*(*)
      integer numparam
      include 'isf_head.h'
      integer start,end,break,mid,i
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: should be the comment format string
      if (line(1:11) .ne. ' (#MEASURE ') then
          isf_error = 'not a phase measurement line: '//line
          read_phase_measure = 20
          return
      end if

      start=12
      do while (line(start:start) .eq. ' ')
          start=start+1
      end do

      end=len(line)
      do while (line(end:end) .eq. ' ' .or. line(end:end) .eq. ')')
          if (line(end:end) .eq. ')') then
              line(end:end) = ' '
          end if
          end=end-1
      end do

      if (end .gt. ISF_COMM_LEN+11) then
          isf_error = 'line too long: '//line
          read_phase_measure = 20
          return
      end if

c     Go through the rest of the line one character at a time, separating 
c     words on ' ' to get param=value pairs and on '=' to get the
c     individual parameters and vales.
      numparam=0
      break = index(line(start:),' ')

      do while (break .ne. 0 .and. start .le. end)
          break = break + start
          mid = index(line(start:break),'=')

          if (mid .eq. 0) then
              isf_error = 'param without value: '//line
              read_phase_measure = 20
              return
          end if

          mid = mid + start
          numparam = numparam+1
          param(numparam) = line(start:mid-2)
          value(numparam) = line(mid:break-2)
          start = break
          break = index(line(start:),' ')
      end do

c     For each resulting value check whether includes an error part.
      do i=1,numparam
          mid = index(value(i),'+')
          if (mid .ne. 0) then
              substr = value(i)
              value(i) = substr(1:mid-1)
              error(i) = substr(mid+1:)
          else
              error(i) = ' '
          end if
      end do

      read_phase_measure = 0
      return
      end


c     Parses a line asuming it to be a minimum phase range line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly phase_min data line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_phase_min(line,timeoffset,azoffset,
     +                        slowoffset,ampoffset,peroffset,magoffset)

      character line*(*)
      real*4 timeoffset,azoffset,slowoffset,ampoffset,peroffset,
     +       magoffset

      include 'isf_head.h'
      integer partline,check_real
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-6: comment format string.
      if (line(1:6) .ne. ' (#MIN') then
          isf_error = 'not a phase_min line: '//line
          read_phase_min = 20
          return
      end if

c     Chars 7-47: spaces.
      if (partline(substr,line,7,41) .ne. 0) then
          isf_error = 'not a phase_min line: '//line
          read_phase_min = 20
          return
      end if

c     Chars 48-54: time offset.
      if (partline(substr,line,48,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad timeoffset: '//line
              read_phase_min = 20
              return
          end if
          timeoffset = ator(substr)
      else
          timeoffset = ISF_NULL
      end if

c     Chars 55-60: spaces.
      if (partline(substr,line,55,5) .ne. 0) then
          isf_error = 'bad format, chars 55-60: '//line
          read_phase_min = 20
          return
      end if

c     Chars 61-66: azimuth offset.
      if (partline(substr,line,61,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad azoffset: '//line
              read_phase_min = 20
              return
          end if
          azoffset = ator(substr)
      else
          azoffset = ISF_NULL
      end if

c     Chars 67-72: spaces.
      if (partline(substr,line,67,6) .ne. 0) then
          isf_error = 'bad format, chars 67-72: '//line
          read_phase_min = 20
          return
      end if

c     Chars 73-79: slowness offset.
      if (partline(substr,line,73,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad slowoffset: '//line
              read_phase_min = 20
              return
          end if
          slowoffset = ator(substr)
      else
          slowoffset = ISF_NULL
      end if

c     Chars 80-85: spaces.
      if (partline(substr,line,80,6) .ne. 0) then
          isf_error = 'bad format, chars 80-85: '//line
          read_phase_min = 20
          return
      end if

c     Chars 86-95: amplitude offset.
      if (partline(substr,line,86,10) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad ampoffset: '//line
              read_phase_min = 20
              return
          end if
          ampoffset = ator(substr)
      else
          ampoffset = ISF_NULL
      end if

c     Chars 96-101: period offset.
      if (partline(substr,line,96,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad peroffset: '//line
              read_phase_min = 20
              return
          end if
          peroffset = ator(substr)
      else
          peroffset = ISF_NULL
      end if

c     Chars 102-105: magnitude offset.
      if (partline(substr,line,102,4) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad magoffset: '//line
              read_phase_min = 20
              return
          end if
          magoffset = ator(substr)
      else
          magoffset = ISF_NULL
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,106,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_phase_min = 20
          return
      end if

      read_phase_min = 0
      return
      end

c     Parses a line asuming it to be a maximum phase range line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly phase_max data line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_phase_max(line,timeoffset,azoffset,
     +                        slowoffset,ampoffset,peroffset,magoffset)

      character line*(*)
      real*4 timeoffset,azoffset,slowoffset,ampoffset,peroffset,
     +       magoffset

      include 'isf_head.h'
      integer partline,check_real
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-6: comment format string.
      if (line(1:6) .ne. ' (#MAX') then
          isf_error = 'not a phase_max line: '//line
          read_phase_max = 20
          return
      end if

c     Chars 7-47: spaces.
      if (partline(substr,line,7,41) .ne. 0) then
          isf_error = 'not a phase_max line: '//line
          read_phase_max = 20
          return
      end if

c     Chars 48-54: time offset.
      if (partline(substr,line,48,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad timeoffset: '//line
              read_phase_max = 20
              return
          end if
          timeoffset = ator(substr)
      else
          timeoffset = ISF_NULL
      end if

c     Chars 55-60: spaces.
      if (partline(substr,line,55,5) .ne. 0) then
          isf_error = 'bad format, chars 55-60: '//line
          read_phase_max = 20
          return
      end if

c     Chars 61-66: azimuth offset.
      if (partline(substr,line,61,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad azoffset: '//line
              read_phase_max = 20
              return
          end if
          azoffset = ator(substr)
      else
          azoffset = ISF_NULL
      end if

c     Chars 67-72: spaces.
      if (partline(substr,line,67,6) .ne. 0) then
          isf_error = 'bad format, chars 67-72: '//line
          read_phase_max = 20
          return
      end if

c     Chars 73-79: slowness offset.
      if (partline(substr,line,73,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad slowoffset: '//line
              read_phase_max = 20
              return
          end if
          slowoffset = ator(substr)
      else
          slowoffset = ISF_NULL
      end if

c     Chars 80-85: spaces.
      if (partline(substr,line,80,6) .ne. 0) then
          isf_error = 'bad format, chars 80-85: '//line
          read_phase_max = 20
          return
      end if

c     Chars 86-95: amplitude offset.
      if (partline(substr,line,86,10) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad ampoffset: '//line
              read_phase_max = 20
              return
          end if
          ampoffset = ator(substr)
      else
          ampoffset = ISF_NULL
      end if

c     Chars 96-101: period offset.
      if (partline(substr,line,96,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad peroffset: '//line
              read_phase_max = 20
              return
          end if
          peroffset = ator(substr)
      else
          peroffset = ISF_NULL
      end if

c     Chars 102-105: magnitude offset.
      if (partline(substr,line,102,4) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad magoffset: '//line
              read_phase_max = 20
              return
          end if
          magoffset = ator(substr)
      else
          magoffset = ISF_NULL
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,106,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_phase_max = 20
          return
      end if

      read_phase_max = 0
      return
      end

c     Parses a line asuming it to be a phase correction line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly phase correction line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_phase_correc(line,timecorr,azcorr,
     +                        slowcorr,ampcorr,percorr,magcorr)

      character line*(*)
      real*4 timecorr,azcorr,slowcorr,ampcorr,percorr,magcorr

      include 'isf_head.h'
      integer partline,check_real
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-8: comment format string.
      if (line(1:8) .ne. ' (#COREC') then
          isf_error = 'not a phase correction line: '//line
          read_phase_correc = 20
          return
      end if

c     Chars 9-47: spaces.
      if (partline(substr,line,9,39) .ne. 0) then
          isf_error = 'not a phase correction line: '//line
          read_phase_correc = 20
          return
      end if

c     Chars 48-54: time correction.
      if (partline(substr,line,48,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad timecorr: '//line
              read_phase_correc = 20
              return
          end if
          timecorr = ator(substr)
      else
          timecorr = ISF_NULL
      end if

c     Chars 55-60: spaces.
      if (partline(substr,line,55,5) .ne. 0) then
          isf_error = 'bad format, chars 55-60: '//line
          read_phase_correc = 20
          return
      end if

c     Chars 61-66: azimuth correction.
      if (partline(substr,line,61,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad azcorr: '//line
              read_phase_correc = 20
              return
          end if
          azcorr = ator(substr)
      else
          azcorr = ISF_NULL
      end if

c     Chars 67-72: spaces.
      if (partline(substr,line,67,6) .ne. 0) then
          isf_error = 'bad format, chars 67-72: '//line
          read_phase_correc = 20
          return
      end if

c     Chars 73-79: slowness correction.
      if (partline(substr,line,73,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad slowcorr: '//line
              read_phase_correc = 20
              return
          end if
          slowcorr = ator(substr)
      else
          slowcorr = ISF_NULL
      end if

c     Chars 80-85: spaces.
      if (partline(substr,line,80,6) .ne. 0) then
          isf_error = 'bad format, chars 80-85: '//line
          read_phase_correc = 20
          return
      end if

c     Chars 86-95: amplitude correction.
      if (partline(substr,line,86,10) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad ampcorr: '//line
              read_phase_correc = 20
              return
          end if
          ampcorr = ator(substr)
      else
          ampcorr = ISF_NULL
      end if

c     Chars 96-101: period correction.
      if (partline(substr,line,96,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad percorr: '//line
              read_phase_correc = 20
              return
          end if
          percorr = ator(substr)
      else
          percorr = ISF_NULL
      end if

c     Chars 102-106: magnitude correction.
      if (partline(substr,line,102,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad magcorr: '//line
              read_phase_correc = 20
              return
          end if
          magcorr = ator(substr)
      else
          magcorr = ISF_NULL
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,107,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_phase_correc = 20
          return
      end if

      read_phase_correc = 0
      return
      end


c     Parses a line asuming it to be an original phase data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted original phase data line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_phase_original(line,chan,sta,yyyy,mm,
     + dd,hh,mi,ss,msec,azim,slow,amp,per,mag)

      character line*(*),sta*(*),chan*(*)
      real*4 azim,slow,amp,per,mag
      integer yyyy,mm,dd,hh,mi,ss,msec

      include 'isf_head.h'
      integer partline,check_int,atoi,check_real,check_whole,isdigit
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: comment format string.
      if (line(1:10) .ne. ' (#ORIG   ') then
          isf_error = 'not an original phase line: '//line
          read_phase_original = 20
          return
      end if

c     Chars 11-13: original channel.
      if (partline(chan,line,11,3) .ne. 0) then
          if ( check_whole(chan) .eq. 1 ) then
              isf_error = 'bad chan: '//line
              read_phase_original = 20
              return
          end if
      else
          chan=' '
      end if

c     Char 14: space.
      if (line(14:14) .ne. ' ' ) then
          isf_error = 'bad format, char 14: '//line
          read_phase_original = 20
          return
      end if

c     Chars 15-22: original station code.
      if (partline(sta,line,15,8) .ne. 0) then
          if ( check_whole(sta) .eq. 1 ) then
              isf_error = 'bad sta: '//line
              read_phase_original = 20
              return
          end if
      else
          sta=' '
      end if

c     Chars 23-37: spaces.
      if (partline(substr,line,23,15) .ne. 0) then
          isf_error = 'bad format, chars 23-37: '//line
          read_phase_original = 20
          return
      end if

c     Chars 38-60: arrival date and time.
      if (partline(substr,line,38,10) .ne. 0) then

c     	38-41: year.
          if (partline(substr,line,38,4) .eq. 0) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          yyyy = atoi(substr)

c     	Char 42: '/' character.
          if (line(42:42) .ne. '/' ) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

c     	Chars 43,44: month.
          if (partline(substr,line,43,2) .eq. 0) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          mm = atoi(substr)

c     	Char 45: '/' character.
          if (line(45:45) .ne. '/' ) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

c     	Chars 46,47: day.
          if (partline(substr,line,46,2) .eq. 0) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          dd = atoi(substr)

c     	Char 48: space.
          if (line(48:48) .ne. ' ' ) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

c     	Chars 49,50: hour.
          if (partline(substr,line,49,2) .eq. 0) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          hh = atoi(substr)

c     	Char 51: ':' character.
          if (line(51:51) .ne. ':' ) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

c     	Chars 52,53: minute.
          if (partline(substr,line,52,2) .eq. 0) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          mi = atoi(substr)

c     	Char 54: ':' character.
          if (line(54:54) .ne. ':' ) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

c     	Chars 55,56: integral second.
          if (partline(substr,line,55,2) .eq. 0) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          ss = atoi(substr)

c     	Char 57-60: msec or spaces.
c     	Allow decimal place without any numbers after it.
          if (partline(substr,line,58,3) .ne. 0) then

c     		Char 57: '.' character.
              if (line(57:57) .ne. '.' ) then
                  isf_error = 'bad date: '//line
                  read_phase_original = 20
                  return
              end if

c     		Chars 58-60: msec.
              if (isdigit(line(58:58)) .eq. 0) then
                  isf_error = 'bad date: '//line
                  read_phase_original = 20
                  return
              end if
              msec = (ichar(line(58:58)) - ichar('0'))*100

              if (isdigit(line(59:59)) .ne. 0) then
                  msec = msec + (ichar(line(59:59)) - ichar('0'))*10
              else if (line(59:59) .ne. ' ' .or. line(60:60) .ne. ' ') 
     +        then
                  isf_error = 'bad date: '//line
                  read_phase_original = 20
                  return
              end if

              if (isdigit(line(60:60)) .ne. 0) then
                  msec = msec + (ichar(line(60:60)) - ichar('0'))
              else if (line(60:60) .ne. ' ') then
                  isf_error = 'bad date: '//line
                  read_phase_original = 20
                  return
              end if
          else
c     		Char 57: '.' character or space.
              if (line(57:57) .ne. ' ' .and. line(57:57) .ne. '.' ) 
     +        then
                  isf_error = 'bad date: '//line
                  read_phase_original = 20
                  return
              end if

              msec = ISF_NULL
          end if
      else
          yyyy = ISF_NULL
          mm   = ISF_NULL
          dd   = ISF_NULL
          hh   = ISF_NULL
          mi   = ISF_NULL
          ss   = ISF_NULL
          msec = ISF_NULL
      end if

c     Char 61: space.
      if (line(61:61) .ne. ' ' ) then
          isf_error = 'bad format, char 61: '//line
          read_phase_original = 20
          return
      end if

c     Chars 62-66: original azimuth.
      if (partline(substr,line,62,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad azim: '//line
              read_phase_original = 20
              return
          end if
          azim = ator(substr)
      else
          azim = ISF_NULL
      end if

c     Chars 67-73: spaces.
      if (partline(substr,line,67,7) .ne. 0) then
          isf_error = 'bad format, chars 67-73: '//line
          read_phase_original = 20
          return
      end if

c     Chars 74-79: original slowness.
      if (partline(substr,line,74,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad slow: '//line
              read_phase_original = 20
              return
          end if
          slow = ator(substr)
      else
          slow = ISF_NULL
      end if

c     Chars 80-86: spaces.
      if (partline(substr,line,80,7) .ne. 0) then
          isf_error = 'bad format, chars 80-86: '//line
          read_phase_original = 20
          return
      end if

c     Chars 87-95:  original amplitude.
      if (partline(substr,line,87,9) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad amp: '//line
              read_phase_original = 20
              return
          end if
          amp = ator(substr)
      else
          amp = ISF_NULL
      end if

c     Char 96: space.
      if (line(96:96) .ne. ' ' ) then
          isf_error = 'bad format, char 96: '//line
          read_phase_original = 20
          return
      end if

c     Chars 97-101: original period.
      if (partline(substr,line,97,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad per: '//line
              read_phase_original = 20
              return
          end if
          per = ator(substr)
      else
          per = ISF_NULL
      end if

c     Char 102: space.
      if (line(102:102) .ne. ' ' ) then
          isf_error = 'bad format, char 102: '//line
          read_phase_original = 20
          return
      end if

c     Chars  103-105: original station magnitude.
      if (partline(substr,line,103,3) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_error = 'bad mag: '//line
              read_phase_original = 20
              return
          end if
          mag = ator(substr)
      else
          mag = ISF_NULL
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,106,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_phase_original = 20
          return
      end if

      read_phase_original = 0
      return
      end


c     Parses a line asuming it to be a comment line.

c     Returns 0 if the line is a properly formatted comment line.
c     Returns 20 and writes a diagnostic to isf_error on error.

      integer function read_comment(line,comment)

      character line*(*),comment*(*)

      include 'isf_head.h'
      integer partline

c     Chars 1-2: comment format string.
      if (line(1:2) .ne. ' (') then
          isf_error = 'not a comment line: '//line
          read_comment = 20
          return
      end if

c     partline will clean off final bracket.
      if (partline(comment,line,3,0) .gt. ISF_LINE_LEN) then
          isf_error = 'comment too long: '//line
          read_comment = 20
          return
      end if

      read_comment = 0
      return
      end


c     Tests a line to discover if it is a stop line.

c     Returns 0 if the line is a stop line.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_stop(line)

      character line*(*)

      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)

c     Chars 1-2: comment format string.
      if (line(1:4) .ne. 'STOP') then
          isf_error = 'not a stop line: '//line
          read_stop = 20
          return
      end if

      if (partline(substr,line,5,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_stop = 20
          return
      end if

      read_stop = 0
      return
      end


c     Data type ARRIVAL:GROUPED
c     =========================

c     Tests a line to discover if it is a grouped arrival block header line.

c     Returns 0 if the line is a phase block header.
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_group_head(line)

      character line*(*)

      include 'isf_head.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(125)
      integer headlen 
      data    headlen /125/

      head = 'Net       Sta  Chan Aux     Date      Time'  //
     +       '       Phase     Azim  Slow   SNR       Amp' //
     +       '   Per Qual   Group C Author       ArrID'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_error = 'not an grouped arrival header: '//line
          read_group_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_group_head = 20
          return
      end if

      read_group_head = 0
      return
      end

c     Parses a line assuming that it is a grouped arrival data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.
                                      
c     Returns 0 if the line is a properly formatted phase line,
c     Returns 20 and writes a diagnostic to isf_error otherwise.

      integer function read_group(line,net,sta,chan,auxid,yyyy,mm,dd,
     + hh,mi,ss,msec,phase,azim,slow,snr,amp,per,picktype,sp_fm,
     + detchar,groupid,conflict,author,arrid)

      character line*(*),net*(*),sta*(*),chan*(*),auxid*(*),phase*(*)
      character groupid*(*),author*(*),arrid*(*)
      character sp_fm,detchar,picktype
      real*4 azim,slow,snr,amp,per
      integer yyyy,mm,dd,hh,mi,ss,msec,conflict

      include 'isf_head.h'
      integer partline,check_real,check_int,check_whole,isdigit,atoi
      real*4 ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-9: network code - can be null.
      if (partline(net,line,1,9) .ne. 0) then
          if ( check_whole(net) .eq. 1 ) then
              isf_error = 'bad net: '//line
              read_group = 20
              return
          end if
      else
          net=' '
      end if

c     Char 10: space.
      if (line(10:10) .ne. ' ' ) then
          isf_error = 'bad format, char 10: '//line
          read_group = 20
          return
      end if

c     Chars 11-15: station code.
      if (partline(sta,line,11,5) .eq. 0) then
          isf_error = 'missing sta: '//line
          read_group = 20
          return
      end if

      if ( check_whole(sta) .eq. 1 ) then
          isf_error = 'bad sta: '//line
          read_group = 20
          return
      end if

c     Char 16: space.
      if (line(16:16) .ne. ' ' ) then
          isf_error = 'bad format, char 16: '//line
          read_group = 20
          return
      end if

c     Chars 17-19: channel - can be null.
      if (partline(chan,line,17,3) .ne. 0) then
          if ( check_whole(chan) .eq. 1 ) then
              isf_error = 'bad chan: '//line
              read_group = 20
              return
          end if
      else
          chan=' '
      end if

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_error = 'bad format, char 20: '//line
          read_group = 20
          return
      end if

c     Chars 21-24: auxid - can be null.
      if (partline(auxid,line,21,4) .ne. 0) then
          if ( check_whole(auxid) .eq. 1 ) then
              isf_error = 'bad auxid: '//line
              read_group = 20
              return
          end if
      else
          auxid=' '
      end if

c     Char 25: space.
      if (line(25:25) .ne. ' ' ) then
          isf_error = 'bad format, char 25: '//line
          read_group = 20
          return
      end if

c     Chars 26-48: date/time - can be null

      if (partline(substr,line,26,23) .ne. 0) then

c       Chars 26-29: year
        if (partline(substr,line,26,4) .eq. 0) then
          isf_error = 'missing year: '//line
          read_group = 20
          return
        end if

        if (check_int(substr) .eq. 1) then
          isf_error = 'bad year: '//line
          read_group = 20
          return
        end if
        yyyy = atoi(substr)

c       Char 30: '/' character.
        if (line(30:30) .ne. '/') then
          isf_error = 'bad date: '//line
          read_group = 20
          return
        end if

c       Chars  31-32: month.
        if (partline(substr,line,31,2) .eq. 0) then
          isf_error = 'missing month: '//line
          read_group = 20
          return
        end if

        if (check_int(substr) .eq. 1) then
          isf_error = 'bad month: '//line
          read_group = 20
          return
        end if
        mm = atoi(substr)

c       Char 33: '/' character.
        if (line(33:33) .ne. '/') then
          isf_error = 'bad date: '//line
          read_group = 20
          return
        end if

c       Chars 34-35: day.
        if (partline(substr,line,34,2) .eq. 0) then
          isf_error = 'missing day: '//line
          read_group = 20
          return
        end if

        if (check_int(substr) .eq. 1) then
          isf_error = 'bad day: '//line
          read_group = 20
          return
        end if
        dd = atoi(substr)

c       Char 36: space.
        if (line(36:36) .ne. ' ' ) then
          isf_error = 'bad format, char 36: '//line
          read_group = 20
          return
        end if

c     	Chars 37,38: hour.
        if (partline(substr,line,37,2) .eq. 0) then
          isf_error = 'missing hour: '//line
          read_group = 20
          return
        end if

        if (check_int(substr) .eq. 1) then
          isf_error = 'bad hour: '//line
          read_group = 20
          return
        end if
        hh = atoi(substr)

c     	Char 39: ':' character.
        if (line(39:39) .ne. ':' ) then
          isf_error = 'bad date: '//line
          read_group = 20
          return
        end if

c     	Chars 40,41: minute.
        if (partline(substr,line,40,2) .eq. 0) then
          isf_error = 'missing minute: '//line
          read_group = 20
          return
        end if

        if (check_int(substr) .eq. 1) then
          isf_error = 'bad minute: '//line
          read_group = 20
          return
        end if
        mi = atoi(substr)

c     	Char 42: ':' character.
        if (line(42:42) .ne. ':' ) then
          isf_error = 'bad date: '//line
          read_group = 20
          return
        end if
      
c     	Chars 43,44: integral second.
        if (partline(substr,line,43,2) .eq. 0) then
          isf_error = 'missing second: '//line
          read_group = 20
          return
        end if

        if (check_int(substr) .eq. 1) then
          isf_error = 'bad second: '//line
          read_group = 20
          return
        end if
        ss = atoi(substr)

c     	Char 45-48: msec or spaces.
c     	Allow decimal place without any numbers after it.
        if (partline(substr,line,45,4) .ne. 0) then

c     	  Char 45: '.' character.
          if (line(45:45) .ne. '.' ) then
            isf_error = 'bad date: '//line
            read_group = 20
            return
          end if

c         Chars 46-48: msec.
          if (isdigit(line(46:46)) .eq. 0) then
            isf_error = 'bad msec: '//line
            read_group = 20
            return
          end if
          msec = (ichar(line(46:46)) - ichar('0'))*100

          if (isdigit(line(47:47)) .ne. 0) then
            msec = msec + (ichar(line(47:47)) - ichar('0'))*10
          else if (line(47:47) .ne. ' ' .or. line(48:48) .ne. ' ') 
     +    then
            isf_error = 'bad msec: '//line
            read_group = 20
            return
          end if

          if (isdigit(line(48:48)) .ne. 0) then
            msec = msec + (ichar(line(48:48)) - ichar('0'))
          else if (line(48:48) .ne. ' ') then
            isf_error = 'bad msec: '//line
            read_group = 20
            return
          end if
        else
c         Char 45: '.' character or space.
          if (line(45:45) .ne. ' ' .and. line(45:45) .ne. '.' ) 
     +    then
            isf_error = 'bad date: '//line
            read_group = 20
            return
           end if
           msec = ISF_NULL
        end if
      else
          yyyy = ISF_NULL
          mm = ISF_NULL
          dd = ISF_NULL
          hh = ISF_NULL
          mi = ISF_NULL
          ss = ISF_NULL
          msec = ISF_NULL
      end if

c     Char 49: space.
      if (line(49:49) .ne. ' ' ) then
          isf_error = 'bad format, char 49: '//line
          read_group = 20
          return
      end if

c     Chars 50-57: phase - can be null.
      if (partline(phase,line,50,8) .ne. 0) then
          if ( check_whole(phase) .eq. 1 ) then
              isf_error = 'bad phase: '//line
              read_group = 20
              return
          end if
      else
          phase=' '
      end if

c     Char 58: space.
      if (line(58:58) .ne. ' ' ) then
          isf_error = 'bad format, char 58: '//line
          read_group = 20
          return
      end if

c     Chars 59-63: observed azimuth, real if there.
      if (partline(substr,line,59,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad azim: '//line
              read_group = 20
              return
          end if
          azim = ator(substr)
      else
          azim = ISF_NULL
      end if


c     Char 64: space.
      if (line(64:64) .ne. ' ' ) then
          isf_error = 'bad format, char 64: '//line
          read_group = 20
          return
      end if

c     Chars 65-69: slowness, real if there.
      if (partline(substr,line,65,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad slow: '//line
              read_group = 20
              return
          end if
          slow = ator(substr)
      else
          slow = ISF_NULL
      end if

c     Char 70: space.
      if (line(70:70) .ne. ' ' ) then
          isf_error = 'bad format, char 70: '//line
          read_group = 20
          return
      end if

c     Chars 71-75: SNR, real if there.
      if (partline(substr,line,71,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad snr: '//line
              read_group = 20
              return
          end if
          snr = ator(substr)
      else
          snr = ISF_NULL
      end if

c     Char 76: space.
      if (line(76:76) .ne. ' ' ) then
          isf_error = 'bad format, char 76: '//line
          read_group = 20
          return
      end if

c     Chars 77-85: amplitude, real if there.
      if (partline(substr,line,77,9) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad amp: '//line
              read_group = 20
              return
          end if
          amp = ator(substr)
      else
          amp = ISF_NULL
      end if

c     Char 86: space.
      if (line(86:86) .ne. ' ' ) then
          isf_error = 'bad format, char 86: '//line
          read_group = 20
          return
      end if

c     Chars 87-91: period, real if there.
      if (partline(substr,line,87,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_error = 'bad per: '//line
              read_group = 20
              return
          end if
          per = ator(substr)
      else
          per = ISF_NULL
      end if

c     Char 92: space.
      if (line(92:92) .ne. ' ' ) then
          isf_error = 'bad format, char 92: '//line
          read_group = 20
          return
      end if

c     Char 93: picktype.
      if (line(93:93) .eq. 'a' .or. line(93:93) .eq. 'm' .or.
     +    line(93:93) .eq. '_') then

          picktype=line(93:93)
      else if (line(93:93) .eq. ' ') then
          picktype='_'
      else
          isf_error = 'bad picktype: '//line
          read_group = 20
          return
      end if

c     Char 94: sp_fm.
      if (line(94:94) .eq. 'c' .or. line(94:94) .eq. 'd' .or.
     +    line(94:94) .eq. '_') then

          sp_fm=line(94:94)
      else if (line(94:94) .eq. ' ') then
          sp_fm='_'
      else
          isf_error = 'bad sp_fm: '//line
          read_group = 20
          return
      end if

c     Char 95: detchar.
      if (line(95:95) .eq. 'i' .or. line(95:95) .eq. 'e' .or.
     +    line(95:95) .eq. 'q' .or. line(95:95) .eq. '_') then

          detchar=line(95:95)
      else if (line(95:95) .eq. ' ') then
          detchar='_'
      else
          isf_error = 'bad detchar: '//line
          read_group = 20
          return
      end if

c     Char 96: space.
      if (line(96:96) .ne. ' ' ) then
          isf_error = 'bad format, char 96: '//line
          read_group = 20
          return
      end if

c     Chars 97-104: group ID, any characters allowed but must be there.
      if (partline(groupid,line,97,8) .eq. 0) then
          isf_error = 'missing groupid: '//line
          read_group = 20
          return
      end if

      if ( check_whole(groupid) .eq. 1 ) then
          isf_error = 'bad groupid: '//line
          read_group = 20
          return
      end if

c     Char 105: space.
      if (line(105:105) .ne. ' ' ) then
          isf_error = 'bad format, char 105: '//line
          read_group = 20
          return
      end if

c     Char 106: conflict - null means no conflict.
      if (line(106:106) .ne. ' ') then
        if (check_int(line(106:106)) .eq. 1) then
          isf_error = 'bad conflict: '//line
          read_group = 20
          return
        end if
        conflict = atoi(line(106:106))
      else
        conflict = ISF_NULL
      end if

c     Char 107: space.
      if (line(107:107) .ne. ' ' ) then
          isf_error = 'bad format, char 107: '//line
          read_group = 20
          return
      end if

c     Chars 108-116: author - can be null.
      if (partline(author,line,108,9) .ne. 0) then
          if ( check_whole(author) .eq. 1 ) then
              isf_error = 'bad author: '//line
              read_group = 20
              return
          end if
      else
          author=' '
      end if

c     Char 117: space.
      if (line(117:117) .ne. ' ' ) then
          isf_error = 'bad format, char 117: '//line
          read_group = 20
          return
      end if

c     Chars 118-125: arrival ID, any characters allowed but must be there.
      if (partline(arrid,line,118,8) .eq. 0) then
          isf_error = 'missing arrid: '//line
          read_group = 20
          return
      end if

      if ( check_whole(arrid) .eq. 1 ) then
          isf_error = 'bad arrid: '//line
          read_group = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,126,0) .ne. 0) then
          isf_error = 'extra characters at end: '//line
          read_group = 20
          return
      end if

      read_group = 0
      return
      end



c     To check whether the current line type is of an ISF type consistent with 
c     the type of the previous line written.
c     The exception is line_type 'data_type' where previous line type is
c     assumed to be undefined.

c     Checks and resets the global variable isf_prev_line_type. 
c     Returns 0 if the current line type is expected to follow the previous one.  
c     Returns 1 if this line type should not follow the previous line type.

      integer function check_prev_line_type(line_type)

      character line_type*(*)

      include 'isf_head.h'
      character allowed(10)*(ISF_LINE_LEN)
      integer i,n

c     included to avoid comparing strings with different length
c     JS, NORSAR, 20 October 2017
c
      integer ilen
      ilen = len_trim(line_type)
c

      i = 1
c
      if ( line_type(1:ilen) .eq. 'data_type') then
          isf_prev_line_type = line_type
          check_prev_line_type = 0
          return
      else if ( line_type(1:ilen) .eq. 'event_id') then
          allowed(i) = 'data_type'
          i=i+1
          allowed(i) = 'phase'
          i=i+1
          allowed(i) = 'phase_com'
          i=i+1
          allowed(i) = 'phase_info'
      else if ( line_type(1:ilen) .eq. 'origin_head') then
          allowed(i) = 'event_id'
      else if ( line_type(1:ilen) .eq. 'origin_com') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'momten'
      else if ( line_type(1:ilen) .eq. 'origin') then
          allowed(i) = 'origin_head'
          i=i+1
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'momten'
      else if ( line_type(1:ilen) .eq. 'momten_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
      else if ( line_type(1:ilen) .eq. 'momten') then
          allowed(i) = 'momten'
          i=i+1
          allowed(i) = 'momten_head'
          i=i+1
          allowed(i) = 'origin_com'
      else if ( line_type(1:ilen) .eq. 'fault_plane_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'momten'
      else if ( line_type(1:ilen) .eq. 'fault_plane') then
          allowed(i) = 'fault_plane_head'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'origin_com'
      else if ( line_type(1:ilen) .eq. 'axes_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'momten'
      else if ( line_type(1:ilen) .eq. 'axes_err_head') then
          allowed(i) = 'axes_head'
      else if ( line_type(1:ilen) .eq. 'axes_err') then
          allowed(i) = 'axes'
      else if ( line_type(1:ilen) .eq. 'axes') then
          allowed(i) = 'axes_head'
          i=i+1
          allowed(i) = 'axes_err_head'
          i=i+1
          allowed(i) = 'origin_com'
      else if ( line_type(1:ilen) .eq. 'netmag_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'momten'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
      else if ( line_type(1:ilen) .eq. 'netmag_com') then
          allowed(i) = 'netmag'
          i=i+1
          allowed(i) = 'netmag_com'
      else if ( line_type(1:ilen) .eq. 'netmag') then
          allowed(i) = 'netmag_head'
          i=i+1
          allowed(i) = 'netmag'
          i=i+1
          allowed(i) = 'netmag_com'
      else if ( line_type(1:ilen) .eq. 'effects_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'momten'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'netmag'
          i=i+1
          allowed(i) = 'netmag_com'
      else if ( line_type(1:ilen) .eq. 'effects') then
          allowed(i) = 'effects_head'
          i=i+1
          allowed(i) = 'effects'
          i=i+1
          allowed(i) = 'effects_com'
      else if ( line_type(1:ilen) .eq. 'phase_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'momten'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'netmag'
          i=i+1
          allowed(i) = 'netmag_com'
          i=i+1
          allowed(i) = 'effects'
          i=i+1
          allowed(i) = 'effects_com'
      else if ( line_type(1:ilen) .eq. 'phase_origid') then
          allowed(i) = 'phase_head'
          i=i+1
          allowed(i) = 'phase_info_head'
      else if ( line_type(1:ilen) .eq. 'phase_info_head') then
          allowed(i) = 'phase'
          i=i+1
          allowed(i) = 'phase_com'
      else if ( line_type(1:ilen) .eq. 'phase_info_com') then
          allowed(i) = 'phase_info'
          i=i+1
          allowed(i) = 'phase_info_com'
      else if ( line_type(1:ilen) .eq. 'phase_info') then
          allowed(i) = 'phase_origid'
          i=i+1
          allowed(i) = 'phase_info'
          i=i+1
          allowed(i) = 'phase_info_com'
          i=i+1
          allowed(i) = 'phase_info_head'
      else if ( line_type(1:ilen) .eq. 'phase_com') then
          allowed(i) = 'phase'
          i=i+1
          allowed(i) = 'phase_com'
          i=i+1
          allowed(i) = 'group_phase'
      else if ( line_type(1:ilen) .eq. 'phase') then
          allowed(i) = 'phase_origid'
          i=i+1
          allowed(i) = 'phase_head'
          i=i+1
          allowed(i) = 'phase'
          i=i+1
          allowed(i) = 'phase_com'
      else if ( line_type(1:ilen) .eq. 'group_head') then
          allowed(i) = 'data_type'
          i=i+1
          allowed(i) = 'group_phase'
          i=i+1
          allowed(i) = 'phase_com'
      else if ( line_type(1:ilen) .eq. 'group_phase') then
          allowed(i) = 'group_head'
          i=i+1
          allowed(i) = 'group_phase'
          i=i+1
          allowed(i) = 'phase_com'
      else if ( line_type(1:ilen) .eq. 'stop') then
          allowed(i) = 'phase'
          i=i+1
          allowed(i) = 'phase_com'
          i=i+1
          allowed(i) = 'phase_info'
          i=i+1
          allowed(i) = 'phase_info_com'
          i=i+1
          allowed(i) = 'group_phase'
      end if
      n = i

      do i=1,n
          if (isf_prev_line_type .eq. allowed(i)) then
              isf_prev_line_type = line_type
              check_prev_line_type = 0
              return
          end if
      end do

      isf_error = 'out of sequence: '//line_type//' following 
     +'//isf_prev_line_type
      isf_prev_line_type = line_type
      check_prev_line_type = 1
      return
      end

c     Writes a real a bit more flexibly than can be achieved with a format.
c     If a number is too big for the ideal precision it is printed with less
c     precision until it fills the field width without a decimal point at all.
c     For example might want 99.9999 => 99.99 but 999.9999 => 999.9.

      subroutine write_real(file,x,width,max_prec)

      integer file,width,max_prec
      real*4 x
      character form*(20)

      integer prec,spare

      if ( x .gt. 0. ) then
          spare = width - 1 - int(log10(abs(x)))
      else if ( x .lt. 0 ) then
          spare = width - 2 - int(log10(abs(x)))
      else
          spare = max_prec
      end if

      if (spare .le. 0) then
          write (form,'(''(i'',i2,'',$)'')') width
          write (file,form) int(x)
      else
c
c JS: 1 Dec 2011 to avoid some formatting errors 
c     when number is negative
c
c         if (spare .ge. max_prec) then
          if (spare .gt. max_prec) then
              prec = max_prec 
          else
c
c JS: dito.
c             prec = spare 
              prec = spare - 1
          end if
          write (form,'(''(f'',i1,''.'',i1,'',$)'')') width,prec
          write (file,form) x
      end if

      end


c     Get a substring, removing leading white space.
c     Expects a string, an offset from the start of the string, and a maximum 
c     length for the resulting substring. If this length is 0 it will take up
c     to the end of the input string.

c     Need to allow for ')' to come after the required field at the end of a 
c     comment.  Discard ')' at end of a string  as long as there's no '(' 
c     before it anywhere.

c     Returns the length of the resulting substring.

      integer function partline (substr,line,offset,numchars)

      character substr*(*), line*(*)
      integer offset, numchars
      integer i, start, end
      integer length
      integer bracket

      length = len(line)
      if (length .le. offset) then
          partline=0
          return
      end if

      start=offset
      if (start .le. 0) then
          start=1
      end if

      if (numchars .eq. 0) then
          end = length
      else
          end = offset + numchars - 1
      end if

      do while (line(start:start) .eq. ' ' .and. start .lt. end)
          start=start+1
      end do

      bracket = 0
      do i=start,end
          if (line(i:i) .eq. '(' ) then
              bracket=1
          end if
      end do    

      if (bracket .eq. 1) then
c changed JS 20 October 2017
c         do while ((line(end:end) .eq. ' ') .and. (end .ge. start))
c 11/12/2018  JS
c         do while ((line(end:end) .eq. ' ') .and. (end .gt. start))
          do while ((line(end:end) .eq. ' ') .and. (end .ge. start)
     +               .and. end.gt.1)
              end=end-1
          end do
      else

          do while ( ((line(end:end) .eq. ' ') .or. (line(end:end)
     +              .eq. ')') ) .and. (end .ge. start) .and. end.gt.1)    
c
c 11/12/2018 JS
c    +              .eq. ')') ) .and. (end .gt. start))    
c changed JS 20 October 2017
c    +              .eq. ')') ) .and. (end .ge. start))    

              end=end-1
          end do
      end if

      substr = line(start:end)
      partline = end-start+1
      return

      end


c     To check that a string has no spaces in it.
c     Returns 0 if there are no spaces or 1 if there is a space.

      integer function check_whole(str)

      character str*(*)
      include 'isf_head.h'
      character substr*(ISF_LINE_LEN)
      integer i,length
      integer partline

      length = partline(substr,str,0,0)

      do i=1,length
          if (substr(i:i) .eq. ' ') then
              check_whole = 1
              return
          end if
      end do

      check_whole = 0
      return
      end


c     Check if a string is composed entirely of white space or not.
c     Returns 1 if it is, 0 if it isn't.

      integer function all_blank(str)

      character str*(*)
      include 'isf_head.h'
      character substr*(ISF_LINE_LEN)
      integer i,length
      integer partline

      length = partline(substr,str,0,0)

      do i=1,length
          if (substr(i:i) .ne. ' ' .and. substr(i:i) .ne. '    ') then
              all_blank = 0
              return
          end if
      end do

      all_blank = 1
      return
      end


c     Check whether a real or integer is null or not.
c     Returns 1 if it is, 0 if it isn't.

      integer function is_null(x)

      real*4 x
      include 'isf_head.h'

      if (int(x) .eq. ISF_NULL) then
          is_null = 1
          return
      end if

      is_null = 0
      return
      end


c     To check that a string contains only sign/number characters and so 
c     is suitable for atoi - atoi itself does no checking.

c     Returns 0 if OK,  1 if not.

      integer function check_int(str)

      character str*(*)
      include 'isf_head.h'
      character substr*(ISF_LINE_LEN)
      integer length,start,i
      integer partline,isdigit

      length = partline(substr,str,0,0)

      start = 1
      if (substr(1:1) .eq. '-' .or. substr(1:1) .eq. '+') then
          start = 2
      end if

      do i=start, length
          if (isdigit(substr(i:i)) .eq. 0) then
              check_int = 1
              return
          end if
      end do

      check_int = 0
      return
      end


c     To check if a character is between 1 and 9
c     Returns 0 if it is,  1 if not.

      integer function isdigit(a)

      character a
      integer i

      i =  ichar(a) - ichar('0')
      if (i .gt. 9 .or. i .lt. 0) then
          isdigit = 0
          return
      end if

      isdigit = 1
      return
      end


c     To check if a character is between A and Z
c     Returns 0 if it is,  1 if not.

      integer function isupper(a)

      character a
      integer i

      i =  ichar(a) - ichar('A')
      if (i .gt. 26 .or. i .lt. 0) then
          isupper = 0
          return
      end if

      isupper = 1
      return
      end


c     Converts a string of numbers into an integer.
c     No checking done so need to run check_int on the string first.

      integer function atoi(str)

      character str*(*)
      include 'isf_head.h'
      character substr*(ISF_LINE_LEN)
      integer length,start,i
      integer partline

      length = partline(substr,str,0,0)

      start = 1
      if (substr(1:1) .eq. '-' .or. substr(1:1) .eq. '+') then
          start = 2
      end if

      atoi = 0
      do i=start, length
          atoi = atoi + (ichar(substr(i:i))-ichar('0')) * 
     +(10**(length-i))
      end do

      if (substr(1:1) .eq. '-') then 
          atoi = -atoi
      end if

      return
      end


c     To check that a string is suitable for ator
c     Returns 0 if OK,  1 if not.

      integer function check_real(str)

      character str*(*)
      include 'isf_head.h'
      character substr*(ISF_LINE_LEN)
      integer length,start,i
      integer partline,isdigit

      length = partline(substr,str,0,0)

      start = 1
      if (substr(1:1) .eq. '-' .or. substr(1:1) .eq. '+') then
          start = 2
      end if

      do i=start, length
          if (isdigit(substr(i:i)) .eq. 0) then
              if (substr(i:i) .ne. '.' ) then
                  check_real = 1
                  return
              end if
          end if
      end do

      check_real = 0
      return
      end


c     Converts a string of numbers into a real.
c     No checking done so need to run check_real on the string first.

      function ator(str)

      real*4 ator

      character str*(*)
      include 'isf_head.h'
      character substr*(ISF_LINE_LEN)
      integer length,start,i,point
      integer partline

      length = partline(substr,str,0,0)

      start = 1
      if (substr(1:1) .eq. '-'  .or. substr(1:1) .eq. '+') then
          start = 2
      end if

      point = length+1
      do i=1,length
          if (substr(i:i) .eq. '.') then
              point = i
          end if
      end do

      ator = 0.
      do i=start, point-1
          ator = ator + (ichar(substr(i:i))-ichar('0')) * 
     +(10.0**(point-i-1))
      end do

      do i=point+1,length
          ator = ator + (ichar(substr(i:i))-ichar('0')) * 
     +          (10.0**(point-i))
      end do

      if (substr(1:1) .eq. '-') then 
          ator = -ator
      end if

      return
      end
