c     Writes the data type line at the top of a GSE report.
c     Format is:  DATA_TYPE data_type:subtype data_format:subformat
c     Only data_type is required.  Only other limitation is that a
c     subformat is not allowed without a data_format.

c     This is the only write routine that does not check the value of
c     'isf_prev_line_type' -  this is the first line of a new report.

c     Returns 0 on a successful write.  
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_data_type(file,data_type,subtype,
     +                                    data_format,subformat)

      integer file
      character data_type*(*),subtype*(*),data_format*(*),subformat*(*)

      include 'isf_head.h'
      integer partline, check_prev_line_type, check_whole
      integer numchar
c     line length so far ('DATA DATA_TYPE ')
      character substr*(ISF_LINE_LEN)
      integer length 
      data    length /10/                

c     Check and write data_type.
      numchar = partline(substr,data_type,1,0)
      if (numchar .eq. 0) then
          isf_error = 'null data_type given'
          write_data_type = 20
          return
      end if

      length = length + numchar

      if (length .gt. ISF_LINE_LEN ) then
          isf_error = 'data_type too long '//data_type
          write_data_type = 20
          return
      end if
      if ( check_whole(data_type) .eq. 1 ) then
          isf_error = 'bad data_type: '//data_type
          write_data_type = 20
          return
      end if
      write (file,'(''DATA_TYPE '',a,$)') data_type(1:numchar)

c     Check and write subtype - if there is one.
      numchar = partline(substr,subtype,0,0)
      if (numchar .ne. 0 .and. subtype(1:numchar).ne.'_') then
          length = length + numchar
          if (length .gt. ISF_LINE_LEN ) then
              isf_error = 'data subtype too long 
     +'//subtype(1:numchar)
              write_data_type = 20
              return
          end if
          if ( check_whole(subtype) .eq. 1 ) then
              isf_error = 'bad subtype: '//subtype(1:numchar)
              write_data_type = 20
              return
          end if
          write (file,'('':'',A ,$)') subtype(1:numchar)
      end if

c     Check and write format - if there is one.
      numchar = partline(substr,data_format,0,0)
      if (numchar .ne. 0) then
          length = length + numchar
          if (length .gt. ISF_LINE_LEN ) then
              isf_error = 'line too long 
     +'//data_format(1:numchar)
              write_data_type = 20
              return
          end if
          if ( check_whole(data_format) .eq. 1 ) then
              isf_error = 'bad data_format: 
     +'//data_format(1:numchar)
              write_data_type = 20
              return
          end if
          write (file,'('' '',A ,$)') data_format(1:numchar)

c     	Check and write subformat - if there is one.
          numchar = partline(substr,subformat,0,0)
          if (numchar .ne. 0) then
              length = length + numchar
              if (length .gt. ISF_LINE_LEN ) then
                  isf_error = 'line too long 
     +'//subformat(1:numchar)
                  write_data_type = 20
                  return
              end if
              write (file,'('':'',A ,$)') subformat(1:numchar)
          end if
      else if (partline(substr,subformat,0,0) .ne. 0) then
          isf_error = 'subformat given without format'
          write_data_type = 20
          return
      end if
      write (file,'()')

c     Set 'isf_prev_line_type' for future calls to check_prev_line_type.
c     Do no actual checking for this line type.
      if (check_prev_line_type('data_type') .ne. 0) then
          write_data_type = 20
          return
      end if

      write_data_type = 0
      return
      end

c     Writes an event title line with a preceding blank line.
c     Requires event ID but will write a line without a region if required.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_event_id(file,evid,region)

      integer file
      character evid*(*),region*(*)

      include 'isf_head.h'
      integer partline, check_prev_line_type, check_whole
      integer numchar
      character substr*(ISF_LINE_LEN)

c     Chars 1-5: the word 'Event'. Chars 7-15: event ID.
      numchar = partline(substr,evid,0,0)
      if (numchar .eq. 0) then
          isf_error = 'missing evid'
          write_event_id = 20
          return
      end if
      if (numchar .gt. ISF_EVID_LEN) then
          isf_error = 'evid too long: '//evid
          write_event_id = 20
          return
      end if
      if ( check_whole(evid) .eq. 1 ) then
          isf_error = 'bad evid: '//evid
          write_event_id = 20
          return
      end if
      write (file,'()')
      write (file,'(''Event '',a10,$)') evid

c     Chars 18-80: geographic region if given.
      numchar = partline(substr,region,0,0)
      if (numchar .ne. 0) then
          if (numchar .gt. ISF_REGION_LEN) then
              isf_error = 'region too long: '//region
              write_event_id = 20
              return
          end if
          write (file,'('' '',a,$)') region(1:numchar)
      end if
      write (file,'()')

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('event_id') .ne. 0) then
          write_event_id = 10
          return
      end if

      write_event_id = 0
      return
      end


c     Writes an origin header line.
c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_origin_head(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      character head*(136)

      head = '   Date       Time        Err   RMS Latitude Longitude' //  
     +       '  Smaj  Smin  Az Depth   Err Ndef Nsta Gap  mdist'      //
     +       '  Mdist Qual   Author      OrigID'

      write (file,'(A)') head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('origin_head') .ne. 0) then
          write_origin_head = 10
          return
      end if

      write_origin_head = 0
      return
      end

c     Writes an origin line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_origin(file,yyyy,mm,dd,hh,mi,ss,msec,
     + timfix,stime,sdobs,lat,lon,epifix,smaj,smin,strike,depth,
     + depfix,sdepth,ndef,nsta,gap,mindist,maxdist,antype,loctype,
     + etype,author,origid)

      integer file
      character author*(*), origid*(*), etype*(*)
      character*1 timfix, epifix, depfix, antype, loctype
      integer yyyy, mm, dd, hh, mi, ss, msec
      integer strike, ndef, nsta, gap
      real*4 stime, sdobs, lat, lon, smaj, smin, depth, sdepth
      real*4 mindist, maxdist

      include 'isf_head.h'
      integer partline, check_prev_line_type, is_null, check_whole
      integer numchar
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: date. Char 11: space.
      if (is_null(real(yyyy)) .eq. 1) then
          isf_error = 'missing year'
          write_origin = 20
          return
      end if

      if (yyyy .lt. 1000 .or. yyyy .gt. 9999) then
          write (isf_error,'(''bad year '',i9)') yyyy
          write_origin = 20
          return
      end if

      if (is_null(real(mm)) .eq. 1) then
          isf_error = 'missing month'
          write_origin = 20
          return
      end if

      if (mm .lt. 1 .or. mm .gt. 12) then
          write (isf_error,'(''bad month '',i9)') mm
          write_origin = 20
          return
      end if

      if (is_null(real(dd)) .eq. 1) then
          isf_error = 'missing day'
          write_origin = 20
          return
      end if

      if (dd .lt. 1 .or. dd .gt. 31) then
          write (isf_error,'(''bad day '',i9)') dd
          write_origin = 20
          return
      end if
      write (file,'(i4.4,''/'',i2.2,''/'',i2.2,'' '',$)') 
     +      yyyy,mm,dd

c     Chars 12-19: time.
      if (is_null(real(hh)) .eq. 1) then
          isf_error = 'missing year'
          write_origin = 20
          return
      end if

      if (hh .lt. 0 .or. hh .gt. 23) then
          write (isf_error,'(''bad hour '',i9)') hh
          write_origin = 20
          return
      end if

      if (is_null(real(mi)) .eq. 1) then
          isf_error = 'missing minute'
          write_origin = 20
          return
      end if

      if (mi .lt. 0 .or. mi .gt. 59) then
          write (isf_error,'(''bad minute  '',i9)') mi
          write_origin = 20
          return
      end if

      if (is_null(real(ss)) .eq. 1) then
          isf_error = 'missing second'
          write_origin = 20
          return
      end if

      if (ss .lt. 0 .or. ss .gt. 59) then
          write (isf_error,'(''bad second '',i9)') ss
          write_origin = 20
          return
      end if
      write (file,'(i2.2,'':'',i2.2,'':'',i2.2,$)') hh,mi,ss

c     Chars 20-22 msec - put blanks here if no msec provided.
      if (is_null(real(msec)) .eq. 1) then
          write(file,'(''   '',$)')
      else
          if (msec .lt. 0 .or. msec .gt. 999) then
              write (isf_error,'(''bad msec '',i9)') msec
              write_origin = 20
              return
          end if
          write(file,'(''.'',i2.2,$)') msec/10
      end if

c     Char 23: fixed time flag. Char 24: space.
      if (timfix .eq. 'F') then
          timfix = 'f'
      else if (timfix .ne. ' ' .and. timfix .ne. 'f') then
          write (isf_error,'(''bad timfix: '',a1)') timfix
          write_origin = 20
          return
      end if
      write(file,'(a1,'' '',$)') timfix

c     Chars 25-29: optional origin time error. Char 30: space.
c     Give at least 2 decimal places but less if number > 99.
      if (is_null(stime) .eq. 1) then
          write(file,'(''      '',$)')
      else
          if (stime .lt. 0 .or. stime .gt. 99999) then
              write (isf_error,'(''bad stime: '',f9.2)') stime
              write_origin = 20
              return
          end if
          call write_real(file,stime,5,2)
          write (file,'('' '',$)')
      end if

c     31-35: optional rms (sdobs). Char 36: space.
c     Give 2 decimal places but less if number > 99.
      if (is_null(sdobs) .eq. 1) then
          write(file,'(''      '',$)')
      else
          if (stime .lt. 0 .or. stime .gt. 99999) then
              write (isf_error,'(''bad sdobs: '',f9.2)') sdobs
              write_origin = 20
              return
          end if
          call write_real(file,sdobs,5,2)
          write (file,'('' '',$)')
      end if

c     37-44: lattitude. Char 45: space.
      if (is_null(lat) .eq. 1) then
          isf_error = 'missing latitude'
          write_origin = 20
          return
      end if

      if (lat .lt. -90 .or. lat .gt. 90) then
          write (isf_error,'(''bad latitude: '',f9.2)') lat
          write_origin = 20
          return
      end if
      write (file,'(f8.4,'' '',$)') lat

c     Chars 46-54: longitude.
      if (is_null(lon) .eq. 1) then
          isf_error = 'missing longitude'
          write_origin = 20
          return
      end if

      if (lon .lt. -180 .or. lon .gt. 180) then
          write (isf_error,'(''bad longitude: '',f9.2)') lon
          write_origin = 20
          return
      end if
      write (file,'(f9.4,$)') lon

c     Char 55: fixed epicentre flag.
      if (epifix .eq. 'F') then
          epifix = 'f'
      else if (epifix .ne. ' ' .and. epifix .ne. 'f') then
          write (isf_error,'(''bad epifix: '',a1)') epifix
          write_origin = 20
          return
      end if
      write(file,'(a1,$)') epifix

c     Char 56 should be a space but then can't have 5 digit smaj.
c     Chars 56-60: optional semi-major axis. Char 61: space.
c     Give 1 decimal place but less if number > 999.
      if (is_null(smaj) .eq. 1) then
          write (file,'(''      '',$)')
      else
          if (smaj .lt. 0 .or. smaj .gt. 99999) then
              write (isf_error,'(''bad smaj: '',f9.2)') smaj
              write_origin = 20
              return
          end if
          call write_real(file,smaj,5,1)
          write (file,'('' '',$)')
      end if

c     Chars 62-66: optional semi-minor axis. Char 67: space.
c     Give 1 decimal place but less if number > 999.
      if (is_null(smin) .eq. 1) then
          write (file,'(''      '',$)')
      else
          if (smin .lt. 0 .or. smin .gt. 99999) then
              write (isf_error,'(''bad smin: '',f9.2)') smin
              write_origin = 20
              return
          end if
          call write_real(file,smin,5,1)
          write (file,'('' '',$)')
      end if

c     Chars 68-70: optional strike. Char 71: space.
c     Strike can be -1, when it's a flag to signify that smaj,smin
c     are really slat,slon.
      if (is_null(real(strike)) .eq. 1) then
          write (file,'(''    '',$)')
      else
          if (strike .lt. -1 .or. strike .gt. 360) then
              write (isf_error,'(''bad strike: '',f9.2)') strike
              write_origin = 20
              return
          end if
          write (file,'(i3,'' '',$)') strike
      end if

c     Chars 72-76: optional depth.
      if (is_null(depth) .eq. 1) then
          write (file,'(''     '',$)')
      else
          if (depth .lt. 0 .or. depth .gt. 999) then
              write (isf_error,'(''bad depth: '',f9.2)') depth
              write_origin = 20
              return
          end if
          write (file,'(f5.1,$)') depth
      end if

c     Char 77: fixed depth flag. Char 78: space.
      if (depfix .eq. 'F') then
          depfix = 'f'
      else if (depfix .eq. 'D') then
          depfix = 'd'
      else if (depfix .ne. ' ' .and. depfix .ne. 'f' .and.  
     +         depfix .ne. 'd') then
          write (isf_error,'(''bad depfix: '',a1)') depfix
          write_origin = 20
          return
      end if
      write(file,'(a1,'' '',$)') depfix

c     Chars 79-82: optional depth error. Char 83: space.
c     Give 1 decimal place or 0 if number > 99.
      if (is_null(sdepth) .eq. 1) then
          write (file,'(''     '',$)')
      else
          if (sdepth .lt. 0 .or. sdepth .gt. 9999) then
              write (isf_error,'(''bad sdepth: '',f9.2)') sdepth
              write_origin = 20
              return
          end if
          call write_real(file,sdepth,4,1)
          write (file,'('' '',$)')
      end if

c     Chars 84-87: optional ndef. Char 88: space.
      if (is_null(real(ndef)) .eq. 1) then
          write (file,'(''     '',$)')
      else
          if (ndef .lt. 0 .or. ndef .gt. 9999) then
              write (isf_error,'(''bad ndef: '',f9.2)') ndef
              write_origin = 20
              return
          end if
          write (file,'(i4,'' '',$)') ndef
      end if

c     Chars 89-92: optional nsta. Char 93: space.
      if (is_null(real(nsta)) .eq. 1) then
          write (file,'(''     '',$)')
      else
          if (nsta .lt. 0 .or. nsta .gt. 9999) then
              write (isf_error,'(''bad nsta: '',f9.2)') nsta
              write_origin = 20
              return
          end if
          write (file,'(i4,'' '',$)') nsta
      end if

c     Chars 94-96: optional gap. Char 97: space.
      if (is_null(real(gap)) .eq. 1) then
          write (file,'(''    '',$)')
      else
          if (gap .lt. 0 .or. gap .gt. 360) then
              write (isf_error,'(''bad gap: '',f9.2)') real(gap)
              write_origin = 20
              return
          end if
          write (file,'(i3,'' '',$)') gap
      end if

c     Chars 98-103: optional minimum distance. Char 104: space.
c     Gives 2 decimal places or less if number > 999.
      if (is_null(mindist) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if (mindist .lt. 0 .or. mindist .gt. 999999) then
              write (isf_error,'(''bad mindist: '',f9.2)')
     +               mindist
              write_origin = 20
              return
          end if
          call write_real(file,mindist,6,2)
          write (file,'('' '',$)')
      end if

c     Chars 105-110: optional maximum distance. Char 111: space.
c     Gives 2 decimal places or less if number > 999.
      if (is_null(maxdist) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if (maxdist .lt. 0 .or. maxdist .gt. 999999) then
              write (isf_error,'(''bad maxdist: '',f9.2)')
     +               maxdist
              write_origin = 20
              return
          end if
          call write_real(file,maxdist,6,2)
          write (file,'('' '',$)')
      end if

c     Char 112: analysis type. Char 113 space.
      if (antype .ne. ' ' .and. antype .ne. 'a' .and. 
     +    antype .ne. 'm' .and. antype .ne. 'g') then

          write (isf_error,'(''bad antype: '',a1)') antype
          write_origin = 20
          return
      end if
      write(file,'(a1,'' '',$)') antype

c     Char 114: location method. Char 115 space.
      if (loctype .ne. ' ' .and. loctype .ne. 'i' .and. loctype .ne. 
     +    'p' .and. loctype .ne. 'g' .and. loctype .ne. 'o') then

          write (isf_error,'(''bad loctype: '',a1)') loctype
          write_origin = 20
          return
      end if
      write(file,'(a1,'' '',$)') loctype

c     Chars 116-117: event type. Char 118 space.
      numchar = partline(substr,etype,0,0)
      if (numchar .eq. 0 .or. etype.eq.' ') then
          write(file,'(''   '',$)')
      else
          if (numchar .ne. ISF_ETYPE_LEN) then
              write (isf_error,'(''bad etype: '',a)') etype
              write_origin = 20
              return
          end if
          write(file,'(a2,'' '',$)') etype
      end if

c     Chars 119-127: author. Char 128: space.
      numchar = partline(substr,author,0,0)
      if (numchar .gt. ISF_AUTHOR_LEN) then
          write (isf_error,'(''author too long: '',a)') author
          write_origin = 20
          return
      end if
      if (numchar .eq. 0) then
          write (isf_error,'(''missing author'')')
          write_origin = 20
          return
      end if
      if ( check_whole(author) .eq. 1 ) then
          isf_error = 'bad author: '//author
          write_origin = 20
          return
      end if
      write(file,'(a9,'' '',$)') author

c     Chars 129-136: origid.
      numchar = partline(substr,origid,0,0)
      if (numchar .gt. ISF_ORIGID_LEN) then
          write (isf_error,'(''origid too long: '',a)') origid
          write_origin = 20
          return
      end if
      if (numchar .eq. 0) then
          write (isf_error,'(''missing origid'')')
          write_origin = 20
          return
      end if
      if ( check_whole(origid) .eq. 1 ) then
          isf_error = 'bad origid: '//origid
          write_origin = 20
          return
      end if
      write(file,'(a8)') origid

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('origin') .ne. 0) then
          write_origin = 10
          return
      end if

      write_origin = 0
      return
      end

c     Writes the comment that can follow an origin line to mark it is as prime.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_origin_prime(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      write (file,'(a)') ' (#PRIME)'

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('origin_com') .ne. 0) then
          write_origin_prime = 10
          return
      end if

      write_origin_prime = 0
      return
      end


c     Writes the comment that can follow an origin line to mark it is a centroid.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_origin_centroid(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      write (file,'(a)') ' (#CENTROID)'

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('origin_com') .ne. 0) then
          write_origin_centroid = 10
          return
      end if

      write_origin_centroid = 0
      return
      end


c     Writes an origin parameter formatted comment.
c     Writes any number of parameter=value pairs, starting new line if necessary.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_origin_param(file,param,value,
     +                                            error,numparam)

      integer file,numparam
      character param(*)*(*),value(*)*(*),error(*)*(*)
      include 'isf_head.h'
      integer partline,check_prev_line_type
      integer i,len,space_left
      integer numchar_param,numchar_value,numchar_error
      character substr*(ISF_LINE_LEN)

      write (file,'(a,$)') ' (#PARAM'
      space_left = ISF_COMM_LEN

      do i=1,numparam
          numchar_param = partline(substr,param(i),1,0)
          numchar_value = partline(substr,value(i),1,0)
          numchar_error = partline(substr,error(i),1,0)
          len = numchar_param + numchar_value + 1
          if (numchar_error .ne. 0) then
              len = len + numchar_error + 1
          end if
          if ( len .gt. ISF_COMM_LEN ) then
              write (isf_error,'(''param=value too long'')')
              write_origin_param = 20
              return
          end if

          if ( space_left .lt. len ) then
              write (file,'('')'')')
              write (file,'(a,$)') ' (#PARAM'
            space_left = ISF_COMM_LEN
          end if

          write (file,'('' '',a,$)') param(i)(1:numchar_param)
          write (file,'(''='',a,$)') value(i)(1:numchar_value)
          if (numchar_error .ne. 0) then
              write (file,'(''+'',a,$)') error(i)(1:numchar_error)
          end if
          space_left = space_left - len
      end do
      write (file,'('')'')')

      if (check_prev_line_type('origin_com') .ne. 0) then
          write_origin_param = 10
          return
      end if

      write_origin_param = 0
      return
      end

c     Writes both moment tensor header lines.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_momten_head(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      character head1*(88)
      character head2*(88)

      head1 = ' (#MOMTENS sc    M0 fCLVD    MRR    MTT    MPP    MRT' //  
     +        '    MTP    MPR NST1 NST2 Author   )'
      head2 = ' (#             eM0 eCLVD    eRR    eTT    ePP    eRT' //
     +        '    eTP    ePR NCO1 NCO2 Duration )'

      write (file,'(a)') head1
      write (file,'(a)') head2

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('momten_head') .ne. 0) then
          write_momten_head = 10
          return
      end if

      write_momten_head = 0
      return
      end


c     Writes both moment tensor data lines.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_momten(file,scale_factor,scalar_moment,
     + fclvd,mrr,mtt,mpp,mrt,mtp,mpr,nsta1,nsta2,author,
     + scalar_moment_unc,fclvd_unc,mrr_unc,mtt_unc,
     + mpp_unc,mrt_unc,mtp_unc,mpr_unc,ncomp1,ncomp2,duration)

      integer file
      character author*(*)
      integer scale_factor,nsta1,nsta2, ncomp1,ncomp2
      real*4 scalar_moment,fclvd,mrr,mtt,mpp,mrt,mtp,mpr
      real*4 scalar_moment_unc,fclvd_unc,mrr_unc,mtt_unc,mpp_unc
      real*4 mrt_unc,mtp_unc,mpr_unc,duration

      include 'isf_head.h'
      integer partline, check_prev_line_type, is_null, check_whole
      integer numchar
      character substr*(ISF_LINE_LEN)

c     Line 1

c     Chars 1-11: comment start string#
      write (file,'(a,$)') ' (#        '

c     Chars 12,13: scale factor. Char 14: space.
      if (is_null(real(scale_factor)) .eq. 1) then
          isf_error = 'missing scale_factor'
          write_momten = 20
          return
      end if

      if ( scale_factor .lt. 0 .or. scale_factor .gt. 99 ) then
          write (isf_error,'(''bad scale_factor: '',f9.2)') 
     + scale_factor
          write_momten = 20
          return
      end if
      write (file,'(i2,'' '',$)') scale_factor

c     Chars 15-19: scalar seismic moment. Char 20: space.
      if (is_null(scalar_moment) .eq. 1) then
          isf_error = 'missing scalar_moment'
          write_momten = 20
          return
      end if

      if ( scalar_moment .lt. 0 .or. scalar_moment .gt. 9.999 ) then
          write (isf_error,'(''bad scalar_moment: '',f9.2)') 
     + scalar_moment
          write_momten = 20
          return
      end if
      write (file,'(f5.3,'' '',$)') scalar_moment

c     Chars 21-25: fCLVD. Char 26: space.
      if (is_null(fclvd) .eq. 1) then
          write (file,'(''      '',$)')
      else
          if ( fclvd .lt. 0 .or. fclvd .gt. 9.999 ) then
              write (isf_error,'(''bad fclvd: '',f9.2)') fclvd
              write_momten = 20
              return
          end if
          write (file,'(f5.3,'' '',$)') fclvd
      end if

c     Chars 27-32: radial-radial element. Char 33: space.
      if (is_null(mrr) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mrr .lt. -9.999 .or. mrr .gt. 9.999 ) then
              write (isf_error,'(''bad mrr: '',f9.2)') mrr
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mrr
      end if

c     Chars 34-39: theta-theta element. Char 40: space.
      if (is_null(mtt) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mtt .lt. -9.999 .or. mtt .gt. 9.999 ) then
              write (isf_error,'(''bad mtt: '',f9.2)') mtt
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mtt
      end if

c     Chars 41-46: phi-phi element. Char 47: space.
      if (is_null(mpp) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mpp .lt. -9.999 .or. mpp .gt. 9.999 ) then
              write (isf_error,'(''bad mpp: '',f9.2)') mpp
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mpp
      end if

c     Chars 48-53: radial-theta element. Char 54: space.
      if (is_null(mrt) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mrt .lt. -9.999 .or. mrt .gt. 9.999 ) then
              write (isf_error,'(''bad mrt: '',f9.2)') mrt
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mrt
      end if

c     Chars 55-60: theta-phi element. Char 61: space.
      if (is_null(mtp) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mtp .lt. -9.999 .or. mtp .gt. 9.999 ) then
              write (isf_error,'(''bad mtp: '',f9.2)') mtp
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mtp
      end if

c     Chars 62-67: phi-radial element. Char 68: space.
      if (is_null(mpr) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mpr .lt. -9.999 .or. mpr .gt. 9.999 ) then
              write (isf_error,'(''bad mpr: '',f9.2)') mpr
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mpr
      end if

c     Chars 69-72: nsta1. Char 73: space.
      if (is_null(real(nsta1)) .eq. 1) then
          write (file,'(''     '',$)')
      else
          if ( nsta1 .lt. 0 .or. nsta1 .gt. 999 ) then
              write (isf_error,'(''bad nsta1: '',i9)') nsta1
              write_momten = 20
              return
          end if
          write (file,'(i4,'' '',$)') nsta1
      end if

c     Chars 74-77: nsta2. Char 78: space.
      if (is_null(real(nsta2)) .eq. 1) then
          write (file,'(''     '',$)')
      else
          if ( nsta2 .lt. 0 .or. nsta2 .gt. 999 ) then
              write (isf_error,'(''bad nsta2: '',i9)') nsta2
              write_momten = 20
              return
          end if
          write (file,'(i4,'' '',$)') nsta2
      end if

c     Chars 79-87 author. Char 87 ')'.
      numchar = partline(substr,author,0,0)
      if (numchar .gt. ISF_AUTHOR_LEN) then
          write (isf_error,'(''author too long: '',a)') author
          write_momten = 20
          return
      end if
      if (numchar .eq. 0) then
          write (isf_error,'(''missing author'')')
          write_momten = 20
          return
      end if
      if ( check_whole(author) .eq. 1 ) then
          isf_error = 'bad author: '//author
          write_momten = 20
          return
      end if
      write(file,'(a9,'')'')') author

c     Line 2.

c     Chars 1-14: comment start string
      write (file,'(a,$)') ' (#           '

c     Chars 15-19: uncertainty in scalar seismic moment. Char 20: space.
      if (is_null(scalar_moment_unc) .eq. 1) then
          write (file,'(''      '',$)')
      else
          if ( scalar_moment_unc .lt. 0 .or. scalar_moment_unc .gt. 
     + 9.999 ) then
              write (isf_error,'(''bad moment unc '',f9.2)') 
     + scalar_moment_unc
              write_momten = 20
              return
          end if
          write (file,'(f5.3,'' '',$)') scalar_moment_unc
      end if

c     Chars 21-25: uncertainty in fCLVD. Char 26: space.
      if (is_null(fclvd_unc) .eq. 1) then
          write (file,'(''      '',$)')
      else
          if ( fclvd_unc .lt. 0 .or. fclvd_unc .gt. 9.999 ) then
              write (isf_error,'(''bad fclvd_unc: '',f9.2)') 
     + fclvd_unc
              write_momten = 20
              return
          end if
          write (file,'(f5.3,'' '',$)') fclvd_unc
      end if

c     Chars 27-32: uncertainty in radial-radial element. Char 33: space.
      if (is_null(mrr_unc) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mrr_unc .lt. 0 .or. mrr_unc .gt. 9.999 ) then
              write (isf_error,'(''bad mrr_unc: '',f9.2)')
     +               mrr_unc
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mrr_unc
      end if

c     Chars 34-39: uncertainty in theta-theta element. Char 40: space.
      if (is_null(mtt_unc) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mtt_unc .lt. 0 .or. mtt_unc .gt. 9.999 ) then
              write (isf_error,'(''bad mtt_unc: .'',f9.2)')
     +               mtt_unc
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mtt_unc
      end if

c     Chars 41-46: uncertainty in phi-phi element. Char 47: space.
      if (is_null(mpp_unc) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mpp_unc .lt. 0 .or. mpp_unc .gt. 9.999 ) then
              write (isf_error,'(''bad mpp_unc: '',f9.2)')
     +               mpp_unc
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mpp_unc
      end if

c     Chars 48-53: uncertainty in radial-theta element. Char 54: space.
      if (is_null(mrt_unc) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mrt_unc .lt. 0 .or. mrt_unc .gt. 9.999 ) then
              write (isf_error,'(''bad mrt_unc: '',f9.2)')
     +               mrt_unc
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mrt_unc
      end if

c     Chars 55-60: uncertainty in theta-phi element. Char 61: space.
      if (is_null(mtp_unc) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mtp_unc .lt. 0 .or. mtp_unc .gt. 9.999 ) then
              write (isf_error,'(''bad mtp_unc: '',f9.2)')
     +               mtp_unc
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mtp_unc
      end if

c     Chars 62-67: uncertainty in phi-radial element. Char 68: space.
      if (is_null(mpr_unc) .eq. 1) then
          write (file,'(''       '',$)')
      else
          if ( mpr_unc .lt. 0 .or. mpr_unc .gt. 9.999 ) then
              write (isf_error,'(''bad mpr_unc: '',f9.2)')
     +               mpr_unc
              write_momten = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') mpr_unc
      end if

c     Chars 69-72: ncomp1. Char 73: space.
      if (is_null(real(ncomp1)) .eq. 1) then
          write (file,'(''     '',$)')
      else
          if ( ncomp1 .lt. 0 .or. ncomp1 .gt. 999 ) then
              write (isf_error,'(''bad ncomp1: '',i9)')
     +               ncomp1
              write_momten = 20
              return
          end if
          write (file,'(i4,'' '',$)') ncomp1
      end if

c     Chars 74-77: ncomp2. Char 78: space.
      if (is_null(real(ncomp2)) .eq. 1) then
          write (file,'(''     '',$)')
      else
          if ( ncomp2 .lt. 0 .or. ncomp2 .gt. 999 ) then
              write (isf_error,'(''bad ncomp2: '',i9)')
     +               ncomp2
              write_momten = 20
              return
          end if
          write (file,'(i4,'' '',$)') ncomp2
      end if

c     Chars 79-86: duration. Char 77: space. Char 88 '')''.
      if (is_null(duration) .eq. 1) then
          write (file,'(''         )'')')
      else
          if ( duration .lt. 0 .or. duration .gt. 99999 ) then
              write (isf_error,'(''bad duration: '',f9.2)') 
     +               duration
              write_momten = 20
              return
          end if
          write (file,'(f8.2,'' )'')') duration
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('momten') .ne. 0) then
          write_momten = 10
          return
      end if

      write_momten = 0
      return
      end

c     Writes a fault plane header line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_fault_plane_head(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      character head*(64)

      head = ' (#FAULT_PLANE Typ Strike   Dip    Rake  NP  NS Plane' //
     +       ' Author   )'

      write (file,'(a)') head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('fault_plane_head') .ne. 0) then
          write_fault_plane_head = 10
          return
      end if

      write_fault_plane_head = 0
      return
      end

c     Writes a fault plane data line.
c     Either first or second plane - only the comment marker at the start changes.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_fault_plane(file,f_type,strike,dip,
     +                                    rake,np,ns,f_plane,author)

      integer file
      character f_plane*(*), f_type*(*), author*(*)
      integer np,ns
      real*4 strike,dip,rake

      integer line_num

      include 'isf_head.h'
      integer partline, check_prev_line_type, is_null, check_whole
      integer numchar
      character substr*(ISF_LINE_LEN)


c     Check if this is the second fault plane to be writen.
c     Chars 1-15 are the respective comment start strings.
      numchar = partline(substr,isf_prev_line_type,1,0)
      if (numchar .eq. 11 .and. substr(1:11) .eq. 'fault_plane') then
          line_num = 2
          write (file,'(a,$)') ' (+            '
      else
          line_num = 1
          write (file,'(a,$)') ' (#            '
      end if

c     Chars 16-18: Fault plane type. Char 19: space.
c     Only put type on first line.
      numchar = partline(substr,f_type,0,0)
      if (numchar .ne. 0 .and. line_num .eq. 1) then
          if (numchar .gt. ISF_F_TYPE_LEN) then
              write (isf_error,'(''f_type too long: '',a)') f_type
              write_fault_plane = 20
              return
          end if
          if ( check_whole(f_type) .eq. 1 ) then
              isf_error = 'bad f_type: '//f_type
              write_fault_plane = 20
              return
          end if
          write(file,'(a3,'' '',$)') f_type
      else
          write(file,'(''    '',$)')
      end if

c     Chars 20-25: strike. Char 26 space.
      if (is_null(strike) .eq. 1) then
          isf_error = 'missing strike'
          write_fault_plane = 20
          return
      end if

      if ( strike .lt. 0 .or. strike .gt. 360 ) then
          write (isf_error,'(''bad strike: '',f9.2)') strike
          write_fault_plane = 20
          return
      end if
      write (file,'(f6.2,'' '',$)') strike

c     Chars 27-31: dip. Char 32 space.
      if (is_null(dip) .eq. 1) then
          isf_error = 'missing dip'
          write_fault_plane = 20
          return
      end if

      if ( dip .lt. 0 .or. dip .gt. 90 ) then
          write (isf_error,'(''bad dip: '',f9.2)') dip
          write_fault_plane = 20
          return
      end if
      write (file,'(f5.2,'' '',$)') dip

c     Chars 33-39: optional rake. Char 40 space.
      if (is_null(rake) .eq. 1) then
          write (file,'(''        '',$)')
      else
          if ( rake .lt. -180 .or. rake .gt. 180 ) then
              write (isf_error,'(''bad rake: '',f9.2)') rake
              write_fault_plane = 20
              return
          end if
          write (file,'(f7.2,'' '',$)') rake
      end if

c     Chars 41-43: optional np. Char 44 space.
      if (is_null(real(np)) .eq. 1) then
          write (file,'(''    '',$)')
      else
          if ( np .lt. 0 .or. np .gt. 999 ) then
              write (isf_error,'(''bad np: '',i9)') np
              write_fault_plane = 20
              return
          end if
          write (file,'(i3,'' '',$)') np
      end if

c     Chars 45-47: optional ns. Char 48 space.
      if (is_null(real(ns)) .eq. 1) then
          write (file,'(''    '',$)')
      else
          if ( ns .lt. 0 .or. ns .gt. 999 ) then
              write (isf_error,'(''bad ns: '',i9)') ns
              write_fault_plane = 20
              return
          end if
          write (file,'(i3,'' '',$)') ns
      end if

c     Chars 49-53: Plane identification. Char 54: space.
      numchar = partline(substr,f_plane,0,0)
      if (numchar .ne. 0) then
          if (numchar .gt. ISF_F_PLANE_LEN) then
              write (isf_error,'(''f_plane too long: '',a)') 
     + f_plane
              write_fault_plane = 20
              return
          end if
          if ( check_whole(f_plane) .eq. 1 ) then
              isf_error = 'bad f_plane: '//f_plane
              write_fault_plane = 20
              return
          end if
          write(file,'(a5,'' '',$)') f_plane
      else
          write(file,'(''     '',$)')
      end if

c     Chars 55-63: author if this is 1st fault plane. Char 64: ')'.
      if (line_num .eq. 1) then
          numchar = partline(substr,author,0,0)
          if (numchar .eq. 0) then
              write (isf_error,'(''missing author'')')
              write_fault_plane = 20
              return
          end if
          if (numchar .gt. ISF_AUTHOR_LEN) then
              write (isf_error,'(''author too long: '',a)') 
     + author
              write_fault_plane = 20
              return
          end if
          if ( check_whole(author) .eq. 1 ) then
              isf_error = 'bad author: '//author
              write_fault_plane = 20
              return
          end if
          write(file,'(a9,'')'')') author
      else
          write (file,'(''         )'')')
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('fault_plane') .ne. 0) then
          write_fault_plane = 10
          return
      end if

      write_fault_plane = 0
      return
      end


c     Writes a principal axes header line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_axes_head(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      character head*(83)

      head = ' (#PRINAX sc  T_val T_azim  T_pl  B_val B_azim  B_pl' //
     +       '  P_val P_azim  P_pl Author   )'

      write (file,'(a)') head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('axes_head') .ne. 0) then
          write_axes_head = 10
          return
      end if

      write_axes_head = 0
      return
      end

c     Writes a principal axes error header line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_axes_err_head(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      character head*(83)

      head = ' (+             eTv    eTa   eTp    eBv    eBa   eBp' //
     +       '    ePv    ePa   ePp fCLVD    )'

      write (file,'(a)') head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('axes_err_head') .ne. 0) then
          write_axes_err_head = 10
          return
      end if

      write_axes_err_head = 0
      return
      end

c     Writes a principal axes data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_axes(file,scale_factor,t_val,t_azim,
     +              t_pl,b_val,b_azim,b_pl,p_val,p_azim,p_pl,author)

      integer file
      character author*(*)
      integer scale_factor
      real*4 t_val,t_azim,t_pl,b_val,b_azim,b_pl,p_val,p_azim,p_pl

      include 'isf_head.h'
      integer partline, check_prev_line_type, is_null, check_whole
      integer numchar
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: Comment start string.
      write (file,'(a,$)') ' (#       '

c     Chars 11,12: scale factor. Char 13: space.
      if (is_null(real(scale_factor)) .eq. 1) then
          write (file,'(a,$)') '   '
      else
          if ( scale_factor .lt. 0 .or. scale_factor .gt. 99 ) then
              write (isf_error,'(''bad scale_factor: '',i9)') 
     + scale_factor
              write_axes = 20
              return
          end if
          write (file,'(i2,'' '',$)') scale_factor
      end if

c     Chars 14-19: t_val. Char 20: space.
      if (is_null(t_val) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( t_val .lt. -9.999 .or. t_val .gt. 9.999 ) then
              write (isf_error,'(''bad t_val: '',f9.2)') t_val
              write_axes = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') t_val
      end if
  
c     Chars 21-26: t_azim. Char 27 space.
      if (is_null(t_azim) .eq. 1) then
          write (isf_error,'(''missing t_azim'')')
          write_axes = 20
          return
      end if
      if ( t_azim .lt. 0 .or. t_azim .gt. 360 ) then
          write (isf_error,'(''bad t_azim: '',f9.2)') t_azim
          write_axes = 20
          return
      end if
      write (file,'(f6.2,'' '',$)') t_azim

c     Chars 28-32: t_pl. Char 33 space.
      if (is_null(t_pl) .eq. 1) then
          write (isf_error,'(''missing t_pl'')')
          write_axes = 20
          return
      end if
      if ( t_pl .lt. 0 .or. t_pl .gt. 90 ) then
          write (isf_error,'(''bad t_pl: '',f9.2)') t_pl
          write_axes = 20
          return
      end if
      write (file,'(f5.2,'' '',$)') t_pl

c     Chars 34-39: b_val. Char 40: space.
      if (is_null(b_val) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( b_val .lt. -9.999 .or. b_val .gt. 9.999 ) then
              write (isf_error,'(''bad b_val: '',f9.2)') b_val
              write_axes = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') b_val
      end if

c     Chars 41-46: b_azim. Char 47 space.
      if (is_null(b_azim) .eq. 1) then
          write (isf_error,'(''missing b_azim'')')
          write_axes = 20
          return
      end if
      if ( b_azim .lt. 0 .or. b_azim .gt. 360 ) then
          write (isf_error,'(''bad b_azim: '',f9.2)') b_azim
          write_axes = 20
          return
      end if
      write (file,'(f6.2,'' '',$)') b_azim

c     Chars 48-52: b_pl. Char 53 space.
      if (is_null(b_pl) .eq. 1) then
          write (isf_error,'(''missing b_pl'')')
          write_axes = 20
          return
      end if
      if ( b_pl .lt. 0 .or. b_pl .gt. 90 ) then
          write (isf_error,'(''bad b_pl: '',f9.2)') b_pl
          write_axes = 20
          return
      end if
      write (file,'(f5.2,'' '',$)') b_pl

c     Chars 54-59: p_val. Char 60: space.
      if (is_null(p_val) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( p_val .lt. -9.999 .or. p_val .gt. 9.999 ) then
              write (isf_error,'(''bad p_val: '',f9.2)') p_val
              write_axes = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') p_val
      end if

c     Chars 61-66: p_azim. Char 67 space.
      if (is_null(p_azim) .eq. 1) then
          write (isf_error,'(''missing p_azim'')')
          write_axes = 20
          return
      end if
      if ( p_azim .lt. 0 .or. p_azim .gt. 360 ) then
          write (isf_error,'(''bad p_azim: '',f9.2)') p_azim
          write_axes = 20
          return
      end if
      write (file,'(f6.2,'' '',$)') p_azim

c     Chars 68-72: p_pl. Char 73 space.
      if (is_null(p_pl) .eq. 1) then
          write (isf_error,'(''missing p_pl'')')
          write_axes = 20
          return
      end if
      if ( p_pl .lt. 0 .or. p_pl .gt. 90 ) then
          write (isf_error,'(''bad p_pl: '',f9.2)') p_pl
          write_axes = 20
          return
      end if
      write (file,'(f5.2,'' '',$)') p_pl

c     Chars 74-82: author. Char 83: close bracket.
      numchar = partline(substr,author,0,0)
      if (numchar .eq. 0) then
          write (isf_error,'(''missing author'')')
          write_axes = 20
          return
      end if
      if (numchar .gt. ISF_AUTHOR_LEN) then
          write (isf_error,'(''author too long: '',a)') author
          write_axes = 20
          return
      end if
      if ( check_whole(author) .eq. 1 ) then
          isf_error = 'bad author: '//author
          write_axes = 20
          return
      end if
      write(file,'(a9,'')'')') author

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('axes') .ne. 0) then
          write_axes = 10
          return
      end if

      write_axes = 0
      return
      end


c     Write principal axes error line - allows anything and everthing to be null.
c     Would be possible to want to write only fCVLD or only errors.
c     Trust user not to send for it if have nothing at all to write.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_axes_err(file,t_val_unc,t_azim_unc,
     + t_pl_unc,b_val_unc,b_azim_unc,b_pl_unc,p_val_unc,
     + p_azim_unc,p_pl_unc,fclvd)

      integer file
      real*4 t_val_unc,t_azim_unc,t_pl_unc,b_val_unc,b_azim_unc,b_pl_unc
      real*4 p_val_unc,p_azim_unc,p_pl_unc,fclvd

      include 'isf_head.h'
      integer check_prev_line_type, is_null

c     Chars 1-14: Comment start string.
      write (file,'(a,$)') ' (+           '

c     Chars 15-19: t_val_unc. Char 20: space.
      if (is_null(t_val_unc) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( t_val_unc .lt. 0 .or. t_val_unc .gt. 9.999 ) then
              write (isf_error,'(''bad t_val_unc: '',f9.2)') 
     + t_val_unc
              write_axes_err = 20
              return
          end if
          write (file,'(f5.3,'' '',$)') t_val_unc
      end if

c     Chars 21-26: t_azim_unc. Char 27: space.
      if (is_null(t_azim_unc) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( t_azim_unc .lt. 0 .or. t_azim_unc .gt. 360 ) then
              write (isf_error,'(''bad t_azim_unc: '',f9.2)') 
     + t_azim_unc
              write_axes_err = 20
              return
          end if
          write (file,'(f6.2,'' '',$)') t_azim_unc
      end if

c     Chars 28-32: t_pl_unc. Char 33,34: spaces.
      if (is_null(t_pl_unc) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( t_pl_unc .lt. 0 .or. t_pl_unc .gt. 90 ) then
              write (isf_error,'(''bad t_pl_unc: '',f9.2)') 
     + t_pl_unc
              write_axes_err = 20
              return
          end if
          write (file,'(f5.2,''  '',$)') t_pl_unc
      end if

c     Chars 35-39: b_val_unc. Char 40: space.
      if (is_null(b_val_unc) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( b_val_unc .lt. 0 .or. b_val_unc .gt. 9.999 ) then
              write (isf_error,'(''bad b_val_unc: '',f9.2)') 
     + b_val_unc
              write_axes_err = 20
              return
          end if
          write (file,'(f5.3,'' '',$)') b_val_unc
      end if

c     Chars 41-46: b_azim_unc. Char 47: space.
      if (is_null(b_azim_unc) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( b_azim_unc .lt. 0 .or. b_azim_unc .gt. 360 ) then
              write (isf_error,'(''bad b_azim_unc: '',f9.2)') 
     + b_azim_unc
              write_axes_err = 20
              return
          end if
          write (file,'(f6.2,'' '',$)') b_azim_unc
      end if

c     Chars 48-52: b_pl_unc. Char 53,54: spaces.
      if (is_null(b_pl_unc) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( b_pl_unc .lt. 0 .or. b_pl_unc .gt. 90 ) then
              write (isf_error,'(''bad b_pl_unc: '',f9.2)') 
     + b_pl_unc
              write_axes_err = 20
              return
          end if
          write (file,'(f5.2,''  '',$)') b_pl_unc
      end if

c     Chars 55-59: p_val_unc. Char 60: space.
      if (is_null(p_val_unc) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( p_val_unc .lt. 0 .or. p_val_unc .gt. 9.999 ) then
              write (isf_error,'(''bad p_val_unc: '',f9.2)') 
     + p_val_unc
              write_axes_err = 20
              return
          end if
          write (file,'(f5.3,'' '',$)') p_val_unc
      end if

c     Chars 61-66: p_azim_unc. Char 67: space.
      if (is_null(p_azim_unc) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( p_azim_unc .lt. 0 .or. p_azim_unc .gt. 360 ) then
              write (isf_error,'(''bad p_azim_unc: '',f9.2)') 
     + p_azim_unc
              write_axes_err = 20
              return
          end if
          write (file,'(f6.2,'' '',$)') p_azim_unc
      end if

c     Chars 68-72: p_pl_unc. Char 73: space.
      if (is_null(p_pl_unc) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( p_pl_unc .lt. 0 .or. p_pl_unc .gt. 90 ) then
              write (isf_error,'(''bad p_pl_unc: '',f9.2)') 
     + p_pl_unc
              write_axes_err = 20
              return
          end if
          write (file,'(f5.2,'' '',$)') p_pl_unc
      end if

c     Chars 74-78: fclvd. Chars 79-82: spaces to line up close brackets.
      if (is_null(fclvd) .eq. 1) then
          write (file,'(a)') '         )'
      else
          if ( fclvd .lt. 0 .or. fclvd .gt. 90 ) then
              write (isf_error,'(''bad fclvd: '',f9.2)') fclvd
              write_axes_err = 20
              return
          end if
          write (file,'(f5.3,''    )'')') fclvd
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('axes_err') .ne. 0) then
          write_axes_err = 10
          return
      end if

      write_axes_err = 0
      return
      end


c     Writes  magnitude header complete with preceding blank line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_netmag_head(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      character head*(38)

      head = 'Magnitude  Err Nsta Author      OrigID'

      write (file,'()')
      write (file,'(a)') head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('netmag_head') .ne. 0) then
          write_netmag_head = 10
          return
      end if

      write_netmag_head = 0
      return
      end

c     Writes a  magnitude data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_netmag(file,magtype,magind,mag,
     +                               magerr,nsta,author,origid)

      integer file
      character magtype*(*), author*(*), origid*(*)
      character magind
      real*4 mag,magerr
      integer nsta

      include 'isf_head.h'
      integer partline,check_prev_line_type, is_null, check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-5: magtype.
      numchar = partline(substr,magtype,0,0)
      if (numchar .eq. 0) then
          write (isf_error,'(''missing magtype'')')
          write_netmag = 20
          return
      end if
      if (numchar .gt. ISF_MAGTYPE_LEN) then
          write (isf_error,'(''magtype too long: '',a)') magtype
          write_netmag = 20
          return
      end if
      if ( check_whole(magtype) .eq. 1 ) then
          isf_error = 'bad magtype: '//magtype
          write_netmag = 20
          return
      end if
      write(file,'(a5,$)') magtype

c     Char 6: less than or greater than indicator.
      if (magind .ne. ' ' .and. magind .ne. '<' .and. magind .ne. '>') 
     +then
          isf_error = 'bad magind: '//magind
          write_netmag = 20
          return
      end if
      write(file,'(a1,$)') magind

c     Chars 7-10: magnitude value. Char 11 space.
      if (is_null(mag) .eq. 1) then
          isf_error = 'missing mag'
          write_netmag = 20
          return
      end if
      if ( mag .lt. -1 .or. mag .gt. 12 ) then
          write (isf_error,'(''bad mag: '',f9.2)') mag
          write_netmag = 20
          return
      end if
      write (file,'(f4.1,'' '',$)') mag

c     Chars 12-14: optional magnitude error. Char 15: space.
      if (is_null(magerr) .eq. 1) then
          write(file,'(''    '',$)')
      else
          if ( magerr .lt. 0 .or. magerr .gt. 9.9 ) then
              write (isf_error,'(''bad magerr: '',f9.2)') magerr
              write_netmag = 20
              return
          end if
          write (file,'(f3.1,'' '',$)') magerr
      end if

c     Chars 16-19 optional number of stations. Char 20: space.
      if (is_null(real(nsta)) .eq. 1) then
          write(file,'(''     '',$)')
      else
          if ( nsta .lt. 0 .or. nsta .gt. 9999 ) then
              write (isf_error,'(''bad nsta: '',f9.2)') nsta
              write_netmag = 20
              return
          end if
          write (file,'(i4,'' '',$)') nsta
      end if

c     Chars 21-29 author. Char 30 space.
      numchar = partline(substr,author,0,0)
      if (numchar .eq. 0) then
          write (isf_error,'(''missing author'')')
          write_netmag = 20
          return
      end if
      if (numchar .gt. ISF_AUTHOR_LEN) then
          write (isf_error,'(''author too long: '',a)') author
          write_netmag = 20
          return
      end if
      if ( check_whole(author) .eq. 1 ) then
          write (isf_error,'(''bad author: '',a)') author
          write_netmag = 20
          return
      end if
      write(file,'(a9,'' '',$)') author

c     Chars 31-38 origid.
      numchar = partline(substr,origid,0,0)
      if (numchar .eq. 0) then
          write (isf_error,'(''missing origid'')')
          write_netmag = 20
          return
      end if
      if (numchar .gt. ISF_ORIGID_LEN) then
          write (isf_error,'(''origid too long '',a)') origid
          write_netmag = 20
          return
      end if
      if ( check_whole(origid) .eq. 1 ) then
          write (isf_error,'(''bad origid '',a)') origid
          write_netmag = 20
          return
      end if
      write(file,'(a8)') origid

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('netmag') .ne. 0) then
          write_netmag = 10
          return
      end if

      write_netmag = 0
      return
      end


c     Writes a list of the stations that were used to calculate a magnitude.
c     Will write any number, starting new lines as necessary.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_netmag_sta(file,sta,n)

      integer file, n
      character sta(*)*(*)

      include 'isf_head.h'
      integer partline,check_prev_line_type, check_whole
      character substr*(ISF_LINE_LEN)
      integer i,numchar
      integer data_len

      write(file,'('' (#STATIONS'',$)')

c     Don't include the space after #STATIONS
      data_len = -1        
      do i=1,n
          numchar = partline(substr,sta(i),0,0)
          if (numchar .gt. ISF_NET_LEN+ISF_STA_LEN) then
              write (isf_error,'(''net/sta code too long '',a)') 
     + sta(i)
              write_netmag_sta = 20
              return
          end if
          if (check_whole(sta(i)) .eq. 1) then
              write (isf_error,'(''bad net/sta code '',a)') 
     + sta(i)
              write_netmag_sta = 20
              return
          end if    
          data_len = data_len + numchar + 1
          if (data_len .gt. ISF_COMM_LEN) then
              write(file,'('')'')')
              write(file,'('' (+        '',$)')
              data_len = numchar + 1
          end if
          write(file,'('' '',a,$)') sta(i)(1:numchar)
      end do
      write(file,'('')'')')

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('netmag_com') .ne. 0) then
          write_netmag_sta = 10
          return
      end if

      write_netmag_sta = 0
      return
      end

c     Writes a netmag basis data line.
c     Only expects one parameter=value pair.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_netmag_basis(file,param,value)

      integer file
      character param*(*), value*(*)

      include 'isf_head.h'
      integer partline,check_prev_line_type, check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar_param, numchar_value

      numchar_param = partline(substr,param,0,0)
      numchar_value = partline(substr,value,0,0)
      if (check_whole(param) .eq. 1) then
          write (isf_error,'(''bad param: '',a)') param
          write_netmag_basis = 20
          return
      end if
      if (check_whole(value) .eq. 1) then
          write (isf_error,'(''bad value: '',a)') value
          write_netmag_basis = 20
          return
      end if
      if (numchar_param + numchar_value + 1 .gt. ISF_COMM_LEN) then
          write (isf_error,'(''too long: '',a,$)') 
     + param(1:numchar_param)
          write (isf_error,'(''='',a)') value(1:numchar_value)
          write_netmag_basis = 20
          return
      end if

      write(file,'('' (#BASIS '',a,$)') param(1:numchar_param)
      write(file,'(''='',a,'')'')') value(1:numchar_value)

      if (check_prev_line_type('netmag_com') .ne. 0) then
          write_netmag_basis = 10
          return
      end if

      write_netmag_basis = 0
      return
      end

c     Writes  effects header complete with preceding blank line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_effects_head(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      character head*(69)

      head = 'Effects              Loctyp Location' //
     +       '           Intensity Scale Author'

      write (file,'()')
      write (file,'(a)') head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('effects_head') .ne. 0) then
          write_effects_head = 10
          return
      end if

      write_effects_head = 0
      return
      end

c     Writes an effects block data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_effects(file,heard,felt,damage,casualties,
     + uplift,subsidence,fault,tsunami,seiche,volcano,acoustic,gravity,
     + t_wave,liquification,geyser,landslide,sandblow,cracks,lights,
     + odours,loctype,lat,lon,dist,azim,country,postcode,net,
     + sta,intensity1,modifier,intensity2,scale,author)

      integer file
      character author*(*),loctype*(*)
      character scale*(*),country*(*),postcode*(*),net*(*),sta*(*)
      character heard,felt,damage,casualties,uplift,subsidence
      character fault,tsunami,seiche,volcano,acoustic,gravity
      character t_wave,liquification,geyser
      character landslide,sandblow,cracks,lights,odours
      character modifier
      real*4 lat,lon,dist,azim,intensity1,intensity2

      include 'isf_head.h'
      integer partline,check_prev_line_type,is_null,check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Char 1: heard flag.
      if (heard .ne. '_' .and. heard .ne. 'H' ) then
          isf_error = 'bad heard flag: '//heard
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') heard

c     Char 2: felt flag.
      if (felt .ne. '_' .and. felt .ne. 'F' ) then
          isf_error = 'bad felt flag: '//felt
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') felt

c     Char 3: damage flag.
      if (damage .ne. '_' .and. damage .ne. 'D' ) then
          isf_error = 'bad damage flag: '//damage
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') damage

c     Char 4: casualties flag.
      if (casualties .ne. '_' .and. casualties .ne. 'C' ) then
          isf_error = 'bad casualties flag: '//casualties
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') casualties

c     Char 5: uplift flag.
      if (uplift .ne. '_' .and. uplift .ne. 'U' ) then
          isf_error = 'bad uplift flag: '//uplift
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') uplift

c     Char 6: subsidence flag.
      if (subsidence .ne. '_' .and. subsidence .ne. 'S' ) then
          isf_error = 'bad subsidence flag: '//subsidence
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') subsidence

c     Char 7: surface faulting flag.
      if (fault .ne. '_' .and. fault .ne. 'F' ) then
          isf_error = 'bad surface faulting flag: '//fault
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') fault

c     Char 8: tsunami flag.
      if (tsunami .ne. '_' .and. tsunami .ne. 'T' .and. tsunami .ne. 
     + 'Q') then
          isf_error = 'bad tsunami flag: '//tsunami
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') tsunami

c     Char 9: seiche flag.
      if (seiche .ne. '_' .and. seiche .ne. 'S' .and. seiche .ne. 'Q') 
     +then
          isf_error = 'bad seiche flag: '//seiche
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') seiche

c     Char 10: volcano flag.
      if (volcano .ne. '_' .and. volcano .ne. 'V' ) then
          isf_error = 'bad volcano flag: '//volcano
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') volcano

c     Char 11: acoustic flag.
      if (acoustic .ne. '_' .and. acoustic .ne. 'A' ) then
          isf_error = 'bad acoustic flag: '//acoustic
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') acoustic

c     Char 12: gravity flag.
      if (gravity .ne. '_' .and. gravity .ne. 'G' ) then
          isf_error = 'bad gravity flag: '//gravity
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') gravity

c     Char 13: t_wave flag.
      if (t_wave .ne. '_' .and. t_wave .ne. 'T' ) then
          isf_error = 'bad t_wave flag: '//t_wave
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') t_wave

c     Char 14: liquification flag.
      if (liquification .ne. '_' .and. liquification .ne. 'L' ) then
          isf_error = 'bad liquification flag: '//liquification
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') liquification

c     Char 15: geyser flag.
      if (geyser .ne. '_' .and. geyser .ne. 'G' ) then
          isf_error = 'bad geyser flag: '//geyser
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') geyser

c     Char 16: landslide flag.
      if (landslide .ne. '_' .and. landslide .ne. 'S' ) then
          isf_error = 'bad landslide flag: '//landslide
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') landslide

c     Char 17: sandblow flag.
      if (sandblow .ne. '_' .and. sandblow .ne. 'B' ) then
          isf_error = 'bad sandblow flag: '//sandblow
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') sandblow

c     Char 18: cracks flag.
      if (cracks .ne. '_' .and. cracks .ne. 'C' ) then
          isf_error = 'bad cracks flag: '//cracks
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') cracks

c     Char 19: lights flag.
      if (lights .ne. '_' .and. lights .ne. 'V' ) then
          isf_error = 'bad lights flag: '//lights
          write_effects = 20
          return
      end if
      write (file,'(a1,$)') lights

c     Char 20: odours flag. Char 21 space.
      if (odours .ne. '_' .and. odours .ne. 'V' ) then
          isf_error = 'bad odours flag: '//odours
          write_effects = 20
          return
      end if
      write (file,'(a1,'' '',$)') odours

c     Chars 22-27 loctype. Char 28 space.
c     Chars 29-46 depend on loctype. Char 47: space.
      if (loctype .eq. 'Summar') then

          write (file,'(a,'' '',$)') loctype

c     	Chars 29-46 blank.
          write (file,'(''                   '',$)')

      elseif (loctype .eq. 'LatLon') then

          write (file,'(a,'' '',$)') loctype

c     	Chars 29-36: lattitude. Char 37: space.
        if ( is_null(lat) .eq. 1 ) then
              isf_error = 'missing lattitude'
              write_effects = 20
              return
          end if
          if ( lat .le. -90 .or. lat .gt. 90 ) then
              write (isf_error,'(''bad lat: '',f9.2)') lat
              write_effects = 20
              return
          end if
          write (file,'(f8.4,'' '',$)') lat

c     	Chars 38-46: longitude. Char 47: space.
        if ( is_null(lon) .eq. 1 ) then
              isf_error = 'missing longitude'
              write_effects = 20
              return
          end if
          if ( lon .lt. -180 .or. lon .gt. 180 ) then
              write (isf_error,'(''bad lon: '',f9.2)') lon
              write_effects = 20
              return
          end if
          write (file,'(f9.4,'' '',$)') lon

      elseif (loctype .eq. 'DistAz') then

          write (file,'(a,'' '',$)') loctype

c     	Chars 29-36: distance. Char 37: space.
        if ( is_null(dist) .eq. 1 ) then
              isf_error = 'missing dist'
              write_effects = 20
              return
          end if
          if ( dist .lt. 0 .or. dist .gt. 99999 ) then
              write (isf_error,'(''bad dist: '',f9.2)') dist
              write_effects = 20
              return
          end if
          write (file,'(f5.1,'' '',$)') dist

c     	Chars 38-42: azimuth. Chars 43-47 space.
        if ( is_null(azim) .eq. 1 ) then
              isf_error = 'missing azim'
              write_effects = 20
              return
          end if
          if ( azim .lt. 0 .or. azim .gt. 360 ) then
              write (isf_error,'(''bad azim: '',f9.2)') azim
              write_effects = 20
              return
          end if
          write (file,'(f5.1,''     '',$)') azim


      elseif (loctype .eq. 'CoPost') then

          write (file,'(a,'' '',$)') loctype

c     	Chars 29-31: country code. Chars 32 space.
          numchar = partline(substr,country,0,0)
          if (numchar .eq. 0) then
              write (isf_error,'(''missing country'')')
              write_effects = 20
              return
          end if
          if (numchar .gt. ISF_COUNTRY_LEN) then
              write (isf_error,'(''country too long: '',a)') 
     + country
              write_effects = 20
              return
          end if
          write(file,'(a3,'' '',$)') country

c     	Chars 33-42: post code. Chars 43-47 space.
          numchar = partline(substr,postcode,0,0)
          if (numchar .eq. 0) then
              write (isf_error,'(''missing postcode'')')
              write_effects = 20
              return
          end if
          if (numchar .gt. ISF_POSTCODE_LEN) then
              write (isf_error,'(''postcode too long: '',a)') 
     + postcode
              write_effects = 20
              return
          end if
          write(file,'(a10,''     '',$)') postcode

      elseif (loctype .eq. 'StaNet') then

          write (file,'(a,'' '',$)') loctype

c     	Chars 29-37: network code. Char 38: space.
          numchar = partline(substr,net,0,0)
          if (numchar .eq. 0) then
              write (isf_error,'(''missing net'')')
              write_effects = 20
              return
          end if
          if (numchar .gt. ISF_NET_LEN) then
              write (isf_error,'(''net too long: '',a)') net
              write_effects = 20
              return
          end if
          write(file,'(a9,'' '',$)') net

c     	Chars 39-43: station code. Chars 44-47: spaces.
          numchar = partline(substr,sta,0,0)
          if (numchar .eq. 0) then
              write (isf_error,'(''missing sta'')')
              write_effects = 20
              return
          end if
          if (numchar .gt. ISF_STA_LEN) then
              write (isf_error,'(''sta too long: '',a)') sta
              write_effects = 20
              return
          end if
          write(file,'(a5,''    '',$)') sta

      else
          isf_error = 'unknown loctype: '//loctype
          write_effects = 20
          return
      end if

c     Chars 48-51: first intensity.
c     If first intensity null then don't allow second one or scale.
      if ( is_null(intensity1) .ne. 1 ) then
          if ( intensity1 .lt. 0 .or. intensity1 .gt. 12 ) then
              write (isf_error,'(''bad intensity1: '',f9.2)') 
     + intensity1
              write_effects = 20
              return
          end if
          write (file,'(f4.1,$)') intensity1

c     	Char 52 intensity modifier.
          if (modifier .ne. ' ' .and. modifier .ne. '+' .and. modifier 
     + .ne. '-') then
              isf_error = 'bad intensity modifier: '//modifier
              write_effects = 20
              return
          end if
          write (file,'(a1,$)') modifier

c     	Chars 53-56: second intensity, only allowed if modifier is '-'.
c     	Char 57: space.
          if (modifier .eq. '-') then
              if ( is_null(intensity2) .eq. 1 ) then
                  isf_error = 'missing intensity2'
                  write_effects = 20
                  return
              end if
              if ( intensity2 .lt. 0 .or. intensity2 .gt. 12 ) then
                  write (isf_error,'(''bad intensity2: '',f9.2)') 
     + intensity2
                  write_effects = 20
                  return
              end if
              write (file,'(f4.1,'' '',$)') intensity2
          else
              if ( is_null(intensity2) .eq. 0 ) then
                  isf_error = 'bad modifier if want intensity2'
                  write_effects = 20
                  return
              end if
              write (file,'(''     '',$)')
          end if

c     	Chars 58-62: intensity scale. Char 63 space.
          numchar = partline(substr,scale,0,0)
          if ( numchar .ne. 0 ) then
              if ( numchar .gt. ISF_I_SCALE_LEN ) then
                  write (isf_error,'(''scale too long: '',a)') 
     + scale
                  write_effects = 20
                  return
              end if
              if ( check_whole(scale) .eq. 1 ) then
                  write (isf_error,'(''bad scale: '',a)') scale
                  write_effects = 20
                  return
              end if
              write(file,'(a5,'' '',$)') scale
          else
              write(file,'(''      '',$)')
          end if
      else
          if ( modifier .ne. ' ' ) then
              isf_error = 'modifier without intensity1'
              write_effects = 20
              return
          end if
          if ( is_null(intensity2) .eq. 0 ) then
              isf_error = 'intensity2 without intensity1'
              write_effects = 20
              return
          end if

          numchar = partline(substr,scale,0,0)
          if ( numchar .ne. 0 ) then
              isf_error = 'scale without intensity1'
              write_effects = 20
              return
          end if
          write(file,'(a16,$)') ' '
      end if

c     Chars 64-72 author.
      numchar = partline(substr,author,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_error,'(''missing author'')')
          write_effects = 20
          return
      end if
      if ( numchar .gt. ISF_AUTHOR_LEN ) then
          write (isf_error,'(''author too long: '',a)') author
          write_effects = 20
          return
      end if
      if ( check_whole(author) .eq. 1 ) then
          write (isf_error,'(''bad author: '',a)') author
          write_effects = 20
          return
      end if
      write(file,'(a9)') author

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('effects') .ne. 0) then
          write_effects = 10
          return
      end if

      write_effects = 0
      return
      end


c     Writes  phase header complete with preceding blank line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_phase_head(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      character head*(122)

      head = 'Sta     Dist  EvAz Phase        Time      TRes  Azim' //
     +       ' AzRes   Slow   SRes Def   SNR       Amp   Per Qual'  //
     +       ' Magnitude    ArrID'

      write (file,'()')
      write (file,'(a)') head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_head') .ne. 0) then
          write_phase_head = 10
          return
      end if

      write_phase_head = 0
      return
      end


c     Writes a phase block data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_phase(file,sta,dist,esaz,phase,hh,mi,
     + ss,msec,timeres,azim,azimres,slow,slowres,timedef,azimdef,
     + slowdef,snr,amp,per,picktype,sp_fm,detchar,magtype,magind,
     + mag,arrid,com)

      integer file
      character sta*(*),arrid*(*),phase*(*),magtype*(*),com*(*)
      character timedef,azimdef,slowdef,sp_fm,detchar,magind,picktype
      real*4 dist,esaz,timeres,azim,azimres,slow,slowres,snr,amp,per,mag
      integer hh,mi,ss,msec

      include 'isf_head.h'
      integer partline,check_prev_line_type,is_null,check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-5: station code. Char 6: space.
      numchar = partline(substr,sta,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_error,'(''missing sta'')')
          write_phase = 20
          return
      end if
      if ( numchar .gt. ISF_STA_LEN ) then
          write (isf_error,'(''sta too long: '',a)') sta
          write_phase = 20
          return
      end if
      if ( check_whole(sta) .eq. 1 ) then
          write (isf_error,'(''bad sta: '',a)') sta
          write_phase = 20
          return
      end if
      write(file,'(a5,'' '',$)') sta

c     Chars 7-12: distance. Char 13: space.
      if (is_null(dist) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( dist .lt. 0 .or. dist .gt. 999.99 ) then
              write (isf_error,'(''bad dist: '',f9.2)') dist
              write_phase = 20
              return
          end if
          write (file,'(f6.2,'' '',$)') dist
      end if

c     Chars 14-18: event to sta azimuth. Char 19: space.
      if (is_null(esaz) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( esaz .lt. 0 .or. esaz .gt. 360 ) then
              write (isf_error,'(''bad esaz: '',f9.2)') esaz
              write_phase = 20
              return
          end if
          write (file,'(f5.1,'' '',$)') esaz
      end if

c     Chars 20-27: phase code - can be null. Char 28: space.
      numchar = partline(substr,phase,0,0)
      if ( numchar .eq. 0 ) then
          write (file,'(a8,'' '',$)') ' '
      else
          if ( numchar .gt. ISF_PHASE_LEN ) then
              write (isf_error,'(''phase too long: '',a)') phase
              write_phase = 20
              return
          end if
          if ( check_whole(phase) .eq. 1 ) then
              write (isf_error,'(''bad phase: '',a)') phase
              write_phase = 20
              return
          end if
          write(file,'(a8,'' '',$)') phase
      end if

c     Chars 29-40: time. Char 41: space.
c     Time can be completely null.
      if (is_null(real(hh)) .eq. 1 .and. is_null(real(mi)) .eq. 1 .and.
     + is_null(real(ss)) .eq. 1) then

          write(file,'(a,$)') '             '
      else
          if (is_null(real(hh)) .eq. 1) then
              isf_error = 'missing hour'
              write_phase = 20
              return
          end if

          if (hh .lt. 0 .or. hh .gt. 23) then
              write (isf_error,'(''bad hour '',i9)') hh
              write_phase = 20
              return
          end if

          if (is_null(real(mi)) .eq. 1) then
              isf_error = 'missing minute'
              write_phase = 20
              return
          end if

          if (mi .lt. 0 .or. mi .gt. 59) then
              write (isf_error,'(''bad minute  '',i9)') mi
              write_phase = 20
              return
          end if

          if (is_null(real(ss)) .eq. 1) then
              isf_error = 'missing second'
              write_phase = 20
              return
          end if

          if (ss .lt. 0 .or. ss .gt. 59) then
              write (isf_error,'(''bad second '',i9)') ss
              write_phase = 20
              return
          end if
          write (file,'(i2.2,'':'',i2.2,'':'',i2.2,$)') hh,mi,ss

c     	Chars 37-40 msec - put blanks here if no msec provided.
          if (is_null(real(msec)) .eq. 1) then
              write(file,'(''     '',$)')
          else
              if (msec .lt. 0 .or. msec .gt. 999) then
                  write (isf_error,'(''bad msec '',i9)') msec
                  write_phase = 20
                  return
              end if
              write(file,'(''.'',i3.3,'' '',$)') msec
          end if
      end if
      
c     Chars 42-46: time residual. Char 47: space.
      if (is_null(timeres) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( timeres .lt. -9999 .or. timeres .gt. 9999 ) then
              write (isf_error,'(''bad timeres: '',f9.2)')
     +               timeres
              write_phase = 20
              return
          end if
          call write_real(file,timeres,5,1)
          write (file,'('' '',$)')
      end if

c     Chars 48-52: observed azimuth. Char 53: space.
      if (is_null(azim) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( azim .lt. 0 .or. azim .gt. 360 ) then
              write (isf_error,'(''bad azim: '',f9.2)')
     +               azim
              write_phase = 20
              return
          end if
          write (file,'(f5.1,'' '',$)') azim
      end if

c     Chars 54-58: azimuth residual. Char 59: space.
      if (is_null(azimres) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( azimres .lt. -360 .or. azimres .gt. 360 ) then
              write (isf_error,'(''bad azimres: '',f9.2)')
     +               azimres
              write_phase = 20
              return
          end if
          call write_real(file,azimres,5,1)
          write (file,'('' '',$)')
      end if

c     Chars 60-65: slowness. Char 66: space.
      if (is_null(slow) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( slow .lt. 0 .or. slow .gt. 999.99 ) then
              write (isf_error,'(''bad slow: '',f9.2)')
     +               slow
              write_phase = 20
              return
          end if
          write (file,'(f6.2,'' '',$)') slow
      end if

c     Chars 67-72: slowness residual. Char 73: space.
      if (is_null(slowres) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( slowres .lt. -99999 .or. slowres .gt. 99999 ) then
              write (isf_error,'(''bad slowres: '',f9.2)')
     +               slowres
              write_phase = 20
              return
          end if
          call write_real(file,slowres,6,1)
          write (file,'('' '',$)')
      end if

c     Char 74: time defining flag.
      if (timedef .eq. ' ') then
          timedef = '_'
c     else if (timedef .ne. '_' .and. timedef .ne. 'T' ) then
      else if (timedef .ne. '_' .and. timedef .ne. 'T' .and. 
     +         timedef .ne. 'x' ) then
          isf_error = 'bad timedef flag: '//timedef
          write_phase = 20
          return
      end if
      write (file,'(a1,$)') timedef

c     Char 75: azimuth defining flag.
      if (azimdef .eq. ' ') then
          azimdef = '_'
      else if (azimdef .ne. '_' .and. azimdef .ne. 'A' .and.
     +         azimdef .ne. 'x' ) then
          isf_error = 'bad azimdef flag: '//azimdef
          write_phase = 20
          return
      end if
      write (file,'(a1,$)') azimdef

c     Char 76: slowness defining flag. Char 77: space.
      if (slowdef .eq. ' ') then
          slowdef = '_'
      else if (slowdef .ne. '_' .and. slowdef .ne. 'S' .and.
     +         slowdef .ne. 'x' ) then
          isf_error = 'bad slowdef flag: '//slowdef
          write_phase = 20
          return
      end if
      write (file,'(a1,'' '',$)') slowdef

c     Chars 78-82: signal-to noise. Char 83: space.
      if (is_null(snr) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( snr .lt. 0. .or. snr .gt. 9999. ) snr = 9999.
c
c JS      write (file,'(f5.1,'' '',$)') snr
          call write_real(file,snr,5,1)
          write (file,'('' '',$)')
      end if

c     Chars 84-92: amplitude. Char 93: space.
      if (is_null(amp) .eq. 1) then
          write (file,'(a,$)') '          '
      else
          if ( amp .lt. 0 .or. amp .gt. 9999999.9 ) then
              write (isf_error,'(''bad amp: '',f9.2)') amp
              write_phase = 20
              return
          end if
          write (file,'(f9.1,'' '',$)') amp
      end if

c     Chars 94-98: period. Char 99: space.
      if (is_null(per) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( per .lt. 0 .or. per .gt. 99.99 ) then
              write (isf_error,'(''bad per: '',f9.2)') per
              write_phase = 20
              return
          end if
          write (file,'(f5.2,'' '',$)') per
      end if

c     Char 100: picktype.
      if (picktype .eq. ' ') then
          picktype = '_'
      else if (picktype .ne. '_' .and. picktype .ne. 'a' .and. picktype 
     + .ne. 'm') then
          isf_error = 'bad picktype: '//picktype
          write_phase = 20
          return
      end if
      write (file,'(a1,$)') picktype

c     Char 101: sp_fm.
      if (sp_fm .eq. ' ') then
          sp_fm = '_'
      else if (sp_fm .eq. 'C') then
          sp_fm = 'c'
      else if (sp_fm .eq. 'D') then
          sp_fm = 'd'
      else if (sp_fm .ne. '_' .and. sp_fm .ne. 'c' .and. sp_fm .ne. 
     + 'd') then
          isf_error = 'bad sp_fm: '//sp_fm
          write_phase = 20
          return
      end if
      write (file,'(a1,$)') sp_fm

c     Char 102: detchar. Char 103: space.
      if (detchar .eq. ' ') then
          detchar = '_'
      else if (detchar .eq. 'E') then
          detchar = 'e'
      else if (detchar .eq. 'I') then
          detchar = 'i'
      else if (detchar .eq. 'Q') then
          detchar = 'q'
      else if (detchar .ne. '_' .and. detchar .ne. 'i'  
     +         .and. detchar .ne. 'e' .and. detchar .ne. 'q') then
          isf_error = 'bad detchar: '//detchar
          write_phase = 20
          return
      end if
      write (file,'(a1,'' '',$)') detchar

c     Chars 104-108: magnitude type.
      numchar = partline(substr,magtype,0,0)
      if ( numchar .eq. 0 .or. magtype.eq.'     ') then
          write (file,'(a5,$)') ' '
      else
          if ( numchar .gt. ISF_MAGTYPE_LEN ) then
              write (isf_error,'(''magtype too long: '',a)') 
     + magtype
              write_phase = 20
              return
          end if
          if ( check_whole(magtype) .eq. 1 ) then
              write (isf_error,'(''bad magtype: '',a)') magtype
              write_phase = 20
              return
          end if
          write(file,'(a5,$)') magtype
      end if

c     Char 109: magnitude indicator.
      if (magind .ne. ' ' .and. magind .ne. '>' .and. magind .ne. '<') 
     + then
          isf_error = 'bad magind: '//magind
          write_phase = 20
          return
      end if
      write (file,'(a1,$)') magind

c     Chars 110-113: magnitude. Char 114: space.
      if (is_null(mag) .eq. 1) then
          write (file,'(a,$)') '     '
      else
c         if ( mag .lt. 0 .or. mag .gt. 10 ) then
c  allow negative magnitudes
c JS 12 Feb 2023
c
          if ( mag .lt. -9.94 .or. mag .gt. 9.94 ) then
              write (isf_error,'(''bad mag: '',f9.2)') mag
              write_phase = 20
              return
          end if
          write (file,'(f4.1,'' '',$)') mag
      end if

c     Chars 115-122: arrival ID.
      numchar = partline(substr,arrid,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_error,'(''missing arrid'')')
          write_phase = 20
          return
      end if
      if ( numchar .gt. ISF_ARRID_LEN ) then
          write (isf_error,'(''arrid too long: '',a)') arrid
          write_phase = 20
          return
      end if
      if ( check_whole(arrid) .eq. 1 ) then
          write (isf_error,'(''bad arrid: '',a)') arrid
          write_phase = 20
          return
      end if

      lc = len_trim(com)
      if(lc.le.0) then
        write(file,'(a8)') arrid
      else
        write(file,'(a8,$)') arrid
        write(file,'(a)') com(1:lc)
      endif

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase') .ne. 0) then
          write_phase = 10
          return
      end if

      write_phase = 0
      return
      end

c     Writes a phase origin-id comment line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_phase_origid(file,origid)

      integer file
      character origid*(*)

      include 'isf_head.h'
      integer check_prev_line_type,partline,check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-10: comment start string and space.
      write (file,'(a,$)') ' (#OrigID '

c     Chars 11-18: origin ID.
      numchar = partline(substr,origid,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_error,'(''missing origid'')')
          write_phase_origid = 20
          return
      end if
      if ( numchar .gt. ISF_ORIGID_LEN ) then
          write (isf_error,'(''origid too long'',a)') origid
          write_phase_origid = 20
          return
      end if
      if ( check_whole(origid) .eq. 1 ) then
          write (isf_error,'(''bad origid'',a)') origid
          write_phase_origid = 20
          return
      end if
      write(file,'(a8,'')'')') origid

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_origid') .ne. 0) then
          write_phase_origid = 10
          return
      end if

      write_phase_origid = 0
      return
      end


c     Writes phase info header complete with preceding blank line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_phase_info_head(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      character head*(124)

      head = 'Net      Chan F Low_F HighF AuthPhas    Date     eTime' //
     +       ' wTime eAzim wAzim  eSlow wSlow      eAmp  ePer eMag'   //     
     +       ' Author      ArrID'

      write (file,'()')
      write (file,'(a)') head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_info_head') .ne. 0) then
          write_phase_info_head = 10
          return
      end if

      write_phase_info_head = 0
      return
      end

c     Writes a phase info block data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_phase_info(file,net,chan,filter,
     + filter_min,filter_max,phase,yyyy,mm,dd,time_unc,time_weight, 
     + azim_unc,azim_weight,slow_unc,slow_weight,amp_unc,
     + per_unc,mag_unc,author,arrid)

      integer file
      character net*(*),chan*(*),author*(*),arrid*(*),phase*(*)
      character filter
      real*4 filter_min,filter_max,time_unc,time_weight,azim_unc
      real*4 azim_weight
      real*4 slow_unc,slow_weight,amp_unc,per_unc,mag_unc
      integer yyyy,mm,dd

      include 'isf_head.h'
      integer check_prev_line_type,partline,check_whole,is_null
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-9: network code. Char 10: space.
      numchar = partline(substr,net,0,0)
      if ( numchar .eq. 0 ) then
          write(file,'(a10,$)') ' '
      else
          if ( numchar .gt. ISF_NET_LEN ) then
              write (isf_error,'(''net too long: '',a)') net
              write_phase_info = 20
              return
          end if
          if ( check_whole(net) .eq. 1 ) then
              write (isf_error,'(''bad net: '',a)') net
              write_phase_info = 20
              return
          end if
          write(file,'(a9,'' '',$)') net
      end if

c     Chars 11-13: channel. Char 14: space.
      numchar = partline(substr,chan,0,0)
      if ( numchar .eq. 0 ) then
          write(file,'(''    '',$)')
      else
          if ( numchar .gt. ISF_CHAN_LEN ) then
              write (isf_error,'(''chan too long: '',a)') chan
              write_phase_info = 20
              return
          end if
          if ( check_whole(chan) .eq. 1 ) then
              write (isf_error,'(''bad chan: '',a)') chan
              write_phase_info = 20
              return
          end if
          write(file,'(a3,'' '',$)') chan
      end if

c     Char 15: filter. Char 16: space.
      if (filter .ne. '0' .and. filter .ne. 'C' .and. filter .ne. ' ') 
     + then
          isf_error = 'bad filter: '//filter
          write_phase_info = 20
          return
      end if
      write (file,'(a1,'' '',$)') filter

c     Chars 17-21: minimum filter frequency. Char 22: space.
      if (is_null(filter_min) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( filter_min .lt. 0 .or. filter_min .gt. 99.99 ) then
              write (isf_error,'(''bad filter_min: '',f9.2)') 
     + filter_min
              write_phase_info = 20
              return
          end if
          write (file,'(f5.2,'' '',$)') filter_min
      end if

c     Chars 23-27: maximum filter frequency. Char 28: space.
      if (is_null(filter_max) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( filter_max .lt. 0 .or. filter_max .gt. 99.99 ) then
              write (isf_error,'(''bad filter_max: '',f9.2)') 
     + filter_max
              write_phase_info = 20
              return
          end if
          write (file,'(f5.2,'' '',$)') filter_max
      end if

c     Chars 29-36: author's phase. Char 37: space.
      numchar = partline(substr,phase,0,0)
      if ( numchar .eq. 0 ) then
          write(file,'(a9,$)') ' '
      else
          if ( numchar .gt. ISF_PHASE_LEN ) then
              write (isf_error,'(''phase too long: '',a)') phase
              write_phase_info = 20
              return
          end if
          if ( check_whole(phase) .eq. 1 ) then
              write (isf_error,'(''bad phase: '',a)') phase
              write_phase_info = 20
              return
          end if
          write(file,'(a8,'' '',$)') phase
      end if


c     Chars 38-47: arrival date. Char 48: space.
      if (is_null(real(yyyy)) .eq. 1 .and. is_null(real(mm)) .eq. 1 
     + .and. is_null(real(dd)) .eq. 1) then

          write(file,'(a,$)') '           '
      else
          if (is_null(real(yyyy)) .eq. 1) then
              isf_error = 'date but no year'
              write_phase_info = 20
              return
          end if

          if (yyyy .lt. 0 .or. yyyy .gt. 9999) then
              write (isf_error,'(''bad year '',i9)') yyyy
              write_phase_info = 20
              return
          end if

          if (is_null(real(mm)) .eq. 1) then
              isf_error = 'year but no month'
              write_phase_info = 20
              return
          end if

          if (mm .lt. 0 .or. mm .gt. 12) then
              write (isf_error,'(''bad month '',i9)') mm
              write_phase_info = 20
              return
          end if

          if (is_null(real(dd)) .eq. 1) then
              isf_error = 'year but no day'
              write_phase_info = 20
              return
          end if

          if (dd .lt. 0 .or. dd .gt. 31) then
              write (isf_error,'(''bad day '',i9)') dd
              write_phase_info = 20
              return
          end if
          write (file,'(i4.4,''/'',i2.2,''/'',i2.2,'' '',$)') yyyy,mm,dd
      end if

c     Chars 49-54: uncertainty in arrival time. Char 55 space.
      if (is_null(time_unc) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( time_unc .lt. 0 .or. time_unc .gt. 99 ) then
              write (isf_error,'(''bad time_unc: '',f9.2)') 
     + time_unc
              write_phase_info = 20
              return
          end if
          write (file,'(f6.3,'' '',$)') time_unc
      end if

c     Chars 56-60: time weight. Char 61 space.
      if (is_null(time_weight) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( time_weight .lt. 0 .or. time_weight .gt. 1 ) then
              write (isf_error,'(''bad time_weight: '',f9.2)') 
     + time_weight
              write_phase_info = 20
              return
          end if
          write (file,'(f5.3,'' '',$)') time_weight
      end if

c     Chars 62-66: azimuth uncertainty. Char 67 space.
      if (is_null(azim_unc) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( azim_unc .lt. 0 .or. azim_unc .gt. 360 ) then
              write (isf_error,'(''bad azim_unc: '',f9.2)') 
     + azim_unc
              write_phase_info = 20
              return
          end if
          write (file,'(f5.1,'' '',$)') azim_unc
      end if

c     Chars 68-72: azimuth weight. Char 73 space.
      if (is_null(azim_weight) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( azim_weight .lt. 0 .or. azim_weight .gt. 1 ) then
              write (isf_error,'(''bad azim_weight: '',f9.2)') 
     + azim_weight
              write_phase_info = 20
              return
          end if
          write (file,'(f5.3,'' '',$)') azim_weight
      end if

c     Chars  74-79: slowness uncertainty. Char 80 space.
      if (is_null(slow_unc) .eq. 1) then
          write (file,'(a,$)') '       '
      else
          if ( slow_unc .lt. 0 .or. slow_unc .gt. 9999.9 ) then
              write (isf_error,'(''bad slow_unc: '',f9.2)') 
     + slow_unc
              write_phase_info = 20
              return
          end if
          write (file,'(f6.1,'' '',$)') slow_unc
      end if

c     Chars 81-85: slowness weight. Char 86 space.
      if (is_null(slow_weight) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( slow_weight .lt. 0 .or. slow_weight .gt. 1 ) then
              write (isf_error,'(''bad slow_weight: '',f9.2)') 
     + slow_weight
              write_phase_info = 20
              return
          end if
          write (file,'(f5.3,'' '',$)') slow_weight
      end if

c     Chars 87-95: amplitude unceratinty. Char 96 space.
      if (is_null(amp_unc) .eq. 1) then
          write (file,'(a,$)') '          '
      else
          if ( amp_unc .lt. 0 .or. amp_unc .gt. 9999999.9 ) then
              write (isf_error,'(''bad amp_unc: '',f9.2)')
     +               amp_unc
              write_phase_info = 20
              return
          end if
          write (file,'(f9.1,'' '',$)') amp_unc
      end if

c     Chars 97-101: period uncertainty. Char 102 space.
      if (is_null(per_unc) .eq. 1) then
          write (file,'(a,$)') '      '
      else
          if ( per_unc .lt. 0 .or. per_unc .gt. 99 ) then
              write (isf_error,'(''bad per_unc: '',f9.2)')
     +               per_unc
              write_phase_info = 20
              return
          end if
          write (file,'(f5.2,'' '',$)') per_unc
      end if

c     Chars 103-105: uncertainty in station magnitude. Char 106 space.
      if (is_null(mag_unc) .eq. 1) then
          write (file,'(a,$)') '    '
      else
          if ( mag_unc .lt. 0 .or. mag_unc .gt. 9.9 ) then
              write (isf_error,'(''bad mag_unc: '',f9.2)')
     +               mag_unc
              write_phase_info = 20
              return
          end if
          write (file,'(f3.1,'' '',$)') mag_unc
      end if

c     Chars 107-115: author. Char 116: space.
      numchar = partline(substr,author,0,0)
      if ( numchar .eq. 0 ) then
          write(file,'(a10,$)') ' '
      else
          if ( numchar .gt. ISF_AUTHOR_LEN ) then
              write (isf_error,'(''author too long: '',a)') 
     + author
              write_phase_info = 20
              return
          end if
          if ( check_whole(author) .eq. 1 ) then
              write (isf_error,'(''bad author: '',a)') author
              write_phase_info = 20
              return
          end if
          write(file,'(a9,'' '',$)') author
      end if

c     Chars 117-124: arrival ID.
      numchar = partline(substr,arrid,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_error,'(''missing arrid'')')
          write_phase_info = 20
          return
      end if
      if ( numchar .gt. ISF_ARRID_LEN ) then
          write (isf_error,'(''arrid too long: '',a)') arrid
          write_phase_info = 20
          return
      end if
      if ( check_whole(arrid) .eq. 1 ) then
          write (isf_error,'(''bad arrid: '',a)') arrid
          write_phase_info = 20
          return
      end if
      write(file,'(a8)') arrid

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_info') .ne. 0) then
          write_phase_info = 10
          return
      end if

      write_phase_info = 0
      return
      end

c     Writes a phase measure formatted comment.
c     Writes any number of parameter=value pairs, starting new line if necessary.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_phase_measure(file,param,value,
     +                                          error,numparam)

      integer file,numparam
      character param(*)*(*),value(*)*(*),error(*)*(*)
      include 'isf_head.h'
      integer partline,check_prev_line_type
      integer i,len,space_left
      integer numchar_param,numchar_value,numchar_error
      character substr*(ISF_LINE_LEN)

      write (file,'(a,$)') ' (#MEASURE'
      space_left = ISF_COMM_LEN

      do i=1,numparam
          numchar_param = partline(substr,param(i),0,0)
          numchar_value = partline(substr,value(i),0,0)
          numchar_error = partline(substr,error(i),0,0)
          len = numchar_param + numchar_value + 1
          if (numchar_error .ne. 0) then
              len = len + numchar_error + 1
          end if
          if ( len .gt. ISF_COMM_LEN ) then
              write (isf_error,'(''param=value too long'')')
              write_phase_measure = 20
              return
          end if

          if ( space_left .lt. len ) then
              write (file,'('')'')')
              write (file,'(a,$)') ' (#MEASURE'
            space_left = ISF_COMM_LEN
          end if

          write (file,'('' '',a,$)') param(i)(1:numchar_param)
          write (file,'(''='',a,$)') value(i)(1:numchar_value)
          if (numchar_error .ne. 0) then
              write (file,'(''+'',a,$)') error(i)(1:numchar_error)
          end if
          space_left = space_left - len
      end do
      write (file,'('')'')')

c     Check that this line's type should follow the previous line's type.
      if (isf_prev_line_type(1:10) .eq. 'phase_info' .or.    
     +    isf_prev_line_type(1:14) .eq. 'phase_info_com') then

          if (check_prev_line_type('phase_info_com') .ne. 0) then
              write_phase_measure = 10
              return
          end if
      else
          if (check_prev_line_type('phase_com') .ne. 0) then
              write_phase_measure = 10
              return
          end if
      end if

      write_phase_measure = 0
      return
      end

c     Writes a minimum phase range line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_phase_min(file,timeoffset,azoffset,
     +                        slowoffset,ampoffset,peroffset,magoffset)

      integer file
      real*4 timeoffset,azoffset,slowoffset,ampoffset,peroffset,
     +       magoffset

      include 'isf_head.h'
      integer check_prev_line_type,is_null

c     Chars 1-6: comment format string. Chars 7-47: spaces.
      write (file,'(a,a41,$)') ' (#MIN',' '

c     Chars 48-54: time offset. Chars 55-60: spaces.
      if (is_null(timeoffset) .eq. 1) then
          write (file,'(a13,$)') ' '
      else
          if ( timeoffset .lt. -99.999 .or. timeoffset .gt. 0 ) then
              write (isf_error,'(''bad timeoffset: '',f9.2)') 
     + timeoffset
              write_phase_min = 20
              return
          end if
          write (file,'(f7.3,a6,$)') timeoffset,' '
      end if

c     Chars 61-66: azimuth offset. Chars 67-72: spaces.
      if (is_null(azoffset) .eq. 1) then
          write (file,'(a12,$)') ' '
      else
          if ( azoffset .lt. -360 .or. azoffset .gt. 0 ) then
              write (isf_error,'(''bad azoffset: '',f9.2)') 
     + azoffset
              write_phase_min = 20
              return
          end if
          write (file,'(f6.1,a6,$)') azoffset, ' '
      end if

c     Chars 73-79: slowness offset. Chars 80-85: spaces.
      if (is_null(slowoffset) .eq. 1) then
          write (file,'(a13,$)') ' '
      else
          if ( slowoffset .lt. -9999.9 .or. slowoffset .gt. 0 ) then
              write (isf_error,'(''bad slowoffset: '',f9.2)') 
     + slowoffset
              write_phase_min = 20
              return
          end if
          write (file,'(f7.1,a6,$)') slowoffset, ' '
      end if

c     Chars 86-95: amplitude offset.
      if (is_null(ampoffset) .eq. 1) then
          write (file,'(a10,$)') ' '
      else
          if ( ampoffset .lt. -9999999.9 .or. ampoffset .gt. 0 ) then
              write (isf_error,'(''bad ampoffset: '',f9.2)') 
     + ampoffset
              write_phase_min = 20
              return
          end if
          write (file,'(f10.1,$)') ampoffset
      end if

c     Chars 96-101: period offset.
      if (is_null(peroffset) .eq. 1) then
          write (file,'(a6,$)') ' '
      else
          if ( peroffset .lt. -999.9 .or. peroffset .gt. 0 ) then
              write (isf_error,'(''bad peroffset: '',f9.2)') 
     + peroffset
              write_phase_min = 20
              return
          end if
          write (file,'(f6.1,$)') peroffset
      end if

c     Chars 102-105: magnitude offset.
      if (is_null(magoffset) .eq. 1) then
          write (file,'(a4,'')'')') ' '
      else
          if ( magoffset .lt. -9.9 .or. magoffset .gt. 0 ) then
              write (isf_error,'(''bad magoffset: '',f9.2)') 
     + magoffset
              write_phase_min = 20
              return
          end if
          write (file,'(f4.1,'')'')') magoffset
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_info_com') .ne. 0) then
          write_phase_min = 10
          return
      end if

      write_phase_min = 0
      return
      end

c     Writes a maximum phase range line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_phase_max(file,timeoffset,azoffset,
     +                        slowoffset,ampoffset,peroffset,magoffset)

      integer file
      real*4 timeoffset,azoffset,slowoffset,ampoffset,peroffset,
     +       magoffset

      include 'isf_head.h'
      integer check_prev_line_type,is_null

c     Chars 1-6: comment format string. Chars 7-47: spaces.
      write (file,'(a,a41,$)') ' (#MAX',' '

c     Chars 48-54: time offset. Chars 55-60: spaces.
      if (is_null(timeoffset) .eq. 1) then
          write (file,'(a13,$)') ' '
      else
          if ( timeoffset .lt. 0 .or. timeoffset .gt. 99.999 ) then
              write (isf_error,'(''bad timeoffset: '',f9.2)') 
     + timeoffset
              write_phase_max = 20
              return
          end if
          write (file,'(f7.3,a6,$)') timeoffset,' '
      end if

c     Chars 61-66: azimuth offset. Chars 67-72: spaces.
      if (is_null(azoffset) .eq. 1) then
          write (file,'(a12,$)') ' '
      else
          if ( azoffset .lt. 0 .or. azoffset .gt. 360 ) then
              write (isf_error,'(''bad azoffset: '',f9.2)') 
     + azoffset
              write_phase_max = 20
              return
          end if
          write (file,'(f6.1,a6,$)') azoffset, ' '
      end if

c     Chars 73-79: slowness offset. Chars 80-85: spaces.
      if (is_null(slowoffset) .eq. 1) then
          write (file,'(a13,$)') ' '
      else
          if ( slowoffset .lt. 0 .or. slowoffset .gt. 9999.9 ) then
              write (isf_error,'(''bad slowoffset: '',f9.2)') 
     + slowoffset
              write_phase_max = 20
              return
          end if
          write (file,'(f7.1,a6,$)') slowoffset, ' '
      end if

c     Chars 86-95: amplitude offset.
      if (is_null(ampoffset) .eq. 1) then
          write (file,'(a10,$)') ' '
      else
          if ( ampoffset .lt. 0 .or. ampoffset .gt. 9999999.9 ) then
              write (isf_error,'(''bad ampoffset: '',f9.2)') 
     + ampoffset
              write_phase_max = 20
              return
          end if
          write (file,'(f10.1,$)') ampoffset
      end if

c     Chars 96-101: period offset.
      if (is_null(peroffset) .eq. 1) then
          write (file,'(a6,$)') ' '
      else
          if ( peroffset .lt. 0 .or. peroffset .gt. 999.9 ) then
              write (isf_error,'(''bad peroffset: '',f9.2)') 
     + peroffset
              write_phase_max = 20
              return
          end if
          write (file,'(f6.1,$)') peroffset
      end if

c     Chars 102-105: magnitude offset.
      if (is_null(magoffset) .eq. 1) then
          write (file,'(a4,'')'')') ' '
      else
          if ( magoffset .lt. 0 .or. magoffset .gt. 9.9 ) then
              write (isf_error,'(''bad magoffset: '',f9.2)') 
     + magoffset
              write_phase_max = 20
              return
          end if
          write (file,'(f4.1,'')'')') magoffset
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_info_com') .ne. 0) then
          write_phase_max = 10
          return
      end if

      write_phase_max = 0
      return
      end

c     Writes a phase correction line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_phase_correc(file,timecorr,azcorr,
     +                        slowcorr,ampcorr,percorr,magcorr)

      integer file
      real*4 timecorr,azcorr,slowcorr,ampcorr,percorr,magcorr

      include 'isf_head.h'
      integer check_prev_line_type,is_null

c     Chars 1-8: comment format string. Chars 9-47: spaces.
      write (file,'(a,a39,$)') ' (#COREC',' '

c     Chars 48-54: time correction. Chars 55-60: spaces.
      if (is_null(timecorr) .eq. 1) then
          write (file,'(a13,$)') ' '
      else
          if ( timecorr .lt. -99.999 .or. timecorr .gt. 99.999 ) then
              write (isf_error,'(''bad timecorr: '',f9.2)') 
     + timecorr
              write_phase_correc = 20
              return
          end if
          write (file,'(f7.3,a6,$)') timecorr,' '
      end if

c     Chars 61-66: azimuth correction. Chars 67-72: spaces.
      if (is_null(azcorr) .eq. 1) then
          write (file,'(a12,$)') ' '
      else
          if ( azcorr .lt. -360 .or. azcorr .gt. 360 ) then
              write (isf_error,'(''bad azcorr: '',f9.2)') azcorr
              write_phase_correc = 20
              return
          end if
          write (file,'(f6.1,a6,$)') azcorr, ' '
      end if

c     Chars 73-79: slowness corr. Chars 80-85: spaces.
      if (is_null(slowcorr) .eq. 1) then
          write (file,'(a13,$)') ' '
      else
          if ( slowcorr .lt. -9999.9 .or. slowcorr .gt. 9999.9 ) then
              write (isf_error,'(''bad slowcorr: '',f9.2)') 
     + slowcorr
              write_phase_correc = 20
              return
          end if
          write (file,'(f7.1,a6,$)') slowcorr, ' '
      end if

c     Chars 86-95: amplitude corr.
      if (is_null(ampcorr) .eq. 1) then
          write (file,'(a10,$)') ' '
      else
          if ( ampcorr .lt. -9999999.9 .or. ampcorr .gt. 9999999.9 ) 
     + then
              write (isf_error,'(''bad ampcorr: '',f9.2)')
     +               ampcorr
              write_phase_correc = 20
              return
          end if
          write (file,'(f10.1,$)') ampcorr
      end if

c     Chars 96-101: period corr.
      if (is_null(percorr) .eq. 1) then
          write (file,'(a6,$)') ' '
      else
          if ( percorr .lt. -999.9 .or. percorr .gt. 999.9 ) then
              write (isf_error,'(''bad percorr: '',f9.2)')
     +               percorr
              write_phase_correc = 20
              return
          end if
          write (file,'(f6.1,$)') percorr
      end if

c     Chars 102-106: magnitude correction.
      if (is_null(magcorr) .eq. 1) then
          write (file,'(a5,'')'')') ' '
      else
          if ( magcorr .lt. -9.99 .or. magcorr .gt. 9.99 ) then
              write (isf_error,'(''bad magcorr: '',f9.2)')
     +               magcorr
              write_phase_correc = 20
              return
          end if
          write (file,'(f5.2,'')'')') magcorr
      end if

      if (check_prev_line_type('phase_info_com') .ne. 0) then
          write_phase_correc = 10
          return
      end if

      write_phase_correc = 0
      return
      end

c     Writes an original phase data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_phase_original(file,chan,sta,yyyy,
     +                  mm,dd,hh,mi,ss,msec,azim,slow,amp,per,mag)

      integer file
      character chan*(*),sta*(*)
      real*4 azim,slow,amp,per,mag
      integer yyyy,mm,dd,hh,mi,ss,msec

      include 'isf_head.h'
      integer check_prev_line_type,is_null,check_whole,partline
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-10: comment format string.
      write (file,'(a,$)') ' (#ORIG   '

c     Chars 11-13: original channel. Char 14: space.
      numchar = partline(substr,chan,0,0)
      if ( numchar .eq. 0 ) then
          write(file,'(''    '',$)')
      end if
      if ( numchar .gt. ISF_CHAN_LEN ) then
          write (isf_error,'(''chan too long: '',a)') chan
          write_phase_original = 20
          return
      end if
      if ( check_whole(chan) .eq. 1 ) then
          write (isf_error,'(''bad chan: '',a)') chan
          write_phase_original = 20
          return
      end if
      write(file,'(a3,'' '',$)') chan

c     Chars 15-19: original station code. Char 20-37: space.
c     Format gives 8 chars for sta but sta can only be 5 chars long in IMS.
      numchar = partline(substr,sta,0,0)
      if ( numchar .eq. 0 ) then
          write(file,'(a23,$)') ' '
      end if
      if ( numchar .gt. ISF_STA_LEN ) then
          write (isf_error,'(''sta too long: '',a)') sta
          write_phase_original = 20
          return
      end if
      if ( check_whole(sta) .eq. 1 ) then
          write (isf_error,'(''bad sta: '',a)') sta
          write_phase_original = 20
          return
      end if
      write(file,'(a5,a18,$)') sta,' '

c     Chars 38-60: arrival date and time.
c     Chars 38-47: date. Char 48: space.
      if (is_null(real(yyyy)) .eq. 1 .and. is_null(real(mm)) .eq. 1 
     + .and. is_null(real(dd)) .eq. 1) then

          write(file,'(a11,$)') ' '
      else
          if (is_null(real(yyyy)) .eq. 1) then
              isf_error = 'date but no year'
              write_phase_original = 20
              return
          end if

          if (yyyy .lt. 0 .or. yyyy .gt. 9999) then
              write (isf_error,'(''bad year '',i9)') yyyy
              write_phase_original = 20
              return
          end if

          if (is_null(real(mm)) .eq. 1) then
              isf_error = 'year but no month'
              write_phase_original = 20
              return
          end if

          if (mm .lt. 0 .or. mm .gt. 12) then
              write (isf_error,'(''bad month  '',i9)') mm
              write_phase_original = 20
              return
          end if

          if (is_null(real(dd)) .eq. 1) then
              isf_error = 'year but no day'
              write_phase_original = 20
              return
          end if

          if (dd .lt. 0 .or. dd .gt. 31) then
              write (isf_error,'(''bad day'',i9)') dd
              write_phase_original = 20
              return
          end if
          write (file,'(i4.4,''/'',i2.2,''/'',i2.2,'' '',$)') yyyy,mm,dd
      end if

c     Chars 49-60: time. Char 61: space.
      if (is_null(real(hh)) .eq. 1 .and. is_null(real(mi)) .eq. 1 .and.
     +    is_null(real(ss)) .eq. 1) then

          write(file,'(a13,$)') ' '
      else
          if (is_null(real(hh)) .eq. 1) then
              isf_error = 'missing hour'
              write_phase_original = 20
              return
          end if

          if (hh .lt. 0 .or. hh .gt. 23) then
              write (isf_error,'(''bad hour '',i9)') hh
              write_phase_original = 20
              return
          end if

          if (is_null(real(mi)) .eq. 1) then
              isf_error = 'missing minute'
              write_phase_original = 20
              return
          end if

          if (mi .lt. 0 .or. mi .gt. 59) then
              write (isf_error,'(''bad minute  '',i9)') mi
              write_phase_original = 20
              return
          end if

          if (is_null(real(ss)) .eq. 1) then
              isf_error = 'missing second'
              write_phase_original = 20
              return
          end if

          if (ss .lt. 0 .or. ss .gt. 59) then
              write (isf_error,'(''bad second '',i9)') ss
              write_phase_original = 20
              return
          end if
          write (file,'(i2.2,'':'',i2.2,'':'',i2.2,$)') hh,mi,ss

c     	Chars 57-60 msec - put blanks here if no msec provided.
          if (is_null(real(msec)) .eq. 1) then
              write(file,'(''     '',$)')
          else
              if (msec .lt. 0 .or. msec .gt. 999) then
                  write (isf_error,'(''bad msec '',i9)') msec
                  write_phase_original = 20
                  return
              end if
              write(file,'(''.'',i3.3,'' '',$)') msec
          end if
      end if

c     Chars 62-66: original azimuth. Chars 67-73: spaces.
      if (is_null(azim) .eq. 1) then
          write (file,'(a12,$)') ' '
      else
          if ( azim .lt. 0 .or. azim .gt. 360 ) then
              write (isf_error,'(''bad azim: '',f9.2)') azim
              write_phase_original = 20
              return
          end if
          write (file,'(f5.1,a7,$)') azim, ' '
      end if

c     Chars 74-79: original slowness. Chars 80-86: spaces.
      if (is_null(slow) .eq. 1) then
          write (file,'(a13,$)') ' '
      else
          if ( slow .lt. -9999.9 .or. slow .gt. 9999.9 ) then
              write (isf_error,'(''bad slow: '',f9.2)') slow
              write_phase_original = 20
              return
          end if
          write (file,'(f6.1,a7,$)') slow, ' '
      end if

c     Chars 87-95: original amplitude.  Char 96: space.
      if (is_null(amp) .eq. 1) then
          write (file,'(a10,$)') ' '
      else
          if ( amp .lt. 0 .or. amp .gt. 9999999.9 ) then
              write (isf_error,'(''bad amp: '',f9.2)') amp
              write_phase_original = 20
              return
          end if
          write (file,'(f9.1,'' '',$)') amp
      end if

c     Chars 97-101: original period. Char 102: space.
      if (is_null(per) .eq. 1) then
          write (file,'(a6,$)') ' '
      else
          if ( per .lt. 0 .or. per .gt. 99.99 ) then
              write (isf_error,'(''bad per: '',f9.2)') per
              write_phase_original = 20
              return
          end if
          write (file,'(f5.2,'' '',$)') per
      end if

c     Chars 103-105: original station magnitude.
      if (is_null(mag) .eq. 1) then
          write (file,'(''   )'')')
      else
          if ( mag .lt. 0 .or. mag .gt. 9.99 ) then
              write (isf_error,'(''bad mag: '',f9.2)') mag
              write_phase_original = 20
              return
          end if
          write (file,'(f3.1,'')'')') mag
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_info_com') .ne. 0) then
          write_phase_original = 10
          return
      end if

      write_phase_original = 0
      return
      end


c     Writes a plain IMS comment.
c     Takes string as its argument, cuts it into lines of less than the maximium
c     allowed length and adds brackets to the start and end of each line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_comment(file,comment)

      integer file
      character comment*(*)

      include 'isf_head.h'
      integer check_prev_line_type,partline
      character substr*(ISF_LINE_LEN)
      integer numchar,i
      character word_break

      numchar = partline(substr,comment,0,0)
      do while (numchar .gt. ISF_LINE_LEN-3)
          i=ISF_LINE_LEN-3
          do while (word_break .ne. ' ')
              word_break = substr(i:i)
              i = i-1
          end do

          write (file,'('' ('',a,'')'')') substr(1:i)
          substr = substr(i+1:)
          numchar = numchar - i
      end do
      write (file,'('' ('',a,'')'')') substr(1:numchar)

c     Check that this line's type should follow the previous line's type.
c     line_type depends on previous line_type.
      if (isf_prev_line_type(1:6) .eq. 'origin' .or.    
     +    isf_prev_line_type(1:4) .eq. 'axes' .or.
     +    isf_prev_line_type(1:8) .eq. 'axes_err' .or.
     +    isf_prev_line_type(1:11) .eq. 'fault_plane' .or.
     +    isf_prev_line_type(1:6) .eq. 'momten' .or.
     +    isf_prev_line_type(1:10) .eq. 'origin_com') then

          if (check_prev_line_type('origin_com') .ne. 0) then
              write_comment = 10
              return
          end if
      else if (isf_prev_line_type(1:6) .eq. 'netmag' .or.    
     +    isf_prev_line_type(1:10) .eq. 'netmag_com') then

          if (check_prev_line_type('netmag_com') .ne. 0) then
              write_comment = 10
              return
          end if
      else if (isf_prev_line_type(1:7) .eq. 'effects' .or.    
     +    isf_prev_line_type(1:11) .eq. 'effects_com') then

          if (check_prev_line_type('effects_com') .ne. 0) then
              write_comment = 10
              return
          end if
      else if (isf_prev_line_type(1:10) .eq. 'phase_info' .or.    
     +    isf_prev_line_type(1:14) .eq. 'phase_info_com') then

          if (check_prev_line_type('phase_info_com') .ne. 0) then
              write_comment = 10
              return
          end if
      else if (isf_prev_line_type(1:5) .eq. 'phase' .or.    
     +    isf_prev_line_type(1:11) .eq. 'group_phase' .or.
     +    isf_prev_line_type(1:9) .eq. 'phase_com') then

          if (check_prev_line_type('phase_com') .ne. 0) then
              write_comment = 10
              return
          end if
      else
          if (check_prev_line_type('comment') .ne. 0) then
              write_comment = 10
              return
          end if
      end if

      write_comment = 0
      return
      end


c     Writes STOP line with a preceding blank line.

      integer function write_stop(file)

      integer file
      integer check_prev_line_type

      write (file,'()')
      write (file,'(a)') 'STOP'

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('stop') .ne. 0) then
          write_stop = 10
          return
      end if

      write_stop = 0
      return
      end


c     Data type ARRIVAL:GROUPED
c     =========================


c     Writes a grouped arrival header line.
c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     On error writes a diagnostic to isf_error.

      integer function write_group_head(file)

      integer file
      include 'isf_head.h'
      integer check_prev_line_type

      character head*(125)

      head = 'Net       Sta  Chan Aux     Date      Time'  //
     +       '       Phase     Azim  Slow   SNR       Amp' //
     +       '   Per Qual   Group C Author       ArrID'

      write (file,'(A)') head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('group_head') .ne. 0) then
          write_group_head = 10
          return
      end if

      write_group_head = 0
      return
      end


c     Writes a grouped arrival data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_error.

      integer function write_group(file,net,sta,chan,auxid,yyyy,mm,dd,
     + hh,mi,ss,msec,phase,azim,slow,snr,amp,per,picktype,sp_fm,
     + detchar,groupid,conflict,author,arrid)

      integer file
      character net*(*),sta*(*),chan*(*),auxid*(*),phase*(*)
      character groupid*(*),author*(*),arrid*(*)
      character sp_fm,detchar,picktype
      real*4 azim,slow,snr,amp,per
      integer yyyy,mm,dd,hh,mi,ss,msec,conflict

      include 'isf_head.h'
      integer partline,check_prev_line_type,is_null,check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-9: network code - can be null. Char 10 space.
      numchar = partline(substr,net,0,0)
      if ( numchar .eq. 0 ) then
          write (file,'(a9,'' '',$)') ' '
      else
          if ( numchar .gt. ISF_NET_LEN ) then
              write (isf_error,'(''net too long: '',a)') net
              write_group = 20
              return
          end if
          if ( check_whole(phase) .eq. 1 ) then
              write (isf_error,'(''bad net: '',a)') net
              write_group = 20
              return
          end if
          write(file,'(a9,'' '',$)') net
      end if

c     Chars 11-15: station code. Char 16: space.
      numchar = partline(substr,sta,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_error,'(''missing sta'')')
          write_group = 20
          return
      end if
      if ( numchar .gt. ISF_STA_LEN ) then
          write (isf_error,'(''sta too long: '',a)') sta
          write_group = 20
          return
      end if
      if ( check_whole(sta) .eq. 1 ) then
          write (isf_error,'(''bad sta: '',a)') sta
          write_group = 20
          return
      end if
      write(file,'(a5,'' '',$)') sta

c     Chars 17-19: channel - can be null. Char 20: space.
      numchar = partline(substr,chan,0,0)
      if ( numchar .eq. 0 ) then
          write (file,'(a3,'' '',$)') ' '
      else
          if ( numchar .gt. ISF_CHAN_LEN ) then
              write (isf_error,'(''chan too long: '',a)') chan
              write_group = 20
              return
          end if
          if ( check_whole(phase) .eq. 1 ) then
              write (isf_error,'(''bad chan: '',a)') chan
              write_group = 20
              return
          end if
          write(file,'(a3,'' '',$)') chan
      end if


c     Chars 21-24: auxid - can be null. Char 25: space.
      numchar = partline(substr,auxid,0,0)
      if ( numchar .eq. 0 ) then
          write (file,'(a4,'' '',$)') ' '
      else
          if ( numchar .gt. ISF_AUXID_LEN ) then
              write (isf_error,'(''auxid too long: '',a)') auxid
              write_group = 20
              return
          end if
          if ( check_whole(phase) .eq. 1 ) then
              write (isf_error,'(''bad auxid: '',a)') auxid
              write_group = 20
              return
          end if
          write(file,'(a4,'' '',$)') auxid
      end if


c     Chars 26-48: date/time - can be null. Char 49: space.
      if (is_null(real(yyyy)) .eq. 1 .and. is_null(real(mm)) .eq. 1
     +  .and. is_null(real(dd)) .eq. 1 .and. is_null(real(hh)) .eq. 1
     +  .and. is_null(real(mi)) .eq. 1 .and. is_null(real(ss)) .eq. 1)
     + then

          write(file,'(a23,'' '',$)') ' '
      else
          if (is_null(real(yyyy)) .eq. 1) then
              isf_error = 'missing year'
              write_group = 20
              return
          end if

          if (yyyy .lt. 1000 .or. yyyy .gt. 3000) then
              write (isf_error,'(''bad year '',i9)') yyyy
              write_group = 20
              return
          end if

          if (is_null(real(mm)) .eq. 1) then
              isf_error = 'missing month'
              write_group = 20
              return
          end if

          if (mm .lt. 1 .or. mm .gt. 12) then
              write (isf_error,'(''bad month '',i9)') mm
              write_group = 20
              return
          end if

          if (is_null(real(dd)) .eq. 1) then
              isf_error = 'missing day'
              write_group = 20
              return
          end if

          if (dd .lt. 1 .or. dd .gt. 31) then
              write (isf_error,'(''bad day '',i9)') dd
              write_group = 20
              return
          end if

          if (is_null(real(hh)) .eq. 1) then
              isf_error = 'missing hour'
              write_group = 20
              return
          end if

          if (hh .lt. 0 .or. hh .gt. 23) then
              write (isf_error,'(''bad hour '',i9)') hh
              write_group = 20
              return
          end if

          if (is_null(real(mi)) .eq. 1) then
              isf_error = 'missing minute'
              write_group = 20
              return
          end if

          if (mi .lt. 0 .or. mi .gt. 59) then
              write (isf_error,'(''bad minute  '',i9)') mi
              write_group = 20
              return
          end if

          if (is_null(real(ss)) .eq. 1) then
              isf_error = 'missing second'
              write_group = 20
              return
          end if

          if (ss .lt. 0 .or. ss .gt. 59) then
              write (isf_error,'(''bad second '',i9)') ss
              write_group = 20
              return
          end if
          write (file,'(i4.4,''/'',i2.2,''/'',i2.2,'' '',$)') yyyy,mm,dd
          write (file,'(i2.2,'':'',i2.2,'':'',i2.2,$)') hh,mi,ss

c     	Chars 45-48 msec - put blanks here if no msec provided. 
          if (is_null(real(msec)) .eq. 1) then
              write(file,'(a4,'' '',$)') ' '
          else
              if (msec .lt. 0 .or. msec .gt. 999) then
                  write (isf_error,'(''bad msec '',i9)') msec
                  write_group = 20
                  return
              end if
              write(file,'(''.'',i3.3,'' '',$)') msec
          end if
      end if

c     Chars 50-57: phase - can be null. Char 58: space.
      numchar = partline(substr,phase,0,0)
      if ( numchar .eq. 0 ) then
          write (file,'(a8,'' '',$)') ' '
      else
          if ( numchar .gt. ISF_PHASE_LEN ) then
              write (isf_error,'(''phase too long: '',a)') phase
              write_group = 20
              return
          end if
          if ( check_whole(phase) .eq. 1 ) then
              write (isf_error,'(''bad phase: '',a)') phase
              write_group = 20
              return
          end if
          write(file,'(a8,'' '',$)') phase
      end if

c     Chars 59-63: observed azimuth. Char 64: space.
      if (is_null(azim) .eq. 1) then
          write (file,'(a5,'' '',$)') ' '
      else
          if ( azim .lt. 0 .or. azim .gt. 360 ) then
              write (isf_error,'(''bad azim: '',f9.2)') azim
              write_group = 20
              return
          end if
          write (file,'(f5.1,'' '',$)') azim
      end if

c     Chars 65-69: slowness. Char 70: space.
      if (is_null(slow) .eq. 1) then
          write (file,'(a5,'' '',$)') ' '
      else
          if ( slow .lt. 0 .or. slow .gt. 999.99 ) then
              write (isf_error,'(''bad slow: '',f9.2)') slow
              write_group = 20
              return
          end if
          write (file,'(f5.1,'' '',$)') slow
      end if

c     Chars 71-75: signal-to noise. Char 76: space.
      if (is_null(snr) .eq. 1) then
          write (file,'(a5,'' '',$)') ' '
      else
          if ( snr .lt. 0 .or. snr .gt. 999 ) then
              write (isf_error,'(''bad snr: '',f9.2)') snr
              write_group = 20
              return
          end if
          write (file,'(f5.1,'' '',$)') snr
      end if

c     Chars 77-85: amplitude. Char 86: space.
      if (is_null(amp) .eq. 1) then
          write (file,'(a9,'' '',$)') ' '
      else
          if ( amp .lt. 0 .or. amp .gt. 9999999.9 ) then
              write (isf_error,'(''bad amp: '',f9.2)') amp
              write_group = 20
              return
          end if
          write (file,'(f9.1,'' '',$)') amp
      end if

c     Chars 87-91: period. Char 92: space.
      if (is_null(per) .eq. 1) then
          write (file,'(a5,'' '',$)') ' '
      else
          if ( per .lt. 0 .or. per .gt. 99.99 ) then
              write (isf_error,'(''bad per: '',f9.2)') per
              write_group = 20
              return
          end if
          write (file,'(f5.2,'' '',$)') per
      end if

c     Char 93: picktype.
      if (picktype .eq. ' ') then
          picktype = '_'
      else if (picktype .ne. '_' .and. picktype .ne. 'a' .and. picktype 
     + .ne. 'm') then
          isf_error = 'bad picktype: '//picktype
          write_group = 20
          return
      end if
      write (file,'(a1,$)') picktype

c     Char 94: sp_fm.
      if (sp_fm .eq. ' ') then
          sp_fm = '_'
      else if (sp_fm .ne. '_' .and. sp_fm .ne. 'c' .and. sp_fm .ne. 
     + 'd') then
          isf_error = 'bad sp_fm: '//sp_fm
          write_group = 20
          return
      end if
      write (file,'(a1,$)') sp_fm

c     Char 95: detchar. Char 103: space.
      if (detchar .eq. ' ') then
          detchar = '_'
      else if (detchar .ne. '_' .and. detchar .ne. 'i'  
     +         .and. detchar .ne. 'e' .and. detchar .ne. 'q') then
          isf_error = 'bad detchar: '//detchar
          write_group = 20
          return
      end if
      write (file,'(a1,'' '',$)') detchar

c     Chars 97-104: group ID, any characters allowed but must be there.
      numchar = partline(substr,sta,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_error,'(''missing groupid'')')
          write_group = 20
          return
      end if
      if ( numchar .gt. ISF_GROUPID_LEN ) then
          write (isf_error,'(''groupid too long: '',a)') groupid
          write_group = 20
          return
      end if
      if ( check_whole(sta) .eq. 1 ) then
          write (isf_error,'(''bad groupid: '',a)') groupid
          write_group = 20
          return
      end if
      write(file,'(a8,'' '',$)') groupid

c     Char 106: conflict - null means no conflict. 107 space.
      if (is_null(real(conflict)) .eq. 1) then
          write (file,'(a1,'' '',$)') ' '
      else
          if ( conflict .lt. 0 .or. conflict .gt. 9 ) then
              write (isf_error,'(''bad conflict: '',i9)') conflict
              write_group = 20
              return
          end if
          write (file,'(i1,'' '',$)') conflict
      end if

c     Chars 108-116: author - can be null. 117 space.
      numchar = partline(substr,author,0,0)
      if ( numchar .eq. 0 ) then
          write (file,'(a9,'' '',$)') ' '
      else
          if ( numchar .gt. ISF_AUTHOR_LEN ) then
              write (isf_error,'(''author too long: '',a)') author
              write_group = 20
              return
          end if
          if ( check_whole(author) .eq. 1 ) then
              write (isf_error,'(''bad author: '',a)') author
              write_group = 20
              return
          end if
          write(file,'(a9,'' '',$)') author
      end if

c     Chars 118-125: arrival ID, any characters allowed but must be there.
      numchar = partline(substr,arrid,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_error,'(''missing arrid'')')
          write_group = 20
          return
      end if
      if ( numchar .gt. ISF_ARRID_LEN ) then
          write (isf_error,'(''arrid too long: '',a)') arrid
          write_group = 20
          return
      end if
      if ( check_whole(arrid) .eq. 1 ) then
          write (isf_error,'(''bad arrid: '',a)') arrid
          write_group = 20
          return
      end if
      write(file,'(a8)') arrid

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('group_phase') .ne. 0) then
          write_group = 10
          return
      end if

      write_group = 0
      return
      end

