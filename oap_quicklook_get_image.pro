FUNCTION OAP_QUICKLOOK_GET_IMAGE, STARTING_VARIABLES, FILE_VARIABLES, RUNNING_VARIABLES, FIRST, LAST, n_buffers= n_buffers, time_array = time_array, data_record = data_record
  ;Unpacks structured varaibles that were defined by the previous functions
  ;This first block is from STARTING_VARIABLES
  PROC_FILE=STARTING_VARIABLES.FIELD01
  DIMG_FILE=STARTING_VARIABLES.FIELD02
  Display_particles_touching_edge=STARTING_VARIABLES.FIELD06
  Display_rejected_particles=STARTING_VARIABLES.FIELD07
  minD=LONG(STARTING_VARIABLES.FIELD08)
  maxD=LONG(STARTING_VARIABLES.FIELD09)
  ;The next variable is from FILE_VARAIBLES
  PRBTYPE=FILE_VARIABLES.PRBTYPE
  ;The last block is from RUNNING_VARIABLES
  POS=RUNNING_VARIABLES.POS
  REC=RUNNING_VARIABLES.REC
  TOUCHING_EDGE=RUNNING_VARIABLES.TOUCHING_EDGE
  AUTO_REJECT=RUNNING_VARIABLES.AUTO_REJECT
  SCNT=RUNNING_VARIABLES.SCNT
  diam=RUNNING_VARIABLES.diameter
  DISP_PARTS=RUNNING_VARIABLES.DISP_PARTS
  POS_DISP=RUNNING_VARIABLES.POS_DISP
  PART_CNT=RUNNING_VARIABLES.PART_CNT
  TIME_DISP=RUNNING_VARIABLES.TIME_DISP
  TMP_LENGTH=RUNNING_VARIABLES.TMP_LENGTH
  TMP_WIDTH=RUNNING_VARIABLES.TMP_WIDTH
  DATA_WIDTH=RUNNING_VARIABLES.DATA_WIDTH
  DATA_LENGTH=RUNNING_VARIABLES.DATA_LENGTH
  HHMMSS=RUNNING_VARIABLES.HHMMSS
  x = 0


  ;opens up DIMG and PROC files
  PROC= ncdf_open(PROC_file)
  DIMG= ncdf_open(DIMG_file)

  ;Intializes/resets varaibles
  stp= -1
  stt= -1
  
  ;The following code locates particles indices via the WHERE() function. The ARRAY_INTERSECTION() function finds the common indicies between each paritcle index array (diam_ind, scnt_ind, etc).
  ;The common indicies between diam_ind, scnt_ind, and autorej_ind are the final indicies of accepted particles. 
  
  ;Finds the indices where the diameter of the particle is less than the maximum diameter, and less than the minimum
  diam_ind = WHERE((diam LT maxD[0]) AND (diam GT minD[0]))
  
  ;Finds the indices where the slice count of the particle are greater than or equal to one.
  scnt_ind = WHERE(scnt GE 1)
  
  ;Finds the inidices where Auto Rejects equal a certain value
  autorej_ind = WHERE((AUTO_REJECT EQ 48) OR (AUTO_REJECT EQ 104) OR (AUTO_REJECT EQ 72) OR (AUTO_REJECT EQ 117) OR (AUTO_REJECT EQ 82) OR (auto_reject EQ 122))  
  
  ;Looks for the common indices between the particle diameter array, and the slice count array, then returns those indices
  part_ind = ARRAY_INTERSECTION(diam_ind, scnt_ind)
  
  ;This line finds the common indicies between particles that passed the auto_reject line, and the part_ind array from above.
  part_ind = ARRAY_INTERSECTION(autorej_ind, part_ind)
  
  ;This line looks for instances where particles are not touching the edge. It is only executed if specified by the user beforehand.
  IF (Display_particles_touching_edge EQ 'off') OR (Display_particles_touching_edge EQ 'OFF') THEN BEGIN
    touch_ind = WHERE( touching_edge LT 1)
    part_ind = ARRAY_INTERSECTION(touch_ind, part_ind)
  ENDIF
  
  ;We only want particles within the specified time range. To do this, we look for particle indicies between the earliest and latest time range. 
  time_index = WHERE(part_ind GE first AND part_ind LE last)
  
  ;Plug the time indices into the partlice_index array, and we have all the accepted particles within the specified time range.
  part_ind = part_ind[time_index]
  
  ;Determines the number of particles in part_ind. We will use this number to loop through all particles in the coming FOR loop
  nparts = N_ELEMENTS(part_ind)
  
  ;These two lines find the first and last particle indices. These indices are used in the next line 
  ;to determine how much rec data to pull from the DIMG file
  Stt = part_ind[0]
  stp = part_ind[-1]
  rec_cnt = rec[[stp]]-rec[[stt]]+1
  
  ;Opens the DIMG file using the NCDF_VARID() function. This data will be called later in a coming FOR loop.
  varid = NCDF_VARID(DIMG, 'data')
  NCDF_VARGET, DIMG, varid, tmp_data, OFFSET=[0,0,rec[Stt]],COUNT=[data_width,data_length,rec_cnt]
  IF prbtype EQ 'CIPG' THEN tmp = LONARR(tmp_width, tmp_length)+3 ELSE tmp = LONARR(tmp_width, tmp_length)

  ;Defines variables to use in the coming FOR loop
  tmp_cnt = 0
  buf_cnt = 0
  ;This FOR loop iterates through each particle, and builds buffers via their slice counts. The loop adds each slice count until
  ;the buffer fills to 1700. Once the buffer is full, the loop goes back one particle, and adds one to the total buffer count.
  
  ;Iterates from the first particle (0) to the last (nparts-1)
  FOR i = 0L, nparts-1 DO BEGIN
    ;Slice counts are added to tmp_cnt until it reaches greater than 1700
    tmp_cnt = tmp_cnt + scnt[part_ind[i]]
    IF (tmp_cnt GT 1700) THEN BEGIN
      ;Now that tmp_cnt is past 1700, go back one particle, so it can be added to the next buffer
      i=i-1
      ;Adding one to the buffer count
      buf_cnt=buf_cnt+1
      ;Reseting the variable
      tmp_cnt=0
      CONTINUE
    ENDIF
  ENDFOR

  ;Need to add one to the buffer count because the last one in the loop was partially filled, and did not trigger the IF statement
  ;to add it to n_buffers. 
  n_buffers = buf_cnt+1
  
  ;Creating an array to put the particle times in. This is used for image generation later. Need to LONG the array because of the high index values.
  time_array = MAKE_ARRAY(2, n_buffers, /LONG, VALUE = 0)
  
  ;Adding the n_buffer dimension to the tmp array
  tmp = LONARR(tmp_width, tmp_length, n_buffers)
  
  ;Defining variables to use in the FOR loop
  arr_pos = 0
  tmp_cnt = 0
  buf_cnt = 0
  
  ;The main concept with this FOR loop is its looping through each accepted particle, and filling the buffers with information. While tmp_cnt
  ;is less than 1700, it adds the particle information from the DIMG file (via tmp_data) to a buffer. When tmp_cnt reaches 1700, it will reset 
  ;tmp_cnt, and begin filling the next buffer with particle information. 
  
  ;Generating the first position of the time array
  time_pos = 0
  ;Looping through each particle from 0 to the last (nparts-1).
  FOR i = 0L, nparts-1 DO BEGIN
    IF tmp_cnt EQ 0 THEN BEGIN
      ;Adding the first particle's time stamp to the time_array
      time_array[0,time_pos] = HHMMSS(part_ind[i])
    ENDIF
    ;Adding to our tmp_cnt. While it is less than 1700, begin adding data from the DIMG file to the buffer.
    tmp_cnt = tmp_cnt + scnt[part_ind[i]]
    
    IF (tmp_cnt LE 1700) THEN BEGIN
      ;Adding the current particle information into the buffer.
      tmp[*,arr_pos:arr_pos+scnt[part_ind[i]]-1, buf_cnt] = tmp_data[*,pos[1,part_ind[i]]-scnt[part_ind[i]]+1:long(pos[1,part_ind[i]]),long(rec[part_ind[i]]-rec[stt])]
      
      ;for 2DS data, if all diodes are blocked, the cdf file shows everything unblocked. The following fixes that
      inds = WHERE( TOTAL(tmp[*,arr_pos:arr_pos+scnt[part_ind[i]]-1],1) EQ 0)
      IF (inds[0] NE -1) THEN tmp[*,arr_pos+inds] = 65535
      
      ;Adding to the increment array position for next iteration of the loop
      arr_pos = arr_pos+scnt[part_ind[i]] 
    
    ENDIF ELSE BEGIN
      ;Once the buffer has filled, the loop has to go back one particle to begin building the next buffer.
      ;Add one to buf_cnt, then reset tmp_cnt and arr_pos to 0 to get the position correct at the start of the next buffer.
      i=i-1
      buf_cnt=buf_cnt+1
      ;Adding the last particle time stamp to the other dimension of the array
      time_array[1,time_pos] = HHMMSS(part_ind[i])
      tmp_cnt=0
      arr_pos = 0
      time_pos = time_pos + 1
      CONTINUE      
    ENDELSE

  ENDFOR
  
  ;adding the last time stamp to the array, because the ENDIF is not called upon in the last iteration of the FOR loop.
  time_array[1,buf_cnt]= HHMMSS(part_ind[nparts-1])
  ;now that we have filled the buffer with our data, we will transpose the data into displayable images.
  ;The following statements build the data records based on probe type
  CASE 1 of
    prbtype EQ '2DS' or prbtype EQ 'HVPS': BEGIN
      if (stp eq -1) then break
      data_record = BYTARR(128,1700, n_buffers)
      ;close,1
      ;convert to binary
      FOR l=0, n_buffers-1 DO BEGIN
        FOR k=0,1700-1 DO BEGIN
          FOR j=0,7 DO BEGIN
            FOR i = 16, 31 DO BEGIN
              pow2 = 2L^(i-16)
              IF (LONG(tmp[j,k,l]) AND pow2) NE 0 THEN $
                data_record[j*16L+(i-16),k,l]=0 ELSE data_record[j*16L+(i-16),k,l]=255
            ENDFOR
            data_record[j*16L:j*16L+15,k,l] = REVERSE(data_record[j*16L:j*16L+15,k,l],1)
          ENDFOR
        ENDFOR
      ENDFOR

      data_record = REFORM(data_record)
      tmp = data_record
      data_record=LONARR(134,1706,n_buffers)
      data_record[3:130,3:1702,*]=tmp
    END

    prbtype EQ 'CIPG' : BEGIN
      data_record=BYTARR(64,850,n_buffers)
      ;convert to binary
      FOR l=0, n_buffers-1 DO BEGIN
        FOR k=0,850-1 DO BEGIN
          FOR j=0,63 DO BEGIN
            IF (LONG(tmp[j,k,l]) LE 2) THEN $
              data_record[j,k,l]=0 ELSE data_record[j,k,l]=255
            data_record[j,k,l] = REVERSE(data_record[j,k,l],1)
          ENDFOR
        ENDFOR
      ENDFOR
      
      data_record = REFORM(data_record)
      ;for CIP data, if all diodes are blocked, the cdf file shows everything unblocked....following fixes that
      end_buf=1
      FOR l=0, n_buffers-1 DO BEGIN
        FOR i=850-1,0,-1 DO BEGIN
          IF(TOTAL(data_record[*,i,l]) EQ 64 ) THEN data_record[*,i,l]=1
        ENDFOR
      ENDFOR

      ;the following draws a border around the buffer
      tmp = data_record
      data_record=LONARR(68,854,n_buffers)
      data_record[2:65,2:851,*]=tmp
    END
  ENDCASE


END   