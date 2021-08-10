FUNCTION array_intersection, a, b, npoints = npoints

  ; This function finds the intersections (common values) in two interger arrays
  ; and returns those values.

  ; Both 'a' and 'b' should be interger or long arrays, they need not be the same size

  ; If the intersection is zero, then a -1 is returned.
  ; The keyword 'npoints' contains the number of points found
  
  
  c = WHERE(HISTOGRAM(a, OMIN=om) GT 0 AND HISTOGRAM(b, MIN=om) GT 0)
  IF (c[0] NE -1) THEN BEGIN
    c = c + om
    npoints = N_ELEMENTS(c)
  ENDIF ELSE npoints = 0
  

  RETURN,c
END