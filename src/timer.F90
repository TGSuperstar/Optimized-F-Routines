!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Timer Function for computing Time of Completion
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  DOUBLE PRECISION function getclock()
  USE CONST

  IMPLICIT NONE

  INTEGER :: v(8)
  REAL(KIND = dp) :: t

  v = 0
  t = DZERO

  CALL DATE_AND_TIME(VALUES = v)
  t = v(8) / 1000.0d0 + DONE * v(7) + 60.0d0 * v(6) + 3600.0d0 * v(5)
  getclock = t

  RETURN
  END
