  SUBROUTINE MatMat()

  USE const
  IMPLICIT NONE

!
! Variable Declarations
!

  INTEGER :: I
  INTEGER :: J
  INTEGER :: N

  REAL(kind=dp) :: getclock
  REAL(kind=dp) :: t0
  REAL(kind=dp) :: Total_C

  REAL(kind=dp),DIMENSION(:,:),ALLOCATABLE :: A
  REAL(kind=dp),DIMENSION(:,:),ALLOCATABLE :: B
  REAL(kind=dp),DIMENSION(:,:),ALLOCATABLE :: C
  
!
! Read in the size of the Matrices
!

  OPEN(10,file = 'info.dat')
  READ(10,*) N
  CLOSE(10)

!
! Allocate the size of the Matrices
!

  ALLOCATE(A(N,N)); A = DZERO 
  ALLOCATE(B(N,N)); B = DZERO 
  ALLOCATE(C(N,N)); C = DZERO 

!
! Create the Matrices 
!

  DO I = 1,N
     DO J = 1,N
        A(I,J) = 1.0d0/(I+J)
        B(I,J) = 1.0d0/((I*I)+(J*J))
     ENDDO
  ENDDO

!
! Start Timer
!

  T0 = getclock()

!
! Solve the Matrix Matrix Product
!

  call DGEMM('N','N',N,N,N,DONE,A,N,B,N,DZERO,C,N)

!
! Stop timer and Write time of completion
!

  T0 = getclock() - T0
  WRITE(*,*)'Time of Completion =',T0

!
! Sum the Computed Matrix
!

  Total_C = DZERO
  DO I = 1,N
     DO J = 1,N
        Total_C = C(I,J) + Total_C
     ENDDO
  ENDDO

  WRITE(*,*)'Total of the C Matrix =',Total_C
  
  DEALLOCATE(A,B,C)
  END SUBROUTINE MatMat  
