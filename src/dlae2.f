      subroutine dlae2(A,B,C,RT1,RT2)
*
*  -- LAPACK auxiliary routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B, C, RT1, RT2
*     ..
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      parameter( one = 1.0d0 )
      DOUBLE PRECISION   TWO
      parameter( two = 2.0d0 )
      DOUBLE PRECISION   ZERO
      parameter( zero = 0.0d0 )
      DOUBLE PRECISION   HALF
      parameter( half = 0.5d0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AB, ACMN, ACMX, ADF, DF, RT, SM, TB
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          abs, sqrt
*     ..
*     .. Executable Statements ..
*
*     Compute the eigenvalues
*
      sm = a + c
      df = a - c
      adf = abs( df )
      tb = b + b
      ab = abs( tb )
      IF( abs( a ).GT.abs( c ) ) THEN
         acmx = a
         acmn = c
      ELSE
         acmx = c
         acmn = a
      END IF
      IF( adf.GT.ab ) THEN
         rt = adf*sqrt( one+( ab / adf )**2 )
      ELSE IF( adf.LT.ab ) THEN
         rt = ab*sqrt( one+( adf / ab )**2 )
      ELSE
*
*        Includes case AB=ADF=0
*
         rt = ab*sqrt( two )
      END IF
      IF( sm.LT.zero ) THEN
         rt1 = half*( sm-rt )
*
*        Order of execution important.
*        To get fully accurate smaller eigenvalue,
*        next line needs to be executed in higher precision.
*
         rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
      ELSE IF( sm.GT.zero ) THEN
         rt1 = half*( sm+rt )
*
*        Order of execution important.
*        To get fully accurate smaller eigenvalue,
*        next line needs to be executed in higher precision.
*
         rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
      ELSE
*
*        Includes case RT1 = RT2 = 0
*
         rt1 = half*rt
         rt2 = -half*rt
      END IF
      RETURN
*
*     End of DLAE2
*
      END SUBROUTINE
