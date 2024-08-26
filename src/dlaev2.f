      subroutine dlaev2(a,b,c,rt1,rt2,cs1,sn1)
*
*  -- LAPACK auxiliary routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B, C, CS1, RT1, RT2, SN1
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
      INTEGER            SGN1, SGN2
      DOUBLE PRECISION   AB, ACMN, ACMX, ACS, ADF, CS, CT, DF, RT, SM,
     $                   TB, TN
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
         sgn1 = -1
*
*        Order of execution important.
*        To get fully accurate smaller eigenvalue,
*        next line needs to be executed in higher precision.
*
         rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
      ELSE IF( sm.GT.zero ) THEN
         rt1 = half*( sm+rt )
         sgn1 = 1
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
         sgn1 = 1
      END IF
*
*     Compute the eigenvector
*
      IF( df.GE.zero ) THEN
         cs = df + rt
         sgn2 = 1
      ELSE
         cs = df - rt
         sgn2 = -1
      END IF
      acs = abs( cs )
      IF( acs.GT.ab ) THEN
         ct = -tb / cs
         sn1 = one / sqrt( one+ct*ct )
         cs1 = ct*sn1
      ELSE
         IF( ab.EQ.zero ) THEN
            cs1 = one
            sn1 = zero
         ELSE
            tn = -cs / tb
            cs1 = one / sqrt( one+tn*tn )
            sn1 = tn*cs1
         END IF
      END IF
      IF( sgn1.EQ.sgn2 ) THEN
         tn = cs1
         cs1 = -sn1
         sn1 = tn
      END IF
      RETURN
*
*     End of DLAEV2
*
      END SUBROUTINE
