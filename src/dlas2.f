      subroutine dlas2( f, g, h, ssmin, ssmax)
*
*  -- LAPACK auxiliary routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   F, G, H, SSMAX, SSMIN
*     ..
*
*  ====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      parameter( zero = 0.0d0 )
      DOUBLE PRECISION   ONE
      parameter( one = 1.0d0 )
      DOUBLE PRECISION   TWO
      parameter( two = 2.0d0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AS, AT, AU, C, FA, FHMN, FHMX, GA, HA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          abs, max, min, sqrt
*     ..
*     .. Executable Statements ..
*
      fa = abs( f )
      ga = abs( g )
      ha = abs( h )
      fhmn = min( fa, ha )
      fhmx = max( fa, ha )
      IF( fhmn.EQ.zero ) THEN
         ssmin = zero
         IF( fhmx.EQ.zero ) THEN
            ssmax = ga
         ELSE
            ssmax = max( fhmx, ga )*sqrt( one+
     $              ( min( fhmx, ga ) / max( fhmx, ga ) )**2 )
         END IF
      ELSE
         IF( ga.LT.fhmx ) THEN
            as = one + fhmn / fhmx
            at = ( fhmx-fhmn ) / fhmx
            au = ( ga / fhmx )**2
            c = two / ( sqrt( as*as+au )+sqrt( at*at+au ) )
            ssmin = fhmn*c
            ssmax = fhmx / c
         ELSE
            au = fhmx / ga
            IF( au.EQ.zero ) THEN
*
*              Avoid possible harmful underflow if exponent range
*              asymmetric (true SSMIN may not underflow even if
*              AU underflows)
*
               ssmin = ( fhmn*fhmx ) / ga
               ssmax = ga
            ELSE
               as = one + fhmn / fhmx
               at = ( fhmx-fhmn ) / fhmx
               c = one / ( sqrt( one+( as*au )**2 )+
     $             sqrt( one+( at*au )**2 ) )
               ssmin = ( fhmn*c )*au
               ssmin = ssmin + ssmin
               ssmax = ga / ( c+c )
            END IF
         END IF
      END IF
      RETURN
*
*     End of DLAS2
*
      end subroutine
