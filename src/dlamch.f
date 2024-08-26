      function dlamch( cmach )
*
*  -- LAPACK auxiliary routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      CHARACTER          CMACH
*     ..
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      parameter( one = 1.0d+0, zero = 0.0d+0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   RND, EPS, SFMIN, SMALL, RMACH
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           lsame
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          digits, epsilon, huge, maxexponent,
     $                   minexponent, radix, tiny
*     ..
*     .. Executable Statements ..
*
*
*     Assume rounding, not chopping. Always.
*
      rnd = one
*
      IF( one.EQ.rnd ) THEN
         eps = epsilon(zero) * 0.5
      ELSE
         eps = epsilon(zero)
      END IF
*
      IF( lsame( cmach, 'E' ) ) THEN
         rmach = eps
      ELSE IF( lsame( cmach, 'S' ) ) THEN
         sfmin = tiny(zero)
         small = one / huge(zero)
         IF( small.GE.sfmin ) THEN
*
*           Use SMALL plus a bit, to avoid the possibility of rounding
*           causing overflow when computing  1/sfmin.
*
            sfmin = small*( one+eps )
         END IF
         rmach = sfmin
      ELSE IF( lsame( cmach, 'B' ) ) THEN
         rmach = radix(zero)
      ELSE IF( lsame( cmach, 'P' ) ) THEN
         rmach = eps * radix(zero)
      ELSE IF( lsame( cmach, 'N' ) ) THEN
         rmach = digits(zero)
      ELSE IF( lsame( cmach, 'R' ) ) THEN
         rmach = rnd
      ELSE IF( lsame( cmach, 'M' ) ) THEN
         rmach = minexponent(zero)
      ELSE IF( lsame( cmach, 'U' ) ) THEN
         rmach = tiny(zero)
      ELSE IF( lsame( cmach, 'L' ) ) THEN
         rmach = maxexponent(zero)
      ELSE IF( lsame( cmach, 'O' ) ) THEN
         rmach = huge(zero)
      ELSE
         rmach = zero
      END IF
*
      dlamch = rmach
      RETURN
*
*     End of DLAMCH
*
      end function
