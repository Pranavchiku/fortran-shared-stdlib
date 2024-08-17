      SUBROUTINE DLAUU2( UPLO, N, A, LDA, INFO )
*
*  -- LAPACK auxiliary routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      parameter( one = 1.0d+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I
      DOUBLE PRECISION   AII
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DDOT
      EXTERNAL           lsame, ddot
*     ..
*     .. External Subroutines ..
      EXTERNAL           dgemv, dscal, xerbla
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          max

*     Quick return if possible
*
      IF( n.EQ.0 )
     $   RETURN
*
      IF( UPLO .eq. 'U' ) THEN
*
*        Compute the product U * U**T.
*
         DO 10 i = 1, n
            aii = a( i, i )
            IF( i.LT.n ) THEN
               a( i, i ) = ddot( n-i+1, a( i, i ), lda, a( i, i ), lda )
               CALL dgemv( 'No transpose', i-1, n-i, one, a( 1, i+1 ),
     $                     lda, a( i, i+1 ), lda, aii, a( 1, i ), 1 )
            ELSE
               CALL dscal( i, aii, a( 1, i ), 1 )
            END IF
   10    CONTINUE
*
      ELSE
*
*        Compute the product L**T * L.
*
         DO 20 i = 1, n
            aii = a( i, i )
            IF( i.LT.n ) THEN
               a( i, i ) = ddot( n-i+1, a( i, i ), 1, a( i, i ), 1 )
               CALL dgemv( 'Transpose', n-i, i-1, one, a( i+1, 1 ), lda,
     $                     a( i+1, i ), 1, aii, a( i, 1 ), lda )
            ELSE
               CALL dscal( i, aii, a( i, 1 ), lda )
            END IF
   20    CONTINUE
      END IF
*
      RETURN
*
*     End of DLAUU2
*
      END
