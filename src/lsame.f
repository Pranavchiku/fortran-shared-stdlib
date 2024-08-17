      LOGICAL FUNCTION LSAME( CA, CB )
*
*  -- LAPACK auxiliary routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
CHARACTER          CA, CB
*     ..
*
* =====================================================================
*
*     .. Intrinsic Functions ..
      CHARACTER         CA, CB
      INTRINSIC          ichar
*     ..
*     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
*     ..
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      lsame = ca.EQ.cb
      IF( lsame )
     $   RETURN
*
*     Now test for equivalence if both characters are alphabetic.
*
      zcode = ichar( 'Z' )
*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      inta = ichar( ca )
      intb = ichar( cb )
*
      IF( zcode.EQ.90 .OR. zcode.EQ.122 ) THEN
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or
*        upper case 'Z'.
*
         IF( inta.GE.97 .AND. inta.LE.122 ) inta = inta - 32
         IF( intb.GE.97 .AND. intb.LE.122 ) intb = intb - 32
*
      ELSE IF( zcode.EQ.233 .OR. zcode.EQ.169 ) THEN
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.
*
         IF( inta.GE.129 .AND. inta.LE.137 .OR.
     $       inta.GE.145 .AND. inta.LE.153 .OR.
     $       inta.GE.162 .AND. inta.LE.169 ) inta = inta + 64
         IF( intb.GE.129 .AND. intb.LE.137 .OR.
     $       intb.GE.145 .AND. intb.LE.153 .OR.
     $       intb.GE.162 .AND. intb.LE.169 ) intb = intb + 64
*
      ELSE IF( zcode.EQ.218 .OR. zcode.EQ.250 ) THEN
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
         IF( inta.GE.225 .AND. inta.LE.250 ) inta = inta - 32
         IF( intb.GE.225 .AND. intb.LE.250 ) intb = intb - 32
      END IF
      lsame = inta.EQ.intb
*
*     RETURN
*
*     End of LSAME
*
      END
