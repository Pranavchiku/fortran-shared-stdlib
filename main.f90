PROGRAM MAIN
        double precision :: A, B, C, ssmin, ssmax
        A = 1.0
        B = 2.0
        C = 3.0
        ssmin = 0.0
        ssmax = 0.0
        CALL DLAS2(A, B, C, ssmin, ssmax)
        print *, "ssmin = ", ssmin
        print *, "ssmax = ", ssmax
END PROGRAM
