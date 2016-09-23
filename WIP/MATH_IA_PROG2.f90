
PROGRAM MAIN


    INTEGER NN(40),NNR(100,100,100,100), NRAB(3)      !Initilization of permanent variables
                                                      !and settings for any given problem
    DIMENSION WC1(100,100,100,100)
    DIMENSION WC2(100,100,100,100)
    DIMENSION WC3(100,100,100,100)
    DIMENSION WC4(100,100,100,100)

    DIMENSION Y(40), STR(108), STR1(40), Y1(40), Y2(40), P(40), X(40)

    JPRINT   = 1                                      !Boolian to write variant calculation
                                                      !(1 print, 0 don't print)
    JDISTORT = 0                                      !Boolian to enable error distortion
                                                      !(distortion by the value of CDISTORT)
    CDISTORT = 20.0                                    !Distortion coeficient

    NDISTORT = 3                                      !Number of distorted values in any given variant

    NVAR = 2                                          !Number of variants of the calculation
    NSTR = 108                                        !Length of array STR

    C1MIN = 0.5                                       !The minimum value for function parameter 'C1'
    C1MAX = 1.5                                       !The maximum value for function parameter 'C1'
    C2MIN = -0.5                                      !The minimum value for function parameter 'C2'
    C2MAX = 0.5                                       !The maximum value for function parameter 'C2'
    C3MIN = -3.5                                      !The minimum value for function parameter 'C3'
    C3MAX = -2.5                                      !The maximum value for function parameter 'C3'
    C4MIN = 1.5                                       !The minimum value for function parameter 'C4'
    C4MAX = 2.5                                       !The maximum value for function parameter 'C4'

    C1IST = 1.0                                       !The true value for function parameter 'C1'
    C2IST = 0.0                                       !The true value for function parameter 'C2'
    C3IST = -3.0                                      !The true value for function parameter 'C3'
    C4IST = 2.0                                       !The true value for function parameter 'C4'

    C = 0.5                                           !The quantitive multiplier for exaduration of error

    N = 40                                            !Number of descrete data points100

    M1 = 100                                          !The number of sub-regions in the horrizoltal for all 'M'
    M2 = 100
    M3 = 100
    M4 = 100
    M5 = 100

    X0 = -0.5                                         !The value of the first X value
    HX = 0.05                                         !The value of the horrizoltal step between the X values


    WRITE(*,*) ' '                                    !Write input variables to console
    WRITE(*,*) ' '
    WRITE(*,*) '****************************************'
    WRITE(*,*) '*  MATH IA PROGRAM 2 - SEPTEMBER 2016  *'
    WRITE(*,*) '*     FOUR PARAMETER APPROXIMATION     *'
    WRITE(*,*) '*          MATH_IA_PROG2 .f90          *'
    WRITE(*,*) '****************************************'
    WRITE(*,*) ' '
    WRITE(*,*) ' '
    WRITE(*,*) '   INPUT VARIABLES*********************'
    WRITE(*,*) '    '
    WRITE(*,*) '     JPRINT         = ', JPRINT
    WRITE(*,*) '     JDISTORT       = ', JDISTORT
    WRITE(*,*) ' '
    WRITE(*,*) '     NVAR           = ', NVAR
    WRITE(*,*) ' '
    WRITE(*,*) '     C1IST          = ', C1IST
    WRITE(*,*) '     C2IST          = ', C2IST
    WRITE(*,*) '     C3IST          = ', C3IST
    WRITE(*,*) '     C4IST          = ', C4IST
    WRITE(*,*) ' '
    WRITE(*,*) '     C              = ', C
    WRITE(*,*) '     N              = ', N
    WRITE(*,*) ' '
    WRITE(*,*) '     M1             = ', M1
    WRITE(*,*) '     M2             = ', M2
    WRITE(*,*) '     M3             = ', M3
    WRITE(*,*) '     M4             = ', M4
    WRITE(*,*) ' '
    WRITE(*,*) '     C1MIN          = ', C1MIN
    WRITE(*,*) '     C1MAX          = ', C1MAX
    WRITE(*,*) '     C2MIN          = ', C2MIN
    WRITE(*,*) '     C2MAX          = ', C2MAX
    WRITE(*,*) '     C3MIN          = ', C3MIN
    WRITE(*,*) '     C3MAX          = ', C3MAX
    WRITE(*,*) '     C4MIN          = ', C4MIN
    WRITE(*,*) '     C4MAX          = ', C4MAX
    WRITE(*,*) ' '
    WRITE(*,*) '     X0             = ', X0
    WRITE(*,*) '     HX             = ', HX
    WRITE(*,*) ' '
    WRITE(*,*) '   ************************************'
    WRITE(*,*) ' '
    WRITE(*,*) '   PROGRAM START'

    AVGC1OPT = 0.0
    AVGC2OPT = 0.0
    AVGC3OPT = 0.0
    AVGC4OPT = 0.0

    AVGDIST  = 0.0
    AVGERW   = 0.0

!MAIN BLOCK 1 - START

    I = N/2
    I=2*I
    IF(I.NE.N) GOTO 1000

    DO 10 I = 1,N                                     !Generating X values with the valid step
    X(I) = X0 + (I-1) * HX
10  CONTINUE

    !WRITE(*,*) '   CREATED ARRAY X(I)'
    DO 11 I = 1,M1                                    !Generating arrays WA and WB
    DO 12 J = 1,M2
    DO 13 K = 1,M3
    DO 14 L = 1,M4

    WC1(I,J,K,L) = C1MIN + (I-1) * (C1MAX - C1MIN) / (M1-1)
    WC2(I,J,K,L) = C2MIN + (J-1) * (C2MAX - C2MIN) / (M2-1)
    WC3(I,J,K,L) = C3MIN + (K-1) * (C3MAX - C3MIN) / (M3-1)
    WC4(I,J,K,L) = C4MIN + (L-1) * (C4MAX - C4MIN) / (M4-1)

14  CONTINUE
13  CONTINUE
12  CONTINUE
11  CONTINUE

    !WRITE(*,*) '   CREATED ARRAYS WC1-WC4'

    DO 15  I = 1,N                                    !Generate true values for data points (without)
    Y(I) = C1IST + C2IST * X(I) + C3IST * (X(I))**2 + C4IST * (X(I))**3
15  CONTINUE

    !WRITE(*,*) '   CREATED Y(I)'

    DATA STR / 0.978,  -0.52 ,  -0.368,   1.69 ,  &   !Giving random values. Temperary solution for
              -1.48 ,   0.985,   1.475,  -0.098,  &   !random number generating, based on the normal law
              -1.633,   2.399,   0.261,  -1.883,  &
              -0.181,   1.675,  -0.324,  -1.029,  &
              -0.185,   0.004,  -0.101,  -1.187,  &
              -0.007,   1.27 ,   0.568,  -1.27 ,  &
               1.405,  -0.111,   2.072,   1.686,  &
               0.728,  -0.417,   0.794,   1.731,  &
               0.04 ,  -0.536,  -0.976,   2.192,  &
               1.609,  -0.19 ,  -0.279,  -1.611,  &
              -1.235,  -1.168,   0.325,   1.421,  &
               2.652,   0.489,  -1.253,   0.27 ,  &
              -1.103,   0.118,  -0.258,   0.638,  &
               0.741,   2.309,  -0.161,   0.679,  &
              -0.157,   0.336,   0.37 ,  -2.277,  &
               0.243,   0.629,  -1.516,   1.973,  &
               0.693,   1.71 ,   0.8  ,  -0.265,  &
               1.218,   0.655,  -0.292,  -1.455,  &
              -1.451,   1.492,  -0.713,   0.821,  &
              -0.031,  -0.078,   1.33 ,   0.977,  &
               0.183,   1.6  ,   0.335,   1.553,  &
               0.889,   0.896,  -0.035,   0.461,  &
               0.486,   1.246,   0.431,  -2.3  ,  &
              -1.081,  -1.37 ,   2.943,   0.653,  &
              -2.523,   0.756,   0.886,  -0.983,  &
               0.70 ,   0.06 ,   0.18 ,   0.68 ,  &
              -1.80 ,  -0.88 ,   0.47 ,  -0.51    &
               /

    IVAR = 0
500 CONTINUE
    IVAR = IVAR + 1
    IF(IVAR.GT.NVAR) GOTO 600
    IF(IVAR*N.LE.NSTR) GOTO 1
    WRITE(*,*)'ERROR 1: THE ARRAY "STR" DOES NOT CONTAIN ENOUGH VALUES.'
    GOTO 700
1   CONTINUE

    WRITE(*,*)' '
    WRITE(*,*)'         START VARIANT ',IVAR

    DO 2 I = 1,N
    STR1(I) = STR(I + (IVAR - 1) * N)
2   CONTINUE

    IF (JDISTORT.EQ.0) GOTO 550
    WRITE(*,*) ''
    WRITE(*,*) '         DISTORTED VALUES:'

    DO 3 I = 1,NDISTORT
    S = -900000000.1

    DO 4 J = 1,N
    IF(STR1(J).LT.S)  GOTO 7

    DO 5 J1 = 1,(I-1)
    IF(J.EQ.NRAB(J1)) GOTO 7

5   CONTINUE
    S = STR1(J)
    NUM = J

7   CONTINUE
4   CONTINUE
    NRAB(I) = NUM
3   CONTINUE

    DO 8 I = 1,NDISTORT
    STR1(NRAB(I)) = STR1(NRAB(I)) * CDISTORT
8   CONTINUE

    DO 9 I = 1,NDISTORT
    WRITE(*,*),'                           ',NRAB(I)
9   CONTINUE
    WRITE(*,*) ''

550 CONTINUE

    S = 0.0
    DO 16 I = 1,N                                     !Generating values with error for given data points
    S = S + (STR1(I)*C)**2
    Y1(I) = Y(I) + STR1(I) * C
16  CONTINUE
    S = S/N
    S = SQRT(S)
    WRITE(*,*)'True average-quadratic value of error:'
    WRITE(*,*)S
    WRITE(*,*)' '

    DO 30 I = 1,N                                     !Nullifying array NN
    NN(I) = 0
30  CONTINUE

    DO 20 I = 1,M1                                     !Loops to find the number of swithing of the sign
    DO 21 J = 1,M2                                     !from negative to positive and vise versa
    DO 22 K = 1,M3
    DO 23 L = 1,M4
    C1 = WC1(I,J,K,L)                                       !in order to find the probability of cantidates for the values
    C2 = WC2(I,J,K,L)
    C3 = WC3(I,J,K,L)
    C4 = WC4(I,J,K,L)                                       !of function parameters 'a' and 'b'
    DO 24 II = 1,N
    Y2(II) = C1 + C2 * X(II) + C3 * (X(II))**2 + C4 * (X(II))**3
24  CONTINUE

    NR = 0
    DO 25 I1 = 1,(N-1)
    IF((Y2(I1+1)-Y1(I1+1)) * (Y2(I1)-Y1(I1)).LT.0.0) NR = NR + 1
25  CONTINUE
    NN(NR + 1) = NN(NR+1)+1
    NNR(I,J,K,L) = NR
23  CONTINUE
22  CONTINUE
21  CONTINUE
20  CONTINUE

    WRITE(*,*) 'NN'
    DO 333 I = 1,N
    WRITE(*,*) NN(I)
333 CONTINUE

    DO 31 I = 1,N                                    !Calculate these propabilities acodring to values found
    IF(I.LE.(N/2)) GOTO 35                           !using P(I) = (J cRn N-1) / 2^(N-1)
    IF(I.GT.(N/2)) P(I) = P(N - I + 1)
    GOTO 31
35  CONTINUE
    S = 1.0
    DO 32 J = 1,(N-1)
    IF(J.LT.(N-I+1)) GOTO 33
    S = S * J
      !WRITE(*,*) 'S = S * J', S
33  CONTINUE
32  CONTINUE
    DO 34 J = 1,I - 1
    S = S/J
      !WRITE(*,*) 'S = S / J', S
34  CONTINUE
    DO 47 I2 = 1,(N-1)
    S = S / 2.0
    !  WRITE(*,*) 'S = S / 2.0', S
47  CONTINUE
    P(I) = S
31  CONTINUE
!END - ARRAY P() IS VALID

     SP = 0.0                                         !Recalculate apriori probabilities to a posteriori ones
     DO 41 J = 1,N
     IF(NN(J).NE.0) SP = SP + P(J)
41   CONTINUE

!WRITE(*,*) SP

     DO 42 I = 1,N                                    !Excluding propabilites which did not give one single
     IF(NN(I).NE.0) P(I) = P(I)/SP                    !change of sign
     IF(NN(I).EQ.0) P(I) = 0.0
42   CONTINUE

!     DO 43 I = 1,N
!       S = P(I)
!     WRITE (*,*) S
!43   CONTINUE

!MAIN BLOCK 1 - END
!MAIN BLOCK 2 - START

     C1OPT = 0.0
     C2OPT = 0.0
     C3OPT = 0.0
     C4OPT = 0.0

     DO 81 I = 1,M1
     DO 82 J = 1,M2
     DO 83 K = 1,M3
     DO 84 L = 1,M4

     C1 = WC1(I,J,K,L)
     C2 = WC2(I,J,K,L)
     C3 = WC3(I,J,K,L)
     C4 = WC4(I,J,K,L)
     NR = NNR(I,J,K,L)
     PR = P(NR+1)/(NN(NR+1))
     C1OPT = C1OPT + C1*PR
     C2OPT = C2OPT + C2*PR
     C3OPT = C3OPT + C3*PR
     C4OPT = C4OPT + C4*PR

84   CONTINUE
83   CONTINUE
82   CONTINUE
81   CONTINUE

     DIST = 0.0
     DO 56 I = 1,N
     DIST = ((C1OPT + C2OPT * X(I) + C3OPT * (X(I)**2) + C4OPT * (X(I))**3) - Y(I))**2
     56 CONTINUE
     DIST = DIST / N
     DIST = SQRT(DIST)


     ERWOPT = 0.0

     DO 181 I = 1,M1
     DO 182 J = 1,M2
     DO 183 K = 1,M3
     DO 184 L = 1,M4

     C1 = WC1(I,J,K,L)
     C2 = WC2(I,J,K,L)
     C3 = WC3(I,J,K,L)
     C4 = WC4(I,J,K,L)
     NR = NNR(I,J,K,L)
     PR = P(NR+1)/(NN(NR+1))
     D = 0.0
     DO 77 I5 = 1,N
     D = D + (((C1OPT + C2OPT * X(I5) + C3OPT * (X(I5))**2 + C4OPT * (X(I5))**3)) &
     - ((C1 + C2 * X(I5) + C3 * (X(I5))**2 + C4 * (X(I5))**3)))**2
     77 CONTINUE
     ERWOPT = ERWOPT + PR * D


184  CONTINUE
183  CONTINUE
182  CONTINUE
181  CONTINUE

ERWOPT = ERWOPT / N
ERWOPT = SQRT(ERWOPT)

    !WRITE(*,*) '   CALCULATED OPTIMAL VALUES'

!CALCULATING AVEGARE VALUES FOR OUTPUT - START

     AVGC1OPT = AVGC1OPT + C1OPT  /NVAR
     AVGC2OPT = AVGC2OPT + C2OPT  /NVAR
     AVGC3OPT = AVGC3OPT + C3OPT  /NVAR
     AVGC4OPT = AVGC4OPT + C4OPT  /NVAR
     AVGDIST  = AVGDIST  + DIST   /NVAR
     AVGERW   = AVGERW   + ERWOPT /NVAR

!CALCULATING AVEGARE VALUES FOR OUTPUT - END

!

    WRITE(*,*)'         DONE          ',IVAR
    WRITE(*,*)' '

    IF(JPRINT.NE.1) GOTO 750

    WRITE(*,*) '   ************************************'
    WRITE(*,*) '         VARIANT #',IVAR
    WRITE(*,*) '   ************************************'
    WRITE(*,*) ' '
    WRITE(*,*) '   OUTPUT VARIABLES********************'
    WRITE(*,*) ' '
    WRITE(*,*) '     PROGRAM METHOD:'
    WRITE(*,*) ' '
    WRITE(*,*) '     C1OPT          = ', C1OPT
    WRITE(*,*) '     C2OPT          = ', C2OPT
    WRITE(*,*) '     C3OPT          = ', C3OPT
    WRITE(*,*) '     C4OPT          = ', C4OPT
    WRITE(*,*) ' '
    WRITE(*,*) '     DIST           = ', DIST
    WRITE(*,*) '     ERWOPT         = ', ERWOPT
    WRITE(*,*) ' '
    WRITE(*,*) '   ************************************'
    WRITE(*,*) ' '

750 CONTINUE
    GOTO 500
600 CONTINUE

    WRITE(*,*) '   AVERAGE VALUES**********************'
    WRITE(*,*) ' '
    WRITE(*,*) '     AVGERW         = ', AVGERW
    WRITE(*,*) '     AVGDIST        = ', AVGDIST
    WRITE(*,*) ' '
    WRITE(*,*) '   ************************************'
    WRITE(*,*) ' '
    WRITE(*,*) '   PROGRAM END'
    WRITE(*,*) ' '


!MAIN BLOCK 2 - END

700   CONTINUE

1000 CONTINUE
stop "N is not an even number, please re-enter N"

END PROGRAM MAIN
