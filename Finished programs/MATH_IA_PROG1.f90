
PROGRAM MAIN


    INTEGER NN(20),NNR(200,100), NRAB(3)              !Initilization of permanent variables
                                                      !and settings for any given problem
    DIMENSION X(20), WA(200,100), WB(200,100), Y(20), STR(108), STR1(20), Y1(20), Y2(20), P(20)


    JPRINT = 1                                        !Boolian to write variant calculation
                                                      !(1 print, 0 don't print)
    JDISTORT = 1                                      !If the error values may be distorted or not
                                                      !(distortion by the value of CDISTORT)
    CDISTORT = 5.0                                    !Distortion coeficient

    NDISTORT = 3                                      !Number of distorted values in any given variant

    NVAR = 5                                          !Number of variants of the calculation
    NSTR = 108                                        !Length of array STR
    AMIN = 1.0                                        !The minimum value for function parameter 'a'
    AMAX = 3.0                                        !The maximum value for function parameter 'a'
    BMIN = 0.0                                        !The minimum value for function parameter 'b'
    BMAX = 1.0                                        !The maximum value for function parameter 'b'
    AIST = 2.0                                        !The true value for function parameter 'a'
    BIST = 0.5                                        !The true value for function parameter 'b'

    C = 0.5                                          !The quantitive multiplier for exaduration of error

    M = 200                                           !The number of sub-regions in the horrizoltal
    K = 100                                            !The number of sub-regions in the vertical

    N = 20                                            !Number of descrete data points

    X0 = 0.0                                          !The value of the first X value
    HX = 0.05                                         !The value of the horrizoltal step between the X values

    WRITE(*,*) ' '                                    !Write input variables to console
    WRITE(*,*) ' '
    WRITE(*,*) '****************************************'
    WRITE(*,*) '*  MATH IA PROGRAM 2 - SEPTEMBER 2016  *'
    WRITE(*,*) '*      TWO PARAMETER & COMPARISON      *'
    WRITE(*,*) '*          MATH_IA_PROG1 .f90          *'
    WRITE(*,*) '****************************************'
    WRITE(*,*) ' '
    WRITE(*,*) ' '
    WRITE(*,*) '   INPUT VARIABLES*********************'
    WRITE(*,*) '    '
    WRITE(*,*) '     JPRINT         = ', JPRINT
    WRITE(*,*) '     NVAR           = ', NVAR
    WRITE(*,*) '     AIST           = ', AIST
    WRITE(*,*) '     BIST           = ', BIST
    WRITE(*,*) '     C              = ', C
    WRITE(*,*) '     N              = ', N
    WRITE(*,*) '     M              = ', M
    WRITE(*,*) '     K              = ', K
    WRITE(*,*) '     AMIN           = ', AMIN
    WRITE(*,*) '     AMAX           = ', AMAX
    WRITE(*,*) '     BMIN           = ', BMIN
    WRITE(*,*) '     BMAX           = ', BMAX
    WRITE(*,*) '     X0             = ', X0
    WRITE(*,*) '     HX             = ', HX
    WRITE(*,*) ' '
    WRITE(*,*) '   ************************************'

    AVGAOPT1 = 0.0
    AVGBOPT1 = 0.0
    AVGERWOPT1 = 0.0
    AVGDIST1 = 0.0

    AVGAOPT2 = 0.0
    AVGBOPT2 = 0.0
    AVGERWOPT2 = 0.0
    AVGDIST2 = 0.0

    AVGDISTLSM1 = 0.0
    AVGDISTLSM2 = 0.0

    AVGDISTLMM1 = 0.0
    AVGDISTLMM2 = 0.0

!MAIN BLOCK 1 - START

    I = N/2
    I=2*I
    IF(I.NE.N) GOTO 1000

    DO 10 I = 1,N                                     !Generating X values with the valid step
    X(I) = X0 + (I-1) * HX
10  CONTINUE


    DO 11 I = 1,M                                     !Generating arrays WA and WB
    DO 12 J = 1,K

    WA(I,J) = AMIN + (I-1) * (AMAX - AMIN) / (M-1)
    WB(I,J) = BMIN + (J-1) * (BMAX - BMIN) / (K-1)

12  CONTINUE
11  CONTINUE

!     DO 13 I = 1,M
!     DO 14 J = 1,K
!         write (*,*) WA(i,j)
! 14  CONTINUE
! 13  CONTINUE



    DO 13  I = 1,N                                    !Generate true values for data points (without)
    Y(I) = AIST + BIST * X(I)
13  CONTINUE

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

    DO 14 I = 1,N                                     !Generating values with error for given data points
    Y1(I) = Y(I) + STR1(I) * C
14  CONTINUE

    DO 30 I = 1,N                                     !Nullifying array NN
    NN(I) = 0
30  CONTINUE

    DO 20 I = 1,M                                     !Loops to find the number of swithing of the sign
    DO 21 J = 1,K                                     !from negative to positive and vise versa
    A = WA(I,J)                                       !in order to find the probability of cantidates for the values
    B = WB(I,J)                                       !of function parameters 'a' and 'b'
    DO 22 II = 1,N
    Y2(II) = A+B*X(II)
22  CONTINUE

    NR = 0
    DO 23 I1 = 1,(N-1)
    IF((Y2(I1+1)-Y1(I1+1)) * (Y2(I1)-Y1(I1)).LT.0.0) NR = NR + 1
23  CONTINUE
    NN(NR + 1) = NN(NR+1)+1
    NNR(I,J) = NR
21  CONTINUE
20  CONTINUE


!     DO 24 I = 1,N
!         write (*,*) NN(I)
!24   CONTINUE

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

     ERWOPT1 = 900000000.1                            !Set the value for expectancy of error to be very large
     ERWOPT2 = 900000000.1

     DO 51 I = 1,M                                    !Loops calculate true expectancy of error and prodicted
     DO 52 J = 1,K                                    !by the program expectancy
     A = WA(I,J)
     B = WB(I,J)

     ERW1 = 0.0
     ERW2 = 0.0
     DO 53 I1 = 1,M
     DO 54 J1 = 1,K
     A0 = WA(I1,J1)
     B0 = WB(I1,J1)

      D1 = ABS(A-A0) + ABS(B-B0)
      D2 = (A - A0)**2 + (B - B0)**2
      NR = NNR(I1,J1)
      PR = P(NR+1)

      DR1 = (D1 * PR)/(NN(NR+1))
      DR2 = (D2 * PR)/(NN(NR+1))

      ERW1 = ERW1 + DR1
      ERW2 = ERW2 + DR2
54    CONTINUE
53    CONTINUE

      IF(ERW1.GT.ERWOPT1) GOTO 100

      ERWOPT1 = ERW1
      AOPT1 = A
      BOPT1 = B

      DIST1 = ABS(AOPT1 - AIST) + ABS(BOPT1 - BIST)

100   CONTINUE

      IF(ERW2.GT.ERWOPT2) GOTO 200

      ERWOPT2 = ERW2
      AOPT2 = A
      BOPT2 = B

      DIST2 = (AOPT2 - AIST)**2 + (BOPT2 - BIST)**2

200   CONTINUE

52    CONTINUE
51    CONTINUE


!      AOPT3 = 0.0    !UNSTABLE CALCULATIONS
!      BOPT3 = 0.0
!      DO 81 I = 1,M
!      DO 82 J = 1,K
!      A = WA(I,J)
!      B = WB(I,J)
!      NR = NNR(I,J)
!      PR = P(NR+1)/(NN(NR+1))
!      AOPT3 = AOPT3 + A*PR
!      BOPT3 = BOPT3 + B*PR
!      82 CONTINUE
!      81 CONTINUE
!      WRITE(*,*) 'TEXT1| AOPT3 = ', AOPT3
!      WRITE(*,*) 'TEXT1| BOPT3 = ', BOPT3

!METHOD OF LEAST SQUARES - START

      C11 = N                                         !Starting to calclate the same data using LSM
      C12 = 0.0
      C22 = 0.0
      R1  = 0.0
      R2  = 0.0

      DO 61 I = 1,N
      C12 = C12 + X(I)
      C22 = C22 + X(I) * X(I)
      R1  = R1 + Y1(I)
      R2  = R2 + X(I) * Y1(I)
61    CONTINUE

      C21 = C12
      F = (C12 * C21 - C11 * C22)
!      WRITE(*,*)'F = ', F

      AOPTLSM = (R2 * C12 - R1 * C22) / (C12 * C21 - C11 * C22)
      BOPTLSM = (R1 * C21 - R2 * C11) / (C12 * C21 - C11 * C22)

      DISTLSM1 = ABS(AOPTLSM - AIST) + ABS(BOPTLSM - BIST)
      DISTLSM2 = (AOPTLSM - AIST)**2 + (BOPTLSM - BIST)**2

!METHOD OF LEAST SQUARES - END
!METHOD OF LEAST MODULUS - START

      SMIN = 900000000.0

      DO 71 I = 1,M
      DO 72 J = 1,K
      A = WA(I,J)
      B = WB(I,J)
      S = 0.0

      DO 73 I1 = 1,N
      S = S + ABS(Y1(I1) - (A + B * X(I1)))
73    CONTINUE

      IF(S.GT.SMIN) GOTO 74
      AOPTLMM = A
      BOPTLMM = B
      SMIN = S

74    CONTINUE
72    CONTINUE
71    CONTINUE

      DISTLMM1 = ABS(AOPTLMM - AIST) + ABS(BOPTLMM - BIST)
      DISTLMM2 = (AOPTLMM - AIST)**2 + (BOPTLMM - BIST)**2


!METHOD OF LEAST MODULUS - END
!CALCULATING AVEGARE VALUES FOR OUTPUT - START

      AVGAOPT1 = AVGAOPT1 + AOPT1/NVAR
      AVGBOPT1 = AVGBOPT1 + BOPT1/NVAR
      AVGERWOPT1 = AVGEWROPT1 + ERWOPT1/NVAR
      AVGDIST1 = AVGDIST1 + DIST1/NVAR

      AVGAOPT2 = AVGAOPT2 + AOPT2/NVAR
      AVGBOPT2 = AVGBOPT2 + BOPT2/NVAR
      AVGERWOPT2 = AVGEWROPT2 + ERWOPT2/NVAR
      AVGDIST2 = AVGDIST2 + DIST2/NVAR

      AVGDISTLSM1 = AVGDISTLSM1 + DISTLSM1/NVAR
      AVGDISTLSM2 = AVGDISTLSM2 + DISTLSM2/NVAR
      AVGDISTLMM1 = AVGDISTLMM1 + DISTLMM1/NVAR
      AVGDISTLMM2 = AVGDISTLMM2 + DISTLMM2/NVAR


!CALCULATING AVEGARE VALUES FOR OUTPUT - END

!

      WRITE(*,*)'         DONE          ',IVAR
      WRITE(*,*)' '

      IF(JPRINT.NE.1) GOTO 750
      WRITE(*,*) ' '
      WRITE(*,*) '   ************************************'
      WRITE(*,*) '         VARIANT #',IVAR
      WRITE(*,*) '   ************************************'
      WRITE(*,*) '   OUTPUT VARIABLES********************'
      WRITE(*,*) ' '
      WRITE(*,*) '     PROGRAM METHOD:'
      WRITE(*,*) ' '
      WRITE(*,*) '     AOPT1          = ', AOPT1
      WRITE(*,*) '     BOPT1          = ', BOPT1
      WRITE(*,*) '     ERWOPT1        = ', ERWOPT1
      WRITE(*,*) '     DIST1          = ', DIST1
      WRITE(*,*) ' '
      WRITE(*,*) '     AOPT2          = ', AOPT2
      WRITE(*,*) '     BOPT2          = ', BOPT2
      WRITE(*,*) '     ERWOPT2        = ', ERWOPT2
      WRITE(*,*) '     DIST2          = ', DIST2
      WRITE(*,*) ' '
      WRITE(*,*) ' '
      WRITE(*,*) '     LEAST SQUARE METHOD:'
      WRITE(*,*) ' '
      WRITE(*,*) '     AOPTLSM        = ', AOPTLSM
      WRITE(*,*) '     BOPTLSM        = ', BOPTLSM
      WRITE(*,*) '     DISTLSM1       = ', DISTLSM1
      WRITE(*,*) '     DISTLSM2       = ', DISTLSM2
      WRITE(*,*) ' '
      WRITE(*,*) ' '
      WRITE(*,*) '     LEAST MODULUS METHOD:'
      WRITE(*,*) ' '
      WRITE(*,*) '     AOPTLMM        = ', AOPTLMM
      WRITE(*,*) '     BOPTLMM        = ', BOPTLMM
      WRITE(*,*) '     DISTLMM1       = ', DISTLMM1
      WRITE(*,*) '     DISTLMM2       = ', DISTLMM2
      WRITE(*,*) ' '
      WRITE(*,*) '   ************************************'
      WRITE(*,*) ' '

750   CONTINUE
      GOTO 500
600   CONTINUE

      WRITE(*,*) '   AVERAGE VALUES**********************'
      WRITE(*,*) ' '
      WRITE(*,*) '     AVGAOPT1       = ',AVGAOPT1
      WRITE(*,*) '     AVGBOPT1       = ',AVGBOPT1
      WRITE(*,*) '     AVGERWOPT1     = ',AVGERWOPT1
      WRITE(*,*) '     AVGDIST1       = ',AVGDIST1
      WRITE(*,*) ' '
      WRITE(*,*) '     AVGAOPT2       = ',AVGAOPT2
      WRITE(*,*) '     AVGBOPT2       = ',AVGBOPT2
      WRITE(*,*) '     AVGERWOPT2     = ',AVGERWOPT2
      WRITE(*,*) '     AVGDIST2       = ',AVGDIST2
      WRITE(*,*) ' '
      WRITE(*,*) '     AVGDISTLMM1    = ',AVGDISTLMM1
      WRITE(*,*) '     AVGDISTLMM2    = ',AVGDISTLMM2
      WRITE(*,*) ' '
      WRITE(*,*) '     AVGDISTLSM1    = ',AVGDISTLSM1
      WRITE(*,*) '     AVGDISTLSM2    = ',AVGDISTLSM2
      WRITE(*,*) ' '
      WRITE(*,*) '   ************************************'
      WRITE(*,*) ' '

!MAIN BLOCK 2 - END

700   CONTINUE


1000 CONTINUE
stop "N is not an even number, please re-enter N"

END PROGRAM MAIN

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
!                                   #@@@@@@@@%
!                                .@@@@@@@@@@@@@(
!                               %@@@@@@@@@@@@@@@/
!                              @@@@@@@@    ,@@@@@/((/,,,
!                             @@@@@@@%       @@@@@@@@@@@@@@@@@&.
!                            @@@@@@@@        ,@@@@@@@@@@@@@@@@@@@#
!     @@@,                  @@@@@@@@          @@@@@@@@@@@@@@@@@@@@@&
!     @@@@#               /@@@@@@@@         .@@@@@@@     .#@@@@@@@@@@/
!     %@@@@@,           %@@@@@@@@&         /@@@@@@@@@.        .@@@@@@@#
!      @@@@@@@@%####%@@@@@@@@@@@*        .@@@@#&@@@@@@,          &@@@@@(
!      &@@@@@@@@@@@@@@@@@@@@@@@.      .%@@@@@*  %@@@@@@(          /@@@@&.
!       &@@@@@@@@@@@@@@@@@@&.      .@@@@@@&       @@@@@@@(        @@@@@@#
!        (@@@@@@@@@@(.          ,@@@@@@@*          *@@@@@@@@*   .%@@@@@@@
!          #@@@@@@@@@@@@@@@@@@@@@@@@%                .&@@@@@@@@@@@@@@@@@@.
!            *@@@@@@@@@@@@@@@@@@@#                       /&@@@@@@@@@@@@@@,
!               #@@@@@@@@@@@@&/                               (@@@@@@@@@@.
!                   .,/@@@@                                    @@@@@@@@@(
!                      @@@@                                   .@@@@@@@@(
!                      @@@@                                   .@@@@@@@(
!                      @@@@                                   .@@@@@@&
!                      &@@@.                                   @@@@@@(
!                      /@@@,                                   &@@@@&
!                      ,@@@,                                   #@@@@
!                      .@@@@                                   #@@@
!                       @@@@                                   @@@%
!                       @@@@.                                  @@@%
!                       %@@@.                                 .@@@#
!                       %@@@.                                 .@@@(
!                       *@@@,                                 .@@@(
!                       ,@@@,                                 .@@@(
!                       ,@@@,                                 .@@@(
!                       ,@@@,                                 .@@@(
!                       ,@@@(                                 ,@@@
!                        @@@@                       ,,        ,@@@
!                        @@@@                   /@@@@@@@/     ,@@@
!           %@@@@@.      #@@@,                 ,@@@@&%@@@     (@@@
!           @@@@@@@@@.%  ,@@@,                  &@,     .     @@@%
!                %%(  ,( ,@@@,                .&.            /@@@.
!                        ,@@@,                               @@@%
!                        ,@@@,                     *&%      %@@@/
!                %@@%,   ,@@@,                    #@@@@*    /@@@@(
!               &@@@@@#  ,@@@#                    *@@@@(      #@@@
!               .@@@@@*  .@@@&             ...      .,        %@@@
!                  .      %@@@     (#  /&/.  /&(&*          #@@@@,
!                         /@@@.     ,@&%#&/(&&&%,*%         @@@%
!                          @@@%           .&.%,  (@.        @@@%
!                          /@@@(        ,%&&&&&*.##         @@@@
!                           /@@@@     /#                    @@@&
!                            .@@@@@%,.                     %@@@/
!                               &@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
!                                 .,(@@@@@@@@@@@@@@@@@@@@@@@&.
!                                       @@@%.@@@/  .....
!                                       @@@&.@@@/
!                             ,@@@@@@@@@@@@@*@@@
!                              @@@@@@@@@@@@@@@@@&/
!                                          .@@@@@@#
!
!------------------------------------------------------------------------------
!-----             CODE BY DAVID SIMON 'DAVZZAR' TETRUASHVILI             -----
!-----                          (C) 2016  BERLIN                          -----
!------------------------------------------------------------------------------
