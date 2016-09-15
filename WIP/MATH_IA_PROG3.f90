
PROGRAM MAIN


    INTEGER NN(20),NNR(200,100), NRAB(3)              !Initilization of permanent variables
                                                      !and settings for any given problem
    DIMENSION X(20), Y(20), Y1(20), Y2(20), P(20)
    DIMENSION WA(200,100), WB(200,100)
    DIMENSION STR(108), STR1(20)
    DIMENSION AAIST(3), AAMIN(3), AAMAX(3), BBIST(3), BBMIN(3), BBMAX(3)


    JPRINT = 1                                        !Boolian to write variant calculation
                                                      !(1 print, 0 don't print)
    JDISTORT = 0                                      !If the error values may be distorted or not
                                                      !(distortion by the value of CDISTORT)
    CDISTORT = 1.0                                    !Distortion coeficient

    NDISTORT = 0                                      !Number of distorted values in any given variant

    NSTR = 108                                        !Length of array STR

    C = 0.005                                         !The quantitive multiplier for exaduration of error

    M = 200                                           !The number of sub-regions in the horrizoltal
    K = 100                                           !The number of sub-regions in the vertical

    N = 20                                            !Number of descrete data points

    X0 = 0.0                                          !The value of the first X value
    HX = 0.05                                         !The value of the horrizoltal step between the X values

    WRITE(*,*) ' '                                    !Write input variables to console
    WRITE(*,*) ' '
    WRITE(*,*) '****************************************'
    WRITE(*,*) '*  MATH IA PROGRAM 3 - SEPTEMBER 2016  *'
    WRITE(*,*) '*STRUCTURAL IDENTIFICATION OF FUNCTIONS*'
    WRITE(*,*) '*          MATH_IA_PROG3 .f90          *'
    WRITE(*,*) '****************************************'
    WRITE(*,*) ' '
    WRITE(*,*) ' '
    WRITE(*,*) '   INPUT VARIABLES*********************'
    WRITE(*,*) '    '
    WRITE(*,*) '     JPRINT         = ', JPRINT
    WRITE(*,*) '     C              = ', C
    WRITE(*,*) '     N              = ', N
    WRITE(*,*) '     M              = ', M
    WRITE(*,*) '     K              = ', K
    WRITE(*,*) '     X0             = ', X0
    WRITE(*,*) '     HX             = ', HX
    WRITE(*,*) ' '
    WRITE(*,*) '   ************************************'

!MAIN BLOCK 1 - START

    DO 10 I = 1,N                                     !Generating X values with the valid step
    X(I) = X0 + (I-1) * HX
10  CONTINUE

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

    DATA AAIST / 2.0, 2.0, 2.0 /
    DATA AAMIN / 1.0, 1.0, 1.0 /
    DATA AAMAX / 3.0, 3.0, 3.0 /
    DATA BBIST / 0.5, 0.5, 0.5 /
    DATA BBMIN / 0.0, 0.0, 0.0 /
    DATA BBMAX / 1.0, 1.0, 1.0 /

    IVAR = 0
500 CONTINUE
    IVAR = IVAR + 1
    IF(IVAR.GT.3) GOTO 600

    WRITE(*,*)' '
    WRITE(*,*)'         START VARIANT ',IVAR

    DO 13  I = 1,N                                    !Generate true values for data points (without)
    Y(I) = AAIST(1) + BBIST(1) * X(I)
13  CONTINUE

    DO 2 I = 1,N
    STR1(I) = STR(I)
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

    DO 11 I = 1,M                                     !Generating arrays WA and WB
    DO 12 J = 1,K
    WA(I,J) = AAMIN(IVAR) + (I-1) * (AAMAX(IVAR) - AAMIN(IVAR)) / (M-1)
    WB(I,J) = BBMIN(IVAR) + (J-1) * (BBMAX(IVAR) - BBMIN(IVAR)) / (K-1)
12  CONTINUE
11  CONTINUE

    DO 30 I = 1,N                                     !Nullifying array NN
    NN(I) = 0
30  CONTINUE

    DO 20 I = 1,M                                     !Loops to find the number of swithing of the sign
    DO 21 J = 1,K                                     !from negative to positive and vise versa
    A = WA(I,J)                                       !in order to find the probability of cantidates for the values
    B = WB(I,J)                                       !of function parameters 'a' and 'b'
    DO 22 II = 1,N
    IF (IVAR.EQ.1) Y2(II) = A + B *     X(II)
    IF (IVAR.EQ.2) Y2(II) = A + B *    (X(II)**2)
    IF (IVAR.EQ.3) Y2(II) = A + B * EXP(X(II))
22  CONTINUE

    NR = 0
    DO 23 I1 = 1,(N-1)
    IF((Y2(I1+1)-Y1(I1+1)) * (Y2(I1)-Y1(I1)).LT.0.0) NR = NR + 1
23  CONTINUE
    NN(NR + 1) = NN(NR+1)+1
    NNR(I,J) = NR
21  CONTINUE
20  CONTINUE

  DO 999 I = 1,N
    WRITE(*,*) NN(I)
    999 CONTINUE

    DO 31 I = 1,N                                    !Calculate these propabilities acodring to values found
    S = 1.0                                          !using P(I) = (J cRn N-1) / 2^(N-1)
    DO 32 J = 1,(N-1)
    IF(J.LT.(N-I+1)) GOTO 33
    S = S * J
33  CONTINUE
32  CONTINUE
    DO 34 J = 1,I - 1
    S = S/J
34  CONTINUE
    P(I) = S / 2**(N-1)
31  CONTINUE

     SP = 0.0                                         !Recalculate apriori probabilities to a posteriori ones
     DO 41 J = 1,N
     IF(NN(J).NE.0) SP = SP + P(J)
41   CONTINUE

     DO 42 I = 1,N                                    !Excluding propabilites which did not give one single
     IF(NN(I).NE.0) P(I) = P(I)/SP                    !change of sign
     IF(NN(I).EQ.0) P(I) = 0.0
42   CONTINUE

!MAIN BLOCK 1 - END
!MAIN BLOCK 2 - START

     ERWOPT  = 900000000.1                            !Set the value for expectancy of error to be very large

     DO 51 I = 1,M                                    !Loops calculate true expectancy of error and prodicted
     DO 52 J = 1,K                                    !by the program expectancy
     A = WA(I,J)
     B = WB(I,J)

     ERW = 0.0
     DO 53 I1 = 1,M
     DO 54 J1 = 1,K
     A0 = WA(I1,J1)
     B0 = WB(I1,J1)

     D = 0.0
     DO 55 I2 = 1,N
     IF(IVAR.EQ.1) D = D + ((A + B * X(I2)) - (A0 + B0 * X(I2)))**2
     IF(IVAR.EQ.2) D = D + ((A + B * X(I2)**2) - (A0 + B0 * X(I2)**2))**2
     IF(IVAR.EQ.3) D = D + ((A + B * EXP(X(I2))) - (A0 + B0 * EXP(X(I2))))**2
55   CONTINUE

     D = D / N
     D = SQRT(D)

     NR = NNR(I1,J1)
     PR = P(NR+1)

     DR = (D * PR)/(NN(NR+1))

     ERW = ERW + DR
54   CONTINUE
53   CONTINUE

     IF(ERW.GT.ERWOPT) GOTO 100

     ERWOPT = ERW
     AOPT = A
     BOPT = B

100   CONTINUE

52    CONTINUE
51    CONTINUE

     DO 56 I = 1,N
     IF(IVAR.EQ.1) DIST = DIST + ((AOPT + BOPT * X(I)) - (AAIST(1) + BBIST(1) * X(I)))**2
     IF(IVAR.EQ.2) DIST = DIST + ((AOPT + BOPT * X(I)**2) - (AAIST(1) + BBIST(1) * X(I)**2))**2
     IF(IVAR.EQ.3) DIST = DIST + ((AOPT + BOPT * EXP(X(I))) - (AAIST(1) + BBIST(1) * EXP(X(I))))**2
56   CONTINUE

     DIST = DIST / N
     DIST = SQRT(DIST)

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
      WRITE(*,*) '     AOPT          = ', AOPT
      WRITE(*,*) '     BOPT          = ', BOPT
      WRITE(*,*) '     ERWOPT        = ', ERWOPT
      WRITE(*,*) '     DIST          = ', DIST
      WRITE(*,*) ' '
      WRITE(*,*) '   ************************************'
      WRITE(*,*) ' '

750   CONTINUE
      GOTO 500
600   CONTINUE


!MAIN BLOCK 2 - END


!WRITE(*,*) '     AIST           = ', AIST
!WRITE(*,*) '     BIST           = ', BIST
!WRITE(*,*) '     AMIN           = ', AMIN
!WRITE(*,*) '     AMAX           = ', AMAX
!WRITE(*,*) '     BMIN           = ', BMIN
!WRITE(*,*) '     BMAX           = ', BMAX

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
