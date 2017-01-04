
PROGRAM MAIN


    INTEGER NN(20),NNR(200,100), NRAB(3)              !Initilization of permanent variables
                                                      !and settings for any given problem
    DIMENSION X(20), WA(200,100), WB(200,100), Y(20), STR(1008), STR1(20), Y1(20), Y2(20), P(20)

    ITRASH = 0

    JPRINT = 1                                        !Boolian to write variant calculation
                                                      !(1 print, 0 don't print)
    JDISTORT = 0                                      !If the error values may be distorted or not
                                                      !(distortion by the value of CDISTORT)
    CDISTORT = 2.0                                    !Distortion coeficient

    NDISTORT = 2                                      !Number of distorted values in any given variant

    NVAR = 5                                          !Number of variants of the calculation
    NSTR = 1008                                       !Length of array STR
    AMIN = 1.0                                        !The minimum value for function parameter 'a'
    AMAX = 3.0                                        !The maximum value for function parameter 'a'
    BMIN = 0.0                                        !The minimum value for function parameter 'b'
    BMAX = 1.0                                        !The maximum value for function parameter 'b'
    AIST = 2.0                                        !The true value for function parameter 'a'
    BIST = 0.5                                        !The true value for function parameter 'b'

    C = 0.1                                           !The quantitive multiplier for exaduration of error

    M = 200                                           !The number of sub-regions in the horrizoltal
    K = 100                                           !The number of sub-regions in the vertical

    N = 20                                            !Number of descrete data points

    X0 = 0.0                                          !The value of the first X value
    HX = 0.05                                         !The value of the horrizoltal step between the X values

    WRITE(*,*) ' '                                    !Write input variables to console
    WRITE(*,*) ' '
    WRITE(*,*) '****************************************'
    WRITE(*,*) '*   MATH IA PROGRAM 1a - JANUARY 2017  *'
    WRITE(*,*) '*      TWO PARAMETER & COMPARISON      *'
    WRITE(*,*) '*          MATH_IA_PROG1 .f90          *'
    WRITE(*,*) '****************************************'
    WRITE(*,*) ' '
    WRITE(*,*) ' '
    WRITE(*,*) '   INPUT VARIABLES*********************'
    WRITE(*,*) '    '
    WRITE(*,*) '     JPRINT         = ', JPRINT
    WRITE(*,*) '     JDISTORT       = ', JDISTORT
    WRITE(*,*) '     ITRASH         = ', ITRASH
    IF(JDISTORT.EQ.1) WRITE(*,*) '     NDISTORT       = ', NDISTORT
    IF(JDISTORT.EQ.1) WRITE(*,*) '     CDISTORT       = ', CDISTORT
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

    AVGAOPT3 = 0.0
    AVGBOPT3 = 0.0
    AVGERWOPT3 = 0.0
    AVGDIST3 = 0.0

    AVGAOPTLSM = 0.0
    AVGBOPTLSM = 0.0
    AVGDISTLSM1 = 0.0
    AVGDISTLSM2 = 0.0

    AVGAOPTLMM = 0.0
    AVGBOPTLMM = 0.0
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
              -1.80 ,  -0.88 ,   0.47 ,  -0.51 ,  &
               0.478, -1.138, 1.247, 0.243, &
                0.892, -0.428, 0.960, 1.544, &
                0.147, 0.885, 0.904, -1.208, &
                0.944, 0.099, 0.340, -0.628, &
                0.655, 2.023, -1.875, 0.873, &
                -0.607, -0.310, 0.263, 1.204, &
                1.391, 0.476, 0.416, -2.088, &
                -1.062, -0.629, -0.604, -0.378, &
                0.093, -1.214, -0.311, -0.560, &
                -0.188, 0.622, -0.058, 0.089, &
                -0.681, -0.263, -0.632, 0.620, &
                0.876, -1.025, -0.540, -1.129, &
                1.764, 0.445, -1.325, 1.966, &
                1.407, -2.203, -0.425, 1.393, &
                -1.133, -0.086, 0.539, 0.103, &
                -0.590, -0.830, -1.007, 0.718, &
                0.345, -0.035, -0.280, 1.566, &
                0.190, 1.408, 0.831, -1.295, &
                -0.697, 0.474, -1.232, 0.161, &
                -1.460, 0.134, 0.363, -1.950, &
                0.100, -0.732, 1.935, 0.106, &
                -0.597, -0.428, -0.372, -0.351, &
                -0.042, -1.780, 0.550, 0.705, &
                -2.107, 1.396, 1.224, -2.036, &
                0.210, -0.606, -0.677, -2.099, &
                1.030, 1.156, 1.322, 1.768, &
                -0.990, -0.757, 0.029, -0.910, &
                -1.011, -0.533, 0.956, -1.658, &
                0.717, -0.134, -0.220, 1.304, &
                -2.289, -0.121, -1.561, 0.772, &
                -0.545, 0.073, 1.022, 1.398, &
                1.219, -0.736, 0.634, 0.228, &
                -0.822, -0.012, 0.947, 0.224, &
                0.575, 0.283, 0.824, 0.524, &
                0.130, -0.553, -0.080, -0.236, &
                -0.303, 0.316, -1.138, 0.395, &
                -0.889, -0.789, -1.901, -0.525, &
                0.428, 0.939, -1.088, -1.168, &
                -0.656, -1.325, 0.369, 0.012, &
                -1.177, 2.036, 0.257, -0.690, &
                -0.666, -0.633, 0.482, 0.299, &
                -0.697, 0.368, -1.097, 1.001, &
                -0.137, 1.802, 0.030, 1.166, &
                -1.264, -0.183, -0.591, -0.211, &
                0.809, 0.049, 0.513, 0.045, &
                -0.630, -0.264, 1.003, 1.309, &
                -0.763, 0.815, 0.314, -1.348, &
                0.574, 0.192, 0.110, 1.271, &
                -2.065, 0.853, -2.323, -0.945, &
                -1.417, -1.202, -0.662, -0.434, &
                -0.614, -0.761, 0.154, -0.237, &
                -0.137, 1.650, -1.917, -0.644, &
                0.225, -1.656, 0.731, 0.155, &
                0.478, -0.044, -0.810, 0.203, &
                1.035, 0.180, 0.566, 1.265, &
                -0.183, 1.354, -0.296, -1.143, &
                -1.891, -0.807, -1.233, -0.389, &
                -1.098, -0.220, -0.400, 0.780, &
                0.107, 0.830, 1.119, -0.376, &
                0.083, -1.453, 0.102, -0.817, &
                -0.026, -0.799, -0.625, 0.541, &
                -0.229, -0.834, -1.599, 0.418, &
                -1.235, -0.554, -1.256, 1.446, &
                0.892, 1.323, -0.320, -0.880, &
                -1.307, 0.017, -0.876, 0.333, &
                0.277, -1.048, 0.219, -0.147, &
                0.258, -0.554, -0.635, -0.345, &
                1.406, -0.273, -1.221, -0.070, &
                -0.082, 0.412, 1.169, 1.396, &
                0.320, 0.722, -1.873, 0.845, &
                -1.284, -0.402, 0.847, -0.142, &
                0.315, -1.245, 0.208, 0.147, &
                1.032, 1.342, -0.604, -0.482, &
                0.974, -0.006, -0.065, 1.856, &
                0.526, 0.116, -0.776, -0.223, &
                0.749, -0.752, 0.070, 0.569, &
                0.368, -0.524, 0.039, 0.439, &
                1.885, 1.385, 0.114, -0.712, &
                -0.641, -0.096, -1.104, -0.851, &
                0.307, 0.719, -0.266, -0.900, &
                1.264, -1.824, 1.004, -0.989, &
                -0.542, 0.627, -0.326, -1.949, &
                0.277, 0.520, -0.918, 0.301, &
                -1.022, -1.867, -0.156, 0.074, &
                -0.297, 0.229, 1.928, -0.816, &
                0.073, 0.707, 0.872, -0.658, &
                -0.503, -0.787, -0.643, -0.509, &
                -0.881, 0.816, 1.225, 0.927, &
                0.632, 0.385, 0.439, -0.369, &
                0.824, 0.092, -1.269, -1.401, &
                -1.298, 0.864, -1.502, 0.463, &
                0.216, 2.787, 0.562, -1.728, &
                -1.076, -1.165, 0.428, -1.421, &
                -0.248, -0.477, 0.068, -1.172, &
                1.280, 1.332, 0.236, -0.385, &
                1.696, -0.592, -1.495, 0.136, &
                -0.648, 2.008, -0.383, -0.671, &
                1.717, -0.255, -0.841, 1.959, &
                0.070, 0.618, -0.463, 0.309, &
                -0.184, 0.353, 0.577, 0.479, &
                0.636, -0.725, -0.179, 0.065, &
                -0.781, -0.341, -0.459, -0.111, &
                -1.164, -0.643, -0.535, 1.508, &
                0.498, -0.086, -0.425, -1.763, &
                -1.538, -0.336, 1.646, 0.441, &
                0.571, -0.208, -1.080, -1.503, &
                -0.056, 0.261, 0.469, 1.408, &
                1.616, -0.861, -0.290, -0.685, &
                0.416, -0.116, 0.923, 0.152, &
                -0.475, 0.150, -0.736, -1.401, &
                -1.110, -2.304, -0.405, -0.236, &
                0.934, 1.217, 0.521, 0.496, &
                -0.930, 0.266, -1.197, 1.402, &
                0.870, 0.089, 0.421, -0.719, &
                -1.325, -0.572, -3.590, -0.498, &
                0.416, 0.043, -1.640, 3.199, &
                -1.008, 0.843, 0.314, -3.442, &
                -0.341, 0.305, -1.071, 0.293, &
                -0.008, -0.256, 0.184, 0.344, &
                0.003, -0.089, -0.020, -0.727, &
                -0.987, -0.330, 0.006, -0.445, &
                0.157, 0.061, 1.041, 1.339, &
                0.876, -1.174, 0.508, -0.682, &
                -0.715, -0.384, 0.576, -0.722, &
                -0.613, -0.281, 0.719, 0.694, &
                0.120, -0.145, -1.775, 0.201, &
                -0.804, 0.233, -0.618, 1.738, &
                0.781, -0.350, -1.594, 0.829, &
                -0.754, -0.607, -0.135, 1.586, &
                0.485, -0.262, -0.219, 1.221, &
                -1.073, 0.206, -0.560, -0.153, &
                1.460, -0.137, -0.015, 0.433, &
                -0.172, -0.638, 1.145, -0.322, &
                0.551, 0.725, -1.013, -0.108, &
                -1.291, 1.940, 0.769, -0.099, &
                -1.165, 1.137, -0.708, -2.188, &
                0.991, 0.677, 1.444, 1.668, &
                -0.768, 1.308, 0.709, -1.058, &
                0.954, -0.862, -0.221, 0.393, &
                -0.562, 0.860, 2.622, -0.964, &
                1.533, 0.398, -0.814, -1.427, &
                -1.495, -0.408, 0.913, -1.459, &
                0.353, -0.000, 0.870, 0.477, &
                0.076, -0.280, -1.066, 0.322, &
                -0.605, 1.182, -0.531, 1.329, &
                -1.605, -0.551, -1.675, -1.360, &
                1.751, 0.121, 0.796, -0.290, &
                0.387, 0.403, -2.415, 0.266, &
                0.134, 1.296, -0.371, -1.595, &
                -0.843, 2.151, -0.239, 0.181, &
                0.824, -0.852, 0.197, 0.149, &
                0.392, 1.776, -1.220, -0.426, &
                -0.746, 0.114, -1.083, 1.642, &
                0.026, -1.072, 1.128, -0.726, &
                -0.078, -1.228, -0.926, 0.406, &
                -0.967, 0.396, 0.883, -0.382, &
                -0.809, 1.156, -0.467, -1.309, &
                -0.249, -0.586, -1.722, -1.291, &
                -0.291, -0.688, -0.061, -0.784, &
                0.507, 1.148, -0.162, 0.665, &
                -1.959, -1.169, 1.842, 0.984, &
                -0.181, -2.208, 0.514, 0.029, &
                2.866, -0.591, 0.956, -0.444, &
                -0.481, -1.669, -0.697, -1.163, &
                1.005, 1.464, 0.121, 0.736, &
                -0.010, 1.586, 0.770, 1.742, &
                0.723, -0.887, 0.616, -0.507, &
                0.389, 0.449, 0.107, 0.144, &
                1.481, -2.347, -0.705, -1.728, &
                0.586, -0.497, -0.529, -0.099, &
                2.041, -0.034, -0.820, 1.692, &
                -0.544, 0.313, -0.267, -0.909, &
                -0.568, 0.433, 0.504, 0.624, &
                0.317, -2.327, 0.786, 1.750, &
                1.824, -0.198, 1.517, 0.497, &
                -0.250, -0.454, -0.247, -1.292, &
                -0.579, 0.661, 0.543, 1.063, &
                1.023, 1.267, -0.193, 0.929, &
                1.513, 0.067, -0.655, -0.039, &
                -0.357, 0.183, -0.600, -0.109, &
                0.722, 0.808, -1.066, -0.343, &
                -0.265, 0.897, 0.074, -0.849, &
                -0.210, 1.083, -0.078, 1.469, &
                -1.077, 0.535, -0.225, 0.348, &
                0.181, -1.533, -0.805, -1.464, &
                -0.718, 0.288, 1.896, -1.022, &
                1.957, -0.289, -0.711, -0.658, &
                0.345, -0.315, -0.189, -1.238, &
                -1.021, -0.974, 0.318, -1.507, &
                1.241, -0.398, -0.327, -0.413, &
                1.200, 1.112, 0.063, 1.157, &
                1.122, -0.578, -0.783, -0.875, &
                1.212, -0.632, -1.733, 0.168, &
                0.567, 0.368, 1.105, -1.556, &
                -1.041, 1.366, 0.776, 2.145, &
                0.355, -0.674, 1.339, 0.667, &
                0.762, 0.156, 0.466, 1.265, &
                0.809, 0.782, -1.448, -0.125, &
                -0.457, -1.658, 0.317, -1.105, &
                -0.278, 0.097, 1.178, 0.513, &
                0.266, -0.377, -0.085, 0.080, &
                -0.446, 0.366, 0.691, -0.953, &
                0.062, -1.574, 0.251, -2.066, &
                -1.545, 0.902, 0.641, 0.581, &
                -0.061, -0.336, 0.224, 0.805, &
                0.397, 2.169, -1.073, -0.060, &
                -0.331, 0.581, -1.601, 0.854, &
                -0.845, -1.076, 1.208, -1.513, &
                0.623, 0.797, 0.268, 1.553, &
                -0.829, 0.237, -0.726, -0.414, &
                -1.709, -0.453, 0.950, 0.891, &
                0.163, -0.646, 0.968, -0.330, &
                1.527, -0.284, 0.435, 1.490, &
                -0.633, -0.399, -1.318, -0.369, &
                -0.592, 0.024, 0.643, -0.535, &
                -0.781, 0.735, 0.059, -0.683, &
                1.338, 1.084, 0.705, 0.665, &
                -0.668, -2.436, 0.469, 1.413, &
                -0.771, -1.158, -1.068, -1.124, &
                0.457, -0.200, -0.465, -1.113, &
                -0.216, -0.647, -1.976, 0.714, &
                1.172, 1.037, 0.768, 1.567, &
                -0.375, 0.652, -1.807, -1.508, &
                1.916, -0.242, 0.065, 0.354, &
                0.538, -0.291, -1.494, 2.480 &

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
33  CONTINUE
32  CONTINUE
    DO 34 J = 1,I - 1
    S = S/J
34  CONTINUE
    DO 47 I2 = 1,(N-1)
    S = S / 2.0
47  CONTINUE
    P(I) = S
31  CONTINUE

!WRITE(*,*)' '
!WRITE(*,*)'PROB APRIORI'
  S = 0.0
  DO 444 I = 1,N
  S = S + P(I)
  444   CONTINUE
  WRITE (*,*) S




     SP = 0.0                                         !Recalculate apriori probabilities to a posteriori ones
     DO 41 J = 1,N
     IF(NN(J).NE.0) SP = SP + P(J)
41   CONTINUE

WRITE(*,*)' '
WRITE(*,*)'SP:'
WRITE(*,*) SP
WRITE(*,*)' '

     DO 42 I = 1,N                                    !Excluding propabilites which did not give one single
     IF(NN(I).NE.0) P(I) = P(I)/SP                    !change of sign
     IF(NN(I).EQ.0) P(I) = 0.0
42   CONTINUE

!WRITE(*,*)'PROB APOSTERIORI'
!DO 43 I = 1,N
!  S = P(I)
!WRITE (*,*) S
!43   CONTINUE
!WRITE(*,*)' '

!MAIN BLOCK 1 - END
!MAIN BLOCK 2 - START

     ERWOPT1 = 900000000.1                            !Set the value for expectancy of error to be very large
     ERWOPT2 = 900000000.1
     ERWOPT3 = 900000000.1

     DO 51 I = 1,M                                    !Loops calculate true expectancy of error and prodicted
     DO 52 J = 1,K                                    !by the program expectancy
     A = WA(I,J)
     B = WB(I,J)

     ERW1 = 0.0
     ERW2 = 0.0
     ERW3 = 0.0
     DO 53 I1 = 1,M
     DO 54 J1 = 1,K
     A0 = WA(I1,J1)
     B0 = WB(I1,J1)

      D1 = ABS(A-A0) + ABS(B-B0)
      D2 = (A - A0)**2 + (B - B0)**2
      D3 = 0.0
      DO 58 I3 = 1,N
      S = (A+B * X(I3)) - (A0 + B0 * X(I3))
      S = ABS(S)
      D3 = D3 + S
      D3 = D3 / N
58 CONTINUE
      NR = NNR(I1,J1)
      IF(NR.LE.ITRASH) GOTO 54
      PR = P(NR+1)

      DR1 = (D1 * PR)/(NN(NR+1))
      DR2 = (D2 * PR)/(NN(NR+1))
      DR3 = (D3 * PR)/(NN(NR+1))

      ERW1 = ERW1 + DR1
      ERW2 = ERW2 + DR2
      ERW3 = ERW3 + DR3
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

      IF(ERW3.GT.ERWOPT3) GOTO 300
      ERWOPT3 = ERW3
      AOPT3 = A
      BOPT3 = B
      DIST3 = 0.0
      DO 59 I3 = 1,N
      S = (AOPT3 + BOPT3 * X(I3)) - (AIST + BIST * X(I3))
      S = ABS(S)
      DIST3 = DIST3 + S
      DIST3 = DIST3 / N
59 CONTINUE
300 CONTINUE

52    CONTINUE
51    CONTINUE

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
      AVGERWOPT1 = AVGERWOPT1 + ERWOPT1/NVAR
      AVGDIST1 = AVGDIST1 + DIST1/NVAR

      AVGAOPT2 = AVGAOPT2 + AOPT2/NVAR
      AVGBOPT2 = AVGBOPT2 + BOPT2/NVAR
      AVGERWOPT2 = AVGERWOPT2 + ERWOPT2/NVAR
      AVGDIST2 = AVGDIST2 + DIST2/NVAR

      AVGAOPT3 = AVGAOPT3 + AOPT3/NVAR
      AVGBOPT3 = AVGBOPT3 + BOPT3/NVAR
      AVGERWOPT3 = AVGERWOPT3 + ERWOPT3/NVAR
      AVGDIST3 = AVGDIST3 + DIST3/NVAR

      AVGDISTLSM1 = AVGDISTLSM1 + DISTLSM1/NVAR
      AVGDISTLSM2 = AVGDISTLSM2 + DISTLSM2/NVAR
      AVGAOPTLSM = AVGAOPTLSM + AOPTLSM/NVAR
      AVGBOPTLSM = AVGBOPTLSM + BOPTLSM/NVAR

      AVGDISTLMM1 = AVGDISTLMM1 + DISTLMM1/NVAR
      AVGDISTLMM2 = AVGDISTLMM2 + DISTLMM2/NVAR
      AVGAOPTLMM = AVGAOPTLMM + AOPTLMM/NVAR
      AVGBOPTLMM = AVGBOPTLMM + BOPTLMM/NVAR

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
      WRITE(*,*) '     AOPT3          = ', AOPT3
      WRITE(*,*) '     BOPT3          = ', BOPT3
      WRITE(*,*) '     ERWOPT3        = ', ERWOPT3
      WRITE(*,*) '     DIST3          = ', DIST3
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
      WRITE(*,*) '     AVGAOPT3       = ',AVGAOPT3
      WRITE(*,*) '     AVGBOPT3       = ',AVGBOPT3
      WRITE(*,*) '     AVGERWOPT3     = ',AVGERWOPT3
      WRITE(*,*) '     AVGDIST3       = ',AVGDIST3
      WRITE(*,*) ' '
      WRITE(*,*) '     AVGDISTLMM1    = ',AVGDISTLMM1
      WRITE(*,*) '     AVGDISTLMM2    = ',AVGDISTLMM2
      WRITE(*,*) '     AVGAOPTLMM     = ',AVGAOPTLMM
      WRITE(*,*) '     AVGBOPTLMM     = ',AVGBOPTLMM
      WRITE(*,*) ' '
      WRITE(*,*) '     AVGDISTLSM1    = ',AVGDISTLSM1
      WRITE(*,*) '     AVGDISTLSM2    = ',AVGDISTLSM2
      WRITE(*,*) '     AVGAOPTLSM     = ',AVGAOPTLSM
      WRITE(*,*) '     AVGBOPTLSM     = ',AVGBOPTLSM
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
