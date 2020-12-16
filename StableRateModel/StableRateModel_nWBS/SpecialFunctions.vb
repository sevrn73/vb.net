'**********************************************************
'Modulus for 32bit-High Precision Special Functions
' rev. 1.1 3-Aug-03
'**********************************************************


Imports System.Math
Public Class SpecialFunctions
    Public BJ0 As Double
    Public DJ0 As Double
    Public BJ1 As Double
    Public DJ1 As Double
    Public BY0 As Double
    Public DY0 As Double
    Public BY1 As Double
    Public DY1 As Double
    Public nm As Double
    Public BJ As Double
    Public dj As Double
    Public By As Double
    Public dy As Double


    Const pi_ = 3.14159265358979
    Public Sub Class_init(Optional ByVal BJ0_ As Double = 0,
                          )
        BJ0 = BJ0_
    End Sub
    Public Function errfun(ByVal x As Double) As Double
        'returns the integral of Gauss' standard error function
        Dim T, p, s, y, A0, A1, g, A2, B2, F2, D, F1, B0, B1 As Double
        Dim i As Integer
        Const MaxLoop = 200
        Const tiny = 10 ^ -15
        If x <= 2 Then
            T = 2 * x * x : p = 1 : s = 1
            For i = 3 To MaxLoop Step 2
                p = p * T / i
                s = s + p
                If p < tiny Then Exit For
            Next
            y = 2 * s * x * Exp(-x * x) / Sqrt(pi_)
        Else
            A0 = 1 : B0 = 0
            A1 = 0 : B1 = 1 : F1 = 0
            For i = 1 To MaxLoop
                g = 2 - (i Mod 2)
                A2 = g * x * A1 + i * A0
                B2 = g * x * B1 + i * B0
                F2 = A2 / B2
                D = Abs(F2 - F1)
                If D < tiny Then Exit For
                A0 = A1 : B0 = B1
                A1 = A2 : B1 = B2
                F1 = F2
            Next
            y = 1 - 2 * Exp(-x * x) / (2 * x + F2) / Sqrt(pi_)
        End If
        errfun = y
        Exit Function
    End Function

    Public Function digamma(ByVal x As Double) As Double
        Dim B1(11) As Double, B2(11) As Double
        Const LIM_LOW = 8

        'Bernoulli's numbers
        B1(0) = 1 : B2(0) = 1
        B1(1) = 1 : B2(1) = 6
        B1(2) = -1 : B2(2) = 30
        B1(3) = 1 : B2(3) = 42
        B1(4) = -1 : B2(4) = 30
        B1(5) = 5 : B2(5) = 66
        B1(6) = -691 : B2(6) = 2730
        B1(7) = 7 : B2(7) = 6
        B1(8) = -3617 : B2(8) = 360
        B1(9) = 43867 : B2(9) = 798
        B1(10) = -174611 : B2(10) = 330
        B1(11) = 854513 : B2(11) = 138
        If x <= LIM_LOW Then
            z = x - 1 + LIM_LOW
        Else
            z = x - 1
        End If
        s = 0
        For k = 1 To 11
            tmp = B1(k) / B2(k) / k / z ^ (2 * k)
            s = s + tmp
        Next
        y = Log(z) + 0.5 * (1 / z - s)

        If x <= LIM_LOW Then
            s = 0
            For i = 0 To LIM_LOW - 1
                s = s + 1 / (x + i)
            Next
            y = y - s
        End If

        digamma = y
    End Function


    Function exp_integr(x)
        'Computes the exponential integral Ei(x) for x >0.
        'Parameters: EPS is the relative error, or absolute error near the zero
        'EULER is Euler's constant ; MAXIT is the maximum number of iterations allowed; FPMIN
        'is a number near the smallest representable oating-point number.
        Const eps = 0.000000000000001, EULER = 0.577215664901532, MAXIT = 100, FPMIN = 1.0E-30
        Dim k As Integer
        Dim Fact#, prev#, Sum#, Term#
        If (x <= 0#) Then Exit Function
        If (x < FPMIN) Then
            exp_integr = Log(x) + EULER
        ElseIf (x <= -Log(eps)) Then 'Use power series.
            Sum = 0#
            Fact = 1.0#
            For k = 1 To MAXIT
                Fact = Fact * x / k
                Term = Fact / k
                Sum = Sum + Term
                If (Term < eps * Sum) Then Exit For
            Next
            exp_integr = Sum + Log(x) + EULER
        Else 'Use asymptotic series.
            Sum = 0# 'Start with second term.
            Term = 1.0#
            For k = 1 To MAXIT
                prev = Term
                Term = Term * k / x
                If (Term < eps) Then Exit For
                If (Term < prev) Then
                    Sum = Sum + Term 'Still converging: add new term.
                Else
                    Sum = Sum - prev 'Diverging: subtract previous term and exit.
                    Exit For
                End If
            Next
            exp_integr = Exp(x) * (1.0# + Sum) / x
        End If

    End Function


    Function expn_integr(x, N)
        'Evaluates the exponential integral En(x).
        'Parameters: MAXIT is the maximum allowed number of iterations; EPS is the desired rel-
        'ative error, not smaller than the machine precision; FPMIN is a number near the smallest
        'representable foating-point number; EULER is Euler's constant .
        Const MAXIT = 100, eps = 0.000000000000001, FPMIN = 1.0E-30, EULER = 0.577215664901532
        nm1 = N - 1
        If (N < 0 Or x < 0 Or (x = 0 And (N = 0 Or N = 1))) Then
            Exit Function
        ElseIf (N = 0) Then 'Special case.
            expn_integr = Exp(-x) / x
        ElseIf (x = 0) Then 'Another special case.
            expn_integr = 1.0# / nm1
        ElseIf (x > 1) Then 'Lentz's algorithm .
            B = x + N
            C = 1.0# / FPMIN
            D = 1.0# / B
            h = D
            For i = 1 To MAXIT
                A = -i * (nm1 + i)
                B = B + 2.0#
                D = 1.0# / (A * D + B) 'Denominators cannot be zero.
                C = B + A / C
                del = C * D
                h = h * del
                If (Abs(del - 1.0#) < eps) Then
                    expn_integr = h * Exp(-x)
                    Exit Function
                End If
            Next
            expn_integr = "?"
            Exit Function 'continued fraction failed '
        Else 'Evaluate series.
            If (nm1 <> 0) Then 'Set rst term.
                expn_integr = 1.0# / nm1
            Else
                expn_integr = -Log(x) - EULER
            End If
            Fact = 1.0#
            For i = 1 To MAXIT
                Fact = -Fact * x / i
                If (i <> nm1) Then
                    del = -Fact / (i - nm1)
                Else
                    Psi = -EULER '.
                    For ii = 1 To nm1
                        Psi = Psi + 1.0# / ii
                    Next
                    del = Fact * (-Log(x) + Psi)
                End If
                expn_integr = expn_integr + del
                If (Abs(del) < Abs(expn_integr) * eps) Then Exit Function
            Next
            expn_integr = "?"
            Exit Function 'series failed in'
        End If

    End Function


    '*****************************************************************************
    'FORTRAN routines for computation of Special Functions
    'written in FORTRAN-77 by Shanjie Zhang and Jianming Jin.
    'All these programs and subroutines are copyrighted.
    'However, authors give kindly permission to incorporate any of these
    'routines into other programs providing that the copyright is acknowledged.
    'We have modified only minimal parts for adapting to VBA.

    Sub JY01A(x, BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1)
        '=======================================================
        ' Purpose: Compute Bessel functions J0(x), J1(x), Y0(x),
        '         Y1(x), and their derivatives
        ' Input :  x   --- Argument of Jn(x) & Yn(x) ( x т 0 )
        ' Output:  BJ0 --- J0(x)
        '          DJ0 --- J0'(x)
        '          BJ1 --- J1(x)
        '          DJ1 --- J1'(x)
        '          BY0 --- Y0(x)
        '          DY0 --- Y0'(x)
        '          BY1 --- Y1(x)
        '          DY1 --- Y1'(x)
        '=======================================================
        'by Shanjie Zhang and Jianming Jin, 2001
        PI = 3.14159265358979
        RP2 = 0.63661977236758
        x2 = x * x
        If (x = 0#) Then
            BJ0 = 1.0#
            BJ1 = 0#
            DJ0 = 0#
            DJ1 = 0.5
            BY0 = -1.0E+300
            BY1 = -1.0E+300
            DY0 = 1.0E+300
            DY1 = 1.0E+300
            Return
        End If
        If (x <= 12.0#) Then
            BJ0 = 1.0#
            r = 1.0#
            For k = 1 To 30
                r = -0.25 * r * x2 / (k * k)
                BJ0 = BJ0 + r
                If (Abs(r) < Abs(BJ0) * 0.000000000000001) Then Exit For
            Next
            BJ1 = 1.0#
            r = 1.0#
            For k = 1 To 30
                r = -0.25 * r * x2 / (k * (k + 1.0#))
                BJ1 = BJ1 + r
                If (Abs(r) < Abs(BJ1) * 0.000000000000001) Then Exit For
            Next
            BJ1 = 0.5 * x * BJ1
            EC = Log(x / 2.0#) + 0.577215664901533
            CS0 = 0#
            W0 = 0#
            r0 = 1.0#
            For k = 1 To 30
                W0 = W0 + 1.0# / k
                r0 = -0.25 * r0 / (k * k) * x2
                r = r0 * W0
                CS0 = CS0 + r
                If (Abs(r) < Abs(CS0) * 0.000000000000001) Then Exit For
            Next
            BY0 = RP2 * (EC * BJ0 - CS0)
            Cs1 = 1.0#
            w1 = 0#
            R1 = 1.0#
            For k = 1 To 30
                w1 = w1 + 1.0# / k
                R1 = -0.25 * R1 / (k * (k + 1)) * x2
                r = R1 * (2.0# * w1 + 1.0# / (k + 1.0#))
                Cs1 = Cs1 + r
                If (Abs(r) < Abs(Cs1) * 0.000000000000001) Then Exit For
            Next
            BY1 = RP2 * (EC * BJ1 - 1.0# / x - 0.25 * x * Cs1)
        Else
            A = Array(-0.0703125, 0.112152099609375,
         -0.572501420974731, 6.07404200127348,
         -110.017140269247, 3038.09051092238,
         -118838.426256783, 6252951.4934348,
         -425939216.504767, 36468400807.0656,
         -3833534661393.94, 485401468685290.0#)
            B = Array(0.0732421875, -0.227108001708984,
          1.72772750258446, -24.3805296995561,
          551.335896122021, -18257.7554742932,
          832859.304016289, -50069589.5319889,
          3836255180.23043, -364901081884.983,
          42189715702841.0#, -5.82724463156691E+15)
            A1 = Array(0.1171875, -0.144195556640625,
          0.676592588424683, -6.88391426810995,
          121.597891876536, -3302.27229448085,
          127641.272646175, -6656367.71881769,
          450278600.305039, -38338575207.4279,
          4011838599133.2, -506056850331473.0#)
            B1 = Array(-0.1025390625, 0.277576446533203,
          -1.9935317337513, 27.2488273112685,
          -603.84407670507, 19718.3759122366,
          -890297.876707068, 53104110.1096852,
          -4043620325.10775, 382701134659.86,
          -44064814178522.8, 6.0650913512227E+15)
            K0 = 12
            If (x >= 35.0#) Then K0 = 10
            If (x >= 50.0#) Then K0 = 8
            t1 = x - 0.25 * PI
            P0 = 1.0#
            Q0 = -0.125 / x
            For k = 1 To K0
                i = k - 1
                P0 = P0 + A(i) * x ^ (-2 * k)
                Q0 = Q0 + B(i) * x ^ (-2 * k - 1)
            Next
            CU = Sqr(RP2 / x)
            BJ0 = CU * (P0 * Cos(t1) - Q0 * Sin(t1))
            BY0 = CU * (P0 * Sin(t1) + Q0 * Cos(t1))
            t2 = x - 0.75 * PI
            p1 = 1.0#
            q1 = 0.375 / x
            For k = 1 To K0
                i = k - 1
                p1 = p1 + A1(i) * x ^ (-2 * k)
                q1 = q1 + B1(i) * x ^ (-2 * k - 1)
            Next
            CU = Sqr(RP2 / x)
            BJ1 = CU * (p1 * Cos(t2) - q1 * Sin(t2))
            BY1 = CU * (p1 * Sin(t2) + q1 * Cos(t2))
        End If
        DJ0 = -BJ1
        DJ1 = BJ0 - BJ1 / x
        DY0 = -BY1
        DY1 = BY0 - BY1 / x
    End Sub


    Sub JYNA(N, x, nm, BJ, dj, By, dy)
        '  ==========================================================
        '       Purpose: Compute Bessel functions Jn(x) & Yn(x) and
        '                their derivatives
        '       Input :  x --- Argument of Jn(x) & Yn(x)  ( x > 0 )
        '                n --- Order of Jn(x) & Yn(x)
        '       Output:  BJ(n) --- Jn(x)
        '                DJ(n) --- Jn'(x)
        '                BY(n) --- Yn(x)
        '                DY(n) --- Yn'(x)
        '                NM --- Highest order computed
        '       Routines called:
        '            (1) JY01A to calculate J0(x), J1(x), Y0(x) & Y1(x)
        '            (2) MSTA1 and MSTA2 to calculate the starting
        '                point for backward recurrence
        '  =========================================================
        'by Shanjie Zhang and Jianming Jin, 2001
        ReDim BJ(N), By(N), dj(N), dy(N)
        nm = N
        If (x < 1.0E-100) Then
            For k = 0 To N
                BJ(k) = 0#
                dj(k) = 0#
                By(k) = -1.0E+300
                dy(k) = 1.0E+300
            Next
            BJ(0) = 1.0#
            dj(1) = 0.5
            Exit Sub
        End If
        Call JY01A(x, BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1)
        BJ(0) = BJ0
        BJ(1) = BJ1
        By(0) = BY0
        By(1) = BY1
        dj(0) = DJ0
        dj(1) = DJ1
        dy(0) = DY0
        dy(1) = DY1
        If (N <= 1) Then Exit Sub
        If (N < Int(0.9 * x)) Then
            For k = 2 To N
                BJK = 2.0# * (k - 1.0#) / x * BJ1 - BJ0
                BJ(k) = BJK
                BJ0 = BJ1
                BJ1 = BJK
            Next
        Else
            m = MSTA1(x, 200)
            If (m < N) Then
                nm = m
            Else
                m = MSTA2(x, N, 15)
            End If
            F2 = 0#
            F1 = 1.0E-100
            For k = m To 0 Step -1
                F = 2.0# * (k + 1.0#) / x * F1 - F2
                If (k <= nm) Then BJ(k) = F
                F2 = F1
                F1 = F
            Next
            If (Abs(BJ0) > Abs(BJ1)) Then
                Cs = BJ0 / F
            Else
                Cs = BJ1 / F2
            End If
            For k = 0 To nm
                BJ(k) = Cs * BJ(k)
            Next
        End If

        For k = 2 To nm
            dj(k) = BJ(k - 1) - k / x * BJ(k)
        Next
        F0 = By(0)
        F1 = By(1)
        For k = 2 To nm
            F = 2.0# * (k - 1.0#) / x * F1 - F0
            By(k) = F
            F0 = F1
            F1 = F
        Next
        For k = 2 To nm
            dy(k) = By(k - 1) - k * By(k) / x
        Next
    End Sub


    Private Function MSTA1(x, mp) As Integer
        '  ===================================================
        '  Purpose: Determine the starting point for backward
        '           recurrence such that the magnitude of
        '           Jn(x) at that point is about 10^(-MP)
        '  Input :  x     --- Argument of Jn(x)
        '           MP    --- Value of magnitude
        '  Output:  MSTA1 --- Starting point
        ' ===================================================
        'by Shanjie Zhang and Jianming Jin, 2001
        A0 = Abs(x)
        N0 = Int(1.1 * A0) + 1
        F0 = ENVJ(N0, A0) - mp
        n1 = N0 + 5
        F1 = ENVJ(n1, A0) - mp
        For it = 1 To 20
            nn = n1 - (n1 - N0) / (1.0# - F0 / F1)
            F = ENVJ(nn, A0) - mp
            If (Abs(nn - n1) < 1) Then Exit For
            N0 = n1
            F0 = F1
            n1 = nn
            F1 = F
        Next
        MSTA1 = nn
    End Function


    Private Function MSTA2(x, N, mp) As Integer
        ' ===================================================
        ' Purpose: Determine the starting point for backward
        '         recurrence such that all Jn(x) has MP
        '         significant digits
        ' Input :  x  --- Argument of Jn(x)
        '          n  --- Order of Jn(x)
        '          MP --- Significant digit
        ' Output:  MSTA2 --- Starting point
        ' ===================================================
        'by Shanjie Zhang and Jianming Jin, 2001
        A0 = Abs(x)
        HMP = 0.5 * mp
        EJN = ENVJ(N, A0)
        If (EJN <= HMP) Then
            obj = mp
            N0 = Int(1.1 * A0) + 1 'bug for x<0.1 - VL, 2-8.2002
        Else
            obj = HMP + EJN
            N0 = N
        End If
        F0 = ENVJ(N0, A0) - obj
        n1 = N0 + 5
        F1 = ENVJ(n1, A0) - obj
        For it = 1 To 20
            nn = n1 - (n1 - N0) / (1.0# - F0 / F1)
            F = ENVJ(nn, A0) - obj
            If (Abs(nn - n1) < 1) Then Exit For
            N0 = n1
            F0 = F1
            n1 = nn
            F1 = F
        Next
        MSTA2 = nn + 10
    End Function

    Private Function ENVJ(N, x)
        ENVJ = 0.5 * Log10(6.28 * N) - N * Log10(1.36 * x / N)
    End Function

    Sub IK01A(x, BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1)
        '=========================================================
        'Purpose: Compute modified Bessel functions I0(x), I1(1),
        '         K0(x) and K1(x), and their derivatives
        'Input :  x   --- Argument ( x т 0 )
        'Output:  BI0 --- I0(x)
        '         DI0 --- I0'(x)
        '         BI1 --- I1(x)
        '         DI1 --- I1'(x)
        '         BK0 --- K0(x)
        '         DK0 --- K0'(x)
        '         BK1 --- K1(x)
        '         DK1 --- K1'(x)
        '=========================================================
        'by Shanjie Zhang and Jianming Jin, 2001
        Const PI = 3.14159265358979
        Const EL = 0.577215664901533
        x2 = x * x
        If (x = 0#) Then
            BI0 = 1.0#
            BI1 = 0#
            BK0 = 1.0E+300
            BK1 = 1.0E+300
            DI0 = 0#
            DI1 = 0.5
            DK0 = -1.0E+300
            DK1 = -1.0E+300
            Exit Sub
        ElseIf (x <= 18.0#) Then
            BI0 = 1.0#
            r = 1.0#
            For k = 1 To 50
                r = 0.25 * r * x2 / (k * k)
                BI0 = BI0 + r
                If (Abs(r / BI0) < 0.000000000000001) Then Exit For
            Next
            BI1 = 1.0#
            r = 1.0#
            For k = 1 To 50
                r = 0.25 * r * x2 / (k * (k + 1))
                BI1 = BI1 + r
                If (Abs(r / BI1) < 0.000000000000001) Then Exit For
            Next
            BI1 = 0.5 * x * BI1
        Else
            A = Array(0.125, 0.0703125,
          0.0732421875, 0.11215209960938,
          0.22710800170898, 0.57250142097473,
          1.7277275025845, 6.0740420012735,
          24.380529699556, 110.01714026925,
          551.33589612202, 3038.0905109224)
            B = Array(-0.375, -0.1171875,
          -0.1025390625, -0.14419555664063,
          -0.2775764465332, -0.67659258842468,
          -1.9935317337513, -6.8839142681099,
          -27.248827311269, -121.59789187654,
          -603.84407670507, -3302.2722944809)
            K0 = 12
            If (x >= 35.0#) Then K0 = 9
            If (x >= 50.0#) Then K0 = 7
            CA = Exp(x) / Sqr(2.0# * PI * x)
            BI0 = 1.0#
            xr = 1.0# / x
            For k = 1 To K0
                i = k - 1
                BI0 = BI0 + A(i) * xr ^ k
            Next
            BI0 = CA * BI0
            BI1 = 1.0#
            For k = 1 To K0
                i = k - 1
                BI1 = BI1 + B(i) * xr ^ k
            Next
            BI1 = CA * BI1
        End If
        If (x <= 9.0#) Then
            Ct = -(Log(x / 2.0#) + EL)
            BK0 = 0#
            W0 = 0#
            r = 1.0#
            For k = 1 To 50
                W0 = W0 + 1.0# / k
                r = 0.25 * r / (k * k) * x2
                BK0 = BK0 + r * (W0 + Ct)
                If (Abs((BK0 - WW) / BK0) < 0.000000000000001) Then Exit For
                WW = BK0
            Next
            BK0 = BK0 + Ct
        Else
            A1 = Array(0.125, 0.2109375,
           1.0986328125, 11.775970458984,
           214.61706161499, 5951.1522710323,
           233476.45606175, 12312234.987631)
            cb = 0.5 / x
            XR2 = 1.0# / x2
            BK0 = 1.0#
            For k = 1 To 8
                i = k - 1
                BK0 = BK0 + A1(i) * XR2 ^ k
            Next
            BK0 = cb * BK0 / BI0
        End If
        BK1 = (1.0# / x - BI1 * BK0) / BI0
        DI0 = BI1
        DI1 = BI0 - BI1 / x
        DK0 = -BK1
        DK1 = -BK0 - BK1 / x

    End Sub

    Sub IKNA(N, x, nm, BI, Di, BK, DK)
        ' ========================================================
        ' Purpose: Compute modified Bessel functions In(x) and
        '          Kn(x), and their derivatives
        ' Input:   x --- Argument of In(x) and Kn(x) ( x т 0 )
        '          n --- Order of In(x) and Kn(x)
        ' Output:  BI(n) --- In(x)
        '          DI(n) --- In'(x)
        '          BK(n) --- Kn(x)
        '          DK(n) --- Kn'(x)
        '          NM --- Highest order computed
        ' Routines called:
        '      (1) IK01A for computing I0(x),I1(x),K0(x) & K1(x)
        '      (2) MSTA1 and MSTA2 for computing the starting
        '          point for backward recurrence
        ' ========================================================
        'by Shanjie Zhang and Jianming Jin, 2001
        ReDim BI(N), Di(N), BK(N), DK(N)
        nm = N
        If (x <= 1.0E-100) Then
            For k = 0 To N
                BI(k) = 0#
                Di(k) = 0#
                BK(k) = 1.0E+300
                DK(k) = -1.0E+300
            Next
            BI(0) = 1.0#
            Di(1) = 0.5
            Exit Sub
        End If
        Call IK01A(x, BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1)
        BI(0) = BI0
        BI(1) = BI1
        BK(0) = BK0
        BK(1) = BK1
        Di(0) = DI0
        Di(1) = DI1
        DK(0) = DK0
        DK(1) = DK1
        If (N <= 1) Then Exit Sub
        If (x > 40.0# And N < Int(0.25 * x)) Then
            h0 = BI0
            h1 = BI1
            For k = 2 To N
                h = -2.0# * (k - 1.0#) / x * h1 + h0
                BI(k) = h
                h0 = h1
                h1 = h
            Next
        Else
            m = MSTA1(x, 200)
            If (m < N) Then
                nm = m
            Else
                m = MSTA2(x, N, 15)
            End If
            F0 = 0#
            F1 = 1.0E-100
            For k = m To 0 Step -1
                F = 2.0# * (k + 1.0#) * F1 / x + F0
                If (k <= nm) Then BI(k) = F
                F0 = F1
                F1 = F
            Next
            S0 = BI0 / F
            For k = 0 To nm
                BI(k) = S0 * BI(k)
            Next
        End If
        G0 = BK0
        G1 = BK1
        For k = 2 To nm
            g = 2.0# * (k - 1.0#) / x * G1 + G0
            BK(k) = g
            G0 = G1
            G1 = g
        Next
        For k = 2 To nm
            Di(k) = BI(k - 1) - k / x * BI(k)
            DK(k) = -BK(k - 1) - k / x * BK(k)
        Next
    End Sub

    Sub CISIA(x, ci, si)
        '=============================================
        ' Purpose: Compute cosine and sine integrals
        '          Si(x) and Ci(x)  ( x т 0 )
        ' Input :  x  --- Argument of Ci(x) and Si(x)
        ' Output:  CI --- Ci(x)
        '          SI --- Si(x)
        '=============================================
        'by Shanjie Zhang and Jianming Jin, 2001
        Dim BJ(101)
        p2 = 1.5707963267949
        EL = 0.577215664901533
        eps = 0.000000000000001
        x2 = x * x
        If (x = 0#) Then
            ci = -1.0E+300
            si = 0#
        ElseIf (x <= 16.0#) Then
            xr = -0.25 * x2
            ci = EL + Log(x) + xr
            For k = 2 To 40
                xr = -0.5 * xr * (k - 1) / (k * k * (2 * k - 1)) * x2
                ci = ci + xr
                If (Abs(xr) < Abs(ci) * eps) Then Exit For
            Next
            xr = x
            si = x
            For k = 1 To 40
                xr = -0.5 * xr * (2 * k - 1) / k / (4 * k * k + 4 * k + 1) * x2
                si = si + xr
                If (Abs(xr) < Abs(si) * eps) Then Exit For
            Next
        ElseIf (x <= 32.0#) Then
            m = Int(47.2 + 0.82 * x)
            XA1 = 0#
            XA0 = 1.0E-100
            For k = m To 1 Step -1
                xa = 4.0# * k * XA0 / x - XA1
                BJ(k) = xa
                XA1 = XA0
                XA0 = xa
            Next
            XS = BJ(1)
            For k = 3 To m Step 2
                XS = XS + 2.0# * BJ(k)
            Next
            BJ(1) = BJ(1) / XS
            For k = 2 To m
                BJ(k) = BJ(k) / XS
            Next
            xr = 1.0#
            XG1 = BJ(1)
            For k = 2 To m
                xr = 0.25 * xr * (2.0# * k - 3.0#) ^ 2 / ((k - 1.0#) * (2.0# * k - 1.0#) ^ 2) * x
                XG1 = XG1 + BJ(k) * xr
            Next
            xr = 1.0#
            XG2 = BJ(1)
            For k = 2 To m
                xr = 0.25 * xr * (2.0# * k - 5.0#) ^ 2 / ((k - 1.0#) * (2.0# * k - 3.0#) ^ 2) * x
                XG2 = XG2 + BJ(k) * xr
            Next
            XCS = Cos(x / 2.0#)
            XSS = Sin(x / 2.0#)
            ci = EL + Log(x) - x * XSS * XG1 + 2 * XCS * XG2 - 2 * XCS * XCS
            si = x * XCS * XG1 + 2 * XSS * XG2 - Sin(x)
        Else
            xr = 1.0#
            Xf = 1.0#
            For k = 1 To 9
                xr = -2.0# * xr * k * (2 * k - 1) / x2
                Xf = Xf + xr
            Next
            xr = 1.0# / x
            XG = xr
            For k = 1 To 8
                xr = -2.0# * xr * (2 * k + 1) * k / x2
                XG = XG + xr
            Next
            ci = Xf * Sin(x) / x - XG * Cos(x) / x
            si = p2 - Xf * Cos(x) / x - XG * Sin(x) / x
        End If
    End Sub

    Sub FCS(x, C, s)
        ' =================================================
        '  Purpose: Compute Fresnel integrals C(x) and S(x)
        '  Input :  x --- Argument of C(x) and S(x)
        '  Output:  C --- C(x)
        '           S --- S(x)
        ' =================================================
        'by Shanjie Zhang and Jianming Jin, 2001

        Const eps = 0.000000000000001
        Const PI = 3.14159265358979
        xa = Abs(x)
        PX = PI * xa
        T = 0.5 * PX * xa
        t2 = T * T
        If (xa = 0#) Then
            C = 0#
            s = 0#
        ElseIf (xa < 2.5) Then
            r = xa
            C = r
            For k = 1 To 50
                r = -0.5 * r * (4.0# * k - 3.0#) / k / (2.0# * k - 1.0#) / (4.0# * k + 1.0#) * t2
                C = C + r
                If (Abs(r) < Abs(C) * eps) Then Exit For
            Next
            s = xa * T / 3.0#
            r = s
            For k = 1 To 50
                r = -0.5 * r * (4.0# * k - 1.0#) / k / (2.0# * k + 1.0#) / (4.0# * k + 3.0#) * t2
                s = s + r
                If (Abs(r) < Abs(s) * eps) Then GoTo Label40
            Next
        ElseIf (xa < 4.5) Then
            m = Int(42.0# + 1.75 * T)
            SU = 0#
            C = 0#
            s = 0#
            F1 = 0#
            F0 = 1.0E-100
            For k = m To 0 Step -1
                F = (2.0# * k + 3.0#) * F0 / T - F1
                If (k = Int(k / 2) * 2) Then
                    C = C + F
                Else
                    s = s + F
                End If
                SU = SU + (2.0# * k + 1.0#) * F * F
                F1 = F0
                F0 = F
            Next
            q = Sqr(SU)
            C = C * xa / q
            s = s * xa / q
        Else
            r = 1.0#
            F = 1.0#
            For k = 1 To 20
                r = -0.25 * r * (4.0# * k - 1.0#) * (4.0# * k - 3.0#) / t2
                F = F + r
            Next
            r = 1.0# / (PX * xa)
            g = r
            For k = 1 To 12
                r = -0.25 * r * (4.0# * k + 1.0#) * (4.0# * k - 1.0#) / t2
                g = g + r
            Next
            t0 = T - Int(T / (2.0# * PI)) * 2.0# * PI
            C = 0.5 + (F * Sin(t0) - g * Cos(t0)) / PX
            s = 0.5 - (F * Cos(t0) + g * Sin(t0)) / PX
        End If
        Exit Sub
Label40:
        If (x < 0#) Then
            C = -C
            s = -s
        End If

    End Sub

    '*****End of Library for computation of Special Functions*****

    Private Function Log10(ByVal x As Double) As Double
        Log10 = Log(x) / Log(10.0#)
    End Function

    Public Function BesselJ(ByVal x As Double, Optional ByVal N As Double = 0) As Double
        'Bessel function first kind, order n, Jn(x)
        If N <= 1 Then
            Call JY01A(x, BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1)
            If N = 0 Then BesselJ = BJ0 Else BesselJ = BJ1
        Else
            Call JYNA(N, x, nm, BJ, dj, By, dy)
            BesselJ = BJ(N)
        End If
    End Function

    Public Function BesselY(ByVal x As Double, Optional ByVal N As Double = 0) As Double
        'Bessel function second kind, order n, Yn(x)
        If N <= 1 Then
            Call JY01A(x, BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1)
            If N = 0 Then BesselY = BY0 Else BesselY = BY1
        Else
            Call JYNA(N, x, nm, BJ, dj, By, dy)
            BesselY = By(N)
        End If
    End Function

    Public Function BesseldJ(ByVal x As Double, Optional ByVal N As Double = 0) As Double
        'First Derivative of Bessel functions first kind, order n, J'n(x)
        If N <= 1 Then
            Call JY01A(x, BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1)
            If N = 0 Then BesseldJ = DJ0 Else BesseldJ = DJ1
        Else
            Call JYNA(N, x, nm, BJ, dj, By, dy)
            BesseldJ = dj(N)
        End If
    End Function

    Public Function BesseldY(ByVal x As Double, Optional ByVal N As Double = 0) As Double
        'First Derivative of Bessel functions second kind, order n, Y'n(x)
        If N <= 1 Then
            Call JY01A(x, BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1)
            If N = 0 Then BesseldY = DY0 Else BesseldY = DY1
        Else
            Call JYNA(N, x, nm, BJ, dj, By, dy)
            BesseldY = dy(N)
        End If
    End Function

    Public Function BesselI(ByVal x As Double, Optional ByVal N As Double = 0) As Double
        'modified Bessel function 1° kind, order n, In(x)
        If N <= 1 Then
            Call IK01A(x, BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1)
            If N = 0 Then BesselI = BI0 Else BesselI = BI1
        Else
            Call IKNA(N, x, nm, BI, Di, BK, DK)
            BesselI = BI(N)
        End If
    End Function

    Public Function BesseldI(ByVal x As Double, Optional ByVal N As Double = 0) As Double
        'derivative modified Bessel function 1° kind, order n, In(x)
        If N <= 1 Then
            Call IK01A(x, BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1)
            If N = 0 Then BesseldI = DI0 Else BesseldI = DI1
        Else
            Call IKNA(N, x, nm, BI, Di, BK, DK)
            BesseldI = Di(N)
        End If
    End Function

    Public Function BesselK(ByVal x As Double, Optional ByVal N As Double = 0) As Double
        'modified Bessel function 2° kind, order n, In(x)
        If IsMissing(N) Then N = 0
        If N <= 1 Then
            Call IK01A(x, BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1)
            If N = 0 Then BesselK = BK0 Else BesselK = BK1
        Else
            Call IKNA(N, x, nm, BI, Di, BK, DK)
            BesselK = BK(N)
        End If
    End Function

    Public Function BesseldK(x, Optional N)
        'derivative of modified Bessel function 2° kind, order n, In(x)
        If IsMissing(N) Then N = 0
        If N <= 1 Then
            Call IK01A(x, BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1)
            If N = 0 Then BesseldK = DK0 Else BesseldK = DK1
        Else
            Call IKNA(N, x, nm, BI, Di, BK, DK)
            BesseldK = DK(N)
        End If
    End Function

    Public Function CosIntegral(ByVal x As Double) As Double
        'returns cos integral ci(x)
        If x > 0 Then
            Call CISIA(x, ci, si)
            CosIntegral = ci
        Else
            CosIntegral = "?"
        End If
    End Function

    Function SinIntegral(x)
        'returns sin integral ci(x)
        If x >= 0 Then
            Call CISIA(x, ci, si)
            SinIntegral = si
        Else
            SinIntegral = "?"
        End If
    End Function

    Function Fresnel_cos(x)
        'returns Fresnel's cos integral
        If x >= 0 Then
            Call FCS(x, Fr_c, Fr_s)
            Fresnel_cos = Fr_c
        Else
            Fresnel_cos = "?"
        End If
    End Function

    Function Fresnel_sin(x)
        'returns Fresnel's sin integral
        If x >= 0 Then
            Call FCS(x, Fr_c, Fr_s)
            Fresnel_sin = Fr_s
        Else
            Fresnel_sin = "?"
        End If
    End Function


    Private Function xrad5(Optional digit_max)
        Dim Constant As String
        If IsMissing(digit_max) Then digit_max = DEFAULT_DIGITS__ 'Default
        If DecSep = "" Then DecSep = Application.International(xlDecimalSeparator)
        Constant = "2" + DecSep +
"236067977499789696409173668731276235440618359611525724270897245410520925637804899414414408378782274969508176150773783504253267724447073863586360121533452708866778173191879165811276645322639856580535761350417533785003423392414064442086432539097252592627228876299517402440681611775908909498492371390729728898482088641542689894099131693577019748678884425089754132956183176921499977424801530434115035957668332512498815"
        If digit_max > 415 Then digit_max = 415
        xrad5 = Left(Constant, digit_max + 1)
    End Function


    Function Zeta(x)
        'Riemman's Zeta function
        Dim Cnk#, k%, N%, s#, S1#, coeff#
        n_max = 1000
        tiny = 0.0000000000000001
        N = 0 : s = 0
        Do
            S1 = 0 : Cnk = 1
            For k = 0 To N
                If k > 0 Then Cnk = Cnk * (N - k + 1) / k
                S1 = S1 + (-1) ^ k * Cnk / (k + 1) ^ x
            Next k
            coeff = S1 / 2 ^ (1 + N)
            s = s + coeff
            N = N + 1
        Loop Until Abs(coeff) < tiny Or N > n_max
        Zeta = s / (1 - 2 ^ (1 - x))
    End Function

    Sub HYGFX(ByVal A As Double, ByVal B As Double, ByVal C As Double, ByVal x As Double, ByRef hf As Double, ByRef ErrorMsg As String)
        ' ====================================================
        '       Purpose: Compute hypergeometric function F(a,b,c,x)
        '       Input :  a --- Parameter
        '                b --- Parameter
        '                c --- Parameter, c <> 0,-1,-2,...
        '                x --- Argument   ( x < 1 )
        '       Output:  HF --- F(a,b,c,x)
        '====================================================
        Dim L0 As Boolean, L1 As Boolean, l2 As Boolean, l3 As Boolean, l4 As Boolean, L5 As Boolean
        Dim EL As Double, eps As Double, GC As Double, GCAB As Double, GCA As Double, GCB As Double, G0 As Double, G1 As Double, G2 As Double
        Dim G3 As Double, nm As Double, r As Double, j As Long, k As Long, AA As Double, BB As Double, x1 As Double, GM As Double, m As Double, GA As Double, GB As Double
        Dim GAM As Double, GBM As Double, PA As Double, Pb As Double, RM As Double, F0 As Double, r0 As Double, R1 As Double, SP0 As Double, SP As Double, c0 As Double
        Dim C1 As Double, F1 As Double, SM As Double, RP As Double, HW As Double, GABC As Double, A0 As Double

        EL = 0.577215664901533
        eps = 0.000000000000001
        L0 = (C = Int(C)) And (C < 0)
        L1 = ((1 - x) < eps) And ((C - A - B) <= 0)
        l2 = (A = Int(A)) And (A < 0)
        l3 = (B = Int(B)) And (B < 0)
        l4 = (C - A = Int(C - A)) And (C - A <= 0)
        L5 = (C - B = Int(C - B)) And (C - B <= 0)
        If (L0 Or L1) Then
            ErrorMsg = "The hypergeometric series is divergent"
            Exit Sub
        End If
        If (x > 0.95) Then eps = 0.00000001
        If (x = 0 Or A = 0 Or B = 0) Then
            hf = 1
            Exit Sub
        ElseIf ((1 - x = eps) And (C - A - B) > 0) Then
            GC = HGamma(C) 'Call HGamma(c, GC)
            GCAB = HGamma(C - A \ -B)    'Call HGamma(c - a \ -b, GCAB)
            GCA = HGamma(C - A) ' Call HGamma(c - a, GCA)
            GCB = HGamma(C - B) 'Call HGamma(c - b, GCB)
            hf = GC * GCAB / (GCA * GCB)
            Exit Sub
        ElseIf ((1 + x <= eps) And (Abs(C - A + B - 1) <= eps)) Then
            G0 = Sqr(pi_) * 2 ^ (-A)
            G1 = HGamma(C)   ' Call HGamma(c, G1)
            G2 = HGamma(1 + A / 2 - B)   'Call HGamma(1 + a / 2 - b, G2)
            G3 = HGamma(0.5 + 0.5 * A)  'Call HGamma(0.5 + 0.5 * a, G3)
            hf = G0 * G1 / (G2 * G3)
            Exit Sub
        ElseIf (l2 Or l3) Then
            If (l2) Then nm = Int(Abs(A))
            If (l3) Then nm = Int(Abs(B))
            hf = 1
            r = 1
            For k = 1 To nm
                r = r * (A + k - 1) * (B + k - 1) / (k * (C + k - 1)) * x
                hf = hf + r
            Next k
            Exit Sub
        ElseIf (l4 Or L5) Then
            If (l4) Then nm = Int(Abs(C - A))
            If (L5) Then nm = Int(Abs(C - B))
            hf = 1
            r = 1
            For k = 1 To nm
                r = r * (C - A + k - 1) * (C - B + k - 1) / (k * (C + k - 1)) * x
                hf = hf + r
            Next k
            hf = (1 - x) ^ (C - A - B) * hf
            Exit Sub
        End If
        AA = A
        BB = B
        x1 = x
        If (x < 0) Then
            x = x / (x - 1)
            If (C > A And B < A And B > 0) Then
                A = BB
                B = AA
            End If
            B = C - B
        End If
        If (x >= 0.75) Then
            GM = 0
            If (Abs(C - A - B - Int(C - A - B)) < 0.000000000000001) Then
                m = Int(C - A - B)
                GA = HGamma(A) '  Call HGamma(a, GA)
                GB = HGamma(B)   'Call HGamma(b, GB)
                GC = HGamma(C)    'Call HGamma(c, GC)
                GAM = HGamma(A + m)   'Call HGamma(a + m, GAM)
                GBM = HGamma(B + m)   ' Call HGamma(b + m, GBM)
                PA = digamma(A) '  Call HDigamma(a, PA)
                Pb = digamma(B)  'Call HDigamma(b, PB)
                If (m <> 0) Then GM = 1
                For j = 1 To Abs(m) - 1
                    GM = GM * j
                Next j
                RM = 1
                For j = 1 To Abs(m)
                    RM = RM * j
                Next j
                F0 = 1
                r0 = 1
                R1 = 1
                SP0 = 0
                SP = 0
                If (m >= 0) Then
                    c0 = GM * GC / (GAM * GBM)
                    C1 = -GC * (x - 1) ^ m / (GA * GB * RM)
                    For k = 1 To m - 1
                        r0 = r0 * (A + k - 1) * (B + k - 1) / (k * (k - m)) * (1 - x)
                        F0 = F0 + r0
                    Next k
                    For k = 1 To m
                        SP0 = SP0 + 1 / (A + k - 1) + 1 / (B + k - 1) - 1 / k
                    Next k
                    F1 = PA + Pb + SP0 + 2 * EL + Log(1 - x)
                    For k = 1 To 250
                        SP = SP + (1 - A) / (k * (A + k - 1)) + (1 - B) / (k * (B + k - 1))
                        SM = 0
                        For j = 1 To m
                            SM = SM + (1 - A) / ((j + k) * (A + j + k - 1)) + 1 / (B + j + k - 1)
                        Next j
                        RP = PA + Pb + 2 * EL + SP + SM + Log(1 - x)
                        R1 = R1 * (A + m + k - 1) * (B + m + k - 1) / (k * (m + k)) * (1 - x)
                        F1 = F1 + R1 * RP
                        If (Abs(F1 - HW) < Abs(F1) * eps) Then GoTo 60
                        HW = F1
                    Next k
60:                 hf = F0 * c0 + F1 * C1
                ElseIf (m < 0) Then
                    m = -m
                    c0 = GM * GC / (GA * GB * (1 - x) ^ m)
                    C1 = -(-1) ^ m * GC / (GAM * GBM * RM)
                    For k = 1 To m - 1
                        r0 = r0 * (A - m + k - 1) * (B - m + k - 1) / (k * (k - m)) * (1 - x)
                        F0 = F0 + r0
                    Next k
                    For k = 1 To m
                        SP0 = SP0 + 1 / k
                    Next k
                    F1 = PA + Pb - SP0 + 2 * EL + Log(1 - x)
                    For k = 1 To 250
                        SP = SP + (1 - A) / (k * (A + k - 1)) + (1 - B) / (k * (B + k - 1))
                        SM = 0
                        For j = 1 To m
                            SM = SM + 1 / (j + k)
                        Next j
                        RP = PA + Pb + 2 * EL + SP - SM + Log(1 - x)
                        R1 = R1 * (A + k - 1) * (B + k - 1) / (k * (m + k)) * (1 - x)
                        F1 = F1 + R1 * RP
                        If (Abs(F1 - HW) < (Abs(F1) * eps)) Then GoTo 85
                        HW = F1
                    Next k
85:                 hf = F0 * c0 + F1 * C1
                End If
            Else
                GA = HGamma(A)    'Call HGamma(a, GA)
                GB = HGamma(B)    'Call HGamma(b, GB)
                GC = HGamma(C)    'Call HGamma(c, GC)
                GCA = HGamma(C - A)    'Call HGamma(c - a, GCA)
                GCB = HGamma(C - B)  'Call HGamma(c - b, GCB)
                GCAB = HGamma(C - A - B)  'Call HGamma(c - a - b, GCAB)
                GABC = HGamma(A + B - C)   'Call HGamma(a + b - c, GABC)
                c0 = GC * GCAB / (GCA * GCB)
                C1 = GC * GABC / (GA * GB) * (1 - x) ^ (C - A - B)
                hf = 0
                r0 = c0
                R1 = C1
                For k = 1 To 250
                    r0 = r0 * (A + k - 1) * (B + k - 1) / (k * (A + B - C + k)) * (1 - x)
                    R1 = R1 * (C - A + k - 1) * (C - B + k - 1) / (k * (C - A - B + k)) * (1 - x)
                    hf = hf + r0 + R1
                    If (Abs(hf - HW) < (Abs(hf) * eps)) Then GoTo 95
                    HW = hf
                Next k
95:             hf = hf + c0 + C1
            End If
        Else
            A0 = 1
            If ((C > A) And (C < (2 * A)) And (C > B) And (C < 2 * B)) Then
                A0 = (1 - x) ^ (C - A - B)
                A = C - A
                B = C - B
            End If
            hf = 1
            r = 1
            For k = 1 To 250
                r = r * (A + k - 1) * (B + k - 1) / (k * (C + k - 1)) * x
                hf = hf + r
                If (Abs(hf - HW) <= (Abs(hf) * eps)) Then GoTo 105
                HW = hf
            Next k
105:        hf = A0 * hf
        End If
        If (x1 < 0) Then
            x = x1
            c0 = 1 / (1 - x) ^ AA
            hf = c0 * hf
        End If
        A = AA
        B = BB
        If (k > 120) Then
            ErrorMsg = "Warning! You should check the accuracy"
            Exit Sub
        End If
    End Sub

    Function Hypergeom(A, B, C, x)
        ' Compute hypergeometric function
        '  a --- Parameter
        '  b --- Parameter
        '  c --- Parameter, c <> 0,-1,-2,...
        '  x --- Argument   ( x < 1 )
        Dim a_ As Double, b_ As Double, c_ As Double, x_ As Double, y As Double, msg As String
        a_ = A : b_ = B : c_ = C : x_ = x

        Call HYGFX(a_, b_, c_, x_, y, msg)

        If msg = "" Then
            Hypergeom = y
        Else
            Hypergeom = "?"
        End If
    End Function

    '-------------------------------------------------------------------------------
    Private Function HGamma(ByVal x As Double)
        'compute y = gamma(x)
        Dim mantissa As Double, expo As Double, z As Double
        Const PI = 3.14159265358979
        If x <= 0 And x - Int(x) = 0 Then 'negative integer
            HGamma = "?" : Exit Function
        End If
        z = Abs(x)
        gamma_split z, mantissa, expo
If x < 0 Then
            tmp = z * Sin(PI * z)
            y = -PI / (mantissa * tmp)
            E = Int(Log(Abs(y)) / Log(10.0#))
            mantissa = y * 10 ^ -E
            expo = E - expo
        End If
        HGamma = mantissa * 10 ^ expo
    End Function

    Sub gamma_split(ByVal x As Double, ByRef mantissa As Double, ByRef expo As Double)
        Dim z As Double, Cf(14) As Double, W As Double, i As Long, s As Double, p As Double
        Const DOUBLEPI As Double = 6.28318530717959
        Const G_ As Double = 4.7421875  '607/128
        z = x - 1
        Cf(0) = 0.999999999999997
        Cf(1) = 57.1562356658629
        Cf(2) = -59.5979603554755
        Cf(3) = 14.1360979747417
        Cf(4) = -0.49191381609762
        Cf(5) = 0.0000339946499848119
        Cf(6) = 0.0000465236289270486
        Cf(7) = -0.0000983744753048796
        Cf(8) = 0.000158088703224912
        Cf(9) = -0.000210264441724105
        Cf(10) = 0.000217439618115213
        Cf(11) = -0.000164318106536764
        Cf(12) = 0.0000844182239838528
        Cf(13) = -0.0000261908384015814
        Cf(14) = 0.00000368991826595316

        W = Exp(G_) / Sqr(DOUBLEPI)
        s = Cf(0)
        For i = 1 To 14
            s = s + Cf(i) / (z + i)
        Next
        s = s / W
        p = Log((z + G_ + 0.5) / Exp(1)) * (z + 0.5) / Log(10)
        'split in mantissa and exponent to avoid overflow
        expo = Int(p)
        p = p - Int(p)
        mantissa = 10 ^ p * s
        'rescaling
        p = Int(Log(mantissa) / Log(10))
        mantissa = mantissa * 10 ^ -p
        expo = expo + p
    End Sub


End Class
