Option Explicit On
Imports System.Math
Public Module WellKilling_Two_Res
    Public Function StableRate_WKs211(ByVal k1 As Double,
                           ByVal por1 As Double,
                           ByVal mu1 As Double,
                           ByVal Ct1 As Double,
                           ByVal Bo1 As Double,
                           ByVal rw1 As Double,
                           ByVal re1 As Double,
                           ByVal h1 As Double,
                           ByVal Po1 As Double,
                           ByVal k2 As Double,
                           ByVal por2 As Double,
                           ByVal mu2 As Double,
                           ByVal Ct2 As Double,
                           ByVal Bo2 As Double,
                           ByVal rw2 As Double,
                           ByVal re2 As Double,
                           ByVal h2 As Double,
                           ByVal Po2 As Double,
                           ByVal Pwf As Double,
                           ByVal q As Double,
                           ByVal Cs As Double,
                           ByVal s As Double,
                           ByVal i As Double) As Double
        'Решение для двух пластов двухпластовой скважины с постоянным давлением на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        'L=(Po2-Po1)/s
        'A=MULT*CHISL/ZNAM
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim q1, q2, L, A1, A2, z1, z2, xe1, xe2, xw1, xw2, q11, q22 As Double
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2, p1, p2, M1, M2, S1, S2, B1, B2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2

        ZNAM1 = spec.BesselI(xe1, 0) * ((s / z1) ^ 0.5 * spec.BesselK(xw1, 1) + ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselK(xw1, 0)) + spec.BesselK(xe1, 0) * ((s / z1) ^ 0.5 * spec.BesselI(xw1, 1) - ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselI(xw1, 0))
        MULT1 = ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 0) * spec.BesselI(xw1, 0) - spec.BesselI(xe1, 0) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselI(xe2, 0) * ((s / z2) ^ 0.5 * spec.BesselK(xw2, 1) + ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselK(xw2, 0)) + spec.BesselK(xe2, 0) * ((s / z2) ^ 0.5 * spec.BesselI(xw2, 1) - ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselI(xw2, 0))
        MULT2 = ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 0) * spec.BesselI(xw2, 0) - spec.BesselI(xe2, 0) * spec.BesselK(xw2, 0)


        M1 = MULT1 * (CHISL1 / ZNAM1)
        M2 = MULT2 * (CHISL2 / ZNAM2)

        S1 = -(mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (CHISL1 / ZNAM1) * (Pwf - Po1)
        S2 = -(mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (CHISL2 / ZNAM2) * (Pwf - Po2)

        L = (Po2 - Po1) / s + S2 - S1

        q1 = (M2 * q / s + L) / (M1 + M2)
        q2 = (M1 * q / s - L) / (M1 + M2)

        If i = 1 Then

            StableRate_WKs211 = q1

        End If

        If i = 2 Then

            StableRate_WKs211 = q2

        End If

        If i = 3 Then

            p1 = M1 * q1 + S1
            StableRate_WKs211 = p1

        End If

        If i = 4 Then

            p2 = M2 * q2 + S2
            StableRate_WKs211 = p2

        End If

        If i = 5 Then

            q11 = (2 * 3.14 * k1 * h1 * rw1) / (mu1 * Bo1) * (s / z1) ^ 0.5 * (spec.BesselK(xe1, 0) * spec.BesselI(xw1, 1) + spec.BesselI(xe1, 0) * spec.BesselK(xw1, 1)) * (MULT1 * q1 - (mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (Pwf - Po1)) / ZNAM1
            StableRate_WKs211 = q11

        End If

        If i = 6 Then

            q22 = (2 * 3.14 * k2 * h2 * rw2) / (mu2 * Bo2) * (s / z2) ^ 0.5 * (spec.BesselK(xe2, 0) * spec.BesselI(xw2, 1) + spec.BesselI(xe2, 0) * spec.BesselK(xw2, 1)) * (MULT2 * q2 - (mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (Pwf - Po2)) / ZNAM2
            StableRate_WKs211 = q22

        End If

    End Function

    Public Function StableRate_WKt211(ByVal N As Integer, ByVal T As Double,
                                      ByVal k1 As Double,
                                      ByVal por1 As Double,
                                      ByVal mu1 As Double,
                                      ByVal Ct1 As Double,
                                      ByVal Bo1 As Double,
                                      ByVal rw1 As Double,
                                      ByVal re1 As Double,
                                      ByVal h1 As Double,
                                      ByVal Po1 As Double,
                                      ByVal k2 As Double,
                                      ByVal por2 As Double,
                                      ByVal mu2 As Double,
                                      ByVal Ct2 As Double,
                                      ByVal Bo2 As Double,
                                      ByVal rw2 As Double,
                                      ByVal re2 As Double,
                                      ByVal h2 As Double,
                                      ByVal Po2 As Double,
                                      ByVal Pwf As Double,
                                      ByVal q As Double,
                                      ByVal Cs As Double,
                                      ByVal i As Double) As Double

        Dim si As Double
        Dim fi, Vi, r, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_WKs211(k1, por1, mu1, Ct1, Bo1, rw1, re1, h1, Po1, k2, por2, mu2, Ct2, Bo2, rw2, re2, h2, Po2, Pwf, q, Cs, si, i)
            Vi = StableRate_Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        StableRate_WKt211 = Sum

    End Function

    Public Function StableRate_WKsr211(ByVal k1 As Double,
                                       ByVal por1 As Double,
                                       ByVal mu1 As Double,
                                       ByVal Ct1 As Double,
                                       ByVal Bo1 As Double,
                                       ByVal rw1 As Double,
                                       ByVal re1 As Double,
                                       ByVal h1 As Double,
                                       ByVal Po1 As Double,
                                       ByVal k2 As Double,
                                       ByVal por2 As Double,
                                       ByVal mu2 As Double,
                                       ByVal Ct2 As Double,
                                       ByVal Bo2 As Double,
                                       ByVal rw2 As Double,
                                       ByVal re2 As Double,
                                       ByVal h2 As Double,
                                       ByVal Po2 As Double,
                                       ByVal Pwf As Double,
                                       ByVal q As Double,
                                       ByVal Cs As Double,
                                       ByVal r As Double,
                                       ByVal s As Double,
                                       ByVal i As Double) As Double
        'Решение для двух пластов двухпластовой скважины с постоянным давлением на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        'L=(Po2-Po1)/s
        'A=MULT*CHISL/ZNAM
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim q1, q2, L, A1, A2, z1, z2, x, xe1, xe2, xw1, xw2, q11, q22 As Double
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2, p1, p2, M1, M2, S1, S2, B1, B2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2

        ZNAM1 = spec.BesselI(xe1, 0) * ((s / z1) ^ 0.5 * spec.BesselK(xw1, 1) + ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselK(xw1, 0)) + spec.BesselK(xe1, 0) * ((s / z1) ^ 0.5 * spec.BesselI(xw1, 1) - ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselI(xw1, 0))
        MULT1 = ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 0) * spec.BesselI(xw1, 0) - spec.BesselI(xe1, 0) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselI(xe2, 0) * ((s / z2) ^ 0.5 * spec.BesselK(xw2, 1) + ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselK(xw2, 0)) + spec.BesselK(xe2, 0) * ((s / z2) ^ 0.5 * spec.BesselI(xw2, 1) - ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselI(xw2, 0))
        MULT2 = ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 0) * spec.BesselI(xw2, 0) - spec.BesselI(xe2, 0) * spec.BesselK(xw2, 0)


        M1 = MULT1 * (CHISL1 / ZNAM1)
        M2 = MULT2 * (CHISL2 / ZNAM2)

        S1 = -(mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (CHISL1 / ZNAM1) * (Pwf - Po1)
        S2 = -(mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (CHISL2 / ZNAM2) * (Pwf - Po2)

        L = (Po2 - Po1) / s + S2 - S1

        q1 = (M2 * q / s + L) / (M1 + M2)
        q2 = (M1 * q / s - L) / (M1 + M2)

        If i = 1 Then

            x = (s / z1) ^ 0.5 * r
            CHISL1 = spec.BesselK(xe1, 0) * spec.BesselI(x, 0) - spec.BesselI(xe1, 0) * spec.BesselK(x, 0)
            S1 = -(mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (CHISL1 / ZNAM1) * (Pwf - Po1)
            p1 = MULT1 * (CHISL1) / ZNAM1 * q1 + S1
            StableRate_WKsr211 = p1

        End If

        If i = 2 Then

            x = (s / z2) ^ 0.5 * r
            CHISL2 = spec.BesselK(xe2, 0) * spec.BesselI(x, 0) - spec.BesselI(xe2, 0) * spec.BesselK(x, 0)
            S2 = -(mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (CHISL2 / ZNAM2) * (Pwf - Po2)
            p2 = MULT2 * (CHISL2) / ZNAM2 * q2 + S2
            StableRate_WKsr211 = p2

        End If

    End Function

    Public Function StableRate_WKtr211(ByVal N As Integer, ByVal T As Double,
                                       ByVal k1 As Double,
                                       ByVal por1 As Double,
                                       ByVal mu1 As Double,
                                       ByVal Ct1 As Double,
                                       ByVal Bo1 As Double,
                                       ByVal rw1 As Double,
                                       ByVal re1 As Double,
                                       ByVal h1 As Double, ByVal Po1 As Double,
                                       ByVal k2 As Double,
                                       ByVal por2 As Double,
                                       ByVal mu2 As Double,
                                       ByVal Ct2 As Double,
                                       ByVal Bo2 As Double,
                                       ByVal rw2 As Double,
                                       ByVal re2 As Double,
                                       ByVal h2 As Double,
                                       ByVal Po2 As Double,
                                       ByVal Pwf As Double,
                                       ByVal q As Double,
                                       ByVal Cs As Double,
                                       ByVal r As Double,
                                       ByVal i As Double) As Double

        Dim si As Double
        Dim fi, Vi, ri, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_WKsr211(k1, por1, mu1, Ct1, Bo1, rw1, re1, h1, Po1, k2, por2, mu2, Ct2, Bo2, rw2, re2, h2, Po2, Pwf, q, Cs, r, si, i)
            Vi = StableRate_Coef(N, j)
            ri = (Log(2) / T) * Vi * fi
            Sum = Sum + ri

        Next j

        StableRate_WKtr211 = Sum

    End Function

    Public Function StableRate_WKs222(ByVal k1 As Double,
                                      ByVal por1 As Double,
                                      ByVal mu1 As Double,
                                      ByVal Ct1 As Double,
                                      ByVal Bo1 As Double,
                                      ByVal rw1 As Double,
                                      ByVal re1 As Double,
                                      ByVal h1 As Double,
                                      ByVal Po1 As Double,
                                      ByVal k2 As Double,
                                      ByVal por2 As Double,
                                      ByVal mu2 As Double,
                                      ByVal Ct2 As Double,
                                      ByVal Bo2 As Double,
                                      ByVal rw2 As Double,
                                      ByVal re2 As Double,
                                      ByVal h2 As Double,
                                      ByVal Po2 As Double,
                                      ByVal Pwf As Double,
                                      ByVal q As Double,
                                      ByVal Cs As Double,
                                      ByVal s As Double,
                                      ByVal i As Double) As Double

        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim q1, q2, L, A1, A2, z1, z2, xe1, xe2, xw1, xw2, q11, q22 As Double
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2, p1, p2, M1, M2, S1, S2, B1, B2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2

        ZNAM1 = spec.BesselK(xe1, 1) * ((s / z1) ^ 0.5 * spec.BesselI(xw1, 1) - ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselI(xw1, 0)) - spec.BesselI(xe1, 1) * ((s / z1) ^ 0.5 * spec.BesselK(xw1, 1) + ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselK(xw1, 0))
        MULT1 = ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 1) * spec.BesselI(xw1, 0) + spec.BesselI(xe1, 1) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselK(xe2, 1) * ((s / z2) ^ 0.5 * spec.BesselI(xw2, 1) - ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselI(xw2, 0)) - spec.BesselI(xe2, 1) * ((s / z2) ^ 0.5 * spec.BesselK(xw2, 1) + ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselK(xw2, 0))
        MULT2 = ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 1) * spec.BesselI(xw2, 0) + spec.BesselI(xe2, 1) * spec.BesselK(xw2, 0)


        M1 = MULT1 * (CHISL1 / ZNAM1)
        M2 = MULT2 * (CHISL2 / ZNAM2)

        S1 = -(mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (CHISL1 / ZNAM1) * (Pwf - Po1)
        S2 = -(mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (CHISL2 / ZNAM2) * (Pwf - Po2)

        L = (Po2 - Po1) / s + S2 - S1

        q1 = (M2 * q / s + L) / (M1 + M2)
        q2 = (M1 * q / s - L) / (M1 + M2)

        If i = 1 Then

            StableRate_WKs222 = q1

        End If

        If i = 2 Then

            StableRate_WKs222 = q2

        End If

        If i = 3 Then

            p1 = M1 * q1 + S1
            StableRate_WKs222 = p1

        End If

        If i = 4 Then

            p2 = M2 * q2 + S2
            StableRate_WKs222 = p2

        End If

        If i = 5 Then

            q11 = (2 * 3.14 * k1 * h1 * rw1) / (mu1 * Bo1) * (s / z1) ^ 0.5 * (spec.BesselK(xe1, 1) * spec.BesselI(xw1, 1) - spec.BesselI(xe1, 1) * spec.BesselK(xw1, 1)) * (MULT1 * q1 - (mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (Pwf - Po1)) / ZNAM1
            StableRate_WKs222 = q11

        End If

        If i = 6 Then

            q22 = (2 * 3.14 * k2 * h2 * rw2) / (mu2 * Bo2) * (s / z2) ^ 0.5 * (spec.BesselK(xe2, 1) * spec.BesselI(xw2, 1) - spec.BesselI(xe2, 1) * spec.BesselK(xw2, 1)) * (MULT2 * q2 - (mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (Pwf - Po2)) / ZNAM2
            StableRate_WKs222 = q22

        End If

    End Function

    Public Function StableRate_WKt222(ByVal N As Integer, ByVal T As Double,
                                      ByVal k1 As Double,
                                      ByVal por1 As Double,
                                      ByVal mu1 As Double,
                                      ByVal Ct1 As Double,
                                      ByVal Bo1 As Double,
                                      ByVal rw1 As Double,
                                      ByVal re1 As Double,
                                      ByVal h1 As Double,
                                      ByVal Po1 As Double,
                                      ByVal k2 As Double,
                                      ByVal por2 As Double,
                                      ByVal mu2 As Double,
                                      ByVal Ct2 As Double,
                                      ByVal Bo2 As Double,
                                      ByVal rw2 As Double,
                                      ByVal re2 As Double,
                                      ByVal h2 As Double,
                                      ByVal Po2 As Double,
                                      ByVal Pwf As Double,
                                      ByVal q As Double,
                                      ByVal Cs As Double,
                                      ByVal i As Double) As Double

        Dim si As Double
        Dim fi, Vi, r, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_WKs222(k1, por1, mu1, Ct1, Bo1, rw1, re1, h1, Po1, k2, por2, mu2, Ct2, Bo2, rw2, re2, h2, Po2, Pwf, q, Cs, si, i)
            Vi = StableRate_Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        StableRate_WKt222 = Sum

    End Function

    Public Function StableRate_WKsr222(ByVal k1 As Double,
                                       ByVal por1 As Double,
                                       ByVal mu1 As Double,
                                       ByVal Ct1 As Double,
                                       ByVal Bo1 As Double,
                                       ByVal rw1 As Double,
                                       ByVal re1 As Double,
                                       ByVal h1 As Double,
                                       ByVal Po1 As Double,
                                       ByVal k2 As Double,
                                       ByVal por2 As Double,
                                       ByVal mu2 As Double,
                                       ByVal Ct2 As Double,
                                       ByVal Bo2 As Double,
                                       ByVal rw2 As Double,
                                       ByVal re2 As Double,
                                       ByVal h2 As Double,
                                       ByVal Po2 As Double,
                                       ByVal Pwf As Double,
                                       ByVal q As Double,
                                       ByVal Cs As Double,
                                       ByVal r As Double,
                                       ByVal s As Double,
                                       ByVal i As Double) As Double

        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim q1, q2, L, A1, A2, z1, z2, x, xe1, xe2, xw1, xw2, q11, q22 As Double
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2, p1, p2, M1, M2, S1, S2, B1, B2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2

        ZNAM1 = spec.BesselK(xe1, 1) * ((s / z1) ^ 0.5 * spec.BesselI(xw1, 1) - ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselI(xw1, 0)) - spec.BesselI(xe1, 1) * ((s / z1) ^ 0.5 * spec.BesselK(xw1, 1) + ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselK(xw1, 0))
        MULT1 = ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 1) * spec.BesselI(xw1, 0) + spec.BesselI(xe1, 1) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselK(xe2, 1) * ((s / z2) ^ 0.5 * spec.BesselI(xw2, 1) - ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselI(xw2, 0)) - spec.BesselI(xe2, 1) * ((s / z2) ^ 0.5 * spec.BesselK(xw2, 1) + ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselK(xw2, 0))
        MULT2 = ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 1) * spec.BesselI(xw2, 0) + spec.BesselI(xe2, 1) * spec.BesselK(xw2, 0)

        M1 = MULT1 * (CHISL1 / ZNAM1)
        M2 = MULT2 * (CHISL2 / ZNAM2)

        S1 = -(mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (CHISL1 / ZNAM1) * (Pwf - Po1)
        S2 = -(mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (CHISL2 / ZNAM2) * (Pwf - Po2)

        L = (Po2 - Po1) / s + S2 - S1

        q1 = (M2 * q / s + L) / (M1 + M2)
        q2 = (M1 * q / s - L) / (M1 + M2)

        If i = 1 Then

            x = (s / z1) ^ 0.5 * r
            CHISL1 = spec.BesselK(xe1, 1) * spec.BesselI(x, 0) + spec.BesselI(xe1, 1) * spec.BesselK(x, 0)
            S1 = -(mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (CHISL1 / ZNAM1) * (Pwf - Po1)
            p1 = MULT1 * (CHISL1) / ZNAM1 * q1 + S1
            StableRate_WKsr222 = p1

        End If

        If i = 2 Then

            x = (s / z2) ^ 0.5 * r
            CHISL2 = spec.BesselK(xe2, 1) * spec.BesselI(x, 0) + spec.BesselI(xe2, 1) * spec.BesselK(x, 0)
            S2 = -(mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (CHISL2 / ZNAM2) * (Pwf - Po2)
            p2 = MULT2 * (CHISL2) / ZNAM2 * q2 + S2
            StableRate_WKsr222 = p2

        End If

    End Function

    Public Function StableRate_WKtr222(ByVal N As Integer, ByVal T As Double,
                                       ByVal k1 As Double,
                                       ByVal por1 As Double,
                                       ByVal mu1 As Double,
                                       ByVal Ct1 As Double,
                                       ByVal Bo1 As Double,
                                       ByVal rw1 As Double,
                                       ByVal re1 As Double,
                                       ByVal h1 As Double,
                                       ByVal Po1 As Double,
                                       ByVal k2 As Double,
                                       ByVal por2 As Double,
                                       ByVal mu2 As Double,
                                       ByVal Ct2 As Double,
                                       ByVal Bo2 As Double,
                                       ByVal rw2 As Double,
                                       ByVal re2 As Double,
                                       ByVal h2 As Double,
                                       ByVal Po2 As Double,
                                       ByVal Pwf As Double,
                                       ByVal q As Double,
                                       ByVal Cs As Double,
                                       ByVal r As Double,
                                       ByVal i As Double) As Double
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim si As Double
        Dim fi, Vi, ri, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_WKsr222(k1, por1, mu1, Ct1, Bo1, rw1, re1, h1, Po1, k2, por2, mu2, Ct2, Bo2, rw2, re2, h2, Po2, Pwf, q, Cs, r, si, i)
            Vi = StableRate_Coef(N, j)
            ri = (Log(2) / T) * Vi * fi
            Sum = Sum + ri

        Next j

        StableRate_WKtr222 = Sum

    End Function

    Public Function StableRate_WKs212(ByVal k1 As Double,
                                      ByVal por1 As Double,
                                      ByVal mu1 As Double,
                                      ByVal Ct1 As Double,
                                      ByVal Bo1 As Double,
                                      ByVal rw1 As Double,
                                      ByVal re1 As Double,
                                      ByVal h1 As Double,
                                      ByVal Po1 As Double,
                                      ByVal k2 As Double,
                                      ByVal por2 As Double,
                                      ByVal mu2 As Double,
                                      ByVal Ct2 As Double,
                                      ByVal Bo2 As Double,
                                      ByVal rw2 As Double,
                                      ByVal re2 As Double,
                                      ByVal h2 As Double,
                                      ByVal Po2 As Double,
                                      ByVal Pwf As Double,
                                      ByVal q As Double,
                                      ByVal Cs As Double,
                                      ByVal s As Double,
                                      ByVal i As Double) As Double

        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim q1, q2, L, A1, A2, z1, z2, xe1, xe2, xw1, xw2, q11, q22 As Double
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2, p1, p2, M1, M2, S1, S2, B1, B2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2

        ZNAM1 = spec.BesselI(xe1, 0) * ((s / z1) ^ 0.5 * spec.BesselK(xw1, 1) + ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselK(xw1, 0)) + spec.BesselK(xe1, 0) * ((s / z1) ^ 0.5 * spec.BesselI(xw1, 1) - ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselI(xw1, 0))
        MULT1 = ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 0) * spec.BesselI(xw1, 0) - spec.BesselI(xe1, 0) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselK(xe2, 1) * ((s / z2) ^ 0.5 * spec.BesselI(xw2, 1) - ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselI(xw2, 0)) - spec.BesselI(xe2, 1) * ((s / z2) ^ 0.5 * spec.BesselK(xw2, 1) + ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselK(xw2, 0))
        MULT2 = ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 1) * spec.BesselI(xw2, 0) + spec.BesselI(xe2, 1) * spec.BesselK(xw2, 0)

        M1 = MULT1 * (CHISL1 / ZNAM1)
        M2 = MULT2 * (CHISL2 / ZNAM2)

        S1 = -(mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (CHISL1 / ZNAM1) * (Pwf - Po1)
        S2 = -(mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (CHISL2 / ZNAM2) * (Pwf - Po2)

        L = (Po2 - Po1) / s + S2 - S1

        q1 = (M2 * q / s + L) / (M1 + M2)
        q2 = (M1 * q / s - L) / (M1 + M2)

        If i = 1 Then

            StableRate_WKs212 = q1

        End If

        If i = 2 Then

            StableRate_WKs212 = q2

        End If

        If i = 3 Then

            p1 = M1 * q1 + S1
            StableRate_WKs212 = p1

        End If

        If i = 4 Then

            p2 = M2 * q2 + S2
            StableRate_WKs212 = p2

        End If

        If i = 5 Then

            q11 = (2 * 3.14 * k1 * h1 * rw1) / (mu1 * Bo1) * (s / z1) ^ 0.5 * (spec.BesselK(xe1, 0) * spec.BesselI(xw1, 1) + spec.BesselI(xe1, 0) * spec.BesselK(xw1, 1)) * (MULT1 * q1 - (mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (Pwf - Po1)) / ZNAM1
            StableRate_WKs212 = q11

        End If

        If i = 6 Then

            q22 = (2 * 3.14 * k2 * h2 * rw2) / (mu2 * Bo2) * (s / z2) ^ 0.5 * (spec.BesselK(xe2, 1) * spec.BesselI(xw2, 1) - spec.BesselI(xe2, 1) * spec.BesselK(xw2, 1)) * (MULT2 * q2 - (mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (Pwf - Po2)) / ZNAM2
            StableRate_WKs212 = q22

        End If

    End Function

    Public Function StableRate_WKt212(ByVal N As Integer, ByVal T As Double,
                                      ByVal k1 As Double,
                                      ByVal por1 As Double,
                                      ByVal mu1 As Double,
                                      ByVal Ct1 As Double,
                                      ByVal Bo1 As Double,
                                      ByVal rw1 As Double,
                                      ByVal re1 As Double,
                                      ByVal h1 As Double,
                                      ByVal Po1 As Double,
                                      ByVal k2 As Double,
                                      ByVal por2 As Double,
                                      ByVal mu2 As Double,
                                      ByVal Ct2 As Double,
                                      ByVal Bo2 As Double,
                                      ByVal rw2 As Double,
                                      ByVal re2 As Double,
                                      ByVal h2 As Double,
                                      ByVal Po2 As Double,
                                      ByVal Pwf As Double,
                                      ByVal q As Double,
                                      ByVal Cs As Double,
                                      ByVal i As Double) As Double

        Dim si As Double
        Dim fi, Vi, r, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_WKs212(k1, por1, mu1, Ct1, Bo1, rw1, re1, h1, Po1, k2, por2, mu2, Ct2, Bo2, rw2, re2, h2, Po2, Pwf, q, Cs, si, i)
            Vi = StableRate_Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        StableRate_WKt212 = Sum

    End Function

    Public Function StableRate_WKsr212(ByVal k1 As Double,
                                       ByVal por1 As Double,
                                       ByVal mu1 As Double,
                                       ByVal Ct1 As Double,
                                       ByVal Bo1 As Double,
                                       ByVal rw1 As Double,
                                       ByVal re1 As Double,
                                       ByVal h1 As Double,
                                       ByVal Po1 As Double,
                                       ByVal k2 As Double,
                                       ByVal por2 As Double,
                                       ByVal mu2 As Double,
                                       ByVal Ct2 As Double,
                                       ByVal Bo2 As Double,
                                       ByVal rw2 As Double,
                                       ByVal re2 As Double,
                                       ByVal h2 As Double,
                                       ByVal Po2 As Double,
                                       ByVal Pwf As Double,
                                       ByVal q As Double,
                                       ByVal Cs As Double,
                                       ByVal r As Double,
                                       ByVal s As Double,
                                       ByVal i As Double) As Double

        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim q1, q2, L, A1, A2, z1, z2, x, xe1, xe2, xw1, xw2, q11, q22 As Double
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2, p1, p2, M1, M2, S1, S2, B1, B2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2

        ZNAM1 = spec.BesselI(xe1, 0) * ((s / z1) ^ 0.5 * spec.BesselK(xw1, 1) + ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselK(xw1, 0)) + spec.BesselK(xe1, 0) * ((s / z1) ^ 0.5 * spec.BesselI(xw1, 1) - ((mu1 * Cs * s) / (2 * 3.14 * k1 * h1 * rw1)) * spec.BesselI(xw1, 0))
        MULT1 = ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 0) * spec.BesselI(xw1, 0) - spec.BesselI(xe1, 0) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselK(xe2, 1) * ((s / z2) ^ 0.5 * spec.BesselI(xw2, 1) - ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselI(xw2, 0)) - spec.BesselI(xe2, 1) * ((s / z2) ^ 0.5 * spec.BesselK(xw2, 1) + ((mu2 * Cs * s) / (2 * 3.14 * k2 * h2 * rw2)) * spec.BesselK(xw2, 0))
        MULT2 = ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 1) * spec.BesselI(xw2, 0) + spec.BesselI(xe2, 1) * spec.BesselK(xw2, 0)

        M1 = MULT1 * (CHISL1 / ZNAM1)
        M2 = MULT2 * (CHISL2 / ZNAM2)

        S1 = -(mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (CHISL1 / ZNAM1) * (Pwf - Po1)
        S2 = -(mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (CHISL2 / ZNAM2) * (Pwf - Po2)

        L = (Po2 - Po1) / s + S2 - S1

        q1 = (M2 * q / s + L) / (M1 + M2)
        q2 = (M1 * q / s - L) / (M1 + M2)

        If i = 1 Then

            x = (s / z1) ^ 0.5 * r
            CHISL1 = spec.BesselK(xe1, 0) * spec.BesselI(x, 0) - spec.BesselI(xe1, 0) * spec.BesselK(x, 0)
            S1 = -(mu1 * Cs) / (2 * 3.14 * k1 * h1 * rw1) * (CHISL1 / ZNAM1) * (Pwf - Po1)
            p1 = MULT1 * (CHISL1) / ZNAM1 * q1 + S1
            StableRate_WKsr212 = p1

        End If

        If i = 2 Then

            x = (s / z2) ^ 0.5 * r
            CHISL2 = spec.BesselK(xe2, 1) * spec.BesselI(x, 0) + spec.BesselI(xe2, 1) * spec.BesselK(x, 0)
            S2 = -(mu2 * Cs) / (2 * 3.14 * k2 * h2 * rw2) * (CHISL2 / ZNAM2) * (Pwf - Po2)
            p2 = MULT2 * (CHISL2) / ZNAM2 * q2 + S2
            StableRate_WKsr212 = p2

        End If

    End Function

    Public Function StableRate_WKtr212(ByVal N As Integer, ByVal T As Double,
                                       ByVal k1 As Double,
                                       ByVal por1 As Double,
                                       ByVal mu1 As Double,
                                       ByVal Ct1 As Double,
                                       ByVal Bo1 As Double,
                                       ByVal rw1 As Double,
                                       ByVal re1 As Double,
                                       ByVal h1 As Double,
                                       ByVal Po1 As Double,
                                       ByVal k2 As Double,
                                       ByVal por2 As Double,
                                       ByVal mu2 As Double,
                                       ByVal Ct2 As Double,
                                       ByVal Bo2 As Double,
                                       ByVal rw2 As Double,
                                       ByVal re2 As Double,
                                       ByVal h2 As Double,
                                       ByVal Po2 As Double,
                                       ByVal Pwf As Double,
                                       ByVal q As Double,
                                       ByVal Cs As Double,
                                       ByVal r As Double,
                                       ByVal i As Double) As Double

        Dim si As Double
        Dim fi, Vi, ri, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_WKsr212(k1, por1, mu1, Ct1, Bo1, rw1, re1, h1, Po1, k2, por2, mu2, Ct2, Bo2, rw2, re2, h2, Po2, Pwf, q, Cs, r, si, i)
            Vi = StableRate_Coef(N, j)
            ri = (Log(2) / T) * Vi * fi
            Sum = Sum + ri

        Next j

        StableRate_WKtr212 = Sum

    End Function

End Module