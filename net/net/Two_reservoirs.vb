Option Explicit On
Imports System.Math
Public Module Two_reservoirs
    Public Function StableRate_Coef(ByVal N As Double, j As Integer)
        Dim g(160) As Double, h(80) As Double
        Dim NH As Integer, SN As Double
        Dim k As Integer, k1 As Integer, k2 As Integer
        Dim i As Integer, fi As Double
        Dim v(0 To 80)
        Dim m As Double

        If m <> N Then
            'm = N 
            g(1) = 1.0#
            NH = N / 2
            For i = 2 To N
                g(i) = g(i - 1) * i
            Next i
            h(1) = 2.0# / g(NH - 1)
            For i = 2 To NH
                fi = i
                If i <> NH Then
                    h(i) = (fi ^ NH) * g(2 * i) / (g(NH - i) * g(i) * g(i - 1))
                Else
                    h(i) = (fi ^ NH) * g(2 * i) / (g(i) * g(i - 1))
                End If
            Next i
            SN = 2 * (NH - (NH \ 2) * 2) - 1
            For i = 1 To N
                v(i) = 0#
                k1 = (i + 1) \ 2
                k2 = i
                If k2 > NH Then k2 = NH
                For k = k1 To k2 ' поменял логику условий
                    If 2 * k - i = 0 Then
                        v(i) = v(i) + h(k) / (g(i - k))
                        'GoTo 40
                    ElseIf i = k Then
                        v(i) = v(i) + h(k) / g(2 * k - i)
                        'GoTo 40
                    Else
                        v(i) = v(i) + h(k) / (g(i - k) * g(2 * k - i))
                    End If
                Next k
                v(i) = SN * v(i)
                SN = -SN
            Next i
        End If
        StableRate_Coef = v(j)
    End Function

    Public Function StableRate_Pt211(ByVal N As Integer,
                          ByVal T As Double,
                          ByVal k1 As Double,
                          ByVal por1 As Double,
                          ByVal mu1 As Double,
                          ByVal Ct1 As Double,
                          ByVal Bo1 As Double,
                          ByVal re1 As Double,
                          ByVal h1 As Double,
                          ByVal Po1 As Double,
                          ByVal rw1 As Double,
                          ByVal k2 As Double,
                          ByVal por2 As Double,
                          ByVal mu2 As Double,
                          ByVal Ct2 As Double,
                          ByVal Bo2 As Double,
                          ByVal re2 As Double,
                          ByVal h2 As Double,
                          ByVal Po2 As Double,
                          ByVal rw2 As Double,
                          ByVal q As Double,
                          ByVal i As Integer) As Double
        'Вычисляет значение забойного давления от времени для двухпластовой скважины, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'i=1 - давление в первом пласте
        'i=2 - давление во втором пласте
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()


        Dim si As Double
        Dim fi As Double
        Dim Vi As Double
        Dim r As Double
        Dim Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_Ps211(k1, por1, mu1, Ct1, Bo1, re1, h1, Po1, rw1, k2, por2, mu2, Ct2, Bo2, re2, h2, Po2, rw2, q, si, i)
            Vi = StableRate_Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        StableRate_Pt211 = Sum

    End Function

    Public Function StableRate_Ps211(ByVal k1 As Double,
                          ByVal por1 As Double,
                          ByVal mu1 As Double,
                          ByVal Ct1 As Double,
                          ByVal Bo1 As Double,
                          ByVal re1 As Double,
                          ByVal h1 As Double,
                          ByVal Po1 As Double,
                          ByVal rw1 As Double,
                          ByVal k2 As Double,
                          ByVal por2 As Double,
                          ByVal mu2 As Double,
                          ByVal Ct2 As Double,
                          ByVal Bo2 As Double,
                          ByVal re2 As Double,
                          ByVal h2 As Double,
                          ByVal Po2 As Double,
                          ByVal rw2 As Double,
                          ByVal q As Double,
                          ByVal s As Double,
                          ByVal i As Integer) As Double

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

        Dim q1, q2, L, A1, A2, xe1, xe2, xw1, xw2 As Double ' z, 
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2, p1, p2 As Double

        Dim z1, z2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2

        ZNAM1 = spec.BesselI(xe1, 0) * spec.BesselK(xw1, 1) + spec.BesselI(xw1, 1) * spec.BesselK(xe1, 0)
        MULT1 = ((z1 ^ 0.5) / (s ^ 0.5)) * ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 0) * spec.BesselI(xw1, 0) - spec.BesselI(xe1, 0) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselI(xe2, 0) * spec.BesselK(xw2, 1) + spec.BesselI(xw2, 1) * spec.BesselK(xe2, 0)
        MULT2 = ((z2 ^ 0.5) / (s ^ 0.5)) * ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 0) * spec.BesselI(xw2, 0) - spec.BesselI(xe2, 0) * spec.BesselK(xw2, 0)

        A1 = MULT1 * (CHISL1 / ZNAM1)
        A2 = MULT2 * (CHISL2 / ZNAM2)

        L = (Po2 - Po1) / s

        q1 = (A2 * q / s + L) / (A1 + A2)
        q2 = (A1 * q / s - L) / (A1 + A2)

        If i = 1 Then

            StableRate_Ps211 = q1

        End If

        If i = 2 Then

            StableRate_Ps211 = q2

        End If

        If i = 3 Then

            p1 = MULT1 * (CHISL1 / ZNAM1) * q1
            StableRate_Ps211 = p1

        End If

        If i = 4 Then

            p2 = MULT2 * (CHISL2 / ZNAM2) * q2
            StableRate_Ps211 = p2

        End If

    End Function
    Public Function StableRate_Psr211(ByVal k1 As Double,
                           ByVal por1 As Double,
                           ByVal mu1 As Double,
                           ByVal Ct1 As Double,
                           ByVal Bo1 As Double,
                           ByVal re1 As Double,
                           ByVal h1 As Double,
                           ByVal Po1 As Double,
                           ByVal rw1 As Double,
                           ByVal k2 As Double,
                           ByVal por2 As Double,
                           ByVal mu2 As Double,
                           ByVal Ct2 As Double,
                           ByVal Bo2 As Double,
                           ByVal re2 As Double,
                           ByVal h2 As Double,
                           ByVal Po2 As Double,
                           ByVal rw2 As Double,
                           ByVal q As Double,
                           ByVal r As Double,
                           ByVal s As Double,
                           ByVal i As Integer) As Double

        'Решение для двух пластов двухпластовой скважины с постоянным давлением на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        'L=(Po2-Po1)/s
        'A=MULT*CHISL/ZNAM
        'Находит значение давления в каждой точке пласта
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim q1, q2, L, A1, A2, xe1, xe2, xw1, xw2 As Double '  z,
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2, p1, p2 As Double
        Dim CHISL11, CHISL22 As Double

        Dim z1, z2, x1, x2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2
        x1 = (s / z1) ^ 0.5 * r
        x2 = (s / z2) ^ 0.5 * r

        ZNAM1 = spec.BesselI(xe1, 0) * spec.BesselK(xw1, 1) + spec.BesselI(xw1, 1) * spec.BesselK(xe1, 0)
        MULT1 = ((z1 ^ 0.5) / (s ^ 0.5)) * ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 0) * spec.BesselI(xw1, 0) - spec.BesselI(xe1, 0) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselI(xe2, 0) * spec.BesselK(xw2, 1) + spec.BesselI(xw2, 1) * spec.BesselK(xe2, 0)
        MULT2 = ((z2 ^ 0.5) / (s ^ 0.5)) * ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 0) * spec.BesselI(xw2, 0) - spec.BesselI(xe2, 0) * spec.BesselK(xw2, 0)

        A1 = MULT1 * (CHISL1 / ZNAM1)
        A2 = MULT2 * (CHISL2 / ZNAM2)

        L = (Po2 - Po1) / s

        q1 = (A2 * q / s + L) / (A1 + A2)
        q2 = (A1 * q / s - L) / (A1 + A2)



        If i = 1 Then

            CHISL11 = spec.BesselK(xe1, 0) * spec.BesselI(x1, 0) - spec.BesselI(xe1, 0) * spec.BesselK(x1, 0)
            p1 = MULT1 * (CHISL11 / ZNAM1) * q1
            StableRate_Psr211 = p1

        End If

        If i = 2 Then

            CHISL22 = spec.BesselK(xe2, 0) * spec.BesselI(x2, 0) - spec.BesselI(xe2, 0) * spec.BesselK(x2, 0)
            p2 = MULT2 * (CHISL22 / ZNAM2) * q2
            StableRate_Psr211 = p2

        End If

    End Function
    Public Function StableRate_Ptr211(ByVal N As Integer,
                           ByVal T As Double,
                           ByVal k1 As Double,
                           ByVal por1 As Double,
                           ByVal mu1 As Double,
                           ByVal Ct1 As Double,
                           ByVal Bo1 As Double,
                           ByVal re1 As Double,
                           ByVal h1 As Double,
                           ByVal Po1 As Double,
                           ByVal rw1 As Double,
                           ByVal k2 As Double,
                           ByVal por2 As Double,
                           ByVal mu2 As Double,
                           ByVal Ct2 As Double,
                           ByVal Bo2 As Double,
                           ByVal re2 As Double,
                           ByVal h2 As Double,
                           ByVal Po2 As Double,
                           ByVal rw2 As Double,
                           ByVal r As Double,
                           ByVal q As Double,
                           ByVal i As Integer) As Double
        'Вычисляет значение забойного давления от времени для двухпластовой скважины, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'i=1 - давление в первом пласте
        'i=2 - давление во втором пласте


        Dim si As Double
        Dim fi As Double
        Dim Vi As Double
        Dim rr As Double
        Dim Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_Psr211(k1, por1, mu1, Ct1, Bo1, re1, h1, Po1, rw1, k2, por2, mu2, Ct2, Bo2, re2, h2, Po2, rw2, q, r, si, i)
            Vi = StableRate_Coef(N, j)
            rr = (Log(2) / T) * Vi * fi
            Sum = Sum + rr

        Next j

        StableRate_Ptr211 = Sum

    End Function

    Public Function StableRate_Ps212(ByVal k1 As Double,
                          ByVal por1 As Double,
                          ByVal mu1 As Double,
                          ByVal Ct1 As Double,
                          ByVal Bo1 As Double,
                          ByVal re1 As Double,
                          ByVal h1 As Double,
                          ByVal Po1 As Double,
                          ByVal rw1 As Double,
                          ByVal k2 As Double,
                          ByVal por2 As Double,
                          ByVal mu2 As Double,
                          ByVal Ct2 As Double,
                          ByVal Bo2 As Double,
                          ByVal re2 As Double,
                          ByVal h2 As Double,
                          ByVal Po2 As Double,
                          ByVal rw2 As Double,
                          ByVal q As Double,
                          ByVal s As Double,
                          ByVal i As Integer) As Double

        'Решение для двух пластов двухпластовой скважины с постоянным давлением на границе(1 пласт) + условие неперетока (2 пласт)
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        'L=(Po2-Po1)/s
        'A=MULT*CHISL/ZNAM
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim q1, q2, L, A1, A2, xe1, xe2, xw1, xw2 As Double 'z,
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2 As Double ', p1, p2 

        Dim z1, z2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2

        ZNAM1 = spec.BesselI(xe1, 0) * spec.BesselK(xw1, 1) + spec.BesselI(xw1, 1) * spec.BesselK(xe1, 0)
        MULT1 = ((z1 ^ 0.5) / (s ^ 0.5)) * ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 0) * spec.BesselI(xw1, 0) - spec.BesselI(xe1, 0) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselI(xw2, 1) * spec.BesselK(xe2, 1) - spec.BesselI(xe2, 1) * spec.BesselK(xw2, 1)
        MULT2 = ((z2 ^ 0.5) / (s ^ 0.5)) * ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 1) * spec.BesselI(xw2, 0) + spec.BesselI(xe2, 1) * spec.BesselK(xw2, 0)

        A1 = MULT1 * (CHISL1 / ZNAM1)
        A2 = MULT2 * (CHISL2 / ZNAM2)

        L = (Po2 - Po1) / s

        q1 = (A2 * q / s + L) / (A1 + A2)
        q2 = (A1 * q / s - L) / (A1 + A2)

        If i = 1 Then

            StableRate_Ps212 = q1

        End If

        If i = 2 Then

            StableRate_Ps212 = q2

        End If

        If i = 3 Then

            StableRate_Ps212 = A1 * q1

        End If

        If i = 4 Then

            StableRate_Ps212 = A2 * q2

        End If

    End Function
    Public Function StableRate_Pt212(ByVal N As Integer,
                          ByVal T As Double,
                          ByVal k1 As Double,
                          ByVal por1 As Double,
                          ByVal mu1 As Double,
                          ByVal Ct1 As Double,
                          ByVal Bo1 As Double,
                          ByVal re1 As Double,
                          ByVal h1 As Double,
                          ByVal Po1 As Double,
                          ByVal rw1 As Double,
                          ByVal k2 As Double,
                          ByVal por2 As Double,
                          ByVal mu2 As Double,
                          ByVal Ct2 As Double,
                          ByVal Bo2 As Double,
                          ByVal re2 As Double,
                          ByVal h2 As Double,
                          ByVal Po2 As Double,
                          ByVal rw2 As Double,
                          ByVal q As Double,
                          ByVal i As Integer)
        'Вычисляет значение забойного давления от времени для двухпластовой скважины: с постоянным
        'давлением на контуре(1 пласт) + неперток на границе(2 пласт), все переменные размерности[Си],
        'выполняя обратное преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'i=1 - давление в первом пласте
        'i=2 - давленеи во втором пласте

        Dim si As Double
        Dim fi As Double
        Dim Vi As Double
        Dim r As Double
        Dim Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_Ps212(k1, por1, mu1, Ct1, Bo1, re1, h1, Po1, rw1, k2, por2, mu2, Ct2, Bo2, re2, h2, Po2, rw2, q, si, i)
            Vi = StableRate_Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        StableRate_Pt212 = Sum

    End Function
    Public Function StableRate_Ps222(ByVal k1 As Double,
                          ByVal por1 As Double,
                          ByVal mu1 As Double,
                          ByVal Ct1 As Double,
                          ByVal Bo1 As Double,
                          ByVal re1 As Double,
                          ByVal h1 As Double,
                          ByVal Po1 As Double,
                          ByVal rw1 As Double,
                          ByVal k2 As Double,
                          ByVal por2 As Double,
                          ByVal mu2 As Double,
                          ByVal Ct2 As Double,
                          ByVal Bo2 As Double,
                          ByVal re2 As Double,
                          ByVal h2 As Double,
                          ByVal Po2 As Double,
                          ByVal rw2 As Double,
                          ByVal q As Double,
                          ByVal s As Double,
                          ByVal i As Integer) As Double

        'Решение для двух пластов двухпластовой скважины с неперетоком на границе на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        'L=(Po2-Po1)/s
        'A=MULT*CHISL/ZNAM
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim q1, q2, L, A1, A2, xe1, xe2, xw1, xw2 As Double ' z,
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2, p1, p2 As Double

        Dim z1, z2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2

        ZNAM1 = spec.BesselI(xw1, 1) * spec.BesselK(xe1, 1) - spec.BesselI(xe1, 1) * spec.BesselK(xw1, 1)
        MULT1 = ((z1 ^ 0.5) / (s ^ 0.5)) * ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 1) * spec.BesselI(xw1, 0) + spec.BesselI(xe1, 1) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselI(xw2, 1) * spec.BesselK(xe2, 1) - spec.BesselI(xe2, 1) * spec.BesselK(xw2, 1)
        MULT2 = ((z2 ^ 0.5) / (s ^ 0.5)) * ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 1) * spec.BesselI(xw2, 0) + spec.BesselI(xe2, 1) * spec.BesselK(xw2, 0)

        A1 = MULT1 * (CHISL1 / ZNAM1)
        A2 = MULT2 * (CHISL2 / ZNAM2)

        L = (Po2 - Po1) / s

        q1 = (A2 * q / s + L) / (A1 + A2)
        q2 = (A1 * q / s - L) / (A1 + A2)

        If i = 1 Then

            StableRate_Ps222 = q1

        End If

        If i = 2 Then

            StableRate_Ps222 = q2

        End If

        If i = 3 Then

            p1 = MULT1 * (CHISL1 / ZNAM1) * q1
            StableRate_Ps222 = p1

        End If

        If i = 4 Then

            p2 = MULT2 * (CHISL2 / ZNAM2) * q2
            StableRate_Ps222 = p2

        End If

    End Function
    Public Function StableRate_Pt222(ByVal N As Integer,
                          ByVal T As Double,
                          ByVal k1 As Double,
                          ByVal por1 As Double,
                          ByVal mu1 As Double,
                          ByVal Ct1 As Double,
                          ByVal Bo1 As Double,
                          ByVal re1 As Double,
                          ByVal h1 As Double,
                          ByVal Po1 As Double,
                          ByVal rw1 As Double,
                          ByVal k2 As Double,
                          ByVal por2 As Double,
                          ByVal mu2 As Double,
                          ByVal Ct2 As Double,
                          ByVal Bo2 As Double,
                          ByVal re2 As Double,
                          ByVal h2 As Double,
                          ByVal Po2 As Double,
                          ByVal rw2 As Double,
                          ByVal q As Double,
                          ByVal i As Integer) As Double
        'Вычисляет значение забойного давления от времени для двухпластовой скважины, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'i=1 - давление в первом пласте
        'i=2 - давление во втором пласте


        Dim si As Double
        Dim fi As Double
        Dim Vi As Double
        Dim r As Double
        Dim Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_Ps222(k1, por1, mu1, Ct1, Bo1, re1, h1, Po1, rw1, k2, por2, mu2, Ct2, Bo2, re2, h2, Po2, rw2, q, si, i)
            Vi = StableRate_Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        StableRate_Pt222 = Sum

    End Function

    Public Function StableRate_Psr222(ByVal k1 As Double,
                           ByVal por1 As Double,
                           ByVal mu1 As Double,
                           ByVal Ct1 As Double,
                           ByVal Bo1 As Double,
                           ByVal re1 As Double,
                           ByVal h1 As Double,
                           ByVal Po1 As Double,
                           ByVal rw1 As Double,
                           ByVal k2 As Double,
                           ByVal por2 As Double,
                           ByVal mu2 As Double,
                           ByVal Ct2 As Double,
                           ByVal Bo2 As Double,
                           ByVal re2 As Double,
                           ByVal h2 As Double,
                           ByVal Po2 As Double,
                           ByVal rw2 As Double,
                           ByVal q As Double,
                           ByVal r As Double,
                           ByVal s As Double,
                           ByVal i As Integer) As Double

        'Решение для двух пластов двухпластовой скважины с неперетоком на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        'L=(Po2-Po1)/s
        'A=MULT*CHISL/ZNAM
        'Находит значение давления в каждой точке пласта
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim q1, q2, L, A1, A2, xe1, xe2, xw1, xw2 As Double ' z,
        Dim ZNAM1, MULT1, CHISL1, ZNAM2, MULT2, CHISL2, p1, p2 As Double
        Dim CHISL11, CHISL22 As Double

        Dim z1, z2, x1, x2 As Double

        z1 = k1 / (por1 * mu1 * Ct1)
        z2 = k2 / (por2 * mu2 * Ct2)

        xe1 = (s / z1) ^ 0.5 * re1
        xw1 = (s / z1) ^ 0.5 * rw1
        xe2 = (s / z2) ^ 0.5 * re2
        xw2 = (s / z2) ^ 0.5 * rw2
        x1 = (s / z1) ^ 0.5 * r
        x2 = (s / z2) ^ 0.5 * r

        ZNAM1 = spec.BesselI(xw1, 1) * spec.BesselK(xe1, 1) - spec.BesselI(xe1, 1) * spec.BesselK(xw1, 1)
        MULT1 = ((z1 ^ 0.5) / (s ^ 0.5)) * ((mu1 * Bo1) / (2 * 3.14 * k1 * h1 * rw1))
        CHISL1 = spec.BesselK(xe1, 1) * spec.BesselI(xw1, 0) + spec.BesselI(xe1, 1) * spec.BesselK(xw1, 0)

        ZNAM2 = spec.BesselI(xw2, 1) * spec.BesselK(xe2, 1) - spec.BesselI(xe2, 1) * spec.BesselK(xw2, 1)
        MULT2 = ((z2 ^ 0.5) / (s ^ 0.5)) * ((mu2 * Bo2) / (2 * 3.14 * k2 * h2 * rw2))
        CHISL2 = spec.BesselK(xe2, 1) * spec.BesselI(xw2, 0) + spec.BesselI(xe2, 1) * spec.BesselK(xw2, 0)

        A1 = MULT1 * (CHISL1 / ZNAM1)
        A2 = MULT2 * (CHISL2 / ZNAM2)

        L = (Po2 - Po1) / s

        q1 = (A2 * q / s + L) / (A1 + A2)
        q2 = (A1 * q / s - L) / (A1 + A2)

        If i = 1 Then

            CHISL11 = spec.BesselK(xe1, 1) * spec.BesselI(x1, 0) + spec.BesselI(xe1, 1) * spec.BesselK(x1, 0)
            p1 = MULT1 * (CHISL11 / ZNAM1) * q1
            StableRate_Psr222 = p1

        End If

        If i = 2 Then

            CHISL22 = spec.BesselK(xe2, 1) * spec.BesselI(x2, 0) + spec.BesselI(xe2, 1) * spec.BesselK(x2, 0)
            p2 = MULT2 * (CHISL22 / ZNAM2) * q2
            StableRate_Psr222 = p2

        End If

    End Function
    Public Function StableRate_Ptr222(ByVal N As Integer,
                           ByVal T As Double,
                           ByVal k1 As Double,
                           ByVal por1 As Double,
                           ByVal mu1 As Double,
                           ByVal Ct1 As Double,
                           ByVal Bo1 As Double,
                           ByVal re1 As Double,
                           ByVal h1 As Double,
                           ByVal Po1 As Double,
                           ByVal rw1 As Double,
                           ByVal k2 As Double,
                           ByVal por2 As Double,
                           ByVal mu2 As Double,
                           ByVal Ct2 As Double,
                           ByVal Bo2 As Double,
                           ByVal re2 As Double,
                           ByVal h2 As Double,
                           ByVal Po2 As Double,
                           ByVal rw2 As Double,
                           ByVal r As Double,
                           ByVal q As Double,
                           ByVal i As Integer) As Double
        'Вычисляет значение забойного давления от времени для двухпластовой скважины, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'i=1 - давление в первом пласте
        'i=2 - давление во втором пласте


        Dim si As Double
        Dim fi As Double
        Dim Vi As Double
        Dim rr As Double
        Dim Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_Psr222(k1, por1, mu1, Ct1, Bo1, re1, h1, Po1, rw1, k2, por2, mu2, Ct2, Bo2, re2, h2, Po2, rw2, q, r, si, i)
            Vi = StableRate_Coef(N, j)
            rr = (Log(2) / T) * Vi * fi
            Sum = Sum + rr

        Next j

        StableRate_Ptr222 = Sum

    End Function

    Public Function StableRate_Qt211(ByVal N As Integer, ByVal T As Double,
                                     ByVal k1 As Double,
                                     ByVal por1 As Double,
                                     ByVal mu1 As Double,
                                     ByVal Ct1 As Double,
                                     ByVal Bo1 As Double,
                                     ByVal re1 As Double,
                                     ByVal h1 As Double,
                                     ByVal Po1 As Double,
                                     ByVal rw1 As Double,
                                     ByVal k2 As Double,
                                     ByVal por2 As Double,
                                     ByVal mu2 As Double,
                                     ByVal Ct2 As Double,
                                     ByVal Bo2 As Double,
                                     ByVal re2 As Double,
                                     ByVal h2 As Double,
                                     ByVal Po2 As Double,
                                     ByVal rw2 As Double,
                                     ByVal Pwf As Double,
                                     ByVal i As Integer) As Double
        'Вычисляет значение забойного давления от времени для двухпластовой скважины, все переменные размерности[Си],
        'выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие постоянного давления на границе пласта

        Dim q1, q2, q As Double

        q1 = StableRate_Qt11(N, T, k1, por1, mu1, Ct1, Bo1, rw1, re1, h1, Pwf, Po1, 2)
        q2 = StableRate_Qt11(N, T, k2, por2, mu2, Ct2, Bo2, rw2, re2, h2, Pwf, Po2, 2)
        q = q1 + q2

        If i = 1 Then

            StableRate_Qt211 = q1

        End If

        If i = 2 Then

            StableRate_Qt211 = q2

        End If

        If i = 3 Then

            StableRate_Qt211 = q

        End If

    End Function

    Public Function StableRate_Qt222(ByVal N As Integer, ByVal T As Double,
                                     ByVal k1 As Double,
                                     ByVal por1 As Double,
                                     ByVal mu1 As Double,
                                     ByVal Ct1 As Double,
                                     ByVal Bo1 As Double,
                                     ByVal re1 As Double,
                                     ByVal h1 As Double,
                                     ByVal Po1 As Double,
                                     ByVal rw1 As Double,
                                     ByVal k2 As Double,
                                     ByVal por2 As Double,
                                     ByVal mu2 As Double,
                                     ByVal Ct2 As Double,
                                     ByVal Bo2 As Double,
                                     ByVal re2 As Double,
                                     ByVal h2 As Double,
                                     ByVal Po2 As Double,
                                     ByVal rw2 As Double,
                                     ByVal Pwf As Double,
                                     ByVal i As Integer) As Double
        'Вычисляет значение забойного давления от времени для двухпластовой скважины, все переменные размерности[Си],
        'выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие неперетока на границе пласта

        Dim q1, q2, q As Double

        q1 = StableRate_Qt12(N, T, k1, por1, mu1, Ct1, Bo1, rw1, re1, h1, Pwf, Po1, 2)
        q2 = StableRate_Qt12(N, T, k2, por2, mu2, Ct2, Bo2, rw2, re2, h2, Pwf, Po2, 2)
        q = q1 + q2

        If i = 1 Then

            StableRate_Qt222 = q1

        End If

        If i = 2 Then

            StableRate_Qt222 = q2

        End If

        If i = 3 Then

            StableRate_Qt222 = q

        End If

    End Function

    Public Function StableRate_Qt212(ByVal N As Integer, ByVal T As Double,
                                     ByVal k1 As Double,
                                     ByVal por1 As Double,
                                     ByVal mu1 As Double,
                                     ByVal Ct1 As Double,
                                     ByVal Bo1 As Double,
                                     ByVal re1 As Double,
                                     ByVal h1 As Double,
                                     ByVal Po1 As Double,
                                     ByVal rw1 As Double,
                                     ByVal k2 As Double,
                                     ByVal por2 As Double,
                                     ByVal mu2 As Double,
                                     ByVal Ct2 As Double,
                                     ByVal Bo2 As Double,
                                     ByVal re2 As Double,
                                     ByVal h2 As Double,
                                     ByVal Po2 As Double,
                                     ByVal rw2 As Double,
                                     ByVal Pwf As Double,
                                     ByVal i As Integer) As Double
        'Вычисляет значение забойного давления от времени для двухпластовой скважины, все переменные размерности[Си],
        'выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие неперетока на границе пласта

        Dim q1, q2, q As Double

        q1 = StableRate_Qt11(N, T, k1, por1, mu1, Ct1, Bo1, rw1, re1, h1, Pwf, Po1, 2)
        q2 = StableRate_Qt12(N, T, k2, por2, mu2, Ct2, Bo2, rw2, re2, h2, Pwf, Po2, 2)
        q = q1 + q2

        If i = 1 Then

            StableRate_Qt212 = q1

        End If

        If i = 2 Then

            StableRate_Qt212 = q2

        End If

        If i = 3 Then

            StableRate_Qt212 = q

        End If

    End Function

End Module