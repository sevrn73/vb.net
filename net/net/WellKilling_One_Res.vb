Option Explicit On
Imports System.Math
Public Module WellKilling_One_Res
    Public Function StableRate_one_WKs11(ByVal k As Double,
                   ByVal por As Double,
                   ByVal mu As Double,
                   ByVal Ct As Double,
                   ByVal Bo As Double,
                   ByVal rw As Double,
                   ByVal re As Double,
                   ByVal h As Double,
                   ByVal Po As Double,
                   ByVal Pwf As Double,
                   ByVal q As Double,
                   ByVal Cs As Double,
                   ByVal s As Double,
                   ByVal i As Double) As Double

        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim z, xe, xw, p, qwf As Double
        Dim A, B, D, E, F, C1, C2 As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw

        A = spec.BesselI(xe, 0)
        B = spec.BesselK(xe, 0)
        D = ((s / z) ^ 0.5) * spec.BesselI(xw, 1) - ((mu * Cs * s) / (2 * 3.14 * k * h * rw)) * spec.BesselI(xw, 0)
        E = -((s / z) ^ 0.5) * spec.BesselK(xw, 1) - ((mu * Cs * s) / (2 * 3.14 * k * h * rw)) * spec.BesselK(xw, 0)
        F = (mu * Bo * q) / (2 * 3.14 * k * h * rw * s) - (mu * Cs) * (Pwf - Po) / (2 * 3.14 * k * h * rw)

        C1 = (-B * F) / (A * E - B * D)
        C2 = (A * F) / (A * E - B * D)

        If i = 1 Then

            qwf = (C1 * (s / z) ^ 0.5 * spec.BesselI(xw, 1) - C2 * (s / z) ^ 0.5 * spec.BesselK(xw, 1)) * (2 * 3.14 * k * h * rw) / (mu)
            StableRate_one_WKs11 = qwf

        End If

        If i = 2 Then

            p = C1 * spec.BesselI(xw, 0) + C2 * spec.BesselK(xw, 0)
            StableRate_one_WKs11 = p

        End If

    End Function

    Public Function StableRate_one_WKt11(ByVal N As Integer,
                          ByVal T As Double,
                          ByVal k As Double,
                          ByVal por As Double,
                          ByVal mu As Double,
                          ByVal Ct As Double,
                          ByVal Bo As Double,
                          ByVal rw As Double,
                          ByVal re As Double,
                          ByVal h As Double,
                          ByVal Po As Double,
                          ByVal Pwf As Double,
                          ByVal q As Double,
                          ByVal Cs As Double,
                          ByVal i As Double) As Double

        Dim si As Double
        Dim fi, Vi, r, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_WKs11(k, por, mu, Ct, Bo, rw, re, h, Po, Pwf, q, Cs, si, i)
            Vi = StableRate_Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        StableRate_one_WKt11 = Sum

    End Function

    Public Function StableRate_one_WKrs11(ByVal k As Double,
                           ByVal por As Double,
                           ByVal mu As Double,
                           ByVal Ct As Double,
                           ByVal Bo As Double,
                           ByVal rw As Double,
                           ByVal re As Double,
                           ByVal h As Double,
                           ByVal Po As Double,
                           ByVal Pwf As Double,
                           ByVal q As Double,
                           ByVal Cs As Double,
                           ByVal r As Double,
                           ByVal s As Double) As Double

        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim z, xe, xw, x, p, qwf As Double
        Dim A, B, D, E, F, C1, C2 As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw
        x = (s / z) ^ 0.5 * r

        A = spec.BesselI(xe, 0)
        B = spec.BesselK(xe, 0)
        D = ((s / z) ^ 0.5) * spec.BesselI(xw, 1) - ((mu * Cs * s) / (2 * 3.14 * k * h * rw)) * spec.BesselI(xw, 0)
        E = -((s / z) ^ 0.5) * spec.BesselK(xw, 1) - ((mu * Cs * s) / (2 * 3.14 * k * h * rw)) * spec.BesselK(xw, 0)
        F = (mu * Bo * q) / (2 * 3.14 * k * h * rw * s) - (mu * Cs) * (Pwf - Po) / (2 * 3.14 * k * h * rw)

        C1 = (-B * F) / (A * E - B * D)
        C2 = (A * F) / (A * E - B * D)

        p = C1 * spec.BesselI(x, 0) + C2 * spec.BesselK(x, 0)
        StableRate_one_WKrs11 = p

    End Function

    Public Function StableRate_one_WKrt11(ByVal N As Integer,
                           ByVal T As Double,
                           ByVal k As Double,
                           ByVal por As Double,
                           ByVal mu As Double,
                           ByVal Ct As Double,
                           ByVal Bo As Double,
                           ByVal rw As Double,
                           ByVal re As Double,
                           ByVal h As Double,
                           ByVal Po As Double,
                           ByVal Pwf As Double,
                           ByVal q As Double,
                           ByVal Cs As Double,
                           ByVal r As Double) As Double

        Dim si As Double
        Dim fi, Vi, rr, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_WKrs11(k, por, mu, Ct, Bo, rw, re, h, Po, Pwf, q, Cs, r, si)
            Vi = StableRate_Coef(N, j)
            rr = (Log(2) / T) * Vi * fi
            Sum = Sum + rr

        Next j

        StableRate_one_WKrt11 = Sum

    End Function

    Public Function StableRate_one_WKs12(ByVal k As Double,
                          ByVal por As Double,
                          ByVal mu As Double,
                          ByVal Ct As Double,
                          ByVal Bo As Double,
                          ByVal rw As Double,
                          ByVal re As Double,
                          ByVal h As Double,
                          ByVal Po As Double,
                          ByVal Pwf As Double,
                          ByVal q As Double,
                          ByVal Cs As Double,
                          ByVal s As Double,
                          ByVal i As Double) As Double

        'Решение для одного пласта с постоянным неперетоком на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель

        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim z, xe, xw, p, qwf As Double
        Dim A, B, D, E, F, C1, C2 As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw

        A = spec.BesselI(xe, 1)
        B = -spec.BesselK(xe, 1)
        D = ((s / z) ^ 0.5) * spec.BesselI(xw, 1) - ((mu * Cs * s) / (2 * 3.14 * k * h * rw)) * spec.BesselI(xw, 0)
        E = -((s / z) ^ 0.5) * spec.BesselK(xw, 1) - ((mu * Cs * s) / (2 * 3.14 * k * h * rw)) * spec.BesselK(xw, 0)
        F = (mu * Bo * q) / (2 * 3.14 * k * h * rw * s) - (mu * Cs) * (Pwf - Po) / (2 * 3.14 * k * h * rw)

        C1 = (-B * F) / (A * E - B * D)
        C2 = (A * F) / (A * E - B * D)

        If i = 1 Then

            qwf = (C1 * (s / z) ^ 0.5 * spec.BesselI(xw, 1) - C2 * (s / z) ^ 0.5 * spec.BesselK(xw, 1)) * (2 * 3.14 * k * h * rw) / (mu)
            StableRate_one_WKs12 = qwf

        End If

        If i = 2 Then

            p = C1 * spec.BesselI(xw, 0) + C2 * spec.BesselK(xw, 0)
            StableRate_one_WKs12 = p

        End If

    End Function

    Public Function StableRate_one_WKt12(ByVal N As Integer,
                          ByVal T As Double,
                          ByVal k As Double,
                          ByVal por As Double,
                          ByVal mu As Double,
                          ByVal Ct As Double,
                          ByVal Bo As Double,
                          ByVal rw As Double,
                          ByVal re As Double,
                          ByVal h As Double,
                          ByVal Po As Double,
                          ByVal Pwf As Double,
                          ByVal q As Double,
                          ByVal Cs As Double,
                          ByVal i As Double) As Double
        'Вычисляет значение забойного давления от времени, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие постоянного давления на границе пласта

        Dim si As Double
        Dim fi, Vi, r, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_WKs12(k, por, mu, Ct, Bo, rw, re, h, Po, Pwf, q, Cs, si, i)
            Vi = StableRate_Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        StableRate_one_WKt12 = Sum

    End Function

    Public Function StableRate_one_WKsr12(ByVal k As Double,
                           ByVal por As Double,
                           ByVal mu As Double,
                           ByVal Ct As Double,
                           ByVal Bo As Double,
                           ByVal rw As Double,
                           ByVal re As Double,
                           ByVal h As Double,
                           ByVal Po As Double,
                           ByVal Pwf As Double,
                           ByVal q As Double,
                           ByVal Cs As Double,
                           ByVal r As Double,
                           ByVal s As Double) As Double

        'Решение для одного пласта с постоянным давлением на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель

        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim z, xe, xw, x, p, qwf As Double
        Dim A, B, D, E, F, C1, C2 As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw
        x = (s / z) ^ 0.5 * r

        A = spec.BesselI(xe, 1)
        B = -spec.BesselK(xe, 1)
        D = ((s / z) ^ 0.5) * spec.BesselI(xw, 1) - ((mu * Cs * s) / (2 * 3.14 * k * h * rw)) * spec.BesselI(xw, 0)
        E = -((s / z) ^ 0.5) * spec.BesselK(xw, 1) - ((mu * Cs * s) / (2 * 3.14 * k * h * rw)) * spec.BesselK(xw, 0)
        F = (mu * Bo * q) / (2 * 3.14 * k * h * rw * s) - (mu * Cs) * (Pwf - Po) / (2 * 3.14 * k * h * rw)

        C1 = (-B * F) / (A * E - B * D)
        C2 = (A * F) / (A * E - B * D)

        p = C1 * spec.BesselI(x, 0) + C2 * spec.BesselK(x, 0)
        StableRate_one_WKsr12 = p

    End Function

    Public Function StableRate_one_WKtr12(ByVal N As Integer,
                           ByVal T As Double,
                           ByVal k As Double,
                           ByVal por As Double,
                           ByVal mu As Double,
                           ByVal Ct As Double,
                           ByVal Bo As Double,
                           ByVal rw As Double,
                           ByVal re As Double,
                           ByVal h As Double,
                           ByVal Po As Double,
                           ByVal Pwf As Double,
                           ByVal q As Double,
                           ByVal Cs As Double,
                           ByVal r As Double) As Double
        'Вычисляет значение забойного давления от времени, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие постоянного давления на границе пласта

        Dim si As Double
        Dim fi, Vi, rr, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = StableRate_WKsr12(k, por, mu, Ct, Bo, rw, re, h, Po, Pwf, q, Cs, r, si)
            Vi = StableRate_Coef(N, j)
            rr = (Log(2) / T) * Vi * fi
            Sum = Sum + rr

        Next j

        StableRate_one_WKtr12 = Sum

    End Function

End Module