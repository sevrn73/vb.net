Option Explicit On
Imports System.Math
Public Module One_reservoir
    Public Function StableRate_Ps11(ByVal k As Double,
                         ByVal por As Double,
                         ByVal mu As Double,
                         ByVal Ct As Double,
                         ByVal Bo As Double,
                         ByVal rw As Double,
                         ByVal re As Double,
                         ByVal h As Double,
                         ByVal Po As Double,
                         ByVal q As Double,
                         ByVal s As Double) As Double

        'Решение для одного пласта с постоянным давлением на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim z, xe, xw, ZNAM, MULT, CHISL, p As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw
        ZNAM = spec.BesselI(xe, 0) * spec.BesselK(xw, 1) + spec.BesselI(xw, 1) * spec.BesselK(xe, 0)
        MULT = ((z ^ 0.5) / (s ^ 1.5)) * ((mu * Bo * q) / (2 * 3.14 * k * h * rw))
        CHISL = spec.BesselK(xe, 0) * spec.BesselI(xw, 0) - spec.BesselI(xe, 0) * spec.BesselK(xw, 0)

        p = MULT * (CHISL / ZNAM)

        StableRate_Ps11 = p

    End Function
    Public Function Psr11(ByVal k As Double,
                          ByVal por As Double,
                          ByVal mu As Double,
                          ByVal Ct As Double,
                          ByVal Bo As Double,
                          ByVal rw As Double,
                          ByVal re As Double,
                          ByVal h As Double,
                          ByVal Po As Double,
                          ByVal q As Double,
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

        Dim z, x, xe, xw, ZNAM, MULT, CHISL, p As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw
        x = (s / z) ^ 0.5 * r

        ZNAM = spec.BesselI(xe, 0) * spec.BesselK(xw, 1) + spec.BesselI(xw, 1) * spec.BesselK(xe, 0)
        MULT = ((z ^ 0.5) / (s ^ 1.5)) * ((mu * Bo * q) / (2 * 3.14 * k * h * rw))
        CHISL = spec.BesselK(xe, 0) * spec.BesselI(x, 0) - spec.BesselI(xe, 0) * spec.BesselK(x, 0)

        p = MULT * (CHISL / ZNAM)

        Psr11 = p

    End Function

    Public Function Pt11(ByVal N As Integer,
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
                         ByVal q As Double) As Double
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
            fi = StableRate_Ps11(k, por, mu, Ct, Bo, rw, re, h, Po, q, si)
            Vi = Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        Pt11 = Sum

    End Function
    Public Function Ptr11(ByVal N As Integer,
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
                          ByVal q As Double,
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
            '    Psr11(k, por, mu, Ct, Bo, rw, re, h, Po, q, r, s As Double)
            fi = Psr11(k, por, mu, Ct, Bo, rw, re, h, Po, q, r, si)
            Vi = Coef(N, j)
            rr = (Log(2) / T) * Vi * fi
            Sum = Sum + rr

        Next j

        Ptr11 = Sum

    End Function

    Public Function Ps12(ByVal k As Double,
                         ByVal por As Double,
                         ByVal mu As Double,
                         ByVal Ct As Double,
                         ByVal Bo As Double,
                         ByVal rw As Double,
                         ByVal re As Double,
                         ByVal h As Double,
                         ByVal Po As Double,
                         ByVal q As Double,
                         ByVal s As Double) As Double

        'Решение для одного пласта с условием неперетока на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim z, xe, xw, ZNAM, MULT, CHISL, p As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw
        ZNAM = spec.BesselI(xw, 1) * spec.BesselK(xe, 1) - spec.BesselI(xe, 1) * spec.BesselK(xw, 1)
        MULT = ((z ^ 0.5) / (s ^ 1.5)) * ((mu * Bo * q) / (2 * 3.14 * k * h * rw))
        CHISL = spec.BesselK(xe, 1) * spec.BesselI(xw, 0) + spec.BesselI(xe, 1) * spec.BesselK(xw, 0)

        p = MULT * (CHISL / ZNAM)

        Ps12 = p

    End Function
    Public Function Psr12(ByVal k As Double,
                          ByVal por As Double,
                          ByVal mu As Double,
                          ByVal Ct As Double,
                          ByVal Bo As Double,
                          ByVal rw As Double,
                          ByVal re As Double,
                          ByVal h As Double,
                          ByVal Po As Double,
                          ByVal q As Double,
                          ByVal r As Double,
                          ByVal s As Double) As Double

        'Решение для одного пласта с условием неперетока на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim z, xe, xw, ZNAM, MULT, CHISL, x, p As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw
        x = (s / z) ^ 0.5 * r

        ZNAM = spec.BesselI(xw, 1) * spec.BesselK(xe, 1) - spec.BesselI(xe, 1) * spec.BesselK(xw, 1)
        MULT = ((z ^ 0.5) / (s ^ 1.5)) * ((mu * Bo * q) / (2 * 3.14 * k * h * rw))
        CHISL = spec.BesselK(xe, 1) * spec.BesselI(x, 0) + spec.BesselI(xe, 1) * spec.BesselK(x, 0)

        p = MULT * (CHISL / ZNAM)

        Psr12 = p

    End Function
    Public Function Pt12(ByVal N As Integer,
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
                         ByVal q As Double) As Double
        'Вычисляет значение забойного давления от времени, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие неперетока на границе пласта

        Dim si As Double
        Dim fi, Vi, r, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = Ps12(k, por, mu, Ct, Bo, rw, re, h, Po, q, si)
            Vi = Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        Pt12 = Sum

    End Function
    Public Function Ptr12(ByVal N As Integer,
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
                          ByVal q As Double,
                          ByVal r As Double) As Double
        'Вычисляет значение забойного давления от времени, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие неперетока на границе пласта

        Dim si As Double
        Dim fi, Vi, rr, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = Psr12(k, por, mu, Ct, Bo, rw, re, h, Po, q, r, si)
            Vi = Coef(N, j)
            rr = (Log(2) / T) * Vi * fi
            Sum = Sum + rr

        Next j

        Ptr12 = Sum

    End Function

    Public Function Qs11(ByVal k As Double,
                         ByVal por As Double,
                         ByVal mu As Double,
                         ByVal Ct As Double,
                         ByVal Bo As Double,
                         ByVal rw As Double,
                         ByVal re As Double,
                         ByVal h As Double,
                         ByVal Pwf As Double,
                         ByVal Po As Double,
                         ByVal s As Double,
                         ByVal i As Integer) As Double

        'Решение для одного пласта с постоянным давлением на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        'i=1 - Давление
        'i=2 - Дебиты
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim z, xe, xw, ZNAM, MULT, CHISL, p, q As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw

        If i = 1 Then

            ZNAM = spec.BesselI(xw, 0) * spec.BesselK(xe, 0) - spec.BesselI(xe, 0) * spec.BesselK(xw, 0)
            MULT = (Pwf - Po) / s
            CHISL = spec.BesselK(xe, 0) * spec.BesselI(xw, 0) - spec.BesselI(xe, 0) * spec.BesselK(xw, 0)

            p = MULT * (CHISL / ZNAM)
            Qs11 = p

        End If

        If i = 2 Then

            MULT = ((2 * 3.14 * k * h * rw) * (Pwf - Po)) / ((mu * Bo) * ((z * s) ^ 0.5))
            CHISL = spec.BesselK(xe, 0) * spec.BesselI(xw, 1) + spec.BesselI(xe, 0) * spec.BesselK(xw, 1)
            ZNAM = spec.BesselI(xw, 0) * spec.BesselK(xe, 0) - spec.BesselI(xe, 0) * spec.BesselK(xw, 0)

            q = MULT * (CHISL / ZNAM)
            Qs11 = q

        End If

    End Function

    Public Function Qt11(ByVal N As Integer,
                         ByVal T As Double,
                         ByVal k As Double,
                         ByVal por As Double,
                         ByVal mu As Double,
                         ByVal Ct As Double,
                         ByVal Bo As Double,
                         ByVal rw As Double,
                         ByVal re As Double,
                         ByVal h As Double,
                         ByVal Pwf As Double,
                         ByVal Po As Double,
                         ByVal i As Integer) As Double
        'Вычисляет значение забойного давления от времени, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие постоянного давления на границе пласта
        'i=1 - Давление
        'i=2 - Дебиты

        Dim si As Double
        Dim fi, Vi, r, Sum As Double
        Dim j As Integer

        If i = 1 Then
            For j = 1 To N

                si = (Log(2) / T) * j
                fi = Qs11(k, por, mu, Ct, Bo, rw, re, h, Pwf, Po, si, i)
                Vi = Coef(N, j)
                r = (Log(2) / T) * Vi * fi
                Sum = Sum + r

                Qt11 = Sum

            Next j
        End If

        If i = 2 Then
            For j = 1 To N

                si = (Log(2) / T) * j
                fi = Qs11(k, por, mu, Ct, Bo, rw, re, h, Pwf, Po, si, i)
                Vi = Coef(N, j)
                r = (Log(2) / T) * Vi * fi
                Sum = Sum + r

                Qt11 = Sum

            Next j

        End If

    End Function

    Public Function Qtr11(ByVal N As Integer,
                          ByVal T As Double,
                          ByVal k As Double,
                          ByVal por As Double,
                          ByVal mu As Double,
                          ByVal Ct As Double,
                          ByVal Bo As Double,
                          ByVal rw As Double,
                          ByVal re As Double,
                          ByVal Pwf As Double,
                          ByVal Po As Double,
                          ByVal x As Double) As Double
        'Вычисляет значение давления от времени в любой точке пласта, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие постоянного давления на границе пласта

        Dim si As Double
        Dim fi, Vi, r, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = Qsr11(k, por, mu, Ct, Bo, rw, re, Pwf, Po, x, si)
            Vi = Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        Qtr11 = Sum

    End Function

    Public Function Qsr11(ByVal k As Double,
                          ByVal por As Double,
                          ByVal mu As Double,
                          ByVal Ct As Double,
                          ByVal Bo As Double,
                          ByVal rw As Double,
                          ByVal re As Double,
                          ByVal Pwf As Double,
                          ByVal Po As Double,
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

        Dim z, xe, xw, ZNAM, MULT, CHISL, x, p As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw
        x = (s / z) ^ 0.5 * r

        ZNAM = spec.BesselI(xw, 0) * spec.BesselK(xe, 0) - spec.BesselI(xe, 0) * spec.BesselK(xw, 0)
        MULT = (Pwf - Po) / s
        CHISL = spec.BesselK(xe, 0) * spec.BesselI(x, 0) - spec.BesselI(xe, 0) * spec.BesselK(x, 0)

        p = MULT * (CHISL / ZNAM)

        Qsr11 = p

    End Function

    Public Function Qs12(ByVal k As Double,
                         ByVal por As Double,
                         ByVal mu As Double,
                         ByVal Ct As Double,
                         ByVal Bo As Double,
                         ByVal rw As Double,
                         ByVal re As Double,
                         ByVal h As Double,
                         ByVal Pwf As Double,
                         ByVal Po As Double,
                         ByVal s As Double,
                         ByVal i As Integer) As Double

        'Решение для одного пласта с неперетоком на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        'i=1 - Давление
        'i=2 - Дебиты
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim z, xe, xw, ZNAM, MULT, CHISL, p, q As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw

        If i = 1 Then

            ZNAM = spec.BesselI(xw, 0) * spec.BesselK(xe, 1) + spec.BesselI(xe, 1) * spec.BesselK(xw, 0)
            MULT = (Pwf - Po) / s
            CHISL = spec.BesselI(xw, 0) * spec.BesselK(xe, 1) + spec.BesselI(xe, 1) * spec.BesselK(xw, 0)

            p = MULT * (CHISL / ZNAM)
            Qs12 = p

        End If

        If i = 2 Then

            MULT = ((2 * 3.14 * k * h * rw) * (Pwf - Po)) / ((mu * Bo) * ((z * s) ^ 0.5))
            CHISL = spec.BesselK(xe, 1) * spec.BesselI(xw, 1) - spec.BesselI(xe, 1) * spec.BesselK(xw, 1)
            ZNAM = spec.BesselI(xw, 0) * spec.BesselK(xe, 1) + spec.BesselI(xe, 1) * spec.BesselK(xw, 0)

            q = MULT * (CHISL / ZNAM)
            Qs12 = q

        End If

    End Function

    Public Function Qt12(ByVal N As Integer,
                         ByVal T As Double,
                         ByVal k As Double,
                         ByVal por As Double,
                         ByVal mu As Double,
                         ByVal Ct As Double,
                         ByVal Bo As Double,
                         ByVal rw As Double,
                         ByVal re As Double,
                         ByVal h As Double,
                         ByVal Pwf As Double,
                         ByVal Po As Double,
                         ByVal i As Integer) As Double
        'Вычисляет значение забойного давления от времени, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие постоянного давления на границе пласта
        'i=1 - Давление
        'i=2 - Дебиты

        Dim si As Double
        Dim fi, Vi, r, Sum As Double
        Dim j As Integer

        If i = 1 Then
            For j = 1 To N

                si = (Log(2) / T) * j
                fi = Qs12(k, por, mu, Ct, Bo, rw, re, h, Pwf, Po, si, i)
                Vi = Coef(N, j)
                r = (Log(2) / T) * Vi * fi
                Sum = Sum + r

                Qt12 = Sum

            Next j
        End If

        If i = 2 Then
            For j = 1 To N

                si = (Log(2) / T) * j
                fi = Qs12(k, por, mu, Ct, Bo, rw, re, h, Pwf, Po, si, i)
                Vi = Coef(N, j)
                r = (Log(2) / T) * Vi * fi
                Sum = Sum + r

                Qt12 = Sum

            Next j

        End If

    End Function
    Public Function Qsr12(ByVal k As Double,
                          ByVal por As Double,
                          ByVal mu As Double,
                          ByVal Ct As Double,
                          ByVal Bo As Double,
                          ByVal rw As Double,
                          ByVal re As Double,
                          ByVal Pwf As Double,
                          ByVal Po As Double,
                          ByVal r As Double,
                          ByVal s As Double) As Double

        'Решение для одного пласта с неперетоком на границе
        'Вычисляет давление в пространстве Лапласа p(s)
        'z - коэффициент пьезопроводности
        'ZNAM - знаменатель
        'MULT - множитель
        'CHISL - числитель
        Dim spec As New [lib].SpecialFunctions
        spec.Class_init()

        Dim z, xe, xw, ZNAM, MULT, CHISL, x, p As Double

        z = k / (por * mu * Ct)
        xe = (s / z) ^ 0.5 * re
        xw = (s / z) ^ 0.5 * rw
        x = (s / z) ^ 0.5 * r

        ZNAM = spec.BesselI(xw, 0) * spec.BesselK(xe, 1) + spec.BesselI(xe, 1) * spec.BesselK(xw, 0)
        MULT = (Pwf - Po) / s
        CHISL = spec.BesselK(xe, 1) * spec.BesselI(x, 0) + spec.BesselI(xe, 1) * spec.BesselK(x, 0)

        p = MULT * (CHISL / ZNAM)

        Qsr12 = p

    End Function

    Public Function Qtr12(ByVal N As Integer,
                          ByVal T As Double,
                          ByVal k As Double,
                          ByVal por As Double,
                          ByVal mu As Double,
                          ByVal Ct As Double,
                          ByVal Bo As Double,
                          ByVal rw As Double,
                          ByVal re As Double,
                          ByVal Pwf As Double,
                          ByVal Po As Double,
                          ByVal x As Double) As Double
        'Вычисляет значение давления от времени в любой точке пласта, все переменные размерности[Си], выполняя обратное
        'преобразование Лапласа и переводя P(s)-->P(t)
        'T - значение времени в секундах
        'N - точность
        'Условие постоянного давления на границе пласта

        Dim si As Double
        Dim fi, Vi, r, Sum As Double
        Dim j As Integer

        For j = 1 To N

            si = (Log(2) / T) * j
            fi = Qsr12(k, por, mu, Ct, Bo, rw, re, Pwf, Po, x, si)
            Vi = Coef(N, j)
            r = (Log(2) / T) * Vi * fi
            Sum = Sum + r

        Next j

        Qtr12 = Sum

    End Function

End Module

