Module Module1

    Sub Main()
        Dim test As Double
        test = net.One_reservoir.StableRate_PtWBS12(N:=12, T:=360000, k:=5 * 0.000000000000001, mu:=0.0015,
por:=0.2, Ct:=0.00005 * 0.000009869, Bo:=1.2, rw:=0.1, re:=300, h:=10, Po:=25331250,
q:=0.00012, Cs:=0.1 / 101325, i:=2)
        Console.WriteLine(CStr(test))
        Console.ReadLine()


    End Sub

End Module
