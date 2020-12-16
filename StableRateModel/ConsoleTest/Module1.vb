Module Module1

    Sub Main()

        Dim test_gl As Double
        'test_gl = StableRateModel_nWBS.Qt11(N:=12, T:=86400, por:=0.2, k:=5 * 10 ^ (-15), mu:=0.0015, Ct:=2.96 * 10 ^ (-9), rw:=0.1, Bo:=1, re:=300, Pwf:=5066250, Po:=25331250, i:=2, h:=10) * 86400
        test_gl = StableRateModel_nWBS.
        'test = JsonConvert.SerializeObject(test_esp)
        'test = u7_excel.u7_Excel_functions_ESP.ESP_head_m(qliq_m3day:=10, )
        Console.WriteLine("test: " + CStr(test_gl))
        Console.ReadKey(True)
    End Sub
End Module
