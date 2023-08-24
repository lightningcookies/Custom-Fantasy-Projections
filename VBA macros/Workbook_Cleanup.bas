Attribute VB_Name = "Module5"
Sub Workbook_Cleanup()
    Dim ws As Worksheet
    For Each ws In ThisWorkbook.Sheets
        ws.Select
        ws.Cells(1, 1).Select
        ActiveWindow.Zoom = 90
    Next ws
End Sub

