Attribute VB_Name = "Module3"
Sub ApplyCustomMediumBlueStyle()
    Dim ws As Worksheet
    Dim tbl As ListObject
    
    ' Set the colors for the medium blue style
    Dim headerColor As Long
    Dim oddRowColor As Long
    Dim evenRowColor As Long
    
    ' Define colors using RGB values (adjust as needed)
    headerColor = RGB(0, 102, 204)       ' Medium blue for header
    oddRowColor = RGB(204, 221, 255)     ' Light blue for odd rows
    evenRowColor = RGB(255, 255, 255)    ' White for even rows
    
    ' Loop through each worksheet in the workbook
    For Each ws In ThisWorkbook.Worksheets
        ' Loop through each table (ListObject) in the worksheet
        For Each tbl In ws.ListObjects
            ' Apply formatting attributes to the table
            tbl.HeaderRowRange.Interior.Color = headerColor
            tbl.DataBodyRange.Interior.Color = xlNone ' Clear existing colors
            For i = 1 To tbl.ListRows.Count
                If i Mod 2 = 1 Then
                    tbl.ListRows(i).Range.Interior.Color = oddRowColor
                Else
                    tbl.ListRows(i).Range.Interior.Color = evenRowColor
                End If
            Next i
        Next tbl
    Next ws
End Sub

