Attribute VB_Name = "Module1"
Sub ConsolidateTables()
    Dim wsSrc As Worksheet
    Dim wsDest4 As Worksheet
    Dim wsDest5 As Worksheet
    Dim wsDest6 As Worksheet
    Dim wsDest7 As Worksheet
    Dim rngSrc As Range
    Dim rngDest4 As Range
    Dim rngDest5 As Range
    Dim rngDest6 As Range
    Dim rngDest7 As Range
    Dim tbl4 As ListObject
    Dim tbl5 As ListObject
    Dim tbl6 As ListObject
    Dim tbl7 As ListObject
    Dim lastRow As Long
    Dim destLastRow4 As Long
    Dim destLastRow5 As Long
    Dim destLastRow6 As Long
    Dim destLastRow7 As Long
    Dim i As Long
    
    ' Set the destination worksheet where you want to consolidate the fourth table
    On Error Resume Next
    Set wsDest4 = ThisWorkbook.Sheets("Combined_QB")
    On Error GoTo 0
    
    ' Create the destination sheet if it doesn't exist
    If wsDest4 Is Nothing Then
        Set wsDest4 = ThisWorkbook.Sheets.Add(After:=ThisWorkbook.Sheets(ThisWorkbook.Sheets.Count))
        wsDest4.Name = "Combined_QB"
    Else
        ' Clear the contents of the existing destination sheet
        wsDest4.Cells.Clear
    End If
    
    destLastRow4 = 1 ' Starting row for the fourth table
    
    ' Loop through each sheet in the workbook
    For Each wsSrc In ThisWorkbook.Worksheets
        ' Skip the destination sheet
        If wsSrc.Name <> wsDest4.Name Then
            ' Check if the sheet has at least four tables
            If wsSrc.ListObjects.Count >= 4 Then
                ' Set the range of the fourth table
                Set rngSrc = wsSrc.ListObjects(4).Range
                
                ' Copy the header row to the destination sheet if it's the first table being consolidated
                If destLastRow4 = 1 Then
                    rngSrc.Rows(1).Copy wsDest4.Cells(destLastRow4, 1)
                    destLastRow4 = destLastRow4 + 1
                End If
                
                ' Copy the data to the destination sheet starting from the next row after the last used row
                rngSrc.Offset(1, 0).Resize(rngSrc.Rows.Count - 1).Copy wsDest4.Cells(destLastRow4, 1)
                
                ' Find the new last used row in the destination sheet after pasting the data
                destLastRow4 = wsDest4.Cells(wsDest4.Rows.Count, "A").End(xlUp).Row
            End If
        End If
    Next wsSrc
    
    ' Autofit columns in the destination sheet
    wsDest4.UsedRange.Columns.AutoFit
    
    ' Set the range of the consolidated fourth table
    Set rngDest4 = wsDest4.Range(wsDest4.Cells(1, 1), wsDest4.Cells(destLastRow4, wsDest4.UsedRange.Columns.Count))
    
    ' Convert the range to a table
    Set tbl4 = wsDest4.ListObjects.Add(xlSrcRange, rngDest4, , xlYes)
    
    ' Define a name for the table
    tbl4.Name = "ConsolidatedFourthTable"
    
    ' Set the table style (optional)
    tbl4.TableStyle = "TableStyleMedium2"
    
    ' Set the destination worksheet where you want to consolidate the fifth table
    On Error Resume Next
    Set wsDest5 = ThisWorkbook.Sheets("Combined_RB")
    On Error GoTo 0
    
    ' Create the destination sheet if it doesn't exist
    If wsDest5 Is Nothing Then
        Set wsDest5 = ThisWorkbook.Sheets.Add(After:=ThisWorkbook.Sheets(ThisWorkbook.Sheets.Count))
        wsDest5.Name = "Combined_RB"
    Else
        ' Clear the contents of the existing destination sheet
        wsDest5.Cells.Clear
    End If
    
    destLastRow5 = 1 ' Starting row for the fifth table
    
    ' Loop through each sheet in the workbook
    For Each wsSrc In ThisWorkbook.Worksheets
        ' Skip the destination sheet
        If wsSrc.Name <> wsDest5.Name Then
            ' Check if the sheet has at least five tables
            If wsSrc.ListObjects.Count >= 5 Then
                ' Set the range of the fifth table
                Set rngSrc = wsSrc.ListObjects(5).Range
                
                ' Copy the header row to the destination sheet if it's the first table being consolidated
                If destLastRow5 = 1 Then
                    rngSrc.Rows(1).Copy wsDest5.Cells(destLastRow5, 1)
                    destLastRow5 = destLastRow5 + 1
                End If
                
                ' Copy the data to the destination sheet starting from the next row after the last used row
                rngSrc.Offset(1, 0).Resize(rngSrc.Rows.Count - 1).Copy wsDest5.Cells(destLastRow5, 1)
                
                ' Find the new last used row in the destination sheet after pasting the data
                destLastRow5 = wsDest5.Cells(wsDest5.Rows.Count, "A").End(xlUp).Row
            End If
        End If
    Next wsSrc
    
    ' Autofit columns in the destination sheet
    wsDest5.UsedRange.Columns.AutoFit
    
    ' Set the range of the consolidated fifth table
    Set rngDest5 = wsDest5.Range(wsDest5.Cells(1, 1), wsDest5.Cells(destLastRow5, wsDest5.UsedRange.Columns.Count))
    
    ' Convert the range to a table
    Set tbl5 = wsDest5.ListObjects.Add(xlSrcRange, rngDest5, , xlYes)
    
    ' Define a name for the table
    tbl5.Name = "ConsolidatedFifthTable"
    
    ' Set the table style (optional)
    tbl5.TableStyle = "TableStyleMedium2"
    
    ' Set the destination worksheet where you want to consolidate the sixth table
    On Error Resume Next
    Set wsDest6 = ThisWorkbook.Sheets("Combined_WR")
    On Error GoTo 0
    
    ' Create the destination sheet if it doesn't exist
    If wsDest6 Is Nothing Then
        Set wsDest6 = ThisWorkbook.Sheets.Add(After:=ThisWorkbook.Sheets(ThisWorkbook.Sheets.Count))
        wsDest6.Name = "Combined_WR"
    Else
        ' Clear the contents of the existing destination sheet
        wsDest6.Cells.Clear
    End If
    
    destLastRow6 = 1 ' Starting row for the sixth table
    
    ' Loop through each sheet in the workbook
    For Each wsSrc In ThisWorkbook.Worksheets
        ' Skip the destination sheet
        If wsSrc.Name <> wsDest6.Name Then
            ' Check if the sheet has at least six tables
            If wsSrc.ListObjects.Count >= 6 Then
                ' Set the range of the sixth table
                Set rngSrc = wsSrc.ListObjects(6).Range
                
                ' Copy the header row to the destination sheet if it's the first table being consolidated
                If destLastRow6 = 1 Then
                    rngSrc.Rows(1).Copy wsDest6.Cells(destLastRow6, 1)
                    destLastRow6 = destLastRow6 + 1
                End If
                
                ' Copy the data to the destination sheet starting from the next row after the last used row
                rngSrc.Offset(1, 0).Resize(rngSrc.Rows.Count - 1).Copy wsDest6.Cells(destLastRow6, 1)
                
                ' Find the new last used row in the destination sheet after pasting the data
                destLastRow6 = wsDest6.Cells(wsDest6.Rows.Count, "A").End(xlUp).Row
            End If
        End If
    Next wsSrc
    
    ' Autofit columns in the destination sheet
    wsDest6.UsedRange.Columns.AutoFit
    
    ' Set the range of the consolidated sixth table
    Set rngDest6 = wsDest6.Range(wsDest6.Cells(1, 1), wsDest6.Cells(destLastRow6, wsDest6.UsedRange.Columns.Count))
    
    ' Convert the range to a table
    Set tbl6 = wsDest6.ListObjects.Add(xlSrcRange, rngDest6, , xlYes)
    
    ' Define a name for the table
    tbl6.Name = "ConsolidatedSixthTable"
    
    ' Set the table style (optional)
    tbl6.TableStyle = "TableStyleMedium2"
    
    ' Set the destination worksheet where you want to consolidate the seventh table
    On Error Resume Next
    Set wsDest7 = ThisWorkbook.Sheets("Combined_TE")
    On Error GoTo 0
    
    ' Create the destination sheet if it doesn't exist
    If wsDest7 Is Nothing Then
        Set wsDest7 = ThisWorkbook.Sheets.Add(After:=ThisWorkbook.Sheets(ThisWorkbook.Sheets.Count))
        wsDest7.Name = "Combined_TE"
    Else
        ' Clear the contents of the existing destination sheet
        wsDest7.Cells.Clear
    End If
    
    destLastRow7 = 1 ' Starting row for the seventh table
    
    ' Loop through each sheet in the workbook
    For Each wsSrc In ThisWorkbook.Worksheets
        ' Skip the destination sheet
        If wsSrc.Name <> wsDest7.Name Then
            ' Check if the sheet has at least seven tables
            If wsSrc.ListObjects.Count >= 7 Then
                ' Set the range of the seventh table
                Set rngSrc = wsSrc.ListObjects(7).Range
                
                ' Copy the header row to the destination sheet if it's the first table being consolidated
                If destLastRow7 = 1 Then
                    rngSrc.Rows(1).Copy wsDest7.Cells(destLastRow7, 1)
                    destLastRow7 = destLastRow7 + 1
                End If
                
                ' Copy the data to the destination sheet starting from the next row after the last used row
                rngSrc.Offset(1, 0).Resize(rngSrc.Rows.Count - 1).Copy wsDest7.Cells(destLastRow7, 1)
                
                ' Find the new last used row in the destination sheet after pasting the data
                destLastRow7 = wsDest7.Cells(wsDest7.Rows.Count, "A").End(xlUp).Row
            End If
        End If
    Next wsSrc
    
    ' Autofit columns in the destination sheet
    wsDest7.UsedRange.Columns.AutoFit
    
    ' Set the range of the consolidated seventh table
    Set rngDest7 = wsDest7.Range(wsDest7.Cells(1, 1), wsDest7.Cells(destLastRow7, wsDest7.UsedRange.Columns.Count))
    
    ' Convert the range to a table
    Set tbl7 = wsDest7.ListObjects.Add(xlSrcRange, rngDest7, , xlYes)
    
    ' Define a name for the table
    tbl7.Name = "ConsolidatedSeventhTable"
    
    ' Set the table style (optional)
    tbl7.TableStyle = "TableStyleMedium2"
End Sub

