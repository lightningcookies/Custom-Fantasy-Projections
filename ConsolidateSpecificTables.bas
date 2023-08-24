Attribute VB_Name = "Module11"
Sub ConsolidateSpecificTables()
    Dim wsSrc As Worksheet
    Dim wsOverall As Worksheet
    Dim rngSrc As Range
    Dim rngDest As Range
    Dim tblOverall As ListObject
    Dim destLastRow As Long
    Dim tableIndex As Long ' To keep track of table index
    
    ' Set the destination worksheet "Overall"
    On Error Resume Next
    Set wsOverall = ThisWorkbook.Sheets("Overall")
    On Error GoTo 0
    
    ' Create the destination sheet if it doesn't exist
    If wsOverall Is Nothing Then
        Set wsOverall = ThisWorkbook.Sheets.Add(After:=ThisWorkbook.Sheets(ThisWorkbook.Sheets.Count))
        wsOverall.Name = "Overall"
    Else
        ' Clear the contents of the existing destination sheet
        wsOverall.Cells.Clear
    End If
    
    destLastRow = 1 ' Starting row for the consolidated table
    
    ' Loop through each sheet in the workbook
    For Each wsSrc In ThisWorkbook.Worksheets
        ' Skip the destination sheet
        If wsSrc.Name <> wsOverall.Name Then
            ' Reset the table index for each sheet
            tableIndex = 1
            
            ' Loop through each table in the source sheet
            For Each tbl In wsSrc.ListObjects
                ' Check if the table index is between 4 and 7
                If tableIndex >= 4 And tableIndex <= 7 Then
                    ' Set the range of the current table
                    Set rngSrc = tbl.Range
                    
                    ' Copy the header row to the destination sheet if it's the first table being consolidated
                    If destLastRow = 1 Then
                        rngSrc.Rows(1).Copy wsOverall.Cells(destLastRow, 1)
                        destLastRow = destLastRow + 1
                    End If
                    
                    ' Copy the data to the destination sheet starting from the next row after the last used row
                    rngSrc.Offset(1, 0).Resize(rngSrc.Rows.Count - 1).Copy wsOverall.Cells(destLastRow, 1)
                    
                    ' Find the new last used row in the destination sheet after pasting the data
                    destLastRow = wsOverall.Cells(wsOverall.Rows.Count, "A").End(xlUp).Row
                End If
                
                tableIndex = tableIndex + 1 ' Increment the table index
            Next tbl
        End If
    Next wsSrc
    
    ' Autofit columns in the destination sheet
    wsOverall.UsedRange.Columns.AutoFit
    
    ' Set the range of the consolidated table
    Set rngDest = wsOverall.Range(wsOverall.Cells(1, 1), wsOverall.Cells(destLastRow, wsOverall.UsedRange.Columns.Count))
    
    ' Convert the range to a table
    Set tblOverall = wsOverall.ListObjects.Add(xlSrcRange, rngDest, , xlYes)
    
    ' Define a name for the table
    tblOverall.Name = "ConsolidatedTable"
    
    ' Set the table style (optional)
    tblOverall.TableStyle = "TableStyleMedium2"
    
    ' Sort the table by the "F_Custom" column in descending order
    Dim sortColumn As ListColumn
    Set sortColumn = tblOverall.ListColumns("F_Custom")
    
    ' Clear existing sorting (optional)
    tblOverall.Sort.SortFields.Clear
    
    ' Add a new sorting field
    tblOverall.Sort.SortFields.Add Key:=sortColumn.Range, _
                                  SortOn:=xlSortOnValues, _
                                  Order:=xlDescending, _
                                  DataOption:=xlSortNormal
    
    ' Perform the sort
    With tblOverall.Sort
        .Header = xlYes ' Change to xlNo if there's no header row
        .MatchCase = False
        .Orientation = xlTopToBottom
        .SortMethod = xlPinYin
        .Apply
    End With
End Sub


