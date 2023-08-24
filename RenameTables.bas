Attribute VB_Name = "Module1"
Sub RenameTables()
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim tblNum As Integer
    
    ' Loop through all sheets in the workbook
    For Each ws In ThisWorkbook.Sheets
        tblNum = 1 ' Initialize table number
        
        ' Loop through all tables in the sheet
        For Each tbl In ws.ListObjects
            tbl.Name = ws.Name & "_Table" & tblNum ' Rename the table
            
            tblNum = tblNum + 1 ' Increment table number
        Next tbl
    Next ws
End Sub


