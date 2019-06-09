Public Class Form1
    Private ndarray(,) As pix
    Private sectionW As Integer
    Private sectionH As Integer
    Private rnd As New Random
    Private cl As Integer = 0
    Private Sub Form1_Paint(sender As Object, e As PaintEventArgs) Handles MyBase.Paint
        'Try
        ' If cl = 321 Then
        Me.DoubleBuffered = True
            Dim gr As Graphics = e.Graphics
            gr.Clear(Color.Black)
            sectionW = Math.Round(Math.Sqrt(Me.Width / 2 - 1), 0)
            sectionH = Math.Round(Math.Sqrt(Me.Height / 2 - 1), 0)
            ReDim Preserve ndarray(Me.Width / sectionW, Me.Height / sectionH)

            ndarray = shiftNdArray(ndarray)
            genLine()

            Render(e.Graphics, 25)


        ' Else
        'End If
        Application.DoEvents()
        Me.Invalidate()
        'Catch
        'End Try

    End Sub

    Sub Render(gr As Graphics, delay As Integer)
        For j = 0 To ndarray.GetUpperBound(1)
            For i = 0 To ndarray.GetUpperBound(0)
                If ndarray(i, j).val < 1 = False Then
                    Dim rcr = RandomChar()

                    'Dim clor = New SolidBrush(Color.FromArgb(0, (((ndarray(i, j).val + 100) / 4) * 255) / 100, 0))  ' Greens
                    'Dim clor = New SolidBrush(Color.FromArgb(rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))) 'Rainbow
                    Dim clor = New SolidBrush(ndarray(i, j).color)                                                  'Random but same

                    Dim offset = ndarray(i, j).offset
                    'gr.DrawRectangle(New Pen(clor), i * sectionW + offset, j * sectionH, gr.MeasureString(rcr, New Font("Consolas", ndarray(i, j).val * 2)).Width, gr.MeasureString(rcr, New Font("Consolas", ndarray(i, j).val * 2)).Height) 'Show Rectangles

                    gr.DrawString(rcr, New Font("Consolas", ndarray(i, j).val * 2), clor, i * sectionW + offset, j * sectionH)

                End If


            Next

        Next
        Threading.Thread.Sleep(delay)
    End Sub

    Public Function RandomChar() As String
        Dim validchars As String = " !""#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
        Return validchars(rnd.Next(0, validchars.Length))
    End Function

    Function shiftNdArray(nd(,) As pix) As pix(,)
        For j = 0 To nd.GetUpperBound(1)
            For i = 0 To nd.GetUpperBound(0)
                If j - 1 >= 0 Then
                    ndarray(i, j - 1) = nd(i, j)
                End If
            Next
        Next
        Return nd
    End Function

    Sub genLine()
        For i = 0 To ndarray.GetUpperBound(0)
            Dim CA = CountAround(ndarray, i, ndarray.GetUpperBound(1) - 1)
            CA = Math.Round(10 - CA, 0) * 1.5
            If rnd.Next(0, CA) < CA / rnd.Next(0, 5) Then
                If getAroundVal(ndarray, i, ndarray.GetUpperBound(1) - 1).ToString = "NaN" Then
                    ndarray(i, ndarray.GetUpperBound(1)).val = 1
                Else
line1:
                    Dim v = rnd.Next(0, rnd.Next(2, getAroundVal(ndarray, i, ndarray.GetUpperBound(1) - 1) * 3) + 1)
                    If v <= 1 Then
                        GoTo line1
                    End If
                    If v > 13 Then v = 1
                    ndarray(i, ndarray.GetUpperBound(1)).val = v
                    ndarray(i, ndarray.GetUpperBound(1)).offset = rnd.Next(0, v * 4)
                    ndarray(i, ndarray.GetUpperBound(1)).color = Color.FromArgb(rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                End If
            Else
                ndarray(i, ndarray.GetUpperBound(1)).val = 0
            End If
        Next
    End Sub

    Function getUpper(nd(,) As pix, x As Integer, y As Integer)
        Return If(y - 1 > 0, nd(x, y - 1), Nothing)
    End Function
    Function getLower(nd(,) As pix, x As Integer, y As Integer)
        Return If(y + 1 < nd.GetUpperBound(1), nd(x, y + 1), Nothing)
    End Function
    Function getLeft(nd(,) As pix, x As Integer, y As Integer)
        Return If(x - 1 > 0, nd(x - 1, y), Nothing)
    End Function
    Function getRight(nd(,) As pix, x As Integer, y As Integer)
        Return If(x + 1 < nd.GetUpperBound(0), nd(x + 1, y), Nothing)
    End Function

    Function getUpperLeft(nd(,) As pix, x As Integer, y As Integer)
        Return If(y - 1 > 0 And x - 1 > 0, nd(x - 1, y - 1), Nothing)
    End Function
    Function getUpperRight(nd(,) As pix, x As Integer, y As Integer)
        Return If(y - 1 > 0 And x + 1 < nd.GetUpperBound(0), nd(x + 1, y - 1), Nothing)
    End Function
    Function getLowerLeft(nd(,) As pix, x As Integer, y As Integer)
        Return If(y + 1 < nd.GetUpperBound(1) And x - 1 > 0, nd(x - 1, y + 1), Nothing)
    End Function
    Function getLowerRight(nd(,) As pix, x As Integer, y As Integer)
        Return If(y + 1 < nd.GetUpperBound(1) And x + 1 < nd.GetUpperBound(0), nd(x + 1, y + 1), Nothing)
    End Function

    Function hasAround(nd(,) As pix, x As Integer, y As Integer) As Boolean
        Return If(CountAround(nd, x, y) > 0, True, False)
    End Function

    Function CountAround(nd(,) As pix, x As Integer, y As Integer) As Integer
        Dim int As Integer = 0
        If getLeft(nd, x, y).val > 0 Then
            int += 1
        End If
        If getRight(nd, x, y).val > 0 Then
            int += 1
        End If
        If getUpper(nd, x, y).val > 0 Then
            int += 1
        End If
        If getLower(nd, x, y).val > 0 Then
            int += 1
        End If

        If getUpperLeft(nd, x, y).val > 0 Then
            int += 1
        End If
        If getUpperRight(nd, x, y).val > 0 Then
            int += 1
        End If
        If getLowerLeft(nd, x, y).val > 0 Then
            int += 1
        End If
        If getLowerRight(nd, x, y).val > 0 Then
            int += 1
        End If
        Return int
    End Function

    Function getAroundVal(nd(,) As pix, x As Integer, y As Integer)
        Dim int As Integer = 0
        int += getLeft(nd, x, y).val
        int += getRight(nd, x, y).val
        int += getUpper(nd, x, y).val
        int += getLower(nd, x, y).val

        int += getUpperLeft(nd, x, y).val
        int += getLowerRight(nd, x, y).val
        int += getUpperRight(nd, x, y).val
        int += getLowerLeft(nd, x, y).val
        Return int / CountAround(nd, x, y)
    End Function
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Click
        cl = 321
    End Sub

    Structure pix
        Property val As Integer
        Property offset As Integer
        Property color As Color
    End Structure
End Class
