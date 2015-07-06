Attribute VB_Name = "ShowInMenu"
Option Explicit
Public HawkPath As String

Dim VHawk As VisualHawk
Dim hawkC As New HawkClass

Public Sub LoadHawk()
' Gets called when we open the document (from ThisDocument.DocOpened)
  HawkPath = VBA.Environ("VISUAL_HAWKROOT") & "\"
  Set VHawk = CreateObject("Haskell.VisualHawk")
  Call hawkC.EnableConnects

   MsgBox "The Hawk Server Version " & VHawk.VersionString & " is Ready to Go", vbOKOnly, "Hawk Demo"
End Sub
Public Sub UnloadHawk()
' Frees the Hawk server on close
  Set VHawk = Nothing
  Set hawkC = Nothing
End Sub
Public Sub ReLoadHawk()
' Makes any changes in the Haskell source immidiatly visible
' Don't know if type-library changes work correctly, might have to restart Visio then
  Call UnloadHawk
  Call LoadHawk
  
  MsgBox "Reloaded: Hawk Server " & VHawk.VersionString, vbOKOnly, "Hawk Demo"
End Sub
Public Sub GenHawk()
Dim Code As String
  Code = HawkPath & "\Nest\CPU.hs"
  VHawk.GenHawk Code, False
  VBA.Shell ("notepad.exe " & Code), vbNormalFocus
End Sub
Public Sub ExecHawk()
Dim Code As String
  Code = HawkPath & "\Nest\CPU.hs"
  VHawk.GenHawk Code, True
  VBA.Shell (HawkPath & "\lib\simulate.bat " & Code)
End Sub
Public Sub CheckWire()
  VHawk.CheckWire
End Sub
Public Sub UpdateMenu()
  VHawk.UpdateMenu
End Sub
Public Sub RewriteWith(Rule As String)
  VHawk.RewriteWith Rule
End Sub
Public Sub OpenProbe()
Dim fil As String
  fil = ActiveWindow.Selection.Item(1)
  
  If Dir(HawkPath & "\Nest\Probes\" & fil) <> "" Then
    VBA.Shell ("notepad.exe " & HawkPath & "\Nest\Probes\" & fil), vbNormalFocus
  Else
    MsgBox "No probe available for this wire (yet)", vbOKOnly, "Probe"
  End If
End Sub
Public Sub OpenUnit()
Dim fil As String
  fil = ActiveWindow.Selection.Item(1).Data1
  
  If Dir(HawkPath & "\ReWriteRules\" & fil & ".vsd") <> "" Then
    Document.Open (HawkPath & "\ReWriteRules\" & fil & ".vsd")
  Else
  If Dir(HawkPath & "\lib\TxtUnits\" & fil & ".txt") <> "" Then
    VBA.Shell ("notepad.exe " & HawkPath & "\lib\TxtUnits\" & fil & ".txt"), vbNormalFocus
  Else
    MsgBox "Primitive function: " & fil, , "Hawk Code"
  End If
  End If
End Sub
Public Sub CustomCode()
Dim Code As String
  Code = ActiveWindow.Selection.Item(1).Cells("Prop.Code").Formula
  Code = Mid(Code, 2, Len(Code) - 2)
  ActiveWindow.Selection.Item(1).Data1 = Code
End Sub
Public Sub CustomCodeText()
Dim Code As String
  Code = ActiveWindow.Selection.Item(1).Cells("Prop.Code").Formula
  Code = Mid(Code, 2, Len(Code) - 2)
  ActiveWindow.Selection.Item(1).Data1 = Code
  ActiveWindow.Selection.Item(1).Text = Mid(Code, 1, 4)
End Sub
