Set oShell = CreateObject ("Wscript.Shell") 
Dim strArgs
strArgs = "cmd /c D:\gitHubRepos\nbtools\emacs\cleanEmacsServerFile.cmd"
oShell.Run strArgs, 0, false