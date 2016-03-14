echo off

tasklist /FI "IMAGENAME eq emacs.exe" | find /I /N "emacs.exe"
if errorlevel 1 del C:\Users\nboyd\AppData\Roaming\.emacs.d\server\server
