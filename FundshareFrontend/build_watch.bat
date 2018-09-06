dotnet script %~dp0\build.csx -- build watch

@echo off
rem do not pause if it's launched from command line
echo %cmdcmdline% | findstr /ic:"%~f0" >nul && ( pause >nul )
echo "Press any key to continue . . ."
