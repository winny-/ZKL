echo Create directories (if needed), copy zkl.exe to ..\Lib
if not exist "..\Bin" mkdir ..\Bin
if not exist "..\Lib" mkdir ..\Lib
copy /y %1zkl.exe ..\Bin
copy /y %1zkl.lib ..\Lib
rem it would probably be a good idea to copy zkl.lib to ..\Lib
rem and have extensions link to it there

