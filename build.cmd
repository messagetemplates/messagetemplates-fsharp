
.paket\paket.exe install
@if ERRORLEVEL 1 goto :failure

call dnu restore MessageTemplates
@if ERRORLEVEL 1 goto :failure

call dnu restore MessageTemplates.Tests
@if ERRORLEVEL 1 goto :failure

msbuild /m /p:Configuration=Release
@if ERRORLEVEL 1 goto :failure

goto :eof

:failure
exit /b 1
