@call build
@if ERRORLEVEL 1 goto :failure

@REM Because it seems DNX changes ECHO to OFF
@ECHO ON
@call test
@if ERRORLEVEL 1 goto :failure

goto :eof

:failure
exit /b 1
