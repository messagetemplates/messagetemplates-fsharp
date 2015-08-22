@call build
@if ERRORLEVEL 1 goto :failure

@call test
@if ERRORLEVEL 1 goto :failure

goto :eof

:failure
exit /b 1
