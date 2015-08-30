dnx MessageTemplates.Tests test
@if ERRORLEVEL 1 goto :failure

packages\xunit.runner.console\tools\xunit.console.x86.exe FsMessageTemplates.Tests\bin\Release\FsMessageTemplates.Tests.dll
@if ERRORLEVEL 1 goto :failure

IF ['%APPVEYOR%'] == ['True'] (
	vstest.console /Logger:AppVeyor MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.exe
  @if ERRORLEVEL 1 goto :failure
) ELSE (
	vstest.console MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.exe
  @if ERRORLEVEL 1 goto :failure
)

IF ['%APPVEYOR%'] == ['True'] (
	"set PATH=%PATH%;C:\\Program Files (x86)\\Microsoft SDKs\\F#\\3.1\\Framework\\v4.0\\""
) 

FsiAnyCPU -O --tailcalls+ --crossoptimize+ --exec MessageTemplates.PerfTests\Script.fsx
@if ERRORLEVEL 1 goto :failure

FsiAnyCPU -O --tailcalls+ --crossoptimize+ --exec MessageTemplates.PerfTests\PerfTests.fsx
@if ERRORLEVEL 1 goto :failure

goto :eof

:failure
exit /b 1
