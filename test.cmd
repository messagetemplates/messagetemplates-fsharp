dnx MessageTemplates.Tests test

packages\xunit.runner.console\tools\xunit.console.x86.exe FsMessageTemplates.Tests\bin\Release\FsMessageTemplates.Tests.dll

IF ['%APPVEYOR%'] == ['True'] (
	vstest.console /Logger:AppVeyor MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.exe
) ELSE (
	vstest.console MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.exe
)

fsi -O --crossoptimize+ --exec MessageTemplates.PerfTests\Script.fsx
