dnx MessageTemplates.Tests test

packages\xunit.runner.console\tools\xunit.console.x86.exe FsMessageTemplates.Tests\bin\Release\FsMessageTemplates.Tests.dll

IF ['%APPVEYOR%'] == ['True'] (
	vstest.console /Logger:AppVeyor MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.exe
) ELSE (
	vstest.console /Logger:timelineLogger MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.exe
)
