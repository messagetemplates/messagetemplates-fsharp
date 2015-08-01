dnx MessageTemplates.Tests test
packages\xunit.runner.console.2.0.0\tools\xunit.console.x86.exe FsMessageTemplates.Tests\bin\Release\FsMessageTemplates.Tests.dll

IF ['%APPVEYOR%'] == ['True'] (
	vstest.console /Logger:AppVeyor MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.dll
) ELSE (
	vstest.console /Logger:timelineLogger MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.dll
)