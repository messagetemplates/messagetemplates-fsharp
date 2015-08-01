dnx MessageTemplates.Tests test
xunit.console.clr4 /appveyor FsMessageTemplates.Tests\bin\Release\FsMessageTemplates.Tests.dll

IF ['%APPVEYOR%'] == ['True'] (
	vstest.console /Logger:AppVeyor MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.dll
) ELSE (
	vstest.console /Logger:timelineLogger MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.dll
)
