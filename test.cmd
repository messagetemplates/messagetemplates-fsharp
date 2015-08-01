dnx MessageTemplates.Tests test
nunit-console-x86 FsMessageTemplates.Tests\bin\Release\FsMessageTemplates.Tests.dll

IF ['%APPVEYOR%'] == ['True'] (
	vstest.console /Logger:AppVeyor MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.dll
) ELSE (
	vstest.console /Logger:timelineLogger MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.dll
)
