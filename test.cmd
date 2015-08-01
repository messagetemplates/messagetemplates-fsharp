dnx MessageTemplates.Tests test
packages\xunit.runner.console\tools\xunit.console.x86.exe FsMessageTemplates.Tests\bin\Release\FsMessageTemplates.Tests.dll
vstest.console /logger:AppVeyor /Logger:timelineLogger MessageTemplates.PerfTests\bin\Release\MessageTemplates.PerfTests.dll