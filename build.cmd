.paket\paket.exe install
call dnu restore MessageTemplates
call dnu restore MessageTemplates.Tests

msbuild /m /p:Configuration=Release
