Push-Location $PSScriptRoot

if(Test-Path .\artifacts) { Remove-Item .\artifacts -Force -Recurse }

& dotnet restore

$revision = @{ $true = $env:APPVEYOR_BUILD_NUMBER; $false = "dev" }[$env:APPVEYOR_BUILD_NUMBER -ne $NULL];

Push-Location src\FsMessageTemplates

& dotnet pack -c Release -o ..\..\.\artifacts --version-suffix=$revision
if($LASTEXITCODE -ne 0) { exit 1 }

Pop-Location

Push-Location test\FsMessageTemplates.Tests

& dotnet test -c Release
if($LASTEXITCODE -ne 0) { exit 2 }

Pop-Location
