mkdir -Force ".\build\" | Out-Null
Invoke-WebRequest "https://raw.githubusercontent.com/dotnet/cli/release/2.0.0/scripts/obtain/dotnet-install.ps1" -OutFile ".\build\installcli.ps1"
$env:DOTNET_INSTALL_DIR = "$pwd\.dotnetcli"
& .\build\installcli.ps1 -InstallDir "$env:DOTNET_INSTALL_DIR" -NoPath -Version 2.0.2
$env:Path = "$env:DOTNET_INSTALL_DIR;$env:Path"

