mkdir -Force ".\build\" | Out-Null
Invoke-WebRequest "https://dot.net/v1/dotnet-install.ps1" -OutFile ".\build\installcli.ps1"
$env:DOTNET_INSTALL_DIR = "$pwd\.dotnetcli"
& .\build\installcli.ps1 -InstallDir "$env:DOTNET_INSTALL_DIR" -NoPath -Version 2.2.300
$env:Path = "$env:DOTNET_INSTALL_DIR;$env:Path"

