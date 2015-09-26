@echo off

IF NOT EXIST .paket\paket.exe (
  .paket\paket.bootstrapper.exe
  if errorlevel 1 (
    exit /b %errorlevel%
  )
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

IF NOT EXIST build.fsx (
  .paket\paket.exe update
  packages\FAKE\tools\FAKE.exe init.fsx
)
packages\FAKE\tools\FAKE.exe build.fsx %*
