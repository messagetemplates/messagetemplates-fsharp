#!/bin/bash

for path in src/*/*.fsproj; do
    dirname="$(dirname "${path}")"
    dotnet restore ${dirname}
    dotnet build ${dirname} -c Release
done

for path in test/*/*.fsproj; do
    dotnet test -f netcoreapp1.0 -c Release ${path}
done
