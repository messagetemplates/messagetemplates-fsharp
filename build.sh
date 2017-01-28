#!/bin/bash

for path in src/*/*.fsproj; do
    dotnet restore ${path}
    dotnet build -c Release ${path} 
done

for path in test/*/*.fsproj; do
    dotnet restore ${path}
    dotnet test -c Release ${path}
done
