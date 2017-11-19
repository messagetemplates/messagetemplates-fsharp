#!/bin/bash

for path in src/*/*.fsproj; do
    dotnet build -c Release ${path}
done

for path in test/*/*.fsproj; do
    dotnet test -c Release ${path}
done
