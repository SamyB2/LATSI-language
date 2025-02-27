#!/bin/sh

if [ "$1" = "build" ]; then
  dune clean
  if [ -e gas_project ]; then
    rm gas_project
  fi
  dune build main.exe
  ln -sf _build/default/main.exe gas_project
  echo "Build complete"
elif [ "$1" = "clean" ]; then
  dune clean
  rm gas_project
  echo "Clean complete"
else
  echo "Unknown command: $1"
  echo "Usage: $0 {build|clean}"
  exit 1
fi
