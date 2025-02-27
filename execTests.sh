#!/bin/bash

for fic in ./Testing-files/*
do
    echo "---------------------------------"
    echo "Processing file $fic"
    ./gas_project < "$fic"
    echo ""
    echo "---------------------------------"
done