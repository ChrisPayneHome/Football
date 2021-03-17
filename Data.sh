#!/bin/bash

echo 'Importing Data!'
python3 Pipeline.py
echo 'Uploaded Data!'

echo 'Cleaning Data!'
Rscript Clean.R
echo 'Cleaned Data!'

echo "Done!"