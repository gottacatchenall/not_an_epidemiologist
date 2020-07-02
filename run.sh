#! /usr/bin/bash

cd data/covid-19-data
git pull
cd ../..

Rscript covid.R
rm Rplots.pdf
