#!/bin/bash

## Extracting folder location
folder=$(dirname "${1}")

## Running script
multiBamSummary bins --bamfiles \
$1 \
--binSize 2000 \
-out ${folder}/readCounts.npz \
--outRawCounts ${folder}/readCounts.tab
