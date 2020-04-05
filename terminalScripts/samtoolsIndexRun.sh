#!/bin/bash

# Reading argument values using for loop and processing for samtools index expression.
# The index file so obtained are transformed into an executable.

for file in "$@"
do
/Users/soumyajauhari/Desktop/srcTools/samtools/samtools index -b $file
done
