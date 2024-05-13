#!/bin/bash

psql -d country -U cobol -f gender_proportions.sql

export COB_LDFLAGS=-Wl,--no-as-needed
export COBCPY=./Copybook

ocesql GenRpt.cbl GenRpt.cob
cobc -locesql -x -o run GenRpt.cob

./run > result.txt
cat result.txt

psql -d country -U cobol -c "SELECT * FROM databank;" \
                         -c "SELECT * FROM phrase;" > tables_description_and_content.txt
