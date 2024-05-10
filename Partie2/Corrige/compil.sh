#!/bin/bash

export COB_LDFLAGS=-Wl,--no-as-needed
export COBCPY=./Copybook

ocesql Corrige.cbl Corrige.cob
cobc -locesql -x -o run Corrige.cob

./run > result.txt
cat result.txt

psql -d country -U cobol -c "SELECT * FROM databank;" \
                         -c "SELECT * FROM phrase;" > tables_description_and_content.txt
