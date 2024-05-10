#!/bin/bash

export COB_LDFLAGS=-Wl,--no-as-needed
export COBCPY=./Copybook

ocesql UpdMaj.cbl UpdMaj.cob
cobc -locesql -x -o run UpdMaj.cob

./run > result.txt
cat result.txt

psql -d country -U cobol -c "SELECT * FROM databank;" \
                         -c "SELECT * FROM phrase;" > tables_description_and_content.txt
