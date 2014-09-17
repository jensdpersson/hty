#!/usr/bin/env python

import os.listdir

basedir = "src/test/inte"

for fixture in os.listdir(basedir):
    print("Running tests for fixture \"%s\"" % (fixture,))
src/main/bin/htyserve src/test/railed/$fixture/etc &
HTY=$!
echo "Hty pid is $HTY"
sleep 5
workdir=target/railed/`date +%Y-%m-%dT%H-%M-%S`
mkdir -p $workdir
for test in `ls src/test/railed/$fixture/tests`
do
echo "Running test \"$test\""
specdir=src/test/railed/$fixture/tests/$test
testdir=$workdir/$test
mkdir $testdir
sh src/test/railed/$fixture/tests/$test/command.sh > $testdir/result-out.txt 2> $testdir/result-err.txt
cat $testdir/result-err.txt | grep '^<' > $testdir/result-headers.txt
if [ -e $specdir/facit-headers.txt ]; then
  diff -q $testdir/result-headers.txt $specdir/facit-headers.txt
  if [ "0" = "" 
fi
done
kill $HTY
done
