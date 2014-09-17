#!/bin/bash

for fixture in `ls src/test/inte`; do

    echo "Running tests for fixture \"$fixture\""
    src/main/bin/htyserve src/test/inte/$fixture/etc &
    HTY=$!
    echo "Hty pid is $HTY"

    #Wait for fixture to start
    sleep 5

    workdir=target/inte/`date +%Y-%m-%dT%H-%M-%S`
    mkdir -p $workdir
    for test in `ls src/test/inte/$fixture/tests`; do
	echo "Running test \"$test\""
	specdir=src/test/inte/$fixture/tests/$test
	testdir=$workdir/$test
	mkdir $testdir
	sh src/test/inte/$fixture/tests/$test/command >$testdir/result
	diff $testdir/result $specdir/facit
	if [ "0" = "$?" ]; then
	    echo "<pass test=\"$fixture/$test\"/>"	    
	else
	    echo "<fail test=\"$fixture/$test\">"
	    echo `diff $testdir/result $specdir/facit`
	    echo "</fail>"
	fi
    done
    kill $HTY
done
