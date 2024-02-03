#!/bin/bash

declare -i NUM_OF_TESTS=5

for i in $(seq 1 1 $NUM_OF_TESTS)
do
	racket test$i.rkt &> testres$i.out
	if [ -s testres$i.out ]; then
		echo "test failed"
		echo $i
		break
	else
		rm -f testres$i.out
		if [ $i -eq $NUM_OF_TESTS ]; then
			echo "All tests successful!"
		fi
	fi
done

cp interpreter.rkt interpreter-with-tests.rkt

echo ";======================" >> solution-with-tests.rkt
echo "; TESTING" >> solution-with-tests.rkt
echo ";======================" >> solution-with-tests.rkt

for i in $(seq 1 1 $NUM_OF_TESTS)
do
	tail -n +5 test$i.rkt >> solution-with-tests.rkt
done

racket solution-with-tests.rkt &> testres-all.out
if [ -s testres-all.out ]; then
	echo "merging failed"
else
	echo "merging successful"
fi
