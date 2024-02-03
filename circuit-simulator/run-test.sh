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
