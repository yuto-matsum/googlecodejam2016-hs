input=`ls A-small*.in | sort -r | head -n1`
stack exec q1a-a < ${input} > ${input%.*}.out
