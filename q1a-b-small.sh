input=`ls B-small*.in | sort -r | head -n1`
stack exec q1a-b < ${input} > ${input%.*}.out
