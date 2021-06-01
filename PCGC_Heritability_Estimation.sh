module add python/3.7.3

#Estimate heritability using PCGC

for i in {1..32};
do
	python /filepath/pcgc_main.py \
	--prodr2 /filepath/ldscores. \
	--M 554059 \
	--sumstats /filepath/pheno${i}. \
	--out /filepath/results	
done

