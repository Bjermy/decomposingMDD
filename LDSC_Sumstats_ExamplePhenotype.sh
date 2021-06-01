#!/bin/bash -l

# Run regression on imputed SNPs using residuals as outcome:
 
for i in `seq 1 22`
do

filepath/plink2 \
--bgen /filepath/imputed_filename.bgen \
--sample /filepath/imputed_filename.sample \
--pheno /filepath/Pheno1_residualised.txt \
--linear \
--out /filepath/Pheno1/chr${i}

done

# merge summary stats for each chr

# 58k controls #
head -n1 chr1.PHENO1.glm.linear > sumstat_pheno1_residualised.txt
for fname in chr*.PHENO1.glm.linear
do
    tail -n+2 $fname >> sumstat_pheno1_residualised.txt
done

