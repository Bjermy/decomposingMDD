module add python/2.7.10

#Munge sumstats for MDD2018_ex23andMeandUKBiobank
/filepath/munge_sumstats.py \
--sumstats /filepath/PGCno23andMeorUKBiobank \
--N-cas-col Nca \
--N-con-col Nco \
--signed-sumstats OR,1 \
--snp SNP \
--info INFO \
--merge-alleles /filepath/w_hm3.snplist \
--a1 A2 \
--a2 A1 \
--p P \
--out /filepath/mungedPGCno23andMeorUKBiobank

#Munge sumstats for Uk Biobank Broad depression phenotype
/filepath/munge_sumstats.py \
--sumstats /filepath/UKBiobank_BroadDepression.txt \
--N 322580 \
--signed-sumstats Beta,0 \
--snp rsid \
--info ImputationAccuracy \
--merge-alleles /filepath/w_hm3.snplist \
--a1 A2 \
--a2 A1 \
--p P \
--out /filepath/munged_UKBiobank_BroadDepression

#Munge sumstats for 23andMe
/filepath/munge_sumstats.py \
--sumstats /filepath/daner_23andMe_v5_170227_pgc_aligned_.assoc \
--N-col N \
--signed-sumstats OR,1 \
--snp SNP \
--info INFO \
--merge-alleles /filepath/w_hm3.snplist \
--a1 A2 \
--a2 A1 \
--p P \
--out 23andMeOnly

#Munge sumstats for the 32 Depression phenotypes
for i in `seq 1 32`
do
/filepath/munge_sumstats.py \
--sumstats /filepath/sumstat_pheno${i}_residualised.txt \
--N-col OBS_CT \
--signed-sumstats BETA,0 \
--merge-alleles /filepath/w_hm3.snplist \
--snp ID \
--a1 REF \
--a2 ALT \
--p P \
--out /filepath/munged_sumstat_pheno${i}_residualised

done

#Compute genetic correlations between phenotypes 1-32 and MDDno23andMeorUKBiobank (PGC)

for i in `seq 1 32`
do
python /filepath/ldsc.py \
--rg /filepath/mungedPGCno23andMeorUKBiobank.sumstats.gz, /filepath/munged_sumstat_pheno${i}_residualised.sumstats.gz \
--ref-ld-chr /filepath/eur_w_ld_chr/ \
--w-ld-chr /filepath/eur_w_ld_chr/ \
--out /filepath/rg_PGCno23andMeorUKB_and_pheno${i}
done

#Compute genetic correlations between phenotypes 1-32 and broad depression

for i in `seq 1 32`
do

python /filepath/ldsc.py \
--rg /filepath/munged_UKBiobank_BroadDepression.sumstats.gz, /filepath/munged_sumstat_pheno${i}_residualised.sumstats.gz \
--ref-ld-chr /filepath/eur_w_ld_chr/ \
--w-ld-chr /filepath/eur_w_ld_chr/ \
--out rg_broaddep_and_pheno${i}

done

#Compute genetic correlations between phenotypes 1-32 and 23andMe only

for i in `seq 1 32`
do

python /filepath/ldsc.py \
--rg /filepath/23andMeOnly.sumstats.gz, /filepath/munged_sumstat_pheno${i}_residualised.sumstats.gz \
--ref-ld-chr /filepath/eur_w_ld_chr/ \
--w-ld-chr /filepath/eur_w_ld_chr/ \
--out rg_23andMe_and_pheno${i}

done
