#!/bin/bash -l

module add python/3.7.3

python Software/S-PCGC/pcgc_sumstats_creator.py \
    --bfile /filepath/binary_files \
    --frqfile /filepath/MAF_EurAnc_UKB. \
    --extract /filepath/QCsnpsNoMHC.txt \
    --pheno /filepath/Pheno1.txt \
    --covar /filepath/Covar1.cov \
    --prev 0.57 \
    --pve /filepath/pve.txt \
    --covars-regress PC1,PC2,PC3,PC4,PC5,PC6 \
    --out /filepath/pheno1_unscreened



