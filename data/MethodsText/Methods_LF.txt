**Title:** Leukocyte Fraction

**Description:** Overall leukocyte content in 10,817 TCGA tumor aliquots was assessed by identifying DNA methylation probes with the greatest differences between pure leukocyte cells and normal tissue, then estimating leukocyte content using a mixture model. From Illumina Infinium DNA methylation platform arrays HumanMethylation450, 2000 loci were identified (200 for HumanMethylation27) that were the most differentially methylated between leukocyte and normal tissues, 1000 in each direction. 

Using the tumor with the least evidence of leukocyte methylation as a
surrogate for the beta value for each locus in the pure tumor, 2000
estimates were made, solving for a coefficient pi. We took the mode of 200 estimates to avoid loci that violate the assumptions. Using the estimated  and the measured  for tumor and leukocyte, with the same linear model, solved for  (deconvoluted value) extracting the leukocyte fraction (LF). Estimates for DLBC, THYM, LAML were masked as their tissues of origin are expected to be related to leukocytes, and therefore there were not enough tissue-specific DNA methylation loci to distinguish the two.

Stromal fraction (SF) was defined as the total non-tumor cellular component, obtained by subtracting tumor purity from unity, with the leukocyte proportion of stromal content R=LF/SF. Tumor purity was generated using ABSOLUTE (Carter et al., 2012),(Taylor et al., 2018). R was estimated by the Pearson correlation coefficient between SF and LF, assessed for individual sample groups (TCGA tumor types, subtypes, and immune subtypes).

[Reference List](https://www.cell.com/immunity/references/S1074-7613\(18\)30121-3)

**Contributors:** Hui Shen, Wanding Zhou, Vesteinn Thorsson
