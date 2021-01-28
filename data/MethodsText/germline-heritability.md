**Title:** Heritability

**Descriptions:** We performed heritability analysis on 139 traits using a mixed-model approach
implemented in genome-wide complex trait analysis (GCTA) genomic-relatedness-based
restricted maximum-likelihood (GREML) method (Yang et al., 2010, 2011) to calculate the
proportion of immune trait variation that is attributable to common genetic variants (Zaitlen and
Kraft, 2012). Heritability analyses were conducted separately within each ancestry subgroup
(NEuropean=7,813, NAfrican=863, NAsian=570, and NAmerican=209 individuals), which were derived from
ancestry analysis

To reduce bias in the heritability estimates, we removed one of each pair of related individuals with Ajk > 0.05 (calculated from SNPs with MAF > 0.01) prior to
running GREML. We calculated heritability using an unconstrained approach (allowing heritability
to be < 0 - those values were truncated in iAtlas). We used the likelihood ratio test (LRT) implemented in GREML to test if heritability is different than zero for each of the
immune traits analyzed and used Benjamini-Hochberg adjustment (Benjamini and Hochberg,
1995) to calculate the FDR. All heritability analyses were run with age, tumor type, sex and PC 1-7 as
covariates.

We also used GREML to determine whether there are any contextual factors that interact
with genome-wide common variant effects, including the major immune subtypes as determined
by Thorsson et al and somatic mutations (divided into tertiles and dichotomized at 10 MB). We
implemented the gene x environment (GxE) feature calculation in the European in GREML. For
those immune traits for which we found nominally significant (p < 0.05) interactions, we
calculated heritability in each stratified subset, as well as with immune subtype as an additional
covariate. For GxE calculations, the LRT tests the significance of the variance of GxE interaction
effects.

[Reference Listing](https://www.cell.com/immunity/references/S1074-7613\(18\)30121-3)

**Contributors:** Rosalyn Sayaman
