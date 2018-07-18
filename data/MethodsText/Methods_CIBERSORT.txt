**Title:** Immune Cellular Fraction Estimates 

**Description:** The relative fraction of 22 immune cell types within the leukocyte compartment were estimated using CIBERSORT (Newman et al., 2015). These proportions were multiplied by LF to yield corresponding estimates in terms of overall fraction in tissue. Further, values were aggregated in various combinations to yield abundance of more comprehensive cellular classes, such as lymphocytes, macrophages and CD4 T cells. More specifically, we applied CIBERSORT to TCGA RNASeq data. CIBERSORT (cell-type identification by estimating relative subsets of RNA transcripts) uses a set of 22 immune cell reference profiles to derive a base (signature) matrix which can be applied to mixed samples to determine relative proportions of immune cells. As several key immune genes used in the signatures are absent from TCGA GAF (Generic Annotation File) Version 3.0,  we applied CIBERSORT to a re-quantification of the TCGA data using Kallisto (Bray et al., 2016) and the Gencode GTF (Harrow et al., 2012)(available from https://www.gencodegenes.org/), which includes the missing genes. A version of the entire TCGA RNA-seq data normalized to Gencode with Kallisto was computed on the ISB Cancer Genomics Cloud by Steve Piccolo's group at BYU (https://osf.io/gqrz9/wiki/home/) (Tatlow and Piccolo, 2016). 

In order to relate to results to other estimates in this study, three aggregation schemes were defined as follows

*Aggregate 1*
(6 classes; Used in Figure 2A, e.g.) Lymphocytes=B.cells.naive+B.cells.memory+T.cells.CD4.naive+T.cells.CD4.memory.resting+T.cells.CD4.memory.activated+T.cells.follicular.helper+T.cells.regulatory..Tregs+T.cells.gamma.delta+T.cells.CD8+NK.cells.resting+NK.cells.activated+Plasma.cells, 
Macrophages=Monocytes + Macrophages.M0 + Macrophages.M1 + Macrophages.M2
Dendritic.cells=Dendritic.cells.resting + Dendritic.cells.activated,
Mast.cells=Mast.cells.resting + Mast.cells.activated, 
Neutrophils=Neutrophils, 
Eosinophils=Eosinophils,

*Aggregate 2*
(9 classes; used for cytokine network, including Figure 7A,B,C) 
T.cells.CD8=T.cells.CD8, 
T.cells.CD4=T.cells.CD4.naive+T.cells.CD4.memory.resting+T.cells.CD4.memory.activated,
B.cells=B.cells.naive + B.cells.memory, 
NK.cells=NK.cells.resting+NK.cells.activated,
Macrophage=Macrophages.M0 + Macrophages.M1 + Macrophages.M2, 
Dendritic.cells=Dendritic.cells.resting + Dendritic.cells.activated,
Mast.cells=Mast.cells.resting + Mast.cells.activated, 
Neutrophils=Neutrophils, 
Eosinophils=Eosinophils

*Aggregate 3*
(11 classes)
T.cells.CD8=T.cells.CD8,
T.cells.CD4=T.cells.CD4.naive+T.cells.CD4.memory.resting+T.cells.CD4.memory.activated+T.cells.follicular.helper+T.cells.regulatory..Tregs, 
T.cells.gamma.delta=T.cells.gamma.delta,
B.cells=B.cells.naive + B.cells.memory, 
NK.cells=NK.cells.resting+NK.cells.activated,
Plasma.cells=Plasma.cells,
Macrophage=Monocytes + Macrophages.M0 + Macrophages.M1 + Macrophages.M2, Dendritic.cells=Dendritic.cells.resting + Dendritic.cells.activated, 
Mast.cells=Mast.cells.resting + Mast.cells.activated,
Neutrophils=Neutrophils,
Eosinophils=Eosinophils

[Reference List](https://www.cell.com/immunity/references/S1074-7613\(18\)30121-3)

**Contributors:** Andrew Gentles, Vesteinn Thorsson, Alex J. Lazar, David L. Gibbs
