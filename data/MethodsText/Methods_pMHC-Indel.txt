**Title:** Neoantigen Prediction from Indels

**Description:** Somatic indel variants were extracted from the MC3 variant file (mc3.v0.2.8.CONTROLLED.maf) with the following filters: FILTER in PASS, wga, native_wga_mix (with no combination with other tags); NCALLERS > 1; barcode in whitelist where do_not_use = False; Variant_Classification = Frame_Shift_Ins, Frame_Shift_Del, In_Frame_Ins, In_Frame_Del, Missense_Mutation, Nonsense_Mutation; and Variant_Type = INS, DEL. For each Indel, the downstream protein sequence was obtained using VEP v87 (Ensembl Variant Effect Predictor) (McLaren et al., 2016) using default settings.

Using 9-mer peptides extracted from VEP downstream protein sequences and the HLA calls from OptiType, for each sample, binding for each pair of mutant peptide-MHC were predicted using pVAC-Seq v4.0.8 pipeline (Hundal et al., 2016) with NetMHCpan v3.0 using default settings, of which an IC50 binding score threshold 500 nM was used to report the predicted binding epitopes as neoantigens.

[Reference List](https://www.cell.com/immunity/references/S1074-7613\(18\)30121-3)

**Contributors:** Nam Sy Vo, Ken Chen
