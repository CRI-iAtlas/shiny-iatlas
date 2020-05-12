This module contains a large table of associations (>3 Million) between copy number variants (by gene) and immune readouts. Initially, all genes and all groups are shown. 
Use the filter controls to limit results to your interests.
                
Within each group (TCGA study, subtype, or immune subtype), a T-test was performed on immune readouts 
between samples with no copy number variation for a given gene,  
and samples with either amplified or deleted regions (two separate tests).
                
There are three components to the module: filter controls, a summary plot, and a table of results.
                
The filter controls remove statistics from the table and plot. It's possible to select multiple groups and genes.
                
The histogram shows the distribution of T statistics, given the filter settings.
                
The x-axis shows the T statistic value, positive if the normal group has higher immune readout scores.
The y-axis represents the number of genes with that statistic.

Immune landscape manuscript context: The results are comparible to those shown in Figure S4A.

Notes: A statistical test is performed only when the number of samples exceeds a minimum required group count (currently 3).
In rare instances all (or all but one) samples within a group contain the alteration and a test cannot be performed.
Only statistics with p-values less than 0.001 are retained.
