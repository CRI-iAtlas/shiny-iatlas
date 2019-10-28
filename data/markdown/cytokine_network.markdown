Select the subset of interest of analysis, depending on the Sample Group selected. The other two parameters required to build a network are:

- *Abundance threshold (%)*: Nodes in the network are selected if they meet the selected abundance threshold. The abundance threshold, in %, is the frequency of samples in the upper two tertiles of cell abundance or gene expression distributions. For example, for an abundance of 66%, a node is entered into the subtype-network if at least 66% of samples within a subtype map to mid or high value bins in a tertile distribution.

- *Concordance threshold*: Edges in the scaffold network between any two abundant nodes is evaluated for inclusion based on the selected concordance threshold. A concordance threshold of 2, for example, selects edges that have both nodes simultaneously highly or lowly expressed at least twice as frequent than samples with one node highly expressed and the other lowly expressed. 

(For more information on the method to build the network, refer to the Data Description module)

In addition, a user can also select a set of cells or genes of interest. This selection will limit the network to the edges in the scaffold with the selected cells and genes that meet the abundance and concordance thresholds. By default, a network will use all cells and the immunomodulator genes (as described in the Immunomodulators module).

Manuscript context:  This module allows you to display networks similar to Figure 7A, 7B, 7C, and S7A. If you are looking at Immune Subtypes, select Immune Subtype C4, 66% abundance threshold, and 1.62 for concordance score to get the extracellular network for C4 present in Figure S7A.

