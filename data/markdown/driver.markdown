This module displays the degree of association between driver mutation in samples and an immune readout.

Every point in the scatter plot corresponds to a comparison of the values of that immune readout in samples in which a particular driver gene is mutated to the values in samples in which is not.
This comparison is made within each cohort among the Sample Groups.
Each point thus corresponds to a single driver gene and cohort. 
The driver-cohort combination can be seen by hovering on a point (separated by a dot).

- The x-axis show the effect size, defined as the ratio of the mean readout value in mutated vs non-mutated samples.
- The y-axis represents the P-value of the significance test comparing the readout in mutated vs non-mutated samples. A line is drawn for P=0.05, with the more significant values above that line.

Click on a point to see a violin plot for the immune readout value distribution in mutated vs non-mutated samples for the selected cohort and driver.

A statistical test is performed in a group only when the number of mutant and wild type samples exceed the parameter. In rare instances all (or all but one) samples within a group contain the mutation and a test cannot be performed.

There are two sections:

- Single Variable
- Mulit-variable

See each section for more specifics.

Driver mutations from Bailey, Tokheim, Porta-Pardo et al. (2018),
[Comprehensive Characterization of Cancer Driver Genes and Mutations, Cell 173.](https://doi.org/10.1016/j.cell.2018.02.060)
