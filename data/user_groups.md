# To create your own sample groups to use in analyses, upload a comma-separated value file ( csv file ) in the following form


*** Formatting attempt 2 - tweaked the example data. Needs better line breaks ***

ParticipantID,Alternate Cancer Subtyping,Gender,Molecular Alteration

TCGA-01-0639,group_a,male,activating

TCGA-02-0007,group_a,female,repressing

TCGA-01-0639,group_c,male,none

TCGA-02-0011,groub_b,female,NA

*** Formatting attempt 1 - is missing the commas ***

| ID | group1 | group2 | groupn |
| ---:| ---:| ---:| ---:|
| TCGA-01-0639  | cancer_type_1 | male   | groupa |
| TCGA-02-0007  | cancer_type_1 | female | groupb |
| ...           | ...           | ...    | ...    |
| TCGA-02-0011  | cancer_type_2 | female | NA     |

## Columns
The first column needs to contain TCGA participant barcode IDs. Subsequent columns (one or more) contain groupings that you can later select for analysis. For example in the above, the third column contains gender, and the values in that column can thus be used to compare immune response in cancer in women to that in men.

## Headers 
The first column can have any heading. A subsequent column heading will be appear as the label of the corresponding group selection dropdown when comparing groups.

## Rows
Rows can be supplied for any subset of TCGA samples. Values of NA in a row will be ignored in comparisons among the corresponding groupings (column values). 


