# To create your own groups submit a csv that looks like:

| ID | group1 | group2 | groupn |
| ---:| ---:| ---:| ---:|
| TCGA-01-0639  | cancer_type_1 | male   | groupa |
| TCGA-02-0007  | cancer_type_1 | female | groupb |
| ...           | ...           | ...    | ...    |
| TCGA-02-0011  | cancer_type_2 | female | NA     |


## Columns:
The first column needs to be TCGA patient ids. Every other column will represent a goruping tha can be selected for analysis. For example in the above group2 is gender, and if that group is selected all analyses will be comparing males vs females.

## Headers: 
The first column can be called anything, it will always be the for ids. The headrs of the rest of the columns will be what appears in the group selection dropdown 

## Rows:
Rows will be filtered out if they have an NA value for the selected group. For example if groupn is selected the bottom row will be removed.

