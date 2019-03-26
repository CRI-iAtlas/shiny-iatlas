**To create your own sample groups to use in analyses, upload a comma-separated value file ( CSV file ) in the form shown in this example, but with commas separating the values. The CSV file must be a plain text file.**


<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> ParticipantID </th>
   <th style="text-align:left;"> Alternate Cancer Subtyping </th>
   <th style="text-align:left;"> Gender </th>
   <th style="text-align:left;"> Molecular Alteration </th>
   <th style="text-align:left;"> Predicted Immunotherapy Outcome </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TCGA-01-0639 </td>
   <td style="text-align:left;"> group_a </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> activating </td>
   <td style="text-align:left;"> Non-responder </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TCGA-02-0007 </td>
   <td style="text-align:left;"> group_a </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> repressing </td>
   <td style="text-align:left;"> Responder </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TCGA-02-0011 </td>
   <td style="text-align:left;"> group_b </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Partial responder </td>
  </tr>
</tbody>
</table>

## Columns
The first column needs to contain (12-digit) TCGA participant barcode IDs. Subsequent columns (one or more) contain sample groupings that you can later select for analysis. For example in the above, the third column contains gender, and the values in that column can thus be used to compare immune response in cancer in women to that in men.

## Headers 
The first column can have any heading. Column headings that follow will be appear as the labels of the group selection dropdown menus in iAtlas.

## Rows
Rows can be supplied for any subset of TCGA samples. Rows will be filtered out if they have an `NA` value for the selected group. For example, if `Molecular Alteration` is selected, the bottom row will be removed. 

## Example files
[Example1](https://raw.githubusercontent.com/CRI-iAtlas/shiny-iatlas/develop/data/example_user_group.csv)

[Example2](https://raw.githubusercontent.com/CRI-iAtlas/shiny-iatlas/develop/data/example_user_group2.csv)
