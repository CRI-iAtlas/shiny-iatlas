**To define your own groups, upload a CSV file that looks like the table below:**

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> ID </th>
   <th style="text-align:left;"> group1 </th>
   <th style="text-align:left;"> group2 </th>
   <th style="text-align:left;"> groupn </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TCGA-01-0639 </td>
   <td style="text-align:left;"> cancer_type_1 </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> groupa </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TCGA-02-0007 </td>
   <td style="text-align:left;"> cancer_type_1 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> groupb </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TCGA-02-0011 </td>
   <td style="text-align:left;"> cancer_type_2 </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>


### Columns

The first column needs to be TCGA patient IDs. Every other column will represent a grouping that can be selected for analysis. For example, in the above `group2` is gender; if that group is selected, then analysis modules can be used to compare males vs. females.

### Headers

The first column can be named anything: it will always be the for TCGA barcode IDs. The headers of the remaining columns will determine what options appear in the group selection dropdown.

### Rows

Rows will be filtered out if they have an `NA` value for the selected group. For example, if `groupn` is selected, the bottom row will be removed.

