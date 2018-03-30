# Shiny-iAtlas

Shiny-iAtlas is an interactive web portal that provides multiple analysis modules to visualize and explore immune response characterizations across cancer types. The app is hosted on shinyapps.io and can be accessed via the main CRI iAtlas page at http://www.cri-iatlas.org/.

The portal is built entirely in **R** and **Shiny** using the **RStudio** development environment. Layout and interactivity within the portal are achieved by heavy use of the following packages:

+ **`shinydashboard`**
+ **`plotly`**
+ **`crosstalk`**

Other data transformation and formatting operations, as well as many other general application tasks, are supported by a variety of packages in the **`tidyverse`**. For a full list of dependencies, see the [`packages.R`]() file.

## TCGA PancanAtlas Immune Response Working Group

## Data

Input data for the Shiny-iAtlas portal were accessed from multiple remote sources, including **Synapse**, the **ISB Cancer Genomics Cloud**, and **Google Drive**. For convenience, we have created locally cached versions of dataframe objects as **`feather`** files:

+ `fmx_df.feather`
+ `feature_df.feather`
+ `feature_method_df.feather`
+ `im_direct_relationships.feather`
+ `im_potential_factors.feather`
+ `im_expr_df.feather`
+ `sample_group_df.feather`

## Methods

While many of the results presented in tables and plots are taken directly from IRWG data (including the main **feature matrix** and various feature and group annotations), we compute some values internally. Unless otherwise noted, the following methods/tools were used to compute summary statistics:

#### Correlation — Spearman's rank-order correlation:

```R
stats::cor(x, y, method = "spearman", use = "pairwise.complete.obs"
```

#### Concordance Index (CI):

_Need to add..._

## Local Shiny-iAtlas Session

To run the app locally, clone this repository and use the following command in the `shiny-iatlas` directory:
```
shiny::runApp()
```
