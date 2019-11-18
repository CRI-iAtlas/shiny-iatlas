# Shiny-iAtlas

Shiny-iAtlas is an interactive web portal that provides multiple analysis modules to visualize and explore immune response characterizations across cancer types. The app is hosted on shinyapps.io at [https://isb-cgc.shinyapps.io/shiny-iatlas/](https://isb-cgc.shinyapps.io/shiny-iatlas/) and can also be accessed via the main CRI iAtlas page at [http://www.cri-iatlas.org/](http://www.cri-iatlas.org/).

The portal is built entirely in **R** and **Shiny** using the **RStudio** development environment. Layout and interactivity within the portal are achieved by heavy use of the following packages:

- [shinydashboard](https://rstudio.github.io/shinydashboard/)
- [plotly](https://plot.ly/r/)
- [crosstalk](https://rstudio.github.io/crosstalk/)

## Requirements

Install:

- R: [https://www.r-project.org/](https://www.r-project.org/)

- RStudio: [https://rstudio.com/products/rstudio/download/](https://rstudio.com/products/rstudio/download/)

## Local Shiny-iAtlas Session

To run the app locally:

1. Clone this repository

1. Start Rstudio

1. Create a "New Project..." from the "File" menu

1. Create a project from an "Existing Directory"

1. Navigate to the cloned project folder.

1. Select the project folder.

1. In the "console" tab in Rstudio, a new R session will start. This will execute `source("renv/activate.R")` from the `.Rprofile.d/default.R` file and `renv` will bootstrap itself in.

1. At the prompt in the "console" tab, restore the dependecies by running:

   ```R
   renv::restore()
   ```

   This may take some time to complete - get something nice to drink :)

1. Start the app by running:

   ```R
   shiny::runApp()
   ```

## Development

When adding any new dependencies to the application, they may be added using:

```R
renv::install()
```

see [https://rstudio.github.io/renv/reference/install.html](https://rstudio.github.io/renv/reference/install.html) for more details.

Once a new package is added, run:

```R
renv::snapshot()
```

This will ensure the new package is added to the renv.lock file.

For more on package management with renv, please see [https://rstudio.github.io/renv/articles/renv.html](https://rstudio.github.io/renv/articles/renv.html)

## Deployment

To deploy, run this line:

```R
options(repos = BiocInstaller::biocinstallRepos())
```

## Data

Input data for the Shiny-iAtlas portal were accessed from multiple remote sources, including **Synapse**, the **ISB Cancer Genomics Cloud**, and **Google Drive**. For convenience, we have created locally cached versions of dataframe objects as **`feather`** files:

- `fmx_df.feather`
- `feature_df.feather`
- `feature_method_df.feather`
- `im_direct_relationships.feather`
- `im_potential_factors.feather`
- `im_expr_df.feather`
- `sample_group_df.feather`

## Methods

While many of the results presented in tables and plots are taken directly from IRWG data (including the main **feature matrix** and various feature and group annotations), we compute some values internally. Unless otherwise noted, the following methods/tools were used to compute summary statistics:

### Correlation — Spearman's rank-order correlation

```R
stats::cor(x, y, method = "spearman", use = "pairwise.complete.obs")
```

### Concordance Index (CI)

Concordance indexes for survival endpoints with respect to different immune readouts were computed using a custom package developed by Tai-Hsien Ou Yang at Columbia University. The **concordanceIndex** package includes a single synonymous function that can be used as follows:

```R
concordanceIndex::concordanceIndex(predictions, observations)
```

... where `predictions` and `observations` are numerical vectors of the same length.
