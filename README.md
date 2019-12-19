# Shiny-iAtlas

Shiny-iAtlas is an interactive web portal that provides multiple analysis modules to visualize and explore immune response characterizations across cancer types. The app is hosted on shinyapps.io at [https://isb-cgc.shinyapps.io/shiny-iatlas/](https://isb-cgc.shinyapps.io/shiny-iatlas/) and can also be accessed via the main CRI iAtlas page at [http://www.cri-iatlas.org/](http://www.cri-iatlas.org/).

The portal is built entirely in **R** and **Shiny** using the **RStudio** development environment. Layout and interactivity within the portal are achieved by heavy use of the following packages:

- [shinydashboard](https://rstudio.github.io/shinydashboard/)
- [plotly](https://plot.ly/r/)
- [crosstalk](https://rstudio.github.io/crosstalk/)

## Install

### Install Core Apps and System libraries:

- R: [https://www.r-project.org/](https://www.r-project.org/)

- RStudio: [https://rstudio.com/products/rstudio/download/](https://rstudio.com/products/rstudio/download/)

- lib cairo: https://www.cairographics.org/

- Docker: [https://www.docker.com/products/docker-desktop](https://www.docker.com/products/docker-desktop)

### Initialize R Packages, Database and run App

To run the app locally:

1. Clone this repository

1. Open `shiny-iatlas.Rproj`

1. Install packages. In the RStudio console, run:

   ```R
   renv::restore()
   ```

   This may take some time to complete - get something nice to drink :)

1. Build the database locally with the following:

   1. Make the database function available by executing the following in the R console:

      ```R
      source("iatlas_db.R")
      ```

   1. Build the database by executing the following in the R console:

      ```R
      build_iatlas_db(reset = "reset")
      ```

1. Start the app by running:

   ```R
   shiny::runApp()
   ```

## Development

When adding any new dependencies to the application, they may be added using (where "useful_package" is the name of the package to add):

```R
renv::install("useful_package")
```

see [https://rstudio.github.io/renv/reference/install.html](https://rstudio.github.io/renv/reference/install.html) for more details.

Once a new package is added, run:

```R
renv::snapshot()
```

This will ensure the new package is added to the renv.lock file.

To remove an installed package, run (where "useful_package" is the name of the package to remove):

```R
renve::remove("useful_package")
```

For more on package management with renv, please see [https://rstudio.github.io/renv/articles/renv.html](https://rstudio.github.io/renv/articles/renv.html)

## Deployment

The first time you deploy, go through the Deployment-Setup instructions below. Afterwards, deploy with:

```R
rsconnect::deployApp()
```

### Deployment Setup (First-Time-Only)

The first time you deploy there are a few things to do. First, due to a presumed bug in renv, there is an extra, one-time step for configuring any packages loaded directly from github.

* More information: https://docs.rstudio.com/shinyapps.io/getting-started.html (section: "2.4.0.1 Important note on GitHub packages")

```R
# configure github dependencies for deployment
# NOTE: renv should take care of this
#       Perhaps in future versions this will be unnecessary.
devtools::install_github("th86/concordanceIndex", force=TRUE)
devtools::install_github("Gibbsdavidl/ImmuneSubtypeClassifier", force=TRUE)
```

Second, you'll need to set up your credentials for shinyapps.io. You can get your codes from:

* https://www.shinyapps.io/admin/#/tokens

Paste and evaluate your tokens in the RStudio console. They look like this:

```R
# shinyapps.io example credentials
rsconnect::setAccountInfo(
  name='shiny-iatlas',
  token='xxx',
  secret='yyy'
)
```

## Data

Input data for the Shiny-iAtlas portal were accessed from multiple remote sources, including **Synapse**, the **ISB Cancer Genomics Cloud**, and **Google Drive**. For convenience, we have created locally cached versions of dataframe objects as **`feather`** files in the `data2` folder:

- `driver_mutations1.feather`
- `driver_mutations2.feather`
- `driver_mutations3.feather`
- `driver_mutations4.feather`
- `driver_mutations5.feather`
- `driver_results1.feather`
- `driver_results2.feather`
- `feature_values_long.feather`
- `features.feather`
- `groups.feather`
- `immunomodulator_expr.feather`
- `immunomodulators.feather`
- `io_target_expr1.feather`
- `io_target_expr2.feather`
- `io_target_expr3.feather`
- `io_target_expr4.feather`
- `io_targets.feather`
- `til_image_links.feather`

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
