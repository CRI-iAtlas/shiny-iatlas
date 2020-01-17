# Shiny-iAtlas

Shiny-iAtlas is an interactive web portal that provides multiple analysis modules to visualize and explore immune response characterizations across cancer types. The app is hosted on shinyapps.io at [https://isb-cgc.shinyapps.io/shiny-iatlas/](https://isb-cgc.shinyapps.io/shiny-iatlas/) and can also be accessed via the main CRI iAtlas page at [http://www.cri-iatlas.org/](http://www.cri-iatlas.org/).

The portal is built entirely in **R** and **Shiny** using the **RStudio** development environment. Layout and interactivity within the portal are achieved by heavy use of the following packages:

- [shinydashboard](https://rstudio.github.io/shinydashboard/)
- [plotly](https://plot.ly/r/)
- [crosstalk](https://rstudio.github.io/crosstalk/)

## Install

### Requirements

- R: https://www.r-project.org/ - v3.6.2

- RStudio: https://rstudio.com/products/rstudio/download

- Docker: https://www.docker.com/products/docker-desktop

  Ensure that the location of the repository is shared via docker:

  - Mac: https://docs.docker.com/docker-for-mac/#file-sharing

  - Windows: https://docs.microsoft.com/en-us/archive/blogs/stevelasker/configuring-docker-for-windows-volumes

- git-lfs: https://git-lfs.github.com

  For installation on the various platforms, please see this [git-lfs wiki](https://github.com/git-lfs/git-lfs/wiki/Installation)

  Some feather files are _very_ large. `git-lfs` is used to store these files.

  **Please note**: `git lfs install` _must_ be executed within the repository directory immediately after cloning the repo.

- libpq (postgres): https://www.postgresql.org/download/

- lib cairo: https://www.cairographics.org/ (only required for iAtlas client)

- gfortran (libgfortran): usually installed with gcc

### MacOS Install instructions

Install brew: https://brew.sh/

Then:

- brew install R
- brew install cairo
- brew install git-lfs
- brew install postgres
- download and install RStudio: https://rstudio.com/products/rstudio/download
- download and install Docker: https://www.docker.com/products/docker-desktop

### Initialize R Packages, Database and run App

To run the app locally:

1. Clone this repository

1. Open `shiny-iatlas.Rproj`

1. Install packages. In the RStudio console, run:

   ```R
   renv::restore()
   ```

   This may take some time to complete - get something nice to drink :)

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
renv::remove("no_longer_useful_package")
```

For more on package management with renv, please see [https://rstudio.github.io/renv/articles/renv.html](https://rstudio.github.io/renv/articles/renv.html)

## Deployment

The first time you deploy, go through the Deployment-Setup instructions below. Afterwards, you can just deploy as needed.

### Deployment Setup (First-Time-Only)

You'll need to set up your credentials for shinyapps.io. You can get your codes from:

- https://www.shinyapps.io/admin/#/tokens

Paste and evaluate your tokens in the RStudio console. They look like this:

```R
# shinyapps.io example credentials
rsconnect::setAccountInfo(
  name='shiny-iatlas',
  token='xxx',
  secret='yyy'
)
```


### Deploy

```R
rsconnect::deployApp()
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

