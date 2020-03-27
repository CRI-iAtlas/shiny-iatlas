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

- lib cairo: https://www.cairographics.org/ (only required for iAtlas client)

- gfortran (libgfortran): usually installed with gcc

### MacOS Install instructions

- Download and install gfortran: https://github.com/fxcoudert/gfortran-for-macOS/releases
or https://cran.r-project.org/bin/macosx/tools/

- Download and install R-3.6.2.pkg: https://cran.r-project.org/bin/macosx/

- Download and install X11 via XQuartz: https://www.xquartz.org/

- Install cairo via Homebrew (https://formulae.brew.sh/formula/cairo ; requires brew: https://brew.sh/ )

- Download and install RStudio: https://rstudio.com/products/rstudio/download

### Initialize R Packages and run App

To run the app locally:

1. Clone this repository

1. Open `shiny-iatlas.Rproj`

1. Install packages. In the RStudio console, run:

   ```R
   renv::restore()
   ```

   This may take some time to complete - walk away from your computer, rest your eyes, and catch up on those stretching exercises you are meant to be doing :)

1. Start the app by running:

   ```R
   shiny::runApp()
   ```

## Development

Please consult the [Contributing Guide](https://github.com/CRI-iAtlas/shiny-iatlas/blob/develop/CONTRIBUTING.md) for pointers on how to get started on a new module.

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

To remove an installed package, run (where "no_longer_useful_package" is the name of the package to remove):

```R
renv::remove("no_longer_useful_package")
```

For more on package management with renv, please see [https://rstudio.github.io/renv/articles/renv.html](https://rstudio.github.io/renv/articles/renv.html)

## Deployment

The first time you deploy, go through the Deployment Setup instructions below. Afterwards, you can just deploy as per sub-section Deploy.

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

Input data for the Shiny-iAtlas portal are to a large extent obtained from the [Immune Landscape of Cancer](https://www.cell.com/immunity/abstract/S1074-7613(18)30121-3) study. These data can be accessed via the [Immunity journal publication page ](https://www.cell.com/immunity/abstract/S1074-7613(18)30121-3), the [manuscript page at NCI's Genomic Data Commons](https://gdc.cancer.gov/about-data/publications/panimmune) and on [iAtlas pages on Synapse](https://www.synapse.org/#!Synapse:syn21247064). This includes the main feature matrix and various feature and group annotations.  Additionally some input files of results were pre-computed specifically for use by this app.

Some of the key input data can be found as dataframe objects in **`feather`** files within the `data` folder:

- `cell_image_id_annotations.feather`
- `driver_mutations.feather`
- `feature_df.feather`
- `feature_method_df.feather`
- `fmx_df.feather`
- `im_direct_relationships.feather`
- `im_expr_df.feather`
- `im_potential_factors.feather`
- `im_target_annotations.feather`
- `im_target_expr_df.feather`
- `sample_group_df.feather`

## Methods

Methods employed by the app are described in [Immune Landscape of Cancer](https://www.cell.com/immunity/abstract/S1074-7613(18)30121-3) study and can also be found in Methods descriptions displayed in the app (for example as seen in Data Description module).

### Concordance Index (CI)

Concordance indexes for survival endpoints with respect to different immune readouts are computed using a custom package developed by Tai-Hsien Ou Yang. The **[concordanceIndex](https://github.com/th86/concordanceIndex)** package includes a single eponymous function that can be used as follows:

```R
concordanceIndex::concordanceIndex(predictions, observations)
```

where `predictions` and `observations` are numerical vectors of identical length.

### Immune Subtype Classifier

The iAtlas Immune Subtype Classifier tool uses the **[ImmuneSubtypeClassifier](https://github.com/CRI-iAtlas/ImmuneSubtypeClassifier)** R package, developed by David L. Gibbs, for classification of immune subtypes, in cancer, using gene expression data.
