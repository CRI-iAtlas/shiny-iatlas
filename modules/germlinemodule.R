germline_ui <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    titleBox(
      "iAtlas Explorer — Germline Analysis"
    ),
    textBox(
      width = 12,
      p("This module provides interactive visualizations related to the manuscript ",
        a(href = "https://doi.org/10.1016/j.immuni.2021.01.011", "Sayaman et al., Germline genetic contribution to the immune landscape of cancer, Immunity (2021)")),
      p("Explore the germline genetic contribution to the immune landscape of cancer with results of heritability analysis, GWAS, and rare variant analysis across 30 non-hematological cancer types characterized by the TCGA. All analyses are adjusted for cancer type, age at diagnosis, sex, and the first seven components from principal component analysis (PCA) done on SNP data, which capture overall genetic ancestry.")
    ),
    sectionBox(
      title = "Heritability",
      germline_heritability_ui(ns("germline_heritability"))
      ),
    sectionBox(
      title = "GWAS",
      germline_gwas_ui(ns("germline_gwas-module"))
    ),
    sectionBox(
      title = "Rare Variants",
      germline_rarevariants_ui(ns("germline_rarevariants"))
    )
  )
}

germline_server <- function(input, output, session){
  
  callModule(
    germline_heritability_server,
    "germline_heritability"
  )
  callModule(
    germline_gwas_server,
    "germline_gwas-module"
  )
  callModule(
    germline_rarevariants_server,
    "germline_rarevariants"
  )
}