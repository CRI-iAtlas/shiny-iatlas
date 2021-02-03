germline_ui <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    titleBox(
      "iAtlas Explorer — Germline Analysis"
    ),
    textBox(
      width = 12,
      
      # p("Explore the germline genetic contribution to the immune landscape of cancer."),
      p("This module relates to the manuscript: Sayaman et al. Germline genetic contribution to the immune landscape of cancer. bioRxiv 2020.01.30.926527; doi: https://doi.org/10.1101/2020.01.30.926527"),
      p("Explore the germline genetic contribution to the immune landscape of cancer with results of heritability analysis, GWAS (N=9,603), and rare variant analysis (N=9,138) across 30 non-hematological cancer types characterized by the TCGA. All analyses adjusted for cancer type, age at diagnosis, sex, and the first seven components from principal component analysis (PCA) done on SNP data, which largely capture genetic ancestry.")
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