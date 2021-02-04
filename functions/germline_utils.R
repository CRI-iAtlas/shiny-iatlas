
add_plotly_label <- function(tbl, title, name, group){
  dplyr::mutate(tbl, label = paste0(
    "<b>", title, ":</b> ", {{name}}, " (", {{group}}, ")"
  ))
}

add_plotly_value_label <- function(tbl, cols){
  tbl %>%
    tidyr::pivot_longer(
      .,
      tidyselect::all_of(cols),
      names_to  = "value_name",
      values_to = "value"
    ) %>%
    dplyr::mutate(value_label = stringr::str_glue(
      "{name}: {value}",
      name = stringr::str_to_upper(.data$value_name),
      value = sprintf("%0.3f", .data$value)
    )) %>%
    dplyr::group_by(.data$label) %>%
    dplyr::mutate(value_label = paste0(
      .data$value_label,
      collapse = "</br>"
    )) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      .,
      names_from = .data$value_name,
      values_from = .data$value
    )
}

create_plotly_label <- function(
  tbl,
  name,
  group,
  cols,
  title = "ParticipantBarcode"
){
  
  tbl %>%
    add_plotly_label(title, {{name}}, {{group}}) %>%
    add_plotly_value_label(tidyselect::all_of(cols)) %>%
    tidyr::unite(
      "label",
      .data$label,
      .data$value_label,
      sep = "</br></br>"
    )
}

#Heritability functions

#' Prepare heritability tibble for plotting
#'
#' @param heritablity_data A tibble
#' @param parameter Parameter to be used for selection of results (eg, ancestry cluster, immune feature)
#' @param group Specific group, in the selected parameter, to be displayed (eg. European - ancestry cluster, NK cells - immune feature)
#' @param pval_thres Maximun p-value to be included
#' @importFrom magrittr %>%

create_heritability_df <- function(
  heritablity_data,
  parameter = "cluster",
  group = "European",
  pval_thres = 0.05,
  ancestry_labels
){

  ancestry_df <- names(ancestry_labels)
  names(ancestry_df) <- ancestry_labels

    df <- heritablity_data %>%
      dplyr::filter(.[[parameter]] == group) %>%
      dplyr::filter(pval <= pval_thres) %>%
      dplyr::filter(Variance >= 0) %>%
      create_plotly_label(
        ., paste(.$display, "- ", ancestry_df[cluster], "Ancestry"),
        paste("\n Immune Trait Category:",.$Annot.Figure.ImmuneCategory, "\n Immune Trait Module:", .$Annot.Figure.ImmuneModule),
        c("Variance", "SE", "pval","FDR"),
        title = "Immune Trait"
      )

  #creating the y label
  if(parameter == "cluster") df <- df %>% mutate(ylabel = display)
  else if (parameter == "Annot.Figure.ImmuneCategory" | parameter == "Annot.Figure.ImmuneModule")
    df <- df %>% mutate(ylabel = paste(ancestry_df[cluster], display, sep = " - "))
  else  df <- df %>% mutate(ylabel = paste(ancestry_df[cluster], .[[parameter]], sep = " - "))
}

format_heritability_plot <- function(p, hdf, fdr = FALSE){
  p <- p %>%
    plotly::layout(
      xaxis = list(
        tickformat = "%"
      )
    )
    if(fdr == TRUE){
      p <- p %>%
            plotly::add_annotations(x = hdf$Variance+hdf$SE+0.01,
                                    y = hdf$ylabel,
                                    text = (hdf$plot_annot),
                                    xref = "x",
                                    yref = "y",
                                    showarrow = F,
                                    font=list(color='black')) %>%
            plotly::add_annotations( text="LRT FDR \n † <= 0.1 \n * <= 0.05 \n ** <= 0.01 \n *** <= 0.001", xref="paper", yref="paper",
                                     x=1.03, xanchor="left",
                                     y=0, yanchor="bottom",
                                     legendtitle=TRUE, showarrow=FALSE )
    }
  p
}

#' Build Manhattan plot tibble
#'
#' @param gwas_df A Tibble with columns sample_id and group
#' @param chr_selected An integer in the id column of the samples table
#' @param bp_min An integer in the id column of the samples table
#' @param bp_max An integer in the id column of the samples table
#' @param feature_selected An integer in the id column of the samples table
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join filter group_by summarise mutate
#' @importFrom rlang .data
build_manhattanplot_tbl <- function(
  gwas_df,
  chr_selected,
  bp_min,
  bp_max,
  to_select,
  to_highlight,
  to_exclude) {
  
  if(to_highlight == FALSE & !is.null(to_exclude)) gwas <- gwas_df %>% dplyr::filter(!(display %in% to_exclude))
  else if(to_highlight == TRUE) gwas <- gwas_df %>% dplyr::filter(display %in% to_select)
  else gwas <- gwas_df

  gwas %>%
    dplyr::filter(chr_col %in% chr_selected) %>%
    dplyr::group_by(chr_col) %>%
    dplyr::summarise(chr_len=max(bp_col), .groups = "drop_last") %>%
    dplyr::mutate(tot=cumsum(as.numeric(chr_len))-chr_len) %>%
    dplyr::select(-chr_len) %>%
    dplyr::left_join(gwas, ., by = "chr_col") %>%
    dplyr::arrange(chr_col, bp_col) %>%
    dplyr::mutate(x_col=bp_col+tot) %>%
    dplyr::mutate( log10p = -log10(PLINK.P),
                   text = paste("<b>",display, "</b>",
                                "\n(Immune Trait Category: ", `Annot.Figure.ImmuneCategory`, ")",
                                "\nSNP name: ", snp_id, "\nSNP: ", snp_col, "\nPosition: ", bp_col, "\nChromosome: ", chr_col,
                                "\nPLINK MAF: ", maf, sep=""))
}

get_mhtplot_xlabel <- function(
  selected_region = input$selection,
  gwas_df = gwas_mht(),
  x_min = selected_min(),
  x_max = selected_max()
){
  if(selected_region == "See all chromosomes"){
    gwas_df %>%
      dplyr::group_by(chr_col) %>%
      dplyr::summarize(center=( max(x_col) + min(x_col) ) / 2 , .groups = "drop") %>%
      dplyr::rename(label = chr_col)
  }else{
    breaks <- c(x_min, (x_min+x_max)/2, x_max)
    data.frame(
      label = paste(format(round(breaks / 1e6, 2), trim = TRUE), "Mb"),
      center = breaks,
      stringsAsFactors = FALSE
    )
  }
}


