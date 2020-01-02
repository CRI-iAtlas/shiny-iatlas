filter_na <- function(value) {
  value <- unique(value)
  if (length(value) > 1 & anyNA(value)) {
    value <- na.omit(value)
    if (length(which(!is.na(value))) == 0) {
      value <- NA %>% as.character
    }
  }
  value <- max(unique(value))
  return(value)
}

is_df_empty <- function(df = data.frame()) {
  if (!identical(class(df), "data.frame") & !tibble::is_tibble(df)) {
    df <- data.frame()
  }
  return(is.null(dim(df)) | dim(df)[1] == 0 | dim(df)[2] == 0)
}

link_to_references <- function(current_link) {
  if (!is.na(current_link)) {
    url <- stringi::stri_extract_first(current_link, regex = "(?<=href=\").*?(?=\")")
    if (!identical(url, "NA") & !is.na(url)) {
      return(paste("{", url, "}", sep = ""))
    }
  }
  return(NA)
}

load_feather_data <- function(folder = "data/test") {
  # Identify all files with feather extension.
  files <- list.files(folder, pattern = "*.feather")
  files <- sprintf(paste0(folder, "/%s"), files)

  df <- dplyr::tibble()

  for (index in 1:length(files)) {
    df <- df %>% dplyr::bind_rows(feather::read_feather(files[[index]]) %>% dplyr::as_tibble())
  }

  return(df)
}

rebuild_gene_relational_data <- function(all_genes, ref_name, field_name = "name", relational_data = dplyr::tibble()) {
  if (is_df_empty(relational_data)) {
    relational_data <- dplyr::tibble() %>%
      tibble::add_column(!!field_name := NA)
  }
  relational_data <- all_genes %>%
    dplyr::select(ref_name) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(!!rlang::sym(ref_name))) %>%
    dplyr::rename_at(ref_name, ~(field_name)) %>%
    dplyr::arrange(!!rlang::sym(field_name))
  return(relational_data)
}

switch_value <- function(current_row, reference_name, field_name, tibble_object = dplyr::tibble()) {
  reference_value <- current_row[[reference_name]]
  current_value <- current_row[[field_name]]
  current_reference_row <- tibble_object %>%
    dplyr::filter(!!rlang::sym(reference_name) == reference_value)
  if (!.GlobalEnv$is_df_empty(current_reference_row)) {
    return(current_reference_row[[field_name]])
  } else if (!is.na(current_value)) {
    return(current_value)
  } else {
    return(NA)
  }
}
