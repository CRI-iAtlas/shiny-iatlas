# IO Response utils functions
# "Auslander, 2018 - SKCM" =  "Auslander 2018",
datasets_options <-  list(
                         "Gide, 2019 - SKCM, Anti-PD1 +/- Anti-CTLA4" =  "Gide 2019", 
                         "Hugo, 2016 - SKCM, Anti-PD1" = "Hugo 2016", 
                         "Riaz, 2017 - SKCM, Anti-PD1" = "Riaz 2017", 
                         "Van Allen, 2015 - SKCM, Anti-CTLA4" = "Van Allen 2015",
                         "IMVigor210 - BLCA, Anti-PDL1" = "IMVigor210", 
                         "Prins, 2019 - GBM, Anti-PD1" = "Prins 2019")



filter_dataset <- function(df, dataset,feature, var1, var2 = NULL){
  df %>% 
    filter(Dataset == dataset) %>% 
    select(Sample_ID, Dataset, feature, var1, var2) %>% 
    tidyr::drop_na()
}

get_responder_annot <- function(df){
  df %>%
  mutate(Responder = dplyr::case_when(
    df$Progression == TRUE ~ "Non-Responder",
    df$Progression == FALSE ~ "Responder"))
}

build_distribution_io_df <- function(
  df, 
  feature, 
  scale_func_choice = "None"
){
  
  scale_function <- switch(
    scale_func_choice,
    "None" = identity, 
    "Log2" = log2,
    "Log2 + 1" = function(x) log2(x + 1),
    "Log10" = log10,
    "Log10 + 1" = function(x) log10(x + 1)
  )
 
  df %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(y = scale_function(feature)) %>% 
    tidyr::drop_na() %>% 
    dplyr::filter(!is.infinite(y))
}

get_lines_pos <- function(samples, y){
  
  n_int <- nrow(samples)
  
  divs <-seq(0, 1, len=n_int+1)
  
  #getting the intervals with the same variable
  int_pos <- divs[1]
  for(i in 1:n_int){
    try(if(samples$var1[i] != samples$var1[i+1]) int_pos <- c(int_pos, divs[i+1]), silent = TRUE)
  }
  int_pos <- c(int_pos, divs[n_int+1])
  
  lines_pos <- "list("
  for (i in 1:(length(int_pos)-1)) {
    
    lines_pos <- paste(lines_pos,
                       "list(line = list(color = 'rgba(68, 68, 68, 0.5)', width = 1), type = 'line', x0 =", 
                       #  (divs[i]+0.01),
                       #  ", x1 =", 
                       #  (divs[i+1]),
                       #  ", xref = 'paper', y0 =", 
                       # y ,
                       # ", y1 =", 
                       # y, 
                       # ", yref = 'paper'),
                       # list(line = list(color = 'rgba(68, 68, 68, 0.5)', width = 1), type = 'line', x0 =", 
                       (int_pos[i]+0.01),
                       ", x1 =", 
                       (int_pos[i]+0.01),
                       ", xref = 'paper', y0 =", 
                       0.2,
                       ", y1 =", 
                       - 0.1, 
                       ", yref = 'paper'),
                       list(line = list(color = 'rgba(68, 68, 68, 0.5)', width = 1), type = 'line', x0 =", 
                       (int_pos[i+1]),
                       ", x1 =", 
                       (int_pos[i+1]),
                       ", xref = 'paper', y0 =", 
                       0.2,
                       ", y1 =", 
                       - 0.1, 
                       ", yref = 'paper')
                       "
                       )
    if(i != (length(int_pos)-1)) lines_pos <- paste(lines_pos, ",")
  }
  paste(lines_pos, ")")
}

get_text_pos <- function(df, col_div, y){
  
  n_int <- dplyr::n_distinct(df[[col_div]])
  
  divs <-seq(0, 1, len=n_int+1)
  
  df_levels <- unique(df[[col_div]]) %>% sort()
  
  df_levels <- gsub(" ", "<br>", df_levels)
  lines_pos <- "list("
  
  for (i in 1:n_int) {
    label_text <- df
    lines_pos <- paste(lines_pos,
                       "list(x =", 
                       ((divs[i]+divs[i+1])/2),
                       ", y =", 
                       y,
                       ", showarrow = FALSE, text = '", 
                       (df_levels[i]),
                       "', xref = 'paper', yref = 'paper')"
    )
    if(i != n_int) lines_pos <- paste(lines_pos, ",")
  }
  paste(lines_pos, ")")
}

combine_groups <- function(df, group1, group2){
  
  df$Comb_feat <-  paste(df[[group1]], "&",
                                  convert_value_between_columns(input_value =group2,
                                                                df = feature_io_df,
                                                                from_column = "FeatureMatrixLabelTSV",
                                                                to_column = "FriendlyLabel"),
                                  ":", df[[group2]], sep = " " )
  
  df
}

create_plot_onegroup <- function(dataset_data, plot_type, dataset, feature, group1, ylabel){
  
  if(group1 == "Progression"){
    dataset_data <- get_responder_annot(dataset_data)
    group1 <- "Responder"
  }

  plot_type(dataset_data, 
                    x_col = as.character(group1),
                    y_col = feature, 
                    xlab = dataset_data[[group1]],
                    ylab = ylabel,
                    custom_data = as.character(dataset),
                    fill_colors = c("#0000FF", "#00FF00", "#FF00FF", "#FF0000"),
                    showlegend = F)  %>%
    add_annotations(
      text = dataset,
      x = 0.5,
      y = 1.1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)
    )
}

create_plot_twogroup <- function(dataset_data, plot_type, dataset, feature, group1, group2, ylabel){
 
  if(group1 == "Progression"){
    dataset_data <- get_responder_annot(dataset_data)
    group1 <- "Responder"
  }
  
  
  samples <- (dataset_data %>% group_by(dataset_data[[group1]], dataset_data[[group2]]) %>% 
                                summarise(samples = n()))
                  colnames(samples) <- c("var1", "var2", "samples")

  #get number of groups to draw lines
  samples <- (dataset_data %>% group_by(dataset_data[[group1]], dataset_data[[group2]]) %>%
                summarise(samples = n()))
  colnames(samples) <- c("var1", "var2", "samples")
  
  combine_groups(dataset_data, group1, group2) %>% 
    plot_type(.,
              x_col = "Comb_feat",
              y_col = feature,
              xlab = (dataset_data[[group2]]),
              ylab = ylabel,
              custom_data = as.character(dataset),
              fill_colors = c("#0000FF", "#00FF00", "#FF00FF", "#FF0000"),
              showlegend = F) %>%
    add_annotations(
      text = dataset,
      x = 0.5,
      y = 1.1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)) %>%
    layout(
      xaxis = list(tickangle = 80),
      shapes = lazyeval::lazy_eval(get_lines_pos(samples, -0.38)),
      margin = list(b = 10)
    )
}
 
get_t_test <- function(df, group_to_split, sel_feature, dataset, paired = FALSE, test = t.test, label = group_to_split){

  data_set <- df %>%
    filter(Dataset == dataset)
  
  if(paired == TRUE){
    #validate(need(group_to_split == "treatment_when_collected"), "The selected sample group has only one sample per patient. Please, select 'Independent'.")
    patients <- data_set %>% 
      dplyr::group_by(Patient_ID) %>% 
      summarise(samples = dplyr::n_distinct(Sample_ID)) %>% 
      filter(samples > 1) %>% 
      select(Patient_ID)
    
    data_set <- data_set %>% 
      dplyr::filter(Patient_ID %in% patients$Patient_ID)
  }

  if(dplyr::n_distinct(data_set[[group_to_split]])>1){
    split_data <- split(data_set, data_set[[group_to_split]])

    comb_groups <- combn(1:length(split_data), 2)

    purrr::map2_dfr(.x = comb_groups[1,], .y = comb_groups[2,], function(x,y){

      test_data <- broom::tidy(test(split_data[[x]][[sel_feature]],
                                      split_data[[y]][[sel_feature]],
                                      paired = paired)) %>%
        dplyr::select(statistic, p.value)

      test_data$Dataset <- as.character(dataset)
      test_data$Test <- paste0(label, ": ", names(split_data)[x], " vs. ", names(split_data)[y])

      test_data %>%
        mutate("-log10(pvalue)" = -log10(p.value)) %>% 
        dplyr::mutate_if(is.numeric, round, digits = 3) %>% 
        dplyr::select(Dataset, Test, statistic, p.value, "-log10(pvalue)")
    })
  }else{
    test_data <- data.frame(Dataset = dataset,
                            Test = "First sample group has only one level for this dataset.",
                            statistic = NA,
                            p.value = NA)
  }
}