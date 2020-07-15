# IO Response utils functions
# "Auslander, 2018 - SKCM" =  "Auslander 2018",
datasets_options <-  list(
                         "Gide, 2019 - SKCM, Anti-PD-1 +/- Anti-CTLA-4" =  "Gide 2019", 
                         "Hugo, 2016 - SKCM, Anti-PD-1" = "Hugo 2016", 
                         "Riaz, 2017 - SKCM, Anti-PD-1" = "Riaz 2017", 
                         "Van Allen, 2015 - SKCM, Anti-CTLA-4" = "Van Allen 2015",
                         "IMVigor210 - BLCA, Anti-PD-L1" = "IMVigor210", 
                         "Prins, 2019 - GBM, Anti-PD-1" = "Prins 2019")

datasets_PFI <- c("Gide 2019", "Van Allen 2015", "Prins 2019")

filter_dataset <- function(df, dataset,feature, var1, var2 = NULL){
  df %>% 
    filter(Dataset == dataset) %>% 
    select(Sample_ID, Dataset, feature, var1, var2) %>% 
    tidyr::drop_na()
}

get_responder_annot <- function(df){
  df %>%
  mutate(Responder = dplyr::case_when(
    df$Progression == TRUE ~ "Progressor",
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

get_hlines_pos <- function(samples){
  
  n_int <- nrow(samples)
  
  divs <-seq(0, 1, len=n_int+1)
  
  #getting the intervals with the same variable
  int_pos <- as.numeric()#divs[1]
  for(i in 1:n_int){
    try(if(samples$var1[i] != samples$var1[i+1]) int_pos <- c(int_pos, divs[i+1]), silent = TRUE)
  }
  int_pos <- c(int_pos, divs[n_int+1])
  
  lines_pos <- "list("
  for (i in 1:(length(int_pos)-1)) {
    
    lines_pos <- paste(lines_pos,
                       "list(line = list(color = 'rgba(68, 68, 68, 0.5)', width = 1), type = 'line', x0 =", 
                       0,
                       ", x1 =",
                       -0.5,
                       ", xref = 'paper', y0 =", 
                       (int_pos[i]),
                       ", y1 =", 
                       (int_pos[i]), 
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

get_group_labels <-  function(df, group){
  ioresponse_data$sample_group_df %>% 
    dplyr::filter(Category == group) %>% 
    select(FeatureValue, FeatureLabel, FeatureHex, order_within_sample_group)
}

combine_colors <- function(color1, color2){

  purrr::map2_chr(.x = color1, .y = color2, function(c1, c2){
    colorRampPalette(c(c1, c2))(3)[2]
  }) 
  
}

combine_groups <- function(df, group1, group2, label1, label2){
  
  label1 <- get_group_labels(df, group1)
 
  df <- merge(df, label1, by.x = group1, by.y = "FeatureValue")
 
  if(group2 == "None" | group1 == group2){
    df <- df %>% 
      dplyr::mutate(group = df$FeatureLabel,
                    color = df$FeatureHex)
  }else if(group2 != "None"  & group1 != group2){
    label2 <- get_group_labels(df, group2)
    df <- merge(df, label2, by.x = group2, by.y = "FeatureValue")
    df <- df %>% 
      dplyr::mutate(group = (paste(df$"FeatureLabel.x", "&",df$"FeatureLabel.y")),
                    color = combine_colors(FeatureHex.x, FeatureHex.y))
  }
  df
}

create_plot_onegroup <- function(dataset_data, plot_type, dataset, feature, group1, ylabel){
  
  xform <- list(automargin = TRUE,
                categoryorder = "array",
                categoryarray = (dataset_data %>%
                                   dplyr::select(group, order_within_sample_group) %>% 
                                   dplyr::group_by(group) %>% 
                                   dplyr::summarise(m = min(order_within_sample_group)) %>% 
                                   dplyr::arrange(m) %>% 
                                   dplyr::pull(group))
  )
  
  group_colors <- unique((dataset_data %>% 
                       dplyr::arrange(order_within_sample_group))$color) 
  names(group_colors) <- xform$categoryarray

  plot_type(dataset_data, 
                    x_col = as.character(group1),
                    y_col = feature, 
                    xlab = dataset_data[[group1]],
                    ylab = ylabel,
                    custom_data = as.character(dataset),
                    fill_colors = group_colors, 
                    showlegend = F)  %>%
    add_title_subplot_plotly(dataset) %>% 
    layout(
      xaxis = xform,
      margin = list(b = 10),
      plot_bgcolor  = "rgb(250, 250, 250)"
    )
}

create_plot_twogroup <- function(dataset_data, plot_type, dataset, feature, group, group1, group2, ylabel){
   
  samples <- (dataset_data %>% group_by(dataset_data[[group1]], dataset_data[[group2]]) %>% 
                                summarise(samples = dplyr::n()))
                  colnames(samples) <- c("var1", "var2", "samples")

  #get number of groups to draw lines
  samples <- (dataset_data %>% 
                group_by(dataset_data[[group1]], dataset_data[[group2]]) %>%
                summarise(m = min(order_within_sample_group.x),
                          n = min(order_within_sample_group.y),
                          samples = dplyr::n()) %>% 
                dplyr::arrange(m,n))
  
  colnames(samples) <- c("var1", "var2", "samples")
  
  xform <- list(automargin = TRUE,
                tickangle = 80,
                categoryorder = "array",
                categoryarray = (dataset_data %>%
                                   dplyr::select(group, order_within_sample_group.x, order_within_sample_group.y) %>%
                                   dplyr::group_by(group) %>%
                                   dplyr::summarise(m = min(order_within_sample_group.x),
                                                    n = min(order_within_sample_group.y)) %>%
                                   dplyr::arrange(m, n) %>%
                                   dplyr::pull(group))
  )
  group_colors <- unique((dataset_data %>% 
                            arrange(order_within_sample_group.x, order_within_sample_group.y))$color) 
  names(group_colors) <- xform$categoryarray
  
  dataset_data %>% 
    plot_type(.,
              x_col = group,
              y_col = feature,
              xlab = (dataset_data[[group]]),
              ylab = ylabel,
              custom_data = as.character(dataset),
              fill_colors = group_colors,
              showlegend = F) %>%
    add_title_subplot_plotly(dataset) %>% 
    layout(
      autosize = TRUE,
      shapes = lazyeval::lazy_eval(get_lines_pos(samples, -0.38)),
      xaxis = xform,
      plot_bgcolor  = "rgb(250, 250, 250)"
    )
}

log2foldchanges <- function(x,y){
  mean(log2(y+1))-mean(log2(x+1))
} 

get_stat_test <- function(df, group_to_split, sel_feature, dataset, paired = FALSE, test = t.test, label = group_to_split){
 
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
      
      if(paired == TRUE & nrow(split_data[[x]]) != nrow(split_data[[y]])){
        test_data <- data.frame(Dataset = dataset,
                                Group1 = paste0("Not available for paired test. ", names(split_data)[x], " (", nrow(split_data[[x]]),")"),
                                Group2 = paste0(names(split_data)[y], " (", nrow(split_data[[y]]), ")"),
                                #Test = paste0("Not available for paired test. ", names(split_data)[x], " (", nrow(split_data[[x]]),") vs. ", names(split_data)[y], " (", nrow(split_data[[y]]), ")"),
                                statistic = NA,
                                p.value = NA)
      }else if(nrow(split_data[[x]]) <=1 | nrow(split_data[[y]]) <=1){
        test_data <- data.frame(Dataset = dataset,
                                Group1 = paste0("Few samples to perform test. ", names(split_data)[x], " (", nrow(split_data[[x]]),")"),
                                Group2 = paste0(names(split_data)[y], " (", nrow(split_data[[y]]), ")"),
                                #Test = paste0("Few samples to perform test.", names(split_data)[x], " (", nrow(split_data[[x]]),") vs. ", names(split_data)[y], " (", nrow(split_data[[y]]), ")"),
                                statistic = NA,
                                p.value = NA)
      }else{
        test_data <- broom::tidy(test(split_data[[x]][[sel_feature]],
                                      split_data[[y]][[sel_feature]],
                                      paired = paired)) %>%
          dplyr::select(statistic, p.value)
        
        test_data$Dataset <- as.character(dataset)
        test_data$Group1 <- paste0(names(split_data)[x], " (", nrow(split_data[[x]]),")")
        test_data$Group2 <- paste0(names(split_data)[y], " (", nrow(split_data[[y]]), ")")
        test_data$FoldChange <- log2foldchanges(split_data[[x]][[sel_feature]],
                                                split_data[[y]][[sel_feature]])
        
        test_data %>%
          mutate("-log10(pvalue)" = -log10(p.value)) %>% 
          dplyr::mutate_if(is.numeric, round, digits = 3) %>% 
          dplyr::select(Dataset, Group1, Group2,  "Log2(FoldChange)" = FoldChange, statistic, p.value, "-log10(pvalue)")
        #dplyr::select(Dataset, Group1, "Group 1 Size" =  n_samples1, Group2,  "Group 2 Size" = n_samples2, statistic, p.value, "-log10(pvalue)")
      }
    })
  }else{
    test_data <- data.frame(Dataset = dataset,
                            Group1 = "Sample group has only one level for this dataset.",
                            Group2 = NA,
                            statistic = NA,
                            p.value = NA)
  }
}

#IO Overview


get_io_overview_table <- function(group){
  
  ioresponse_data$fmx_df %>%
    dplyr::mutate("Sample Group" = dplyr::case_when(
      is.na(.[[group]]) ~ "Not annotated",
      !is.na(.[[group]]) ~ as.character(.[[group]])
    )) %>% 
    dplyr::group_by(Dataset, `Sample Group`) %>% 
    dplyr::summarise(n = dplyr::n_distinct(Sample_ID)) %>% 
    tidyr::pivot_wider(
      names_from = Dataset, 
      values_from = n) %>%
    dplyr::mutate("Group Name" = purrr::map_chr(.[["Sample Group"]], function(x){
      if(x == "Not annotated") return("")
      convert_value_between_columns(input_value = x,
                                    df = ioresponse_data$sample_group_df %>% filter(Category == group),
                                    from_column = "FeatureValue",
                                    to_column = "FeatureName")}),
      "Plot Color" = purrr::map_chr(.[["Sample Group"]], function(x){
        if(x == "Not annotated")return("#C9C9C9")
        convert_value_between_columns(input_value = x,
                                      df = ioresponse_data$sample_group_df %>% filter(Category == group),
                                      from_column = "FeatureValue",
                                      to_column = "FeatureHex")}),
      "Order" = purrr::map_chr(.[["Sample Group"]], function(x){
        if(x == "Not annotated")return("0")
        convert_value_between_columns(input_value = x,
                                      df = ioresponse_data$sample_group_df %>% filter(Category == group),
                                      from_column = "FeatureValue",
                                      to_column = "order_within_sample_group") %>% as.integer()
      })
    )
}

get_io_mosaic_df <- function(fdf, group1, group2){
  
  #getting the labels
  label1 <- get_group_labels(ioresponse_data$fmx_df, group1)
  label2 <- get_group_labels(ioresponse_data$fmx_df, group2)
  
  not_annot <- data.frame("FeatureValue" = NA_character_, 
                          "FeatureLabel" = "Not annotated", 
                          "FeatureHex"="#C9C9C9", 
                          "order_within_sample_group" = 0)
  label1 <- rbind(label1, not_annot)
  label2 <- rbind(label2, not_annot)

  df_mosaic <- merge(fdf %>% 
                      dplyr::select(Sample_ID, Dataset, group1, group2), 
                    label1, by.x = group1, by.y = "FeatureValue") 
  
  df_mosaic <- merge(df_mosaic, 
              label2, by.x = group2, by.y = "FeatureValue") 
  
  df_mosaic <- df_mosaic %>% 
                mutate(x= paste(Dataset, FeatureLabel.y)) %>% 
                arrange(order_within_sample_group.y) %>% 
                select(x, y = FeatureLabel.x, plot_color = FeatureHex.x) 
  
   df_mosaic$x <-  as.factor(df_mosaic$x)
   df_mosaic$y <-  factor(df_mosaic$y, levels = (label1 %>% 
                                                    dplyr::arrange(order_within_sample_group))$FeatureLabel)
  
   df_mosaic
}

#IO Survival

get_feature_by_dataset <- function(datasets, features, feature_df, group_df, fmx_df){
  
  all_comb <- tidyr::crossing(dataset = datasets, feature = features) %>% 
    merge(., feature_df %>% dplyr::select(FeatureMatrixLabelTSV, FriendlyLabel,VariableType, `Variable Class Order`),
         by.x = "feature", by.y ="FeatureMatrixLabelTSV")

  num_cols <- all_comb[which(all_comb$VariableType == "Numeric"),]
  cat_cols <- all_comb[which(all_comb$VariableType == "Categorical"),]
  #Organize numerical features
  if(nrow(num_cols)>0){
    num_cols <- num_cols %>%
      dplyr::select(dataset,
                    group = feature,
                    group_label = FriendlyLabel,
                    order_within_sample_group = `Variable Class Order`) %>% 
      dplyr::mutate(feature=group,
                    ft_label = "Immune Feature")
    
  }
  #Check which datasets have more than one level for categorical features
  if(nrow(cat_cols)>0){
    cat_values <- purrr::map2_dfr(.x = cat_cols$dataset, .y = cat_cols$feature, .f = function(x, y){
      uvalue <- unique((fmx_df %>% 
                          dplyr::filter(Dataset == x))[[y]]) 
      
      if(length(uvalue)>1) data.frame(dataset = as.character(x), 
                                      feature = as.character(y), 
                                      gname = as.character(uvalue),
                                      stringsAsFactors = FALSE) %>% 
        dplyr::mutate(group = paste0(feature, gname))
      else return()
    })
  
    if(nrow(cat_values)>0) cat_cols <- merge(cat_values, cat_cols, by = c("dataset", "feature")) %>% 
      merge(., group_df, 
            by.x = c("gname", "feature"), by.y = c("FeatureValue", "Category")) %>% 
      select(dataset, feature, ft_label = FriendlyLabel, group, group_label = FeatureLabel, order_within_sample_group)
    else return()
    
    rbind(cat_cols, num_cols)
  }else{
    num_cols
  }
  
}

fit_coxph <- function(dataset1, data, feature, time, status, ft_labels, multivariate = FALSE){
  
  data_cox <- data %>% 
    filter(Dataset == dataset1)
  
  #checking which features have more than one level for the dataset
  #valid_ft <- purrr::keep(feature, function(x) dplyr::n_distinct(data_cox[[x]])>1)
  valid_ft <- unique((ft_labels %>% 
    dplyr::filter(dataset == dataset1))$feature)
  
    if(multivariate == FALSE){
      purrr::map_dfr(.x = valid_ft, function(x){
        cox_features <- as.formula(paste(
          "survival::Surv(", time, ",", status, ") ~ ", x)) 
        
        survival::coxph(cox_features, data_cox)%>% 
          create_ph_df(dataset = dataset1)
      })
    }else{
      mult_ft <- paste0(valid_ft, collapse  = " + ")
    
      cox_features <- as.formula(paste(
        "survival::Surv(", time, ",", status, ") ~ ", 
        mult_ft)
      )
      survival::coxph(cox_features, data_cox) %>% 
        create_ph_df(dataset = dataset1)
    }
}

create_ph_df <- function(coxphList, dataset){
  
  coef_stats <- as.data.frame(summary(coxphList)$conf.int)
  coef_stats$dataset <- dataset
  coef_stats$group <- row.names(coef_stats)
  coef_stats$pvalue <- (coef(summary(coxphList))[,5])
  
  coef_stats %>% 
    dplyr::mutate(logHR = log10(`exp(coef)`),
                  logupper = log10(`upper .95`),
                  loglower = log10(`lower .95`),
                  difflog=logHR-loglower,
                  logpvalue = -log10(pvalue))
}

build_coxph_df <- function(datasets, data, feature, time, status, ft_labels, multivariate = FALSE){
 
  purrr::map_dfr(.x = datasets, .f= fit_coxph,
                 data = data, 
                 feature = feature,
                 time = time,
                 status = status,
                 ft_labels = ft_labels,
                 multivariate = multivariate) %>% 
  {suppressMessages(dplyr::right_join(x = ., ft_labels))} %>%
  dplyr::mutate(group_label=replace(group_label, is.na(logHR), paste("(Ref.)", .$group_label[is.na(logHR)]))) %>%
  dplyr::mutate_all(~replace(., is.na(.), 0))
}

build_forestplot_dataset <- function(x, coxph_df, xname){
  
  subset_df <- coxph_df %>%
    dplyr::filter(dataset == x) %>%
    dplyr::arrange(ft_label, desc(abs(logHR)))
  
  if(dplyr::n_distinct(coxph_df$group) == 1){
    plot_title = "" 
    ylabel = x 
  }else{
    plot_title = x
    ylabel = factor(subset_df$group_label, levels = subset_df$group_label)
  }
  
  p <-  create_forestplot_plotly(x = subset_df$logHR,
                                 y = ylabel,
                                 error = subset_df$difflog,
                                 plot_title = plot_title,
                                 xlab = xname)
  
  if(dplyr::n_distinct(coxph_df$group) == 1){
    p <- p %>% 
      layout(
        title = unique(subset_df$group_label)
      )
  }
  
  if(dplyr::n_distinct(subset_df$ft_label)>1){ #categorical data selected
    p <- p %>%
      layout(
        shapes = lazyeval::lazy_eval(get_hlines_pos(subset_df %>% dplyr::select(var1 = ft_label)))
      )
  }
  p
}

build_heatmap_df <- function(coxph_df){
   df <- coxph_df %>%
    dplyr::filter(!stringr::str_detect(group_label, '(Ref.)')) %>%
    dplyr::arrange(feature) %>% 
    dplyr::select(dataset, group_label, logHR) %>%
    tidyr::pivot_wider(names_from = group_label, values_from = logHR) %>% 
    as.data.frame()
  
  row.names(df) <- df$dataset
  df$dataset <- NULL
  
  t(as.matrix(df))
}

add_BH_annotation <- function(coxph_df, p){
  
  fdr_corrected <- coxph_df %>%
    dplyr::filter(!stringr::str_detect(group_label, '(Ref.)')) %>%
    dplyr::group_by(dataset) %>% 
    dplyr::mutate(FDR = p.adjust(pvalue, method = "BH")) %>% 
    dplyr::mutate(heatmap_annotation = dplyr::case_when(
      pvalue > 0.05 | FDR > 0.2 ~ "",
      pvalue <= 0.05 & FDR <= 0.2 & FDR > 0.05 ~ "*",
      pvalue <= 0.05 & FDR <= 0.05 ~ "**"
    )) %>% 
    dplyr::select(dataset, group_label, pvalue, FDR, heatmap_annotation)
  
  p %>%
    add_annotations(x = fdr_corrected$dataset,
                    y = fdr_corrected$group_label,
                    text = fdr_corrected$heatmap_annotation, 
                    showarrow = F,
                    font=list(color='black')) %>% 
    add_annotations( text="BH pValue \n * <= 0.2 \n ** <= 0.05", xref="paper", yref="paper",
                     x=1.03, xanchor="left",
                     y=0, yanchor="bottom", 
                     legendtitle=TRUE, showarrow=FALSE )
} 

  