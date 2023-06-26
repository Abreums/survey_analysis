# survey_plot.R
#
# Plot survey monkey data


# summary plot
plot_survey_by <- function(df, a_column) {
  df |>
    count({{a_column}}, sort = T) |> 
    mutate(prop = n/sum(n)) |> 
    ggplot(aes(fct_reorder({{a_column}}, desc(n)), n)) +
    geom_col(fill = "steelblue") +
    labs(title = "Número de respostas recebidas até o momento",
         x = str_to_title(deparse(substitute(a_column))),
         y = NULL) +
    theme_minimal() 
}


# Colors for likert plot
lik_colors = c(
  "1" = "#C71E1D",
  "2" = "#D55457",
  "3" = "#E48F8F",
  "4" = "#B5AAB9",
  "5" = "#748CA5",
  "6" = "#17627B",
  "7" = "#174664"
)

plot_all_likert_data <- function(survey_data) {
  
  survey_data  |>
    select(matches("^Q[01][[:digit:]][[:blank:]]")) |> 
    pivot_longer(cols = everything(),
                 names_to = "question",
                 values_to = "answer") |> 
    filter(!is.na(answer)) |> 
    group_by(question) |>
    count(answer) |>
    mutate(p = n / sum(n)) |> 
    ggplot(aes(
      fct_rev(question),
      p,
      group = question,
      label = paste0(as.character(round(100 * p, 1)), "%"),
      fill = forcats::fct_rev(answer)
    )) +
    geom_col(position = "stack") +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme_minimal() +
    labs(title = "Respostas ",
         x = NULL,
         y = NULL,
         fill = NULL) +
    scale_fill_manual(values = lik_colors) +
    scale_x_discrete(labels = label_wrap_gen(45)) +
    coord_flip() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

plot_likert_by <- function(survey_data, group_category = NULL) {
  
  if(is.null(group_category)) {
    error(-1)
  }
  
  group_category <- cat_vision
  survey_data  |>
    select(matches("^Q[01][[:digit:]][[:blank:]]")) |> 
    pivot_longer(cols = everything(),
                 names_to = "question",
                 values_to = "answer") |> 
    mutate(question_index = str_extract(question, "^Q[01][[:digit:]]")) |>
    filter(question_index %in% group_category) |> 
    group_by(question) |>
    count(answer) |>
    mutate(p = n / sum(n)) |> 
    ggplot(aes(
      fct_rev(question),
      p,
      group = question,
      label = paste0(as.character(round(100 * p, 1)), "%"),
      fill = forcats::fct_rev(answer)
    )) +
    geom_col(position = "stack", width = 0.5) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme_minimal() +
    labs(title = "Respostas ",
         x = NULL,
         y = NULL,
         fill = NULL) +
    scale_fill_manual(values = lik_colors) +
    scale_x_discrete(labels = label_wrap_gen(45)) +
    annotate("text", x = 0.5, y = 0, label = "Discordo totalmente", color = lik_colors[1], hjust = 0, vjust = 0) +
    annotate("text", x = 0.5, y = 1, label = "Concordo totalmente", color = lik_colors[7], hjust = 1, vjust = 0) +
    coord_flip() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}
