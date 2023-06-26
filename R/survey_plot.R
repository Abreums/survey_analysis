# 
# nested <-
#   qt_answers |>
#   nest(data = c(-question))
# 
# plot_data <-
#   nested |>
#   filter(question == "q_10") |>
#   pull(data)
# 
# plot_data <- plot_data[[1]]
# 
# plot_data |>
#   count(answer) |>
#   mutate(p = n / sum(n))


lik_colors = c(
  "1" = "#C71E1D",
  "2" = "#D55457",
  "3" = "#E48F8F",
  "4" = "#B5AAB9",
  "5" = "#748CA5",
  "6" = "#17627B",
  "7" = "#174664"
)

plot_answers <- function(df) {
  qt_answers  |>
#    filter(question %in% questions) |>
    group_by(question) |>
    count(answer) |>
    mutate(p = n / sum(n)) |>
    ggplot(aes(
      question,
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
    facet_wrap( ~ question, ncol = 1) +
    coord_flip() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
      )
}

# summary plot
plot_survey_by <- function(df, a_column) {
#   the_table <-
#     df |>
#     count({{a_column}}, sort = T) |>
#     mutate(Prop = scales::percent(n / sum(n))) |> 
#     select({{a_column}}, Prop)
    
  df |>
    count({{a_column}}, sort = T) |> 
    mutate(prop = n/sum(n)) |> 
    ggplot(aes(fct_reorder({{a_column}}, desc(n)), n)) +
    geom_col(fill = "steelblue") +
    labs(title = "Número de respostas recebidas até o momento",
         x = str_to_title(deparse(substitute(a_column))),
         y = NULL) +
    theme_minimal() 
  # +
  #   annotate(geom = "table",
  #            x = 7,
  #            y = 50, 
  #            label = list(the_table))
  # 
}

