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
    #geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    annotate("text", x = 0.0, y = 0, label = "Discordo totalmente", 
             color = lik_colors[1], hjust = 0, vjust = 0) +
    annotate("text", x = 0.0, y = 1, label = "Concordo totalmente", 
             color = lik_colors[7], hjust = 1, vjust = 0) +
    theme_minimal() +
    labs(title = NULL, # "Sumário de todas as respostas:",
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

  survey_data  |>
    select(matches("^Q[01][[:digit:]][[:blank:]]")) |> 
    pivot_longer(cols = everything(),
                 names_to = "question",
                 values_to = "answer") |> 
    mutate(question_index = str_extract(question, "^Q[01][[:digit:]]")) |>
    filter(question_index %in% group_category) |> 
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
    geom_col(position = "stack", width = 0.5) +
    geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white") +
    theme_minimal() +
    labs(title = "",
         x = NULL,
         y = NULL,
         fill = NULL) +
    scale_fill_manual(values = lik_colors) +
    scale_x_discrete(labels = label_wrap_gen(25)) +
    annotate("text", x = 0.5, y = 0, label = "Discordo totalmente", 
             color = lik_colors[1], hjust = 0, vjust = 0) +
    annotate("text", x = 0.5, y = 1, label = "Concordo totalmente", 
             color = lik_colors[7], hjust = 1, vjust = 0) +
    coord_flip() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}



facet_linha_likert_by <- function(survey_data, group_category = NULL) {
  
  if(is.null(group_category)) {
    error(-1)
  }
  
  df <- 
    survey_data  |>
    select(matches("^(Q|O)")) |> 
    pivot_longer(cols = matches("^Q[01][[:digit:]][[:blank:]]"),
                 names_to = "question",
                 values_to = "answer") |>  
    mutate(question_index = str_extract(question, "^Q[01][[:digit:]]")) 
  
  new_names <- names(df)
  names(df) <- c(c("linha", "turno", "contrato"), new_names[4:6])
  
  df |> 
    filter(question_index %in% group_category) |> 
    group_by(question, linha) |>
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
    geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white") +
    theme_minimal() +
    labs(title = NULL,
         x = NULL,
         y = NULL,
         fill = NULL) +
    scale_fill_manual(values = lik_colors) +
    scale_x_discrete(labels = label_wrap_gen(45)) +
    annotate("text", x = 0, y = 0, label = "Discordo totalmente", 
             color = lik_colors[1], hjust = 0, vjust = 0) +
    annotate("text", x = 0, y = 1, label = "Concordo totalmente", 
             color = lik_colors[7], hjust = 1, vjust = 0) +
    facet_wrap(~ factor(linha, levels = c("M&M", "SNICKERS", "TWIX", "Manutenção", "Outra")), ncol = 1) +
    coord_flip() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

facet_turno_likert_by <- function(survey_data, group_category = NULL) {
  
  if(is.null(group_category)) {
    error(-1)
  }
  
  df <- 
    survey_data  |>
    select(matches("^(Q|O)")) |> 
    pivot_longer(cols = matches("^Q[01][[:digit:]][[:blank:]]"),
                 names_to = "question",
                 values_to = "answer") |>  
    mutate(question_index = str_extract(question, "^Q[01][[:digit:]]")) 
  
  new_names <- names(df)
  names(df) <- c(c("linha", "turno", "contrato"), new_names[4:6])
  
  df |> 
    filter(question_index %in% group_category) |> 
    group_by(question, turno) |>
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
    geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white") +
    theme_minimal() +
    labs(title = NULL,
         x = NULL,
         y = NULL,
         fill = NULL) +
    scale_fill_manual(values = lik_colors) +
    scale_x_discrete(labels = label_wrap_gen(45)) +
    annotate("text", x = 0, y = 0, label = "Discordo totalmente", 
             color = lik_colors[1], hjust = 0, vjust = 0) +
    annotate("text", x = 0, y = 1, label = "Concordo totalmente", 
             color = lik_colors[7], hjust = 1, vjust = 0) +
    facet_wrap(~ factor(turno, levels = c("A", "B", "C", "F", "Adm")), ncol = 1) +
    coord_flip() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

facet_contrato_likert_by <- function(survey_data, group_category = NULL) {
  
  if(is.null(group_category)) {
    error(-1)
  }
  
  df <- 
    survey_data  |>
    select(matches("^(Q|O)")) |> 
    pivot_longer(cols = matches("^Q[01][[:digit:]][[:blank:]]"),
                 names_to = "question",
                 values_to = "answer") |>  
    mutate(question_index = str_extract(question, "^Q[01][[:digit:]]")) 
  
  new_names <- names(df)
  names(df) <- c(c("linha", "turno", "contrato"), new_names[4:6])
  
  df |> 
    filter(question_index %in% group_category) |> 
    group_by(question, contrato) |>
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
    geom_text(size = 2, position = position_stack(vjust = 0.5), color = "white") +
    theme_minimal() +
    labs(title = NULL,
         x = NULL,
         y = NULL,
         fill = NULL) +
    scale_fill_manual(values = lik_colors) +
    scale_x_discrete(labels = label_wrap_gen(45)) +
    annotate("text", x = 0, y = 0, label = "Discordo totalmente", 
             color = lik_colors[1], hjust = 0, vjust = 0) +
    annotate("text", x = 0, y = 1, label = "Concordo totalmente", 
             color = lik_colors[7], hjust = 1, vjust = 0) +
    facet_wrap(~ factor(contrato, levels = c("Associado por tempo indeterminado", 
                                             "Associado por tempo determinado",
                                             "Temporário",
                                             "Outro")), ncol = 1) +
    coord_flip() +
    theme(
      axis.text.x = element_text(size = 2),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}


# Retorna os dados em quantidades por valor
get_survey_answers_by_question <- function(survey_data, question) {
  answers <- get_survey_answers(survey_data)
  answers |> 
    select(Q01) |> 
    count(Q01) |> 
    mutate(prop = scales::percent(n/sum(n))) |> 
    arrange(Q01) |> 
    gt()
}

plot_bar_CI <- function(survey_data, question) {
  #question <- "Q09"
  the_title <-
    survey_data |>
    select(starts_with(question)) |>
    names()
  
  q4 <-
    survey_data |>
    select(starts_with(question))
  names(q4) <- question
  q4 <-
    q4 |>
    count(!!sym(question)) |>
    pull(n)
  q4
  
  tq4 <-
    as_tibble(MultinomCI(q4, conf.level = 0.95, method = "wilson")) |>
    mutate(answer = as.character(row_number()))
  
  tq4 |>
    ggplot(aes(answer, est, fill = answer)) +
    geom_col() +
    geom_text(
      aes(label = scales::percent(round(est, 2))),
      vjust = -1,
      hjust = -0.2,
      size = 3
    ) +
    geom_pointrange(aes(ymin = lwr.ci, ymax = upr.ci), color = "#CA7700") +
    scale_y_continuous(labels = scales::percent_format(),
                       breaks = c(0, 0.2, 0.4, 0.6)) +
    scale_fill_manual(values = lik_colors) +
    theme_minimal() +
    labs(title =   the_title,
         x = NULL,
         y = NULL) +
    theme(
      axis.text.y = element_blank(),
      legend.position = "none",
      panel.grid = element_blank()
    ) +
    coord_flip() +
    geom_text(aes(
      x = 1,
      y = 0,
      label = "Discordo totalmente ",
      vjust = 1,
      hjust = 1
    ),
    color = lik_colors[1]) +
    geom_text(aes(
      x = dim(tq4)[1],
      y = 0,
      label = "Concordo totalmente ",
      vjust = 1,
      hjust = 1
    ),
    color = lik_colors[7]) +
    expand_limits(y = -0.2)
  
  
}

