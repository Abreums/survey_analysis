# read_data.R
#
# Marcos Abreu
#
# This package has functions to help to read data from specific files.

# --------------
# Survey Monkey data has a weird header with 2 lines.
# This functions reads the first header line to be used as actual head line.
# Than it skips the 2 first lines and read the data
# Finally it applies the first line as header of the data 
#
readSurveyMonkeyData <- function(folder, file_name) {
  
  file_list <- list.files(path = folder,
                          pattern = "*.xlsx",
                          full.names = TRUE)
  
  file <- file_list[str_detect(file_list, file_name)]
  
  line1_header <- 
    read_excel(file,
               n_max = 1) |> 
    names()
  
  # Add "Q1, Q2..." to the begin of the question string.
  # Extract  "7 = Concordo Totalmente..." string
  questions_header <- line1_header[13:27]
  questions_header <- map2_chr(1:length(questions_header), 
                               questions_header, ~ str_c("Q", 
                                                         sprintf("%02d", .x), 
                                                         " ", 
                                                         str_extract(.y, "^.+?(?=7)")))
  the_header <- c(line1_header[1:12],
                  questions_header,
                  line1_header[28:29])
  
  survey_data <- read_excel(file,
                            sheet = "Sheet",
                            skip = 2,
                            col_names = FALSE)
  
  colnames(survey_data) <- the_header
  
  survey_data <- 
    survey_data |> 
    mutate(across(last_col(16):last_col(2), ~ factor(as.character(.), 
                                                     levels = c("1", "2", "3", "4", "5", "6", "7"),
                                                     ordered = TRUE)))
}



# Get Quantitative Answers 
getQtAnswers <- function(survey_data) {
  df <- 
    survey_data |> 
    select(last_col(19):last_col(2)) |> 
    pivot_longer(cols = c(last_col(14):last_col()),
                 names_to = "question",
                 values_to = "answer") |> 
    mutate(question = factor(str_trim(question, side = "both"))) |> 
    mutate(question_index = str_extract(question, "([^[[:space:]]]+)"))
}

# Get Qualitative Answers
getQlAnswers <- function(survey_data) {
  df <- 
    survey_data |> 
    select(10, 11, 12, last_col(1):last_col())
}

# Get respondent data
get_respondent_data <- function(survey_data, extra_columns) {
  if(missing(extra_columns)) {
    df <- 
      survey_data |> 
      select(1:8)
    names(df) <- c(names(df[1:8]))
  } else {
    df <- 
      survey_data |> 
      select(1:8, extra_columns)
    names(df) <- c(names(df[1:8]), names(extra_columns))  
  }
  
  df
}

