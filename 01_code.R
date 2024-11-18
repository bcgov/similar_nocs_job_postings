#libraries-------------
library(tidyverse)
library(tidytext)
library(here)
library(conflicted)
conflicts_prefer(dplyr::filter)
#constants--------------------
n <- 100 #the number of terms to keep
keep_cols <- c("MIN_YEARS_EXPERIENCE", "SKILLS_NAME", "CERTIFICATIONS_NAME", "EDUCATION_LEVELS_NAME", "NOC_2021_5_NAME", "CIP4_NAME")
#functions------------------
clean_it <- function(strng){
  vec <- strng|>
    str_split(",")|>
    unlist()|>
    str_remove_all(pattern="\n")

  clean <- gsub("[^[:alnum:][:space:]]","", vec)|>
    trimws()

  tibble(value=clean)
}

topn <- function(tbbl, var, number){
  tbbl|>
    select(noc_2021_5_name, value, {{  var  }})|>
    group_by(noc_2021_5_name)|>
    slice_max({{  var  }}, n = number, with_ties = FALSE)|>
    write_rds(here::here("out", paste0(deparse(substitute(var)),".rds")))
}

#read and clean the data---------------
tbbl <- vroom::vroom(here::here("data","job_postings_May29_2024.csv"), col_select = all_of(keep_cols), na = "[]")|>
  janitor::clean_names()|>
  mutate(min_years_experience=as.character(min_years_experience))|>
  group_by(noc_2021_5_name)|>
  mutate(num_postings=n())|>
  filter(num_postings > 6)|> #filter out hunters and trappers and all occupations less common than
  select(-num_postings)|>
  pivot_longer(cols=-noc_2021_5_name)|>
  mutate(value=map(value, clean_it))|>
  unnest(value)|>
  na.omit()|>
  unite(value, name, value, sep=": ")|>
  group_by(noc_2021_5_name, value)|>
  count()
# calculate tf and tfidf----------------
tfidf <- tbbl|>
  bind_tf_idf(value, noc_2021_5_name, n)

topn(tfidf, tf, n)

topn(tfidf, tf_idf, n)


