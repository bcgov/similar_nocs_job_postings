#libraries-------------
library(tidyverse)
library(tidytext)
library(vroom)
library(janitor)
library(factoextra)
library(here)
library(conflicted)
conflicts_prefer(dplyr::filter)
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

#read and clean the data---------------
tbbl <- vroom(here("data","job_postings_May29_2024.csv"), col_select = all_of(keep_cols), na = "[]")|>
  clean_names()|>
  mutate(min_years_experience=as.character(min_years_experience))|>
  group_by(noc_2021_5_name)|>
  mutate(num_postings=n())|>
  filter(num_postings > 6)|> #filter out hunters and trappers and all occupations less common
  select(-num_postings)|>
  pivot_longer(cols=-noc_2021_5_name)|>
  mutate(value=map(value, clean_it))|>
  unnest(value)|>
  na.omit()|>
  unite(value, name, value, sep=": ")|>
  group_by(noc_2021_5_name, value)|>
  count()

# calculate the term frequency inverse document frequency, make it wide----------------
tfidf <- tbbl|>
  bind_tf_idf(value, noc_2021_5_name, n)

tf <- tfidf|>
  select(noc_2021_5_name, value, tf)|>
  group_by(noc_2021_5_name)|>
  mutate(num_features=n())|>
  slice_max(tf, n = 50, with_ties = FALSE) #keep the top 50 most common terms for each NOC.

write_rds(tf, here("out","tf.rds"))

tfidf <- tfidf|>
  select(noc_2021_5_name, value, tf_idf)|>
  group_by(noc_2021_5_name)|>
  mutate(num_features=n())|>
  slice_max(tf_idf, n = 50, with_ties = FALSE) #keep the top 50 NOC defining terms.

write_rds(tfidf, here("out","tfidf.rds"))

#move this stuff to the app(so user can choose number of terms <= 50 to retain)

wide_tfidf <- tfidf|>
  pivot_wider(names_from = "value", values_from = "tf_idf")

wide_tf <- tf|>
  pivot_wider(names_from = "value", values_from = "tf")

tictoc::tic()
wide_tfidf[is.na(wide_tfidf)] <- 0
tictoc::toc()

library(data.table)
tictoc::tic()
setDT(wide_tfidf)  # Convert the data.frame to a data.table in-place
wide_tfidf[is.na(wide_tfidf)] <- 0
tictoc::toc()

wide_tf[is.na(wide_tf)] <- 0

#do principal component analysis----------------
pca_tfidf <- wide_tfidf|>
  column_to_rownames("noc_2021_5_name")|>
  scale()|>
  prcomp()

pca_tf<- wide_tf|>
  column_to_rownames("noc_2021_5_name")|>
  scale()|>
  prcomp()

first_tfidf <- pca_tfidf$x[, 1:100] #keep first 100 dimensions

first_tf <- pca_tf$x[, 1:100] #keep first 100 dimensions

write_rds(first_tfidf, here("out","first_tfidf.rds"))

write_rds(first_tf, here("out","first_tf.rds"))

