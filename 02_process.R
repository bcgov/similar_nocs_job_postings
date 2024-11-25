#libraries---------------------
library(tidyverse)
library(tidytext)
library(here)
library(conflicted)
conflicts_prefer(dplyr::filter)

cip_counts <- read_rds(here("data","cip_counts.rds"))
skill_counts <- read_rds(here("data","skill_counts.rds"))
min_experience_counts <- read_rds(here("data", "min_experience_counts.rds"))
education_counts <- read_rds(here("data", "education_counts.rds"))


#bind features together, calculate tf_idf
tf_idf <- bind_rows(cip_counts, skill_counts, min_experience_counts, education_counts)|>
  ungroup()|>
  filter(!name %in% c("skill: English Language",
                      "skill: French Language",
                      "education: No Education Listed"))|>
  bind_tf_idf(name, noc_2021_5_name, n)

tf_idf|>
  group_by(noc_2021_5_name)|>
  slice_max(tf, n=100)|>
  select(noc_2021_5_name, name, tf)|>
  write_rds(here("out", "tf.rds"))

tf_idf|>
  group_by(noc_2021_5_name)|>
  slice_max(tf_idf, n=100)|>
  select(noc_2021_5_name, name, tf_idf)|>
  write_rds(here("out", "tf_idf.rds"))
