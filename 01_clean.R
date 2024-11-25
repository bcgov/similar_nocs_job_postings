#libraries---------------------
library(DBI)
library(dbplyr)
library(tidyverse)
library(furrr)
library(here)
library(conflicted)
conflicts_prefer(dplyr::filter)
plan(multisession, workers=15)
#functions--------------------
clean_it <-  function(strng) { #apparently native pipe |> doesn't support pronouns .
  clean <- strng %>%
    str_split(",") %>%
    unlist() %>%
    gsub("[^[:alnum:][:space:]]", "", .) %>%
    trimws() %>%
    str_remove_all(pattern = "\n")

  tibble(name = clean)
}

column_counts <- function(tbbl, var, var_short){
  tbbl|>
    select(noc_2021_5_name, {{  var  }})|>
 #   head(10000)|> #just a subset for testing
    collect()|>
    mutate(names=future_map({{  var  }}, clean_it))|>
    unnest(names)|>
    group_by(noc_2021_5_name, name)|>
    count()|>
    mutate(name=paste0(var_short, ": ", name))
}
#connect to database------------------------------
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = "WALT,1521",  # server name and port combined
  Database = "Sandbox_DPBT",
  Trusted_Connection = "Yes"
)

tbbl <- tbl(con, "job_postings")
#find column names to clean and count-----------------------
colnames(tbbl)

#skill counts--------------------------
tictoc::tic()
column_counts(tbbl, skills_name, "skill")|>
  filter(name!="skill: ")|> #empty skills
  write_rds(here::here("data", "skill_counts.rds"))

#cip counts--------------------------

column_counts(tbbl, cip4, "cip4")|>
  filter(name!="cip4: ")|>
  write_rds(here::here("data", "cip_counts.rds"))

#education counts--------------------------

column_counts(tbbl, education_levels_name, "education")|>
  filter(name!="education: ")|>
  write_rds(here::here("data", "education_counts.rds"))

#min experience counts (min experience is tidy, so no need to clean)

tbbl|>
  select(noc_2021_5_name, min_years_experience)|>
  group_by(noc_2021_5_name, min_years_experience)|>
  count()|>
  collect()|>
  na.omit()|>
  rename(name=min_years_experience)|>
  mutate(name=paste0("min_experience: ", name))|>
  write_rds(here::here("data","min_experience_counts.rds"))
tictoc::toc()
dbDisconnect(con)
