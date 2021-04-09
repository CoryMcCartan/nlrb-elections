library(here)
library(tidyverse)
library(janitor)
library(lubridate)

raw = read_csv(here("data-raw/recent_elections.csv"))

d = clean_names(raw) %>%
    filter(status == "Closed",
           str_detect(ballot_type, "Single"),
           is.na(labor_union2), is.na(labor_union3)) %>%
    select(-status, -matches("union[23]"), -matches("unit_unit_[abcd]"),
           -union_to_certify) %>%
    group_by(case) %>%
    filter(length(unique(unit_id)) == 1) %>% #  one voting unit
    ungroup() %>%
    rename(eligible=no_of_eligible_voters,
           union=labor_union1,
           votes_yes=votes_for_labor_union1,
           votes_no=votes_against,
           votes_counted=total_ballots_counted,
           votes_challenged=challenged_ballots,
           challenges_determ=challenges_are_determinative) %>%
    filter(votes_yes <= votes_counted,
           votes_no <= votes_counted,
           votes_counted <= eligible) %>%
    select(-unit_id) %>%
    mutate(across(c(case_name, city, union), str_to_upper),
           across(contains("date"), mdy)) %>%
    distinct()

write_rds(d, here("data/one_union_elec.rds"), compress="xz")
