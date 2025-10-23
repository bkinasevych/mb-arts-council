
# Import packages ---------------------------------------------------------

library(surveymonkey)
library(tidyverse)


# Import data -------------------------------------------------------------

# get survey monkey API token
options(sm_oauth_token = Sys.getenv("SM_TOKEN"))

# get survey data
survey_df <- 525018787 |> 
  fetch_survey_obj() |> 
  parse_survey()

# set new column headers
col_names <- (
  c("x_survey_id",
    "x_collector_id",
    "id",
    "date_created",
    "x_date_modified",
    "response_status",
    "x_ip_address",
    "funded_past_3_yrs",
    "applied_not_funded",
    "org_role",
    "org_role_other_txt",
    "primary_focus",
    "primary_focus_other_txt",
    "postal_code",
    "purpose",
    "confident_in_mac_ability",
    "deadline_comms",
    "guidelines_comms",
    "manipogo",
    "trasparent_granting",
    "response_enquiries",
    "response_results",
    "feedback",
    "complaints_process",
    "improve",
    "connection_to_arts_community",
    "disconnect_explain_txt",
    "rank_advocacy",
    "rank_measure_impact",
    "rank_board_trg",
    "rank_financial_trg",
    "rank_leadership_trg",
    "rank_shared_services",
    "rank_loan_access",
    "rank_networking",
    "rank_marketing",
    "future_impact_txt",
    "comments_txt"
  )
)

# rename columns
names(survey_df) <- col_names


# Clean and export survey -------------------------------------------------

survey_df <- survey_df |>
  select(-starts_with("x")) |> 
  mutate(date_created = date(date_created))

write_rds(survey_df, "data/survey_data_clean.rds")