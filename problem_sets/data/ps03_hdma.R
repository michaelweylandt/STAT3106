library(tidyverse)
MSA <- read_csv("https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/metro-area-geography-reference/msa_county_reference22.txt") |>
    select(msa, msa_name) |>
    distinct() |>
    mutate(msa=as.character(msa))

MSA2 <- tibble::tribble(
    ~msa,                                                                 ~msa_name,
    "12060",                                       "Atlanta-Sandy Springs-Roswell, GA",
    "12054",                 "Atlanta-Sandy Springs-Roswell, GA Metropolitan Division",
    "31924",                                      "Marietta, GA Metropolitan Division",
    "14460",                                          "Boston-Cambridge-Newton, MA-NH",
    "14454",                                        "Boston, MA Metropolitan Division",
    "15764",                   "Cambridge-Newton-Framingham, MA Metropolitan Division",
    "40484",            "Rockingham County-Strafford County, NH Metropolitan Division",
    "16980",                                         "Chicago-Naperville-Elgin, IL-IN",
    "16984",                 "Chicago-Naperville-Schaumburg, IL Metropolitan Division",
    "20994",                                         "Elgin, IL Metropolitan Division",
    "29404",                                   "Lake County, IL Metropolitan Division",
    "29414",       "Lake County-Porter County-Jasper County, IN Metropolitan Division",
    "19100",                                         "Dallas-Fort Worth-Arlington, TX",
    "19124",                           "Dallas-Plano-Irving, TX Metropolitan Division",
    "23104",                "Fort Worth-Arlington-Grapevine, TX Metropolitan Division",
    "19820",                                             "Detroit-Warren-Dearborn, MI",
    "19804",                      "Detroit-Dearborn-Livonia, MI Metropolitan Division",
    "47664",                  "Warren-Troy-Farmington Hills, MI Metropolitan Division",
    "31080",                                      "Los Angeles-Long Beach-Anaheim, CA",
    "11244",                      "Anaheim-Santa Ana-Irvine, CA Metropolitan Division",
    "31084",               "Los Angeles-Long Beach-Glendale, CA Metropolitan Division",
    "33100",                               "Miami-Fort Lauderdale-West Palm Beach, FL",
    "22744",         "Fort Lauderdale-Pompano Beach-Sunrise, FL Metropolitan Division",
    "33124",                     "Miami-Miami Beach-Kendall, FL Metropolitan Division",
    "48424",       "West Palm Beach-Boca Raton-Delray Beach, FL Metropolitan Division",
    "35620",                                      "New York-Newark-Jersey City, NY-NJ",
    "29484",                        "Lakewood-New Brunswick, NJ Metropolitan Division",
    "35004",                  "Nassau County-Suffolk County, NY Metropolitan Division",
    "35084",                                        "Newark, NJ Metropolitan Division",
    "35614",          "New York-Jersey City-White Plains, NY-NJ Metropolitan Division",
    "37980",                             "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
    "15804",                                        "Camden, NJ Metropolitan Division",
    "33874", "Montgomery County-Bucks County-Chester County, PA Metropolitan Division",
    "37964",                                  "Philadelphia, PA Metropolitan Division",
    "48864",                              "Wilmington, DE-MD-NJ Metropolitan Division",
    "41860",                                       "San Francisco-Oakland-Fremont, CA",
    "36084",                      "Oakland-Fremont-Berkeley, CA Metropolitan Division",
    "41884",          "San Francisco-San Mateo-Redwood City, CA Metropolitan Division",
    "42034",                                    "San Rafael, CA Metropolitan Division",
    "42660",                                             "Seattle-Tacoma-Bellevue, WA",
    "21794",                                       "Everett, WA Metropolitan Division",
    "42644",                         "Seattle-Bellevue-Kent, WA Metropolitan Division",
    "45104",                               "Tacoma-Lakewood, WA Metropolitan Division",
    "45300",                                     "Tampa-St. Petersburg-Clearwater, FL",
    "41304",               "St. Petersburg-Clearwater-Largo, FL Metropolitan Division",
    "45294",                                         "Tampa, FL Metropolitan Division",
    "47900",                            "Washington-Arlington-Alexandria, DC-VA-MD-WV",
    "11694",                "Arlington-Alexandria-Reston, VA-WV Metropolitan Division",
    "23224",               "Frederick-Gaithersburg-Bethesda, MD Metropolitan Division",
    "47764",                                 "Washington, DC-MD Metropolitan Division"
)

MSA <- bind_rows(MSA, MSA2)

download_if_needed <- function(url, local_destination){
    if(!file.exists(local_destination)){
        download.file(url, local_destination)
    }
    local_destination
}

PROCESSED <- download_if_needed("https://files.ffiec.cfpb.gov/static-data/snapshot/2025/2025_public_lar_csv.zip",
                                "~/Downloads/2025_public_lar_csv.zip") |>
    read_csv(col_select=c(city=derived_msa_md,
                      state=state_code,
                      conforming_loan_limit,
                      starts_with("derived"),
                      action_taken,
                      preapproval,
                      loan_type,
                      loan_amount,
                      combined_loan_to_value_ratio,
                      interest_rate,
                      loan_term,
                      property_value,
                      income,
                      debt_to_income_ratio,
                      starts_with("tract"))) |>
    mutate(city = replace_values(as.character(city),
                                      from=MSA$msa,
                                      to=MSA$msa_name),
           city = if_else(city=="99999", "Rural", city)) |>
    filter_out(str_detect(derived_loan_product_type, "subordinate")) |>
    mutate(loan_type = str_extract(derived_loan_product_type, "(.*):.*",group=1)) |>
    select(-derived_loan_product_type) |>
    filter_out(is.na(conforming_loan_limit)) |>
    filter_out(conforming_loan_limit == "U") |>
    mutate(loan_size=replace_values(conforming_loan_limit,
                                    "C" ~ "standard",
                                    "NC" ~ "jumbo")) |>
    select(-conforming_loan_limit) |>
    filter(str_detect(derived_dwelling_category, "Single")) |>
    select(-derived_dwelling_category) |>
    mutate(hispanic_or_latino = case_when(
        derived_ethnicity == "Hispanic or Latino" ~ TRUE,
        derived_ethnicity == "Not Hispanic or Latino" ~ FALSE,
        TRUE ~ NA)) |>
    select(-derived_ethnicity) |>
    rename(race=derived_race,
           sex=derived_sex) |>
    mutate(approved=action_taken %in% c(1,2,6)) |>
    select(-action_taken) |>
    mutate(prior_preapproval=(preapproval==1)) |>
    select(-preapproval) |>
    mutate(ltv = as.numeric(combined_loan_to_value_ratio)) |>
    select(-combined_loan_to_value_ratio) |>
    mutate(interest_rate = as.numeric(interest_rate)) |>
    mutate(debt_to_income_ratio = recode_values(debt_to_income_ratio,
        "<20%" ~ "20",
        ">60%" ~ "61",
        "20%-<30%" ~ "30",
        "30%-36%" ~ "36",
        "50%-60%" ~ "60",
        "Exempt" ~ NA) |> as.numeric()) |>
    mutate(property_value=as.numeric(property_value)) |>
    filter_out(is.na(property_value)) |>
    select(-interest_rate) |>
    drop_na() |>
    filter_out(is.na(loan_term)) |>
    filter_out(income < 0)

write_csv(PROCESSED, "problem_sets/data/ps03_hdma.csv.gz")
