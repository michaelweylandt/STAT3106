### Download and prep STA 9890 Competition Data

### Target NYS Test Scores
library(tidyverse)
library(fs)
library(mdbr)
library(rvest)
library(digest)
library(glue)

conflicted::conflicts_prefer(dplyr::select)


set.seed(9890)
SALT <- "STA9890-2026-SPRING-NYSED"

sha1_salt <- function(x, trim_to=8){
    n_x <- n_distinct(x)

    Digest <- Vectorize(digest, "object")
    res <- Digest(paste0(SALT, as.character(x)), algo="sha1")

    n_res <- res |> str_sub(end=trim_to) |> n_distinct()

    if(n_res == n_x) return(res |> str_sub(end=trim_to))

    stop("TRIMMING ERROR")
}


DIR <- dir_create("problem_sets/data/ps01_raw")

options(timeout=300)

download_if_needed <- function(url){
    destfile <- fs::path(DIR, basename(url))
    if(file_exists(destfile)) return(destfile)
    download.file(url,
                  destfile=fs::path(DIR, basename(url)),
                  mode="wb")
    destfile
}

YEAR_OF_ANALYSIS <- 2024

LINKS <- c(
    "https://data.nysed.gov/files/essa/23-24/SRC2024.zip",
    "https://data.nysed.gov/files/enrollment/23-24/enrollment_2024.zip",
    "https://data.nysed.gov/files/gradrate/23-24/gradrate.zip",
    "https://data.nysed.gov/files/studed/23-24/STUDED2024.zip",
    "https://data.nysed.gov/files/apib/2324/APIB24.zip"
)

LINKS |>
    map(download_if_needed, .progress=TRUE) |>
    str_subset("zip$") |>
    walk(\(zip) if(!dir_exists(zip |> str_sub(end=-5))) unzip(zip, exdir=DIR))

## The table "BOCES and N/RC" gives a table of all schools aligned to district
## and county info
#SRC_FILE <- dir_ls(path(DIR, glue("SRC{YEAR_OF_ANALYSIS}")), glob = "*mdb") |> first()
SRC_FILE <- path(DIR, "SRC2024_Group5.mdb")
SCHOOLS <- read_mdb(SRC_FILE, "BOCES and N/RC") |>
    filter(YEAR == YEAR_OF_ANALYSIS) |>
    mutate(DISTRICT_TYPE = case_when(
        NEEDS_INDEX == 1 ~ "NYC",
        NEEDS_INDEX == 2 ~ "Other Large City",
        NEEDS_INDEX == 3 ~ "High-Need Urban/Suburban",
        NEEDS_INDEX == 4 ~ "High-Need Rural",
        NEEDS_INDEX == 5 ~ "Average Need",
        NEEDS_INDEX == 6 ~ "Low Need",
        NEEDS_INDEX == 7 ~ "Charter School",
        .unmatched = "error"
    )) |>
    select(-NEEDS_INDEX, -NEEDS_INDEX_DESCRIPTION) |>
    filter(YEAR == YEAR_OF_ANALYSIS)

NYS_REGIONS <- read_html("https://en.wikipedia.org/wiki/Category:Regions_of_New_York_(state)") |>
    html_elements("dd") |>
    map(\(dd) dd |> html_elements("a") |> html_attr("title")) |>
    keep(\(x) length(x) > 0) |>
    map(\(title_vec) data.frame(REGION=title_vec[1], COUNTY=title_vec[-1])) |>
    bind_rows() |>
    filter(str_detect(COUNTY, "County")) |>
    mutate(COUNTY = str_extract(COUNTY, "^(.+) County", group=1),
           COUNTY = str_to_upper(COUNTY),
           COUNTY = str_replace(COUNTY, fixed("ST."), "SAINT"))

NYS_REGIONS <- rbind(NYS_REGIONS,
                     data.frame(REGION = "New York City",
                                COUNTY = "NYC CENTRAL OFFICE"))

SCHOOLS <- inner_join(SCHOOLS, NYS_REGIONS, join_by(COUNTY_NAME == COUNTY))

## Next, let's pull out all of the assessment data we have
SRC_EM_TABLES <- mdb_tables(SRC_FILE) |>
    str_subset("Annual EM") |>
    map(\(tabname) read_mdb(SRC_FILE, tabname) |>
            filter(!is.na(INSTITUTION_ID)) |>
            filter(INSTITUTION_ID %in% SCHOOLS$INSTITUTION_ID) |>
            filter(!str_detect(ASSESSMENT_NAME, "_")) |>
            filter(YEAR == YEAR_OF_ANALYSIS) |>
            filter(SUBGROUP_NAME %in% c("All Students",
                                        "Male",
                                        "Female",
                                        "Economically Disadvantaged",
                                        "Not Economically Disadvantaged"
            )) |>
            select(INSTITUTION_ID,
                   SUBGROUP_NAME,
                   ASSESSMENT_NAME,
                   N_STUDENTS = NUM_TESTED,
                   PERCENT_PROFICIENT = PER_PROF)) |>
    bind_rows()

stopifnot(NROW(SRC_EM_TABLES) == n_distinct(SRC_EM_TABLES))

SRC_REGENTS_TABLES <- read_mdb(SRC_FILE, "Annual Regents Exams") |>
    filter(!is.na(INSTITUTION_ID)) |>
    filter(INSTITUTION_ID %in% SCHOOLS$INSTITUTION_ID) |>
    filter(YEAR == YEAR_OF_ANALYSIS) |>
    filter(SUBGROUP_NAME %in% c("All Students",
                                "Male",
                                "Female",
                                "Economically Disadvantaged",
                                "Not Economically Disadvantaged"
    )) |>
    select(INSTITUTION_ID,
           SUBGROUP_NAME,
           ASSESSMENT_NAME = SUBJECT,
           N_STUDENTS = TESTED,
           PERCENT_PROFICIENT = PER_PROF)


stopifnot(NROW(SRC_REGENTS_TABLES) == n_distinct(SRC_REGENTS_TABLES))


ALL_ASSESSMENTS <- rbind(SRC_EM_TABLES, SRC_REGENTS_TABLES) |>
    filter(N_STUDENTS > 0) |>
    filter(PERCENT_PROFICIENT != "s") |>
    mutate(PERCENT_PROFICIENT = as.integer(PERCENT_PROFICIENT))

MATH_ASSESSMENTS_ALL <- ALL_ASSESSMENTS |>
    filter(SUBGROUP_NAME == "All Students",
           str_detect(ASSESSMENT_NAME, "(Math|Algebra|Geometry)"))

# This gives us ~450K scores to predict. Let's now work on loading more
# demographic covariates
ENROLLMENT_FILE <- path(DIR, "ENROLL2024_20241105..mdb")
ENROLLMENT_BEDS <- read_mdb(ENROLLMENT_FILE, "BEDS Day Enrollment") |>
    filter(ENTITY_CD %in% SCHOOLS$ENTITY_CD,
           YEAR==YEAR_OF_ANALYSIS) |>
    mutate(K = KHALF + KFULL) |>
    select(ENTITY_CD,
           PRE_K = PK,
           K,
           GRADE_01 = `1`,
           GRADE_02 = `2`,
           GRADE_03 = `3`,
           GRADE_04 = `4`,
           GRADE_05 = `5`,
           GRADE_06 = `6`,
           GRADE_07 = `7`,
           GRADE_08 = `8`,
           GRADE_09 = `9`,
           GRADE_10 = `10`,
           GRADE_11 = `11`,
           GRADE_12 = `12`
    )

ENROLLMENT_DEMO <- read_mdb(ENROLLMENT_FILE, "Demographic Factors") |>
    filter(ENTITY_CD %in% SCHOOLS$ENTITY_CD,
           YEAR==YEAR_OF_ANALYSIS) |>
    select(ENTITY_CD,
           PERCENT_MALE = PER_MALE,
           PERCENT_FEMALE = PER_FEMALE,
           PERCENT_ENGLISH_LANGUAGE_LEANERS = PER_ELL,
           PERCENT_AMERICAN_INDIAN = PER_AM_IND,
           PERCENT_BLACK = PER_BLACK,
           PERCENT_ASIAN = PER_ASIAN,
           PERCENT_HISPANIC = PER_HISP,
           PERCENT_WHITE = PER_WHITE,
           PERCENT_MULTIRACIAL = PER_Multi,
           PERCENT_WITH_DISABILITIES = PER_SWD,
           PERCENT_ECONOMICALLY_DISADVANTAGED = PER_ECDIS,
           PERCENT_MIGRANT = PER_MIGRANT,
           PERCENT_HOMELESS = PER_HOMELESS,
           PERCENT_IN_FOSTER_CARE = PER_FOSTER,
           PERCENT_PARENT_ARMED_FORCES = PER_ARMED
    )

STUDENT_POPULATION <-
    inner_join(ENROLLMENT_BEDS,
               ENROLLMENT_DEMO,
               join_by(ENTITY_CD))

## Let's give this at the district level to make it a hair more challenging
DISTRICT_GRAD_PROFILE <- path(DIR,
                              "2024_GRADUATION_RATE.mdb") |>
    read_mdb("GRAD_RATE_AND_OUTCOMES_2024") |>
    filter(aggregation_type == "District",
           membership_code==9,
           subgroup_name == "All Students") |>
    select(INSTITUTION_ID,
           AGGREGATION_CODE = aggregation_code,
           PERCENT_DIPLOMA = grad_pct,
           PERCENT_NON_DIPLOMA = non_diploma_credential_pct,
           PERCENT_STILL_ENROLLED = still_enr_pct,
           PERCENT_GED = ged_pct,
           PERCENT_DROPOUT = dropout_pct) |>
    mutate(across(starts_with("PERCENT"), \(x) str_remove(x, "%") |> str_replace("-", "0") |> as.numeric())) |>
    filter(AGGREGATION_CODE %in% paste0(SCHOOLS$DISTRICT_CD, "0000"))

STUDENT_EDUCATOR_FILE <- path(DIR, "STUDED_2024.mdb")

ATTENDANCE <- read_mdb(STUDENT_EDUCATOR_FILE, "Attendance") |>
    filter(YEAR == YEAR_OF_ANALYSIS,
           ENTITY_CD %in% SCHOOLS$ENTITY_CD) |>
    select(ENTITY_CD, ATTENDANCE_RATE)

CLASS_SIZES <- read_mdb(STUDENT_EDUCATOR_FILE, "Average Class Size") |>
    filter(YEAR == YEAR_OF_ANALYSIS,
           ENTITY_CD %in% SCHOOLS$ENTITY_CD) |>
    select(ENTITY_CD, CLASS_DESCRIPTION, AVERAGE_CLASS_SIZE) |>
    filter(!is.na(CLASS_DESCRIPTION)) |>
    mutate(
        CLASS_DESCRIPTION = str_remove(CLASS_DESCRIPTION, fixed(" (Common Core)")),
        CLASS_DESCRIPTION = case_when(
            CLASS_DESCRIPTION %in% c("Algebra I",
                                     "Algebra II",
                                     "Geometry") ~ "Mathematics",
            CLASS_DESCRIPTION %in% c("ELA III", "ELA III (Common Core)") ~ "Language Arts",
            CLASS_DESCRIPTION %in% c("Biology",
                                     "Earth Science",
                                     "Chemistry",
                                     "Physics",
                                     "Science (grade 5)",
                                     "Science (grade 8)") ~ "Science",
            str_detect(CLASS_DESCRIPTION, "Language Arts") ~ "Language Arts",
            str_detect(CLASS_DESCRIPTION, "Mathematics") ~ "Mathematics",
            str_detect(CLASS_DESCRIPTION, "History") ~ "History, Government, and Geography",
            CLASS_DESCRIPTION %in% c("Grade 1", "Grade 2", "Kindergarten") ~ CLASS_DESCRIPTION,
            .unmatched="error"
        )) |>
    group_by(ENTITY_CD, CLASS_DESCRIPTION) |>
    summarize(AVERAGE_CLASS_SIZE = mean(AVERAGE_CLASS_SIZE, na.rm=TRUE)) |>
    ungroup() |>
    pivot_wider(values_from=AVERAGE_CLASS_SIZE,
                names_from=CLASS_DESCRIPTION) |>
    rename_with(\(x) str_to_upper(x) |>
                    str_replace_all(" ", "_") |>
                    str_remove_all(",") |>
                    paste0("_AVERAGE_CLASS_SIZE"),
                c(-`ENTITY_CD`))

LUNCH_DISC <- read_mdb(STUDENT_EDUCATOR_FILE, "Free Reduced Price Lunch") |>
    filter(YEAR == YEAR_OF_ANALYSIS,
           ENTITY_CD %in% SCHOOLS$ENTITY_CD) |>
    select(ENTITY_CD,
           PERCENT_FREE_LUNCH = PER_FREE_LUNCH,
           PERCENT_REDUCED_LUNCH = PER_REDUCED_LUNCH)

STAFF <- read_mdb(STUDENT_EDUCATOR_FILE, "Staff") |>
    filter(YEAR == YEAR_OF_ANALYSIS,
           ENTITY_CD %in% SCHOOLS$ENTITY_CD) |>
    select(ENTITY_CD,
           NUMBER_OF_TEACHERS = NUM_TEACH,
           NUMBER_OF_COUNSELORS = NUM_COUNSELORS,
           NUMBER_OF_SOCIAL_WORKERS = NUM_SOCIAL,
           TEACHER_TURNOVER_RATE = PER_TURN_ALL)

SUSPENSIONS <- read_mdb(STUDENT_EDUCATOR_FILE, "Suspensions") |>
    filter(YEAR == YEAR_OF_ANALYSIS,
           ENTITY_CD %in% SCHOOLS$ENTITY_CD) |>
    select(ENTITY_CD,
           PERCENT_OF_STUDENTS_SUSPENDED = PER_SUSPENSIONS)

EXPENDITURES <- read_mdb(SRC_FILE, "Expenditures per Pupil") |>
    filter(YEAR == YEAR_OF_ANALYSIS,
           ENTITY_CD %in% SCHOOLS$ENTITY_CD) |>
    select(ENTITY_CD,
           N_PUPILS=PUPIL_COUNT_TOT,
           FEDERAL_FUNDING_PER_PUPIL=PER_FEDERAL_EXP,
           LOCAL_FUNDING_PER_PUPIL=PER_STATE_LOCAL_EXP)

SCHOOL_INFO <- reduce(list(ATTENDANCE, CLASS_SIZES, LUNCH_DISC, STAFF, SUSPENSIONS, EXPENDITURES, STUDENT_POPULATION),
                      \(x, y) inner_join(x, y, join_by(ENTITY_CD)))

SCHOOLS_X <- left_join(SCHOOLS, SCHOOL_INFO, join_by(ENTITY_CD)) |>
    select(-ends_with("CD"),
           -starts_with("GRADE"),
           -"K",
           -"PRE_K",
           -ends_with("CLASS_SIZE"),
           "MATHEMATICS_AVERAGE_CLASS_SIZE",
           -starts_with("BOCES"),
           -"TEACHER_TURNOVER_RATE") |> drop_na()

FINAL_DATA <- inner_join(MATH_ASSESSMENTS_ALL,
                         SCHOOLS_X,
                         join_by(INSTITUTION_ID)) |>
    select(-INSTITUTION_ID,
           -SUBGROUP_NAME,
           -YEAR) |>
    rename(NUMBER_TOOK_EXAM=N_STUDENTS,
           NUMBER_TOTAL_STUDENTS_IN_SCHOOL=N_PUPILS) |>
    select(PERCENT_PROFICIENT,
           where(is.numeric),
           everything())

write_csv(FINAL_DATA, "problem_sets/data/ps01_nysed.csv")
