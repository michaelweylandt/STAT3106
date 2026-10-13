library(tidyverse)
library(lubridate)
library(nasapower)

start_date <- as.Date("2022-01-01")
end_date   <- as.Date("2023-12-31")

months_seq <- format(seq(start_date, end_date, by = "1 month"), "%Y%m")
base_s3    <- "https://s3.amazonaws.com/capitalbikeshare-data/"

# Directory to store raw zip archives
raw_data_dir <- fs::path("problem_sets", "data", "raw_bikeshare_zips")
if (!fs::dir_exists(raw_data_dir)) fs::dir_create(raw_data_dir)

options(timeout = max(300, getOption("timeout")))

get_hourly_trips <- map(months_seq, function(m) {
    zip_filename <- paste0(m, "-capitalbikeshare-tripdata.zip")
    csv_filename <- paste0(m, "-capitalbikeshare-tripdata.csv")
    local_zip    <- fs::path(raw_data_dir, zip_filename)
    zip_url      <- paste0(base_s3, zip_filename)

    if (!fs::file_exists(local_zip) || fs::file_info(local_zip)$size < 1e6) {
        cat(sprintf("Downloading %s...\n", zip_filename))
        unlink(local_zip)
        download.file(zip_url, destfile = local_zip, mode = "wb", quiet = TRUE)
    }

    `%not.in%` <- Negate(`%in%`)
    if(csv_filename %not.in% unzip(local_zip, list=TRUE)$Name){
        unz_choices <- unzip(local_zip, list=TRUE)$Name |>
            str_subset("csv$") |>
            str_subset("MACOSX", negate=TRUE)

        if(length(unz_choices) == 1){
            cat("Replacing CSV - ", csv_filename, "- with ", unz_choices, "\n")
            csv_filename <- unz_choices
        } else {
            browser()
        }
    }

    # Stream only the started_at timestamp
    read_csv(
        unz(local_zip, csv_filename),
        col_select = started_at,
        col_types = cols_only(started_at = col_datetime()),
        show_col_types = FALSE
    ) |>
        transmute(time = floor_date(started_at, unit = "hour")) |>
        count(time, name = "ridership")
}) |>
    bind_rows() |>
    # Ensure all consecutive hours are present (fill zero-ride hours)
    complete(
        time = seq(
            make_datetime(2022, 1, 1, 0, 0, 0),
            make_datetime(2023, 12, 31, 23, 0, 0),
            by = "1 hour"
        ),
        fill = list(ridership = 0)
    )

weather_cache_file <- fs::path("problem_sets", "data", "weather_dc_2022_2023.rds")

if (!file.exists(weather_cache_file)) {
    cat("Querying NASA POWER weather API...\n")
    weather_raw <- get_power(
        community = "re",
        pars = c("T2M", "RH2M", "WS10M", "PRECTOTCORR"),
        temporal_api = "hourly",
        lonlat = c(-77.03, 38.90),
        dates = c("2022-01-01", "2023-12-31")
    ) |>
        as_tibble()

    saveRDS(weather_raw, weather_cache_file)
} else {
    weather_raw <- readRDS(weather_cache_file)
}

weather_df <- weather_raw |>
    transmute(
        time       = ymd_h(paste(YEAR, MO, DY, HR)),
        temp       = T2M,
        humidity   = RH2M,
        wind_speed = WS10M,
        precip     = PRECTOTCORR
    )

dc_bike <- hourly_trips |>
    inner_join(weather_df, by = "time") |>
    mutate(
        hour          = hour(time),
        day_of_year   = yday(time),
        day_of_week   = wday(time, label = TRUE, abbr = TRUE),
        is_weekend    = factor(if_else(day_of_week %in% c("Sat", "Sun"), "Weekend", "Weekday")),
        year          = factor(year(time)),
        log_ridership = log(ridership + 1)
    ) |>
    arrange(time)

cat(sprintf("Ready: %d hourly rows with %d columns.\n", nrow(dc_bike), ncol(dc_bike)))

write_csv(dc_bike, fs::path("problem_sets", "data", "ps02_dc_bike.csv"))
fs::file_copy(fs::path("problem_sets", "data", "ps02_dc_bike.csv"),
              fs::path("docs", "problem_sets", "data", "ps02_dc_bike.csv"),
              overwrite = TRUE)
