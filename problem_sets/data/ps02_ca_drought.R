library(tidyverse)
library(ncdf4)
library(lubridate)

sst_url  <- "https://downloads.psl.noaa.gov/Datasets/noaa.ersst.v5/sst.mnmean.nc"
sst_file <- "sst.mnmean.nc"
if (!file.exists(sst_file)) download.file(sst_url, sst_file, mode = "wb")

pdsi_url  <- "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/statewide/time-series/4/pdsi/1/0/1960-2023/data.csv"
pdsi_file <- "ca_pdsi_1960_2023.csv"
if (!file.exists(pdsi_file)) download.file(pdsi_url, pdsi_file, mode = "wb")

pdsi_df <- read_csv(
    pdsi_file,
    skip = 4,
    col_names = c("date_code", "pdsi"),
    col_types = "cd"
) |>
    mutate(
        date = make_date(
            year  = as.integer(substr(date_code, 1, 4)),
            month = as.integer(substr(date_code, 5, 6)),
            day   = 1
        )
    ) |>
    select(date, pdsi) |>
    filter(year(date) >= 1960, year(date) <= 2023)

nc <- nc_open(sst_file)

lons      <- ncvar_get(nc, "lon")       # 0 to 358 (step 2)
lats      <- ncvar_get(nc, "lat")       # -88 to 88 (step 2)
time_vals <- ncvar_get(nc, "time")      # Days since 1854-01-01
sst_dates <- as.Date("1854-01-01") + time_vals

target_idx <- which(year(sst_dates) >= 1960 & year(sst_dates) <= 2023)
n_months   <- length(target_idx)

# Subsample to 4° x 4° spatial grid
lon_step <- seq(1, length(lons), by = 2)
lat_step <- seq(1, length(lats), by = 2)

selected_lons <- lons[lon_step]
selected_lats <- lats[lat_step]

raw_sst <- ncvar_get(
    nc,
    "sst",
    start = c(1, 1, min(target_idx)),
    count = c(length(lons), length(lats), n_months)
)
nc_close(nc)

sub_sst <- raw_sst[lon_step, lat_step, ]

# Flatten [lon, lat, time] -> [time, grid_cells]
X_raw <- t(apply(sub_sst, 3, as.vector))

# Construct column names: "sst_lon180_lat+12"
coords <- expand.grid(lon = selected_lons, lat = selected_lats)
colnames(X_raw) <- sprintf("sst_lon%03.0f_lat%+03.0f", coords$lon, coords$lat)

# 1. Filter out land (cells containing NAs)
ocean_idx <- which(colSums(is.na(X_raw)) == 0)
X_ocean   <- X_raw[, ocean_idx]

# 2. Filter out perennial sea ice (cells with zero or negligible variance)
col_variances <- apply(X_ocean, 2, var)
active_ocean_idx <- which(col_variances > 1e-4)
X_active <- X_ocean[, active_ocean_idx]

# 3. De-seasonalize: compute anomalies relative to monthly climatology
month_vec <- month(sst_dates[target_idx])
X_anom    <- X_active

for (m in 1:12) {
    idx <- which(month_vec == m)
    means <- colMeans(X_active[idx, , drop = FALSE])
    X_anom[idx, ] <- sweep(X_active[idx, , drop = FALSE], 2, means, "-")
}

sst_tbl <- as_tibble(X_anom) |>
    mutate(date = make_date(year(sst_dates[target_idx]), month(sst_dates[target_idx]), 1))

ca_drought <- pdsi_df |>
    inner_join(sst_tbl, by = "date") |>
    relocate(date, pdsi)


cat(sprintf("Ready: %d monthly rows with %d columns.\n", nrow(ca_drought), ncol(ca_drought)))

write_csv(ca_drought, fs::path("problem_sets", "data", "ps02_ca_drought.csv"))
fs::file_copy(fs::path("problem_sets", "data", "ps02_ca_drought.csv"),
              fs::path("docs", "problem_sets", "data", "ps02_ca_drought.csv"),
              overwrite = TRUE)
