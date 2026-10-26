options(scipen = 9999)
try(dev.off(), silent = T)

# installing and loading required packages ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE)) {
    stop("Package pacman not found")
  }
}
library(pacman)

packages_CRAN <- c(
  "tidyverse",
  "ungroup",
  "rstan",
  "scales"
)

# Install required CRAN packages if not available yet
if (sum(!p_isinstalled(packages_CRAN)) > 0) {
  install_these <- packages_CRAN[!p_isinstalled(packages_CRAN)]
  for (i in 1:length(install_these)) {
    install.packages(install_these[i], dependencies = "Depends")
  }
}

# Load the required CRAN packages
p_load(packages_CRAN, character.only = TRUE)


# extract the posterior draws of conflict deaths (D_x_gen) from the Stan CSV files
gimeconfdeaths <- function(file_names = file_names23) {
  model_out <- read_stan_csv(paste0(results_dir, file_names, ".csv"))

  ### extract the model-generated mortality distributions (incl WPP deaths)
  du_x = rstan::extract(model_out, pars = c("D_x_gen"))$D_x_gen

  # all-cause
  dim(du_x)
  du_x[1, 1, ] # females
  du_x[1, 2, ] # males

  dx_f <-
    du_x[, 1, ] %>%
    as_tibble() %>%
    setNames(x) %>%
    mutate(it = 1:n()) %>%
    gather(-it, key = age, value = dx) %>%
    mutate(sex = "f", cause = "all")

  dx_m <-
    du_x[, 2, ] %>%
    as_tibble() %>%
    setNames(x) %>%
    mutate(it = 1:n()) %>%
    gather(-it, key = age, value = dx) %>%
    mutate(sex = "m", cause = "all")

  dxs <-
    bind_rows(dx_f, dx_m) %>%
    mutate(age = age %>% as.integer())

  return(dxs)
}

lifetable <- function(dt_in) {
  x <- dt_in$age
  mx <- dt_in$mx
  sex <- unique(dt_in$sex)
  year <- unique(dt_in$year)

  m <- length(x)
  n <- c(diff(x), NA)
  ax <- rep(0, m)
  if (x[1] != 0 | x[2] != 1) {
    ax <- n / 2
    ax[m] <- 1 / mx[m]
  } else {
    if (sex == "f") {
      if (mx[1] < 0.01724) {
        ax[1] <- 0.14903 - 2.05527 * mx[1]
      } else if (mx[1] >= 0.01724 & mx[1] < 0.06891) {
        ax[1] <- 0.04667 + 3.88089 * mx[1]
      } else {
        ax[1] <- 0.31411
      }
    }
    if (sex == "m") {
      if (mx[1] < 0.02300) {
        ax[1] <- 0.14929 - 1.99545 * mx[1]
      } else if (mx[1] >= 0.02300 & mx[1] < 0.08307) {
        ax[1] <- 0.02832 + 3.26021 * mx[1]
      } else {
        ax[1] <- 0.29915
      }
    }
    ax[-1] <- n[-1] / 2
    ax[m] <- 1 / mx[m]
  }
  qx <- n * mx / (1 + (n - ax) * mx)
  qx[m] <- 1
  px <- 1 - qx
  lx <- cumprod(c(1, px)) * 100000
  dx <- -diff(lx)
  Lx <- n * lx[-1] + ax * dx
  lx <- lx[-(m + 1)]
  Lx[m] <- lx[m] / mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  dt_out <- data.frame(age = x, mx, ex)
  return(dt_out)
}

