## ############################################################################
## parse health.txt
## ------------------------------------------------------------------------
## Created 18 Jan 2018
## Author Ingo Nader
## ############################################################################

## [[to do]]
## * Make shiny app and host it on shiny.io (+ grab file from dropbox via R?) or on azure?

## ============================================================================
## load packages
## ============================================================================

# devtools::install_github("ingonader/tskeyvalparser")

## [[to do]]
## * use packrat, checkpoint,  or some other solution.
library(readr)
library(stringr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scales)
library(testthat)
library(tibble)

library(tskeyvalparser)

## ========================================================================== ##
## global variables
## ========================================================================== ##

options(tibble.width = Inf)

## ========================================================================== ##
## function definitions
## ========================================================================== ##

#source("/Users/ingonader/Dropbox/lists/health-parse/health-parse-functions.R")

## ============================================================================
## analysis
## ============================================================================

path <- get_paths()
current_age <- floor(as.numeric(difftime(now(), as.Date("1978-10-19"), units = "days") / 365.242))
## [[to do]]
## correct this: needs to be the age at the time of the event!

dat_txt <- read_healthfiles(
  path = path$dat,
  filename_regex = "^health-[0-9]{4}.*\\.txt"
)

## remove comments:
dat_txt <- remove_comments(dat_txt)

#dat_txt <- dat_txt[-(383:385)]


## construct wide table for specific keys:
dat_wide <- tibble::tibble(
  "datetime" = get_timestamp(dat_txt),
  "weight" = get_value_num(dat_txt, key = "weight"),
  "bodyfat_value_txt" = get_value_text(dat_txt, key = "caliper"),
  "bodyfat_caliper" = calc_bodyfat(bodyfat_value_txt, age = current_age),
  "bodyfat_scale" = get_value_num(dat_txt, key = "body_fat") * 100,
  "bodyfat" = dplyr::coalesce(bodyfat_caliper, bodyfat_scale),
  "caliper_brust_li" = get_subkey_value_mean(bodyfat_value_txt, subkey = "brust-li"),
  "caliper_brust_re" = get_subkey_value_mean(bodyfat_value_txt, subkey = "brust-re"),
  "caliper_bauch_li" = get_subkey_value_mean(bodyfat_value_txt, subkey = "bauch-li"),
  "caliper_bauch_re" = get_subkey_value_mean(bodyfat_value_txt, subkey = "bauch-re"),
  "caliper_bein_li" = get_subkey_value_mean(bodyfat_value_txt, subkey = "bein-li"),
  "caliper_bein_re" = get_subkey_value_mean(bodyfat_value_txt, subkey = "bein-re"),
  "note" = get_value_text(dat_txt, key = "note"),
  "event" = get_value_text(dat_txt, key = "event"),
  "orig" = dat_txt
)

## add year and relative year:
dat_wide %<>%
  dplyr::mutate(
    xyear = year(datetime),
    datetime_rel = as.Date(datetime)  ## year will be changed to 1970 below
  )
year(dat_wide$datetime_rel) <- 1970

head(dat_wide)
dat_wide %>% dplyr::select(-orig) %>% tail()

## create a version of only events with appropriate y (weight) values for plotting labels:
dat_wide_eventonly <- dat_wide %>%
  dplyr::filter(event != "") %>%
  dplyr::mutate(
    weight_min = min(weight, na.rm = TRUE),
    bodyfat_min = min(bodyfat, na.rm = TRUE))

names(dat_wide)
tail(dat_wide)

dat_long <- get_data_long(dat_txt) %>% tibble::as_tibble()

dat_long_num <- dat_wide %>%
  dplyr::select(-note, -event, -orig, -bodyfat_value_txt) %>%
  reshape2::melt(id.vars = c("datetime", "xyear", "datetime_rel"))
head(dat_long_num)
tail(dat_long_num)
str(dat_long_num)

## [[here]]
## * adjust scales: show only month and not 1970
## * add full plot (without facets)
## * include NAs to devide bewtween long time spans
## * include adding relative timestamps in it's own function


add_events <- function(plot, eventdata, ypos, xpos = "rel") {
  plot <- plot + geom_text(
    data = eventdata,
    aes_string(y = ypos,
        label = "event"),
    alpha = .5, hjust = 0, vjust = -.3,
    size = 3, angle = 90
  )
  if (xpos == "rel") {
    plot <- plot + geom_vline(
      data = eventdata,
      aes(xintercept = as.numeric(datetime_rel)),
      color = "darkgrey", linetype = 2, alpha = .7, size = .5
    )
  } else if (xpos == "abs") {
    plot <- plot + geom_vline(
      data = eventdata,
      aes(xintercept = datetime),
      color = "darkgrey", linetype = 2, alpha = .7, size = .5
    )
  }
  return(plot)
}

## try a plot:
p <- ggplot(dat_wide, aes(x = datetime_rel, y = weight)) +
  geom_point(alpha = .6) +
  geom_smooth(span = .3) +
  facet_wrap(~ xyear, ncol = 3) +
  scale_x_date(
    labels = date_format("%b"),
    date_breaks = "2 month") +
  coord_cartesian(
    ylim = range(dat_wide$weight, na.rm = TRUE) + c(-1, +1)
  )
p %<>% add_events(dat_wide_eventonly, ypos = "weight_min", xpos = "rel")
p


# bodyfat trial:
p <- ggplot(dat_wide, aes(x = datetime_rel, y = bodyfat)) +
  geom_point(alpha = .6) +
  geom_smooth(span = .3) +
  facet_wrap(~ xyear, ncol = 3) +
  scale_x_date(
    labels = date_format("%b"),
    date_breaks = "2 month") +
  coord_cartesian(
    ylim = range(dat_wide$bodyfat, na.rm = TRUE) + c(-.5, +.5)
  )
p %<>% add_events(dat_wide_eventonly, ypos ="bodyfat_min", xpos = "rel")
p

dat_wide %>% dplyr::select(datetime, weight, bodyfat) %>% tail(n = 20)

## Plot for 12 + 2 months

## subset data:
mindate <- max(dat_wide$datetime, na.rm = TRUE) %m-% months(14)
dat_wide_14 <- dat_wide %>%
  dplyr::filter(datetime >= mindate)
dat_wide_eventonly_14 <- dat_wide_eventonly %>%
  dplyr::filter(datetime >= mindate)

dat_long_14 <- dat_long %>%
  dplyr::filter(datetime >= mindate)
dat_long_num_14 <- dat_long_num %>%
  dplyr::filter(datetime >= mindate)


## plot:
p <- ggplot(dat_wide_14, aes(x = datetime, y = weight)) +
  geom_point(alpha = .6) +
  geom_smooth(span = .3) +
  scale_x_date(
    labels = date_format("%b-%Y"),
    date_breaks = "2 month") +
  coord_cartesian(
    ylim = range(dat_wide$weight, na.rm = TRUE) + c(-1, +1)
  )
p %<>% add_events(dat_wide_eventonly_14, ypos = "weight_min", xpos = "abs")
p

## plot:
p <- ggplot(dat_wide_14, aes(x = datetime, y = bodyfat)) +
  geom_point(alpha = .6) +
  geom_smooth(span = .3) +
  scale_x_date(
    labels = date_format("%b-%Y"),
    date_breaks = "2 month") +
  coord_cartesian(
    ylim = range(dat_wide$bodyfat, na.rm = TRUE) + c(-.5, +.5)
  )
p %<>% add_events(dat_wide_eventonly_14, ypos = "bodyfat_min", xpos = "abs")
p

tail(dat_wide$orig, n = 40)
dat_wide %>% dplyr::select(bodyfat) %>% na.omit() %>% tail(n = 10)


tail(dat_long_14)

#caliper_keys <- stringr::str_subset(dat_long_14$key, "caliper")

str(dat_long_num_14)

## plot caliper values:
dat_long_num_14 %>% na.omit() %>%
  dplyr::filter(!variable %in% c("weight", "bodyfat", "bodyfat_caliper")) %>%
  ggplot(aes(x = datetime, y = value, color = variable, group = variable)) +
  geom_line() +
  geom_smooth()

dat_long_num_14

dat_caliper <- dat_long_num_14 %>% na.omit() %>%
  dplyr::filter(!variable %in% c("weight", "bodyfat", "bodyfat_caliper")) %>%
  dplyr::mutate(
  side = stringr::str_split_fixed(variable, "_", n = 3)[, 3],
  position = stringr::str_split_fixed(variable, "_", n = 3)[, 2]
)

## plot caliper values:
p <- dat_caliper %>%
  ggplot(aes(x = datetime, y = value, color = position, linetype = side)) +
  geom_line() +
  geom_smooth(alpha = .1, size = 2)
#p %<>% add_events(dat_long_14, ypos = "weight_min", xpos = "abs")
p

## [[to do]]
## write tests



#with(dat_wide, cor(dat_wide[stringr::str_subset(names(dat_wide), "caliper")] %>% na.omit(), use = "pair"))


