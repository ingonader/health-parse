## ############################################################################
## parse health.txt
## ------------------------------------------------------------------------
## Created 18 Jan 2018
## Author Ingo Nader
## ############################################################################

# rstudioapi::restartSession()

## [[to do]]
## * Make shiny app and host it on shiny.io (+ grab file from dropbox via R?) or on azure?
## * use age at the time of the event for calculation of body fat
## * export resulting plots and data: to dropbox?
## * use packrat
## * make interactive plots (e.g., plotly?)

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

## ============================================================================
## additional functions
## ============================================================================

source("function-library-health-parse.R")

## ============================================================================
## analysis
## ============================================================================

## get paths:
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

# head(dat_wide)
# dat_wide %>% dplyr::select(-orig) %>% tail()

## create a version of only events with appropriate y (weight) values for plotting labels:
dat_wide_eventonly <- dat_wide %>%
  dplyr::mutate(
    weight_min = min(weight, na.rm = TRUE),
    bodyfat_min = min(bodyfat, na.rm = TRUE),
    caliper_min = min(c(caliper_brust_li, caliper_brust_re,
                        caliper_bauch_li, caliper_bauch_re,
                        caliper_bein_li, caliper_bein_re), na.rm = TRUE)) %>%
  dplyr::filter(event != "")
  
# names(dat_wide)
# tail(dat_wide)

dat_long <- get_data_long(dat_txt) %>% tibble::as_tibble()

dat_long_num <- dat_wide %>%
  dplyr::select(-note, -event, -orig, -bodyfat_value_txt) %>%
  reshape2::melt(id.vars = c("datetime", "xyear", "datetime_rel"))

# head(dat_long_num)
# tail(dat_long_num)
# str(dat_long_num)

## ------------------------------------------------------------------------- ##
## function to add events to different plots ####
## ------------------------------------------------------------------------- ##

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

## ========================================================================= ##
## plot all data ####
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## weight (all data)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## try a plot:
p <- ggplot(dat_wide, aes(x = datetime_rel, y = weight)) +
  geom_point(alpha = .6) +
  geom_smooth(span = .3) +
  facet_wrap(~ xyear, ncol = 2) +
  scale_x_date(
    labels = date_format("%b"),
    date_breaks = "2 month") +
  coord_cartesian(
    ylim = range(dat_wide$weight, na.rm = TRUE) + c(-1, +1)
  )
p %<>% add_events(dat_wide_eventonly, ypos = "weight_min", xpos = "rel")
p

## save plot to dropbox (in dropbox directory):
ggsave(file.path(path$out, "healthplot-weight-all.jpg"), 
       width = 5, height = 10, unit = "in", dpi = 300)


## [[to do]]
## * add path$out to get_paths() function in tskeyvalparser package
## * move plot to output directory
## * change plot to have ncol = 1

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## bodyfat (all data) ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# bodyfat trial:
p <- ggplot(dat_wide, aes(x = datetime_rel, y = bodyfat)) +
  geom_point(alpha = .6) +
  geom_smooth(span = .3) +
  facet_wrap(~ xyear, ncol = 2) +
  scale_x_date(
    labels = date_format("%b"),
    date_breaks = "2 month") +
  coord_cartesian(
    ylim = range(dat_wide$bodyfat, na.rm = TRUE) + c(-.5, +.5)
  )
p %<>% add_events(dat_wide_eventonly, ypos ="bodyfat_min", xpos = "rel")
p

## save plot to dropbox (in dropbox directory):
ggsave(file.path(path$out, "healthplot-bodyfat-all.jpg"), 
       width = 5, height = 10, unit = "in", dpi = 300)

## ========================================================================= ##
## plot subsets of data
## ========================================================================= ##

dat_wide %>% dplyr::select(datetime, weight, bodyfat) %>% tail(n = 20)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Plot for 12 + 2 months ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

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

## ------------------------------------------------------------------------- ##
## plot weight
## ------------------------------------------------------------------------- ##

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

## save plot to dropbox (in dropbox directory):
ggsave(file.path(path$out, "healthplot-weight-12p2.jpg"), 
       width = 10, height = 5, unit = "in", dpi = 300)

## ------------------------------------------------------------------------- ##
## plot bodyfat
## ------------------------------------------------------------------------- ##

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

## save plot to dropbox (in dropbox directory):
ggsave(file.path(path$out, "healthplot-bodyfat--12p2.jpg"), 
       width = 10, height = 5, unit = "in", dpi = 300)


## ========================================================================= ##
## plot multiple subsets of data
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Plot for 12 x 3 months, relative ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## subset data:
mindate <- max(dat_wide$datetime, na.rm = TRUE) %m-% months(12 * 3)
dat_wide_36 <- dat_wide %>%
  dplyr::filter(datetime >= mindate)

dat_wide_36
#dat_wide_36 %>% dplyr::select(datetime, weight, xyear, datetime_rel, datetime_offset, datetime_offset_2, datetime_label) %>% View()

maxdate <- max(dat_wide_36$datetime)
maxreldate <- dat_wide_36 %>% dplyr::filter(datetime == maxdate) %>% dplyr::pull(datetime_rel) %>% .[1]

## add datetime offset:
dat_wide_36 <- dat_wide_36 %>% dplyr::mutate(
  datetime_offset = (as.period(max(datetime)  - datetime)) %/% months(12),
  datetime_offset_2 = (max(xyear) - xyear) -
    ifelse(datetime_rel > maxreldate, 1, 0),
  datetime_rel_to_offset = datetime_rel %m-% months(
    ifelse(datetime_rel > maxreldate, 12, 0)
  )
)

## create label field:
dat_tmp_labels <- dat_wide_36 %>% 
  dplyr::group_by(datetime_offset) %>% 
  dplyr::summarize(
    xyear_min = min(xyear),
    xyear_max = max(xyear)
  ) %>%
  dplyr::mutate(
    datetime_label = paste0(xyear_min, " - ", xyear_max)
  )

## add label field to wide dataset:
dat_wide_36 <- dat_wide_36 %>%
  dplyr::left_join(dat_tmp_labels,
                   by = "datetime_offset")

## inspect:
dat_wide_36[c("datetime_offset", "xyear")] %>% table()

## ------------------------------------------------------------------------- ##
## plot weight
## ------------------------------------------------------------------------- ##

p <- ggplot(dat_wide_36, aes(x = datetime_rel_to_offset, 
                             y = weight, 
                             group = datetime_label,
                             color = datetime_label)) +
  geom_point(alpha = .6) +
  geom_smooth(aes(alpha = datetime_label), span = .3) +
  scale_x_date(
    labels = date_format("%b"),
    date_breaks = "2 month") +
  coord_cartesian(
    ylim = range(dat_wide$weight, na.rm = TRUE) + c(-1, +1)) +
  scale_colour_manual(name = "Years", 
                      values = c("#ff9999", 
                                 "#ff7777", 
                                 "#ff0000")) +
  scale_alpha_discrete(name = "Years",
                       range = c(.2, .4))
print(p)

## save plot to dropbox (in dropbox directory):
ggsave(file.path(path$out, "healthplot-weight-3x1y-rel.jpg"), 
       width = 10, height = 5, unit = "in", dpi = 300)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Plot for 12 x 3 months, absolute ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## subset data:
dat_wide_36_abs <- dat_wide %>%
  dplyr::filter(xyear >= max(xyear) - 2)

dat_wide_36_abs <- dplyr::mutate(dat_wide_36_abs,
                          xyear = as.factor(xyear))
dat_wide_36_abs
#dat_wide_36_abs %>% dplyr::select(datetime, weight, xyear, datetime_rel, datetime_offset, datetime_offset_2, datetime_label) %>% View()

## create a separate dataset for the smoothed loess lines:
## (same data, but remove years with not enough data):
wch_year <- dat_wide_36_abs %>% 
  dplyr::filter(!is.na(weight)) %>%
  dplyr::group_by(xyear) %>% 
  dplyr::summarize(cnt = length(xyear)) %>%
  dplyr::filter(cnt > 5) %>% 
  dplyr::pull(xyear)
dat_wide_36_abs_smooth  <- dat_wide_36_abs %>%
  dplyr::filter(xyear %in% wch_year)

p <- ggplot(dat_wide_36_abs, aes(x = datetime_rel, 
                             y = weight, 
                             group = xyear,
                             color = xyear)) +
  geom_point(alpha = .6) +
  geom_smooth(data = dat_wide_36_abs_smooth,
              aes(alpha = xyear), span = .3) +
  scale_x_date(
    labels = date_format("%b"),
    date_breaks = "2 month") +
  coord_cartesian(
    ylim = range(dat_wide$weight, na.rm = TRUE) + c(-1, +1)) +
  scale_colour_manual(name = "Years", 
                      values = c("#ff9999", 
                                 "#ff7777", 
                                 "#ff0000")) +
  scale_alpha_discrete(name = "Years",
                       range = c(.2, .4))
print(p)

## save plot to dropbox (in dropbox directory):
ggsave(file.path(path$out, "healthplot-weight-3x1y-abs.jpg"), 
       width = 10, height = 5, unit = "in", dpi = 300)

## ========================================================================= ##
## various stuff
## ========================================================================= ##

## ------------------------------------------------------------------------- ##
## descriptive stats
## ------------------------------------------------------------------------- ##

#tail(dat_wide$orig, n = 40)
#dat_wide %>% dplyr::select(bodyfat) %>% na.omit() %>% tail(n = 10)

#tail(dat_long_14)

#caliper_keys <- stringr::str_subset(dat_long_14$key, "caliper")

#str(dat_long_num_14)

## ------------------------------------------------------------------------- ##
## plot caliper values
## ------------------------------------------------------------------------- ##

# dat_long_num_14 %>% na.omit() %>%
#   dplyr::filter(!variable %in% c("weight", "bodyfat", "bodyfat_caliper")) %>%
#   ggplot(aes(x = datetime, y = value, color = variable, group = variable)) +
#   geom_line() +
#   geom_smooth()

dat_caliper <- dat_long_num_14 %>% na.omit() %>%
  dplyr::filter(!variable %in% c("weight", "bodyfat", "bodyfat_caliper")) %>%
  dplyr::mutate(
  side = stringr::str_split_fixed(variable, "_", n = 3)[, 3],
  position = stringr::str_split_fixed(variable, "_", n = 3)[, 2]
)

## add columns in plot to event data (otherwise, add_events() function will fail):
dat_wide_eventonly_14["position"] <- NA
dat_wide_eventonly_14["side"] <- NA

## plot caliper values:

p <- dat_caliper %>%
  ggplot(aes(x = datetime, y = value, color = position, linetype = side)) +
  geom_line() +
  geom_smooth(alpha = .2, size = 1.0, span = .5) +
  scale_x_date(
    labels = date_format("%b-%Y"),
    date_breaks = "2 month")
p %<>% add_events(dat_wide_eventonly_14, ypos = "caliper_min", xpos = "abs")
p

## save plot to dropbox (in dropbox directory):
ggsave(file.path(path$out, "healthplot-bodyfat-caliper-12p2.jpg"), 
       width = 10, height = 5, unit = "in", dpi = 300)



#with(dat_wide, cor(dat_wide[stringr::str_subset(names(dat_wide), "caliper")] %>% na.omit(), use = "pair"))


