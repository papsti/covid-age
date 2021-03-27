######################################################################
## figure-1.R
##
## Script associated with Figure 1 in Papst et al. 2021
## DOI: 10.1101/2020.09.01.20186395
##
## Downloaded from https://github.com/papsti/covid-age
######################################################################

################
## PLOT SETUP ##
################

source("figure-setup.R")
fig.height <- 2.75

## read data file
ts_df <- read_csv("../data/figure-1.csv")

## important dates
date_first_KI <- as_date("2020-01-23")
date_state_emerg <- as_date("2020-03-17")
date_ccmp_max <- as_date("2021-02-16")

## colours and sizes
ts_cols <- c(
  colorspace::lighten(rKIs_palette[1], amount = 0.3),
  rKIs_palette[1]
)
annotation_colour <- "grey20"
annotation_size <- font_size-6.5
annotation_alpha <- 1

## axes
## get pretty rounded value for the max y limit
ylim_max <- ts_df %>%
  group_by(date) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  filter(count == max(count)) %>%
  pull(count) %>%
  ## round to a (larger) "pretty" number
  max(pretty(1.2*.))

## set shared height of (most) annotations
annotation_y <- 0.5*ylim_max

## dates to annotate on the x-axis
scale_date_labs <- unique(floor_date(ts_df$date, "month"))

## set up df with stage dates, fills, and labels for background
shutdown_fill <- "#e1adbc"
stage1_fill <- "#e09b5a"
stage2_fill <- "#d9c55f"
stage3_fill <- "#96cc83"
modstage2_fill <- stage2_fill

shutdown_fill <- "grey10"
stage1_fill <- "grey40"
stage2_fill <- "grey55"
stage3_fill <- "grey70"
modstage2_fill <- stage2_fill
lockdown_fill <- stage1_fill
shutdown2_fill <-"grey25"
shutdown3_fill <- shutdown_fill

stage_dates <- c(lubridate::as_date(
  c("2020-03-23", "2020-05-19", "2020-06-24",
    "2020-07-31", "2020-10-10", "2020-11-23",
    "2020-12-26", "2021-01-14")), date_ccmp_max+1)

stages <- tibble(
  stage = c("shutdown", "stage 1", "stage 2", "stage 3",
   "modified stage 2", "lockdown", "shutdown",
   "stay-at-home order"),
  start_date = head(stage_dates, -1),
  end_date = tail(stage_dates, -1),
  y = annotation_y*c(rep(1, 5), 1.2, 1.75, 1.45),
  fill = c(shutdown_fill, stage1_fill, stage2_fill,
           stage3_fill, modstage2_fill, lockdown_fill,
           shutdown2_fill, shutdown3_fill)
) %>%
  mutate(mid_date = start_date + (end_date - start_date)/2)

###############
## MAKE PLOT ##
###############

fig1 <- (ts_df
  %>% ggplot(aes(x = date, y = count, fill = resolved_by_end_date))
  ## annotate date of first case in the province
  + geom_vline(xintercept = date_first_KI,
               colour = annotation_colour,
               linetype = "dashed",
               alpha = annotation_alpha,
               size = 0.4)
  + annotate("text",
             x = date_first_KI-5,
             y = annotation_y,
             label = "first case reported",
             colour = annotation_colour,
             alpha = annotation_alpha,
             angle = 90,
             size = annotation_size)
  ## annotate state of emergency date
  + geom_vline(xintercept = date_state_emerg,
               colour = annotation_colour,
               alpha = annotation_alpha,
               linetype = "dashed",
               size = 0.4)
  + annotate("text",
             x = date_state_emerg-5,
             y = annotation_y,
             label = "state of emergency declared",
             colour = annotation_colour,
             alpha = annotation_alpha,
             angle = 90,
             size = annotation_size)
  ## add shaded regions for stages
  + annotate("rect",
             xmin = stages$start_date,
             xmax = stages$end_date,
             ymin = -Inf,
             ymax = Inf,
             fill = stages$fill,
             alpha = 0.5)
  ## add counts as bars
  + geom_bar(stat = "identity",
             width = 1,
             size = 0)
  + annotate("text",
             x = stages$mid_date,
             y = stages$y,
             label = stages$stage,
             angle = 90,
             alpha = annotation_alpha,
             size = annotation_size,
             colour = annotation_colour)
  ## fix scales
  + scale_x_date(
      breaks = lubridate::as_date(scale_date_labs),
      date_labels = "%b",
      expand = expansion(mult = c(0.04,0))
      )
  + scale_y_continuous(
      limits = c(0, ylim_max),
      expand = expansion(mult = c(0, 0))
      )
  + scale_fill_manual(values = ts_cols)
  + labs(x = "report date",
         y = "KIs",
         fill = paste("resolved by",
                      day(date_ccmp_max),
                      month(date_ccmp_max,
                      label = TRUE, abbr=FALSE),
  					year(date_ccmp_max)
                      )
         )
)

###############
## SAVE PLOT ##
###############

pdf(file = "figure-1.pdf",
    width = fig.width,
    height = fig.height)
print(fig1)
dev.off()
