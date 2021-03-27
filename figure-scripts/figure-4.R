######################################################################
## figure-4.R
##
## Script associated with Figure 4 in Papst et al. 2021
## DOI: 10.1101/2020.09.01.20186395
##
## Downloaded from https://github.com/papsti/covid-age
######################################################################

################
## PLOT SETUP ##
################

source("figure-setup.R")
fig.height <- 4

## read data files
hosp_given_KI <- read_csv("../data/figure-4a.csv")
surv_given_hosp <- read_csv("../data/figure-4b.csv")

## Helper function to make nice age group labels for x-axis
## from scale breaks
age_group_labeller <- function(breaks){
  as_tibble(breaks) %>%
    rename(lower_edge = value) %>%
    mutate(upper_edge = lower_edge + 1) %>%
    mutate(labels = paste0(lower_edge, "-", upper_edge)) %>%
    pull(labels) -> labels

  labels <- str_replace(labels, "100-101", "100+")

  return(labels)
}

## Helper function to make axis breaks every x units
breaks_by <- function(vals, x = 0.1){
  rng <- round(range(vals), 1)
  return(seq(rng[1], rng[2], by = x))
}

## Function to draw base panel
base_panel_cond_probs <- function(df,
                                  method = "glm",
                                  formula = y~x,
                                  xaxis = FALSE,
                                  facet_lab = "",
                                  size_breaks = NULL,
                                  ...) {
  ## set up default scale breaks
  if(is.null(size_breaks)){
    from <- floor(log10(min(df$tot)))
    to <- round(log10(max(df$tot)))
    size_breaks <- 10^(from:to)
  }

  df %<>%
    mutate(facet = facet_lab)

  p <- (ggplot(df, aes(x = lower_age, y = prop))
         ## point indicating sample size
          + geom_point(aes(size = tot),
                       alpha=0.25,
                       colour = "grey40")
         ## confidence interval
          + geom_linerange(aes(ymin = lwr, ymax = upr),
                           alpha=0.5)
         ## model fit
          + geom_smooth(aes(weight = tot),
                        method = method,
                        method.args = list(family=quasibinomial),
                        formula = formula,
                        se = TRUE,
                        size = 0.75,
                        colour = "#2c7ac9",
                        fill = "#2c7ac9", alpha = 0.3
                        )
        ## control expansion factor of axes
          + scale_x_continuous(
            breaks = seq(0, 100, by = 10),
            label = age_group_labeller,
            expand = expansion(mult = c(0.01, 0.01))
            )
          + scale_y_continuous(
            breaks = breaks_by
          )
          + scale_size_area(
            limits = range(size_breaks),
            breaks = size_breaks,
            labels = as.character(size_breaks)
          )
        ## add plot label as facet
          + facet_grid(rows = vars(facet))
          + labs(x = "age group",
                 y = NULL,
                 size = "sample size")
          + guides(size = guide_legend(nrow = 1))
  )

  ## conditional plot elements
  ## conditional plot elements
  if(xaxis){
    p <- p + theme(legend.position = "bottom",
                   ...)
  } else {
    p <- p + theme(axis.title.x = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   legend.position = "bottom",
                   ...)
  }

  return(p)
}

###############
## MAKE PLOT ##
###############

max_sample_size_scale <- hosp_given_KI %>%
  filter(tot == max(tot)) %>%
  pull(tot) %>%
  max(pretty(.))

size_breaks <- c(10^seq(log(1), log10(max_sample_size_scale)), max_sample_size_scale)

## hospitalization given KI panel
(hosp_given_KI
  %>% ungroup()
  %>% summarise(x = max(lower_age)-2,
                y = max(upr))) -> annotate_params

hosp_given_KI %>%
  base_panel_cond_probs(method = "gam",
                        formula = y~s(x),
                        facet_lab = "hospitalization given KI",
                        size_breaks = size_breaks) +
  annotate("text", annotate_params$x, annotate_params$y,
           label = "A",
           hjust = "inward", vjust = "inward") -> hosp_panel

## survival given hospitalization panel
(surv_given_hosp
  %>% ungroup()
  %>% summarise(x = max(lower_age)-2,
                y = max(upr))) -> annotate_params

surv_given_hosp %>%
  base_panel_cond_probs(facet_lab = "survival given hospitalization",
                        size_breaks = size_breaks,
                        xaxis = TRUE) +
  annotate("text", annotate_params$x, annotate_params$y,
           label = "B",
           hjust = "inward", vjust = "inward") -> surv_panel

## shared y-axis label (a little hacky, but ok)
R.devices::nulldev()
ylabGrob <- ((ggplot() +
  geom_blank() +
  theme(text = element_text(size = font_size)) +
  labs(y = "probability")) %>%
  ggplotGrob())
dev.off()

## combine panels
fig4 <- (cowplot::ggdraw(ylabGrob[7,3]) + (hosp_panel / surv_panel)
  + plot_layout(widths = c(1, 30),
                guides = "collect"))

###############
## SAVE PLOT ##
###############

pdf(file = "figure-4.pdf",
    width = fig.width,
    height = fig.height)
print(fig4)
dev.off()
