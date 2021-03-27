######################################################################
## figure-3.R
##
## Script associated with Figure 3 in Papst et al. 2021
## DOI: 10.1101/2020.09.01.20186395
##
## Downloaded from https://github.com/papsti/covid-age
######################################################################

################
## PLOT SETUP ##
################

source("figure-setup.R")
fig.height <- 4

## read data file
outcomes_df <- (read_csv("../data/figure-3.csv")
  %>% mutate(age_group = as_factor(age_group))
  %>% mutate(outcome = as_factor(outcome))
)

#' Plot basic panel for serious outcomes multipanel plot
#'
#' @param df data frame from which to plot
#' @param palette colour palette vector for this panel
#' @param xaxis (logical) should the x-axis elements be
#'   plotted?
#' @param drop (logical) should unused levels be dropped
#'   from the fill legend?
#' @param ... additional arguments to ggplot2::theme
#'
#' @return
#' @export
#'
#' @examples
base_panel_outcomes <- function(df,
                                palette,
                                xaxis = FALSE,
                                drop = FALSE,
                                ...){

  p <- df %>%
    ggplot(aes(x = age_group, y = count,
               fill = outcome)) +
    geom_bar(stat = "identity",
             width = 0.8) +
    facet_grid(rows = vars(facet_var)) +
    scale_x_discrete(breaks = breaks_every_n(df$age_group,
                                             n = 5),
                     expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult =
                                            c(0, 0.1))) +
    scale_fill_manual(values = palette,
                      drop = FALSE) +
    labs(x = "age group",
         y = NULL)

  ## conditional elements
  if (xaxis == FALSE){
    p <- p + theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none",
      ...
    )
  } else{
    p <- p + theme(
      legend.position = "none",
      ...
    )
  }

  return(p)
}

###############
## MAKE PLOT ##
###############

## make death outcomes list for filtering
death_outcomes_list <- c("death, no hospitalization", "death and hospitalization")

(outcomes_df
  %>% mutate(panel = case_when(
    outcome %in% death_outcomes_list ~ "death",
    T ~ "hosp"
  ))
  %>% group_by(age_group, panel)
  %>% summarize(count = sum(count), .groups = "drop")
  %>% group_by(panel)
  %>% summarize(y = max(count), .groups = "drop")) -> annotate_params

## make hosp panel
outcomes_df %>%
  filter(!(outcome %in% death_outcomes_list)) %>%
  mutate(facet_var = "hospitalized individuals") %>%
  group_by(age_group) %>%
  base_panel_outcomes(palette =
                        outcomes_palette) +
  annotate("text", 2,
           (annotate_params
            %>% filter(panel == "hosp")
            %>% pull(y)),
           label = "A",
           hjust = "inward", vjust = "inward") -> hosp_panel

## make death panel
outcomes_df %>%
  filter(outcome %in% death_outcomes_list) %>%
  mutate(facet_var = "deaths") %>%
  base_panel_outcomes(palette = outcomes_palette,
                      xaxis = TRUE) +
  annotate("text", 2,
           (annotate_params
            %>% filter(panel == "death")
            %>% pull(y)),
           label = "B",
           hjust = "inward", vjust = "inward") -> death_panel

## combine panels
fig3 <-((hosp_panel / death_panel)
  + theme(
      legend.position = "bottom",
      legend.title = element_blank()
    ))

###############
## SAVE PLOT ##
###############

pdf(file = "figure-3.pdf",
    width = fig.width,
    height = fig.height)
print(fig3)
dev.off()
