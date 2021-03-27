######################################################################
## figure-2.R
##
## Script associated with Figure 2 in Papst et al. 2021
## DOI: 10.1101/2020.09.01.20186395
##
## Downloaded from https://github.com/papsti/covid-age
######################################################################

################
## PLOT SETUP ##
################

source("figure-setup.R")
fig.height <- 7.5

## read data file
rKIs_df <- (read_csv("../data/figure-2.csv")
  %>% mutate(age_group = as_factor(age_group))
)

#' Plot basic panel for resolved KI multipanel plot
#'
#' @param df data frame from which to plot
#' @param palette colour palette vector for this panel
#' @param bar (logical) should a bar plot be drawn? if FALSE, line plot is drawn
#' @param facet_var (character) variable with which to facet
#' (for stacked bars)
#' @param xaxis (logical) should the x-axis elements be
#'   plotted?
#' @param scale_y_trans (character) transformation for
#'   y-axis (corresponding to y argument in scale_y_continuous)
#' @param useSI (logical) use SI units for labels on y axis?
#' @param ... additional arguments to ggplot2::theme
#'
#' @return a ggplot object
#' @export
#'
#' @examples
base_panel_rKIs <- function(df,
                            palette,
                            bar = TRUE,
                            facet_var = NULL,
                            xaxis = FALSE,
                            scale_y_trans = NULL,
                            useSI = TRUE,
                            ...){
  if(is.null(facet_var)){
    facet_var <- "value_type"
  }

  if(is.null(scale_y_trans)){
    trans <- "identity"
  } else{
    trans <- scale_y_trans
  }

  ## base panel
  p <- ggplot(
    data = df,
    mapping = aes(x = age_group,
                  y = value,
                  group = value_type)) +
    facet_grid(rows = vars(!!sym(facet_var)),
               scales = "free_y") +
    scale_colour_manual(values = palette) +
    scale_fill_manual(values = palette) +
    labs(x = "age group",
         y = NULL)


  ## conditional plot elements
  if(bar){
    p <- p + geom_bar(aes(fill = value_type),
                      stat = "identity",
                      width = 0.8)
    ## calculate values for scales
    x_expand <- c(0, 0)
    y_expand <- c(0, 0.1)
  } else {
    p <- p +
      geom_point(aes(colour = value_type)) +
      geom_line(aes(colour = value_type))
    ## calculate values for scales
    x_expand <- c(0.008, 0.008)
    y_expand <- c(0.1, 0.1)
  }

  if(xaxis){
    p <- p + theme(legend.position = 'none',
                   ...)
  } else {
    p <- p + theme(axis.title.x = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.y = element_blank(),
                   legend.position = 'none',
                   ...) +
      theme(plot.margin = unit(c(0,0,1,0), "mm"))
  }

  y_labels <- ifelse(useSI, scales::label_number_si, waiver)

  ## update scales
  p <- p +
    scale_x_discrete(
      breaks = breaks_every_n(df$age_group, n = 5),
      expand = x_expand
      ) +
    scale_y_continuous(
      trans = trans,
      labels = y_labels(),
      expand = expansion(mult = y_expand)
    )

  return(p)
}

## get heights for panel letter
annotate_params <- (rKIs_df
    %>% group_by(value_type)
    %>% summarise(y = max(value)))

###############
## MAKE PLOT ##
###############

## plot panels

## make rKI panel
rKIs_df %>%
  filter(value_type == "resolved KIs") %>%
  base_panel_rKIs(palette = rKIs_palette[1]) +
  annotate("text", 2,
           (annotate_params
            %>% filter(value_type == "resolved KIs")
            %>% pull(y)),
           label = "A",
           vjust = "inward", hjust = "inward") -> rKI_panel

## make population panel
rKIs_df %>%
  filter(value_type == "population") %>%
  base_panel_rKIs(palette = rKIs_palette[2],
                  text = element_text(size = font_size)) +
  annotate("text", 2,
           (annotate_params
            %>% filter(value_type == "population")
            %>% pull(y)),
           label = "B",
           vjust = "inward", hjust = "inward") -> pop_panel

## make rKIs per capita panel
rKIs_df %>%
  filter(str_detect(value_type, "resolved KIs per")) %>%
  base_panel_rKIs(palette = rKIs_palette[3],
                  bar = FALSE,
                  scale_y_trans = "log10") +
  annotate("text", 2,
           (annotate_params
            %>% filter(str_detect(value_type, "resolved KIs per"))
            %>% pull(y)),
           label = "C",
           vjust = "inward", hjust = "inward") -> rKI_per_cap_panel

## make total tests panel
rKIs_df %>%
  filter(value_type %in% c("positive",
                           "total")) %>%
  mutate(facet_label = "tests per 10K pop") %>%
  base_panel_rKIs(palette = rKIs_palette[4:5],
                  bar = FALSE,
                  facet_var = "facet_label",
                  scale_y_trans = "log10"
                  ) +
  labs(colour = "tests") +
  ## add direct labels for the two curves in this panel
  annotate("text",
           50, 7e3,
           label = "total tests",
           colour = rKIs_palette[4],
           hjust = "inward",
           size = 2.5) +
  annotate("text",
           50, 150,
           label = "positive tests",
           colour = rKIs_palette[5],
           hjust = "inward",
           size = 2.5) +
  ## annotate panel letter
  annotate("text", 2,
           (annotate_params
            %>% filter(value_type == "total")
            %>% pull(y)),
           label = "D",
           vjust = "inward", hjust = "inward") +
  theme(legend.position = "none") -> total_tests_panel

## make test pos panel
rKIs_df %>%
  filter(value_type == "test positivity rate") %>%
  rowwise() %>%
  base_panel_rKIs(palette = rKIs_palette[6],
                  bar = FALSE,
                  xaxis = TRUE,
                  useSI = FALSE) +
  annotate("text", 2,
           (annotate_params
            %>% filter(value_type == "test positivity rate")
            %>% pull(y)),
           label = "E",
           vjust = "inward", hjust = "inward") -> pos_rate_panel

## combine panels
fig2 <- (rKI_panel / pop_panel / rKI_per_cap_panel / total_tests_panel / pos_rate_panel)

###############
## SAVE PLOT ##
###############

pdf(file = "figure-2.pdf",
    width = fig.width,
    height = fig.height)
print(fig2)
dev.off()
