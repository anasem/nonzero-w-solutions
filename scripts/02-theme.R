# Helper functions, themes and styling options

# Plotting options -------------------------------------------------------------

main_palette <- c("#19D418", "#0099FF", "#00CEFF", "#A3AF9E", "#3F4A3C",
                  "#FF6A0D", "#FF386D")
alt_palette <- c("#DEF8D6", "#D1F5FF", "#A3AF9E", "#96AFB8")

mm <- main_palette
names(mm) <- main_palette
method_pallete = c(STEA = main_palette[3], EC = main_palette[6])

legend_inside_left <-
  theme(
    legend.position = c(0, 0),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = alpha("white", .6))
  )


mp <- function(...) {
    chosen_colors <- list(...)

    if (length(chosen_colors) == 0) {
        ggplot(data.frame(num = 1:length(main_palette),
                          colorval = main_palette,
                          y = 10)) + 
            geom_col(aes(x = num, y = y, fill = colorval)) + 
            geom_text(aes(x = num, y = 5, label = num))+
            scale_fill_manual(values = mm)
    } else {
        return(main_palette[unlist(chosen_colors)])
    }
}

geom_1to1  <- function(intercept = 0, ...) {
    geom_abline(slope = 1, intercept = intercept, ...)
}


# Sampling channel palette for is_updraft
sampling_channel_palett <- c("FALSE" = main_palette[2],
                             "TRUE" =  main_palette[1])

state_point_palette <- c("Downdraft" = main_palette[2], 
                         "Updraft" =  main_palette[1],
                         "Deadband" = main_palette[6])

sampling_channel_alt_palette <- c("FALSE" = alt_palette[2],
                                  "TRUE" =  alt_palette[1])

# Theme suitable for the journal
theme_copernicus <- function() {
  return (theme_light() + 
    theme(text = element_text(family = "Carrois Gothic"),
            panel.grid.major.y = element_line(color = "gray90"),
            panel.grid.major.x = element_line(color = "gray90"),
            panel.grid.minor.y = element_blank(),
            plot.subtitle = element_text(color = "gray50"),
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(color = "gray50"),
            axis.title.y = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.text.x= element_text(size = 12),
            axis.text.y = element_text(size = 12),
            plot.margin=grid::unit(c(1,4,1,1), "mm"),
            legend.position = c(1, 0),
            legend.box = "horizontal",
            legend.direction = "horizontal",
            legend.margin = margin(1,1,1,1),
            legend.box.margin = margin(1,1,1,1),
            legend.title = element_text(size = 10, face = "bold",
                                        margin = margin(0,5,0,0)),
            legend.text= element_text(size = 10, margin = margin(0,7,0,0)),
            legend.justification = c(1, 0),
            legend.spacing.x = unit(1,'mm'),
            legend.key.size = unit(4, "mm"),
            legend.background = element_rect(fill = alpha("white", .8))
        ))
}

# Move the legend below
legend_below <- function() {
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.direction = "horizontal",
          legend.margin = margin(1,1,1,1),
          legend.box.margin = margin(1,1,1,1),
          legend.title = element_text(size = 10, face = "bold",
                                      margin = margin(0,5,0,0)),
          legend.text= element_text(size = 10, margin = margin(0,7,0,0)),
          legend.justification = c(1, 0),
          legend.spacing.x = unit(1,'mm'),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill = alpha("white", .8)))
}


# Wrapper function to save ggplot plots
psave <- function(name = "", p = last_plot(),  width = 8.3,
                  height = 3.5, scale = 2,
                  device = cairo_pdf, units = "cm", output_dir) {
    name <- file.path(output_dir, name)
    
    ggsave(name, plot = p, 
           width = width, height = height, scale = scale,
           device = device, units = units)
    
}


geom_model_equation <- function(model, x, y,slope.digits = 2, r2.digits =2, ...){
    if (is(model, "lm")) {
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        R2 <- summary(model)$r.squared
    } else {
        intercept <- model[["regression.results"]][3,"Intercept"]
        slope <- model[["regression.results"]][3,"Slope"]
        R2 <- model[["rsquare"]]
    }

    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
         list(a = format(unname(intercept), digits = 2),
              b = format(unname(slope), digits = slope.digits),
             r2 = format(R2, digits = r2.digits)))
    equation_txt <- as.character(as.expression(eq))
    geom_text(mapping = aes(x,y), label = equation_txt, parse = TRUE, 
              check_overlap = TRUE, inherit.aes = FALSE, ...) 

}


geom_model_abline <- function(model,...){
    if (is(model, "lm")) {
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        R2 <- summary(model)$r.squared
    } else {
        intercept <- model[["regression.results"]][3,"Intercept"]
        slope <- model[["regression.results"]][3,"Slope"]
        R2 <- model[["rsquare"]]
    }
    return(geom_abline(slope = slope, intercept = intercept, ...))

}


no_grid <- function() {
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank())
}

stability_colors <- 
    c(main_palette[c(2, 5, 6)])

names(stability_colors) <- c('unstable: ζ (-∞,-0.033)', 
                             'neutral: ζ [-0.033, 0.002)',
                             'stable: ζ [0.002, +∞)')


stability_color_scale <-  function() {
        scale_color_manual("Stability class", 
                           values = stability_colors)
}

stability_fill_scale <-  function() {
        scale_fill_manual("Stability class", 
                          values = stability_colors)
}


large_margins <- function() theme(plot.margin=grid::unit(c(10,10,10,10), "mm"))


# Expected value of |w|
abs_w_expectation <- function (w, w_sd) {
    (erf(w / w_sd * sqrt(2) / 2) *
        sqrt(pi) * w + sqrt(2) * w_sd * exp(-w ^ 2 / w_sd ^ 2 / 2)) *
    pi ^ (-.5)
}

# Flux error ratio
# E[α]*E[w]/E[|w|]
flux_error_ratio <- function(w, w_sd) {
    w/abs_w_expectation(w, w_sd)*
        alpha_analytic(w, w_sd)
}


# Gauss error function
erf <- function(x) {
    2*pnorm(x*sqrt(2)) -1
}


# E[α] alpha expectation cov(abs(x), y)/cov(x, y)
alpha_analytic <- function(w_mean, w_sigma) {
    erf(w_mean/(sqrt(2)*w_sigma))
}

# Cut stability into three classes
cut_zL3 <- function(zL) {
    zL_labels = c('unstable: ζ (-∞,-0.033)', 
                  'neutral: ζ [-0.033, 0.002)',
                  'stable: ζ [0.002, +∞)')
    cut(zL, breaks = c(-Inf, -.033, 0.002, Inf), right = FALSE, zL_labels)
}


method_color_scale <- 
    scale_color_manual("TEA method", 
                       values = c("ES21" = main_palette[3], 
                                  'T09' = main_palette[5], 
                                  "HM86" = main_palette [6]), 
                       labels = c("ES21" = 
                                  expression(current~study~using~alpha[theta]),
                              "T09" = "Turnipseed et al, 2009", 
                              "HM86" = "Hicks and McMillen, 1986"))  

method_fill_scale <- 
    scale_fill_manual("TEA method", 
                       values = c("ES21" = main_palette[3], 
                                  'T09' = main_palette[5], 
                                  "HM86" = main_palette [6]), 
                       labels = c("ES21" = 
                                  expression(current~study~using~alpha[theta]),
                              "T09" = "Turnipseed et al, 2009", 
                              "HM86" = "Hicks and McMillen, 1986")) 


sessionInfo()
