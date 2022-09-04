# ----- TOC -----
# different_methods_comparison_fig1
# values_of_alpha_fig_2
# flux_error_and_alpha_similarity_fig_3

source("01-deps.R")

theme_set(theme_copernicus())
plot_output_dir <- path.expand("../figures")

# Load data --------------------------------------------------------------------
simulation_fluxes <- readRDS(file = results_file)

# Exclude rep number 1 which is the control; choose 2:4
data_choice <- simulation_fluxes[, abs(w_mean/w_sigma) < 2 & rep %in% 2:4]

# Define data quality filter
quality_filter  <- simulation_fluxes[, abs(RNcov) < 0.3 & 
                                     abs(r_Tsw) > 0.20 & 
                                     abs(Ts_alpha) < 1 ] & data_choice

# different_methods_comparison_fig1 --------------------------------------------

nonzero_w_methods_regression <- 
  ggplot(simulation_fluxes[quality_filter]) + 
      aes(x = ec_flux)  +
      geom_point(aes(y =tea_flux_hicks,
                     col= 'HM86'), alpha = .5, size = 1, shape = 15)  +
      geom_point(aes(y =  tea_flux_turnipseed, col= 'T09'), 
                 shape = 15, alpha = .5, size = 1) +
      geom_point(aes(y =  tea_flux_emad, col= 'ES21'), 
                 shape = 15, alpha = .5, size = 1) +
      geom_abline(slope = 1, alpha = .2) +
      scale_color_discrete("Formula")  + 
      method_color_scale + 
      theme_copernicus() +
      no_grid() +
      scale_x_continuous(expand = c(0.05,0.05)) +
      scale_y_continuous(expand = c(0.05,0.05)) +
      labs (x= expression (paste(EC~CO[2], " flux [", mu,mol~m^-2,~s^-1, "]")),
            y = expression (paste(TEA~CO[2],
                                  " flux [", mu,mol~m^-2,~s^-1, "]")))  +
      guides(colour = "none")

nonzero_w_methods_density <- 
    ggplot(simulation_fluxes[quality_filter]) +  
        geom_density(aes((tea_flux_turnipseed - ec_flux)/ec_flux,
                         fill = "T09", col = "T09"), alpha = .3) +
        geom_density(aes((tea_flux_hicks - ec_flux)/ec_flux,
                         fill = "HM86", col = "HM86"), alpha =  .7) +
        geom_density(aes((tea_flux_emad - ec_flux)/ec_flux,
                         fill= "ES21", col = "ES21"), alpha =.4) +
        geom_vline(xintercept = 0, lty = 2) +
        labs(x = "Flux error ratio") +
        xlim(-.75,.75) + 
        method_fill_scale +
        method_color_scale  +
        theme_copernicus()  +
        no_grid() 

nonzero_w_methods_comparison <- 
    nonzero_w_methods_regression + 
        nonzero_w_methods_density +
        plot_annotation(tag_levels = "a", tag_suffix = ")") +
        plot_layout(guides = "collect",
                    widths = c(1,1.25)) & 
            theme(legend.position = "bottom",
                  legend.box = "horizontal",
                  legend.direction = "horizontal",
                  legend.margin = margin(1,1,1,1),
                  legend.box.margin = margin(1,1,1,1),
                  legend.title = element_text(size = 10, 
                                              face = "bold",
                                              margin = margin(0,5,0,0)),
                  legend.text= element_text(size = 10,
                                            margin = margin(0,7,0,0)),
                  legend.justification = c(1, 0),
                  legend.spacing.x = unit(1,'mm'),
                  legend.key.size = unit(4, "mm"),
                  legend.background = element_rect(fill = alpha("white", .8)))

psave("Fig01.pdf", 
      p = nonzero_w_methods_comparison, 
      height = 6.2, width = 12.5, scale = 1.4,
      output_dir = plot_output_dir)

# ------------------------------------------------------------------------------
# values_of_alpha_fig_2

plot_alpha_c <- 
    ggplot(simulation_fluxes[rep == 1] ) + 
        geom_hline(yintercept = c(-1,0,1), size = .2, alpha = .5) +
        geom_vline(xintercept = c(.15), size = .2, alpha = .5, lty = 2) +
        geom_point(aes(x = abs(r_cw), C_alpha, col = cut_zL3(zL), shape = flag), 
                   alpha = .7)  +
        theme_copernicus() +
        stability_color_scale() +
        ylim(-4,4) +
        scale_shape_manual(name = "Flag", 
                           values = c(daytime=15, nighttime = 3, 
                                      instationary = 21))  +
        no_grid() +
        labs(x = expression(paste("|", rho[w.CO[2]], "|")), 
             y = expression(alpha[CO[2]]))

plot_alpha_w <- 
    ggplot(simulation_fluxes[rep == 1] ) + 
        geom_hline(yintercept = c(-1,0,1), size = .2, alpha = .5) +
        geom_vline(xintercept = c(.15), size = .2, alpha = .5, lty = 2) +
        geom_point(aes(x = abs(r_Ww), W_alpha, col = cut_zL3(zL), shape = flag), 
                   alpha = .7)  +
        theme_copernicus() +
        stability_color_scale() +
        ylim(-4,4) +
        scale_shape_manual(name = "Flag", 
                           values = c(daytime=15, nighttime = 3, 
                                      instationary = 21))  +
        no_grid() +
        labs(x = expression(paste("|", rho[w.H[2]~O], "|")), 
             y = expression(alpha[H[2]~O]))

plot_alpha_ts <- 
    ggplot(simulation_fluxes[rep == 1] ) + 
        geom_vline(xintercept = c(.15), size = .2, alpha = .5, lty = 2) +
        geom_hline(yintercept = c(-1,0,1), size = .2, alpha = .5) +
        geom_point(aes(abs(r_Tsw), Ts_alpha, col = cut_zL3(zL), shape = flag), 
                   alpha = .7)  +
        theme_copernicus() +
        stability_color_scale() +
        ylim(-4,4) +
        scale_shape_manual(name = "Flag", 
                           values = c(daytime=15, nighttime = 3,
                                      instationary = 21))  +
        no_grid() +
        labs(x = expression(paste("|", rho[w.~theta], "|")),
             y = expression(alpha[theta]))

plot_alpha_u <- 
    ggplot(simulation_fluxes[rep == 1] ) + 
        geom_vline(xintercept = c(.15), size = .2, alpha = .5, lty = 2) +
        geom_hline(yintercept = c(-1,0,1), size = .2, alpha = .5) +
        geom_point(aes(x = abs(r_uw), u_alpha, col = cut_zL3(zL), shape = flag), 
                   alpha = .7)  +
        theme_copernicus() +
        stability_color_scale() +
        ylim(-4,4) +
        scale_shape_manual(name = "Flag", 
                           values = c(daytime=15, nighttime = 3, 
                                      instationary = 21))  +
        no_grid() +
        labs(x = expression(paste("|", rho[w.u], "|")),
             y = expression(alpha[u]))

# Figure 2.
p_alpha_values <- 
    plot_alpha_c + plot_alpha_w + plot_alpha_ts + plot_alpha_u +
    plot_annotation(tag_levels = "a", tag_suffix = ")") +
    plot_layout(guides = "collect") &
    theme(legend.box = "vertical", 
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.margin = margin(1,4,1,1),
          legend.box.margin = margin(4,4,4,4),
          legend.title = element_text(size = 12),
          legend.text= element_text(size = 10, color = main_palette[5]),
          legend.justification = "center",
          legend.spacing.x = unit(2,'mm'),
          legend.key.size = unit(3, "mm"),
          legend.background = element_rect(fill = alpha("white", .8)))
        
psave(name = "Fig02.pdf", p = p_alpha_values, 
      height = 11, width = 12, scale = 1.4,
      output_dir = plot_output_dir)




# ------------------------------------------------------------------------
# flux_error_and_alpha_similarity_fig_3
# Figure 3.

# Analytical values 
w_mean_vals <- seq(-2,2,.005)
w_sd_vals   <- c(.65)
dt          <- data.table(expand.grid(w_mean_vals, w_sd_vals))

setnames(dt, c('w', 'sd'))
dt[, flux_error := flux_error_ratio(w, sd)]
dt[, w_sd_r := w/sd]
dt[, alpha := alpha_analytic(w, sd)]


Error0.1limit <- abs(dt[which.min(abs(flux_error - 0.1)), w_sd_r])
Error0.5limit <- abs(dt[which.min(abs(flux_error - 0.5)), w_sd_r])

# 0.1 Error, w/sw = 
Error0.1_text <- paste0('', TeX("0.1 Error, $\\bar{w}/ \\sigma_{w}$= \\pm", 
                               Error0.1limit))

fit_dt <- data.table(w_mean = seq(-5,5, 0.01), w_sigma = 1)


C_Ts_model <- lm(C_alpha ~ Ts_alpha,
                 data = simulation_fluxes[quality_filter])

simulation_fluxes[, C_alpha_predicted := 
           predict(C_Ts_model, newdata = simulation_fluxes)]

simulation_fluxes[, C_alpha_res := 
           C_alpha_predicted - C_alpha]

flux_error_ratio_analytical_and_observed <- 
    ggplot(simulation_fluxes[quality_filter]) + 
    aes(x = w_mean/w_sigma, 
        y = C_alpha*(w_mean/w_abs_mean), 
        col = cut_zL3(zL), shape = flag) + 
    geom_point(alpha = .6, size = 1) +
    geom_line(data = fit_dt, inherit.aes = FALSE,
              aes(x = w_mean/w_sigma, y = flux_error_ratio(w_mean, w_sigma)), 
                  col = main_palette[[5]], size = 1, alpha = .5) +
    ylim(c(-0.1,1.1))  +
    xlim(c(-2.0,2.0)) + 
    geom_segment(aes(x = Error0.1limit, y = -.08, 
                     xend = Error0.1limit, yend = 0.1), lty = 2, size = .2, 
                 inherit.aes = FALSE) +
    geom_segment(aes(x = -2.0, y = .5,
                     xend = -Error0.5limit, yend = 0.5), lty = 2, size = .2,
                 inherit.aes = FALSE) +
    geom_segment(aes(x = -Error0.1limit, y = -.08,
                     xend = -Error0.1limit, yend = 0.1), lty = 2, size = .2,
                 inherit.aes = FALSE) +
    geom_segment(aes(x = -1.8, y = 0.1,
                     xend = -Error0.1limit, yend = 0.1), lty = 2, size = .2, 
                 inherit.aes = FALSE) + 
    geom_text(aes(x = -1.6, y = 0.13,
                  label = "bar(w)==0.32~sigma[w]"),
                  parse = TRUE, size = 3, check_overlap = TRUE, col = "black") +
    geom_text(aes(x = -1.6, y = 0.53, label = "bar(w)==0.87~sigma[w]"),
                  parse = TRUE, size = 3, check_overlap = TRUE, col = "black") +
    labs(x = TeX("$\\bar{w}/ \\sigma_{w}$"), y = TeX("CO_2 Flux error ratio")) +
    theme_copernicus() +
    stability_color_scale() +
    no_grid() +
    scale_shape_manual(name = "Time of the day", 
                       values = c(nighttime = 3, daytime = 15)) 

C_Ts_alpha_regression_plot <- 
    ggplot(simulation_fluxes[quality_filter])  + 
        aes(Ts_alpha, C_alpha, col = cut_zL3(zL), shape = flag) +
        geom_point(alpha = .6, size = 1) +
        xlim(-.9,.9) +
        ylim(-1, 1) +
        no_grid () + 
        geom_abline(slope = 1, alpha = .3)+
        geom_model_equation(C_Ts_model, -.1, 0.95, size = 3, slope.digits = 3) +
        labs(y = expression(alpha[CO[2]]),
        x = expression(alpha[theta]))  + 
        stability_color_scale()  +
        theme_copernicus() +
        scale_shape_manual(name = "Time of the day", 
                           values = c(nighttime = 3, daytime = 15))  +
        no_grid() 

# Figure 3.
flux_error_and_alpha_regression <- 
    flux_error_ratio_analytical_and_observed + C_Ts_alpha_regression_plot +
        plot_annotation(tag_levels = "a", tag_suffix = ")") +
        plot_layout(guides = "collect",
                    widths = c(1.25,1)) & 
            theme(legend.position = "bottom",
                  legend.direction = "horizontal",
                  legend.box = "vertical",
                  legend.margin = margin(1,1,1,1),
                  legend.box.margin = margin(1,1,1,1),
                  legend.justification = "center",
                  legend.title = element_text(size = 11, face = "bold",
                                              margin = margin(0,5,0,0)),
                  legend.text= element_text(size = 10, margin = margin(0,7,0,0)),
                  legend.spacing.x = unit(1,'mm'),
                  legend.key.size = unit(4, "mm"),
                  legend.background = element_rect(fill = alpha("white", .8)))

psave("Fig03.pdf",
      p = flux_error_and_alpha_regression, 
      height = 6.2, width = 12, scale = 1.4,
      output_dir = plot_output_dir)

