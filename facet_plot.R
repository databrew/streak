facet_plot <- function (data) {
  data$id <- data$activity_id
  data$lon <- data$lng
  summary <- data %>% dplyr::group_by(id) %>% 
    dplyr::summarise(lon = mean(range(lon)), 
                     lat = mean(range(lat)))
  p <- ggplot2::ggplot() + ggplot2::geom_path(ggplot2::aes(lon, 
                                                           lat, group = id), data, size = 0.35, lineend = "round") + 
    ggplot2::facet_wrap(~id, scales = "free") + ggplot2::theme_void() + 
    ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"), 
                   strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_blank(), 
                   plot.margin = ggplot2::unit(rep(1, 4), "cm"))

  p
}