plot_confidence_by_group <- function(
  data,
  group_var,
  title = "Confidence in MAC's Ability to Fulfill Its Purpose"
) {
  # Filter out missing values and unwanted responses
  plot_data <- data |>
    filter(
      !confident_in_mac_ability %in% c("Don’t know enough to answer", "Missing")
    ) |>
    drop_na({{ group_var }}) |>
    filter({{ group_var }} != "Missing") |>
    # Drop unused factor levels
    mutate(
      confident_in_mac_ability = fct_drop(confident_in_mac_ability),
      {{ group_var }} := fct_drop({{ group_var }})
    )

  # Count unique levels in the grouping variable
  n_groups <- plot_data |>
    pull({{ group_var }}) |>
    n_distinct()

  # Define color palettes based on number of groups
  colors <- if (n_groups == 2) {
    c(blue, green)
  } else if (n_groups == 3) {
    c(blue, green, mustard)
  } else {
    # For more groups, create a palette
    colorRampPalette(c(blue, green, mustard, orange))(n_groups)
  }

  # Create the plot
  plot_data |>
    ggplot(aes(
      x = confident_in_mac_ability,
      y = after_stat(prop),
      fill = {{ group_var }}
    )) +
    geom_bar(
      aes(group = {{ group_var }}),
      position = "dodge",
      colour = "white"
    ) +
    geom_text(
      aes(
        label = paste0("n = ", after_stat(count)),
        y = after_stat(prop),
        group = {{ group_var }}
      ),
      stat = "count",
      position = position_dodge(width = 0.9),
      vjust = -0.5,
      size = 4
    ) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 0.75)) +
    scale_fill_manual(values = colors) +
    labs(title = title, x = "", y = "", fill = NULL) +
    plot_theme() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom"
    )
}
