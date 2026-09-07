output[["trajectory_projection_export"]] <- downloadHandler(
  filename = function() {
    paste0("trajectory_", format(Sys.Date()), ".pdf")
  },
  content = function(file) {
    req(
      trajectory_selection_ok(),
      input[["trajectory_point_color"]],
      input[["trajectory_percentage_cells_to_show"]],
      input[["trajectory_point_size"]],
      input[["trajectory_point_opacity"]],
      !is.null(input[["trajectory_projection_point_border"]])
    )

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("The 'ggplot2' package is required to export trajectory plots.")
    }

    trajectory_data <- getTrajectory(
      input[["trajectory_selected_method"]],
      input[["trajectory_selected_name"]]
    )

    cells_df <- mergeTrajectoryWithMetaData(trajectory_data) %>%
      dplyr::filter(!is.na(pseudotime))
    cells_df <- randomlySubsetCells(
      cells_df,
      input[["trajectory_percentage_cells_to_show"]]
    )
    cells_df <- cells_df[sample(seq_len(nrow(cells_df))), ]

    color_variable <- input[["trajectory_point_color"]]
    categorical <- identical(color_variable, "state") ||
      !is.numeric(cells_df[[color_variable]])
    if (categorical) {
      cells_df[[color_variable]] <- factor(cells_df[[color_variable]])
    }

    stroke <- if (isTRUE(input[["trajectory_projection_point_border"]])) {
      0.2
    } else {
      0
    }
    plot <- ggplot() +
      geom_point(
        data = cells_df,
        aes(
          x = .data[["DR_1"]],
          y = .data[["DR_2"]],
          fill = .data[[color_variable]]
        ),
        shape = 21,
        size = input[["trajectory_point_size"]] / 3,
        stroke = stroke,
        color = "#c4c4c4",
        alpha = input[["trajectory_point_opacity"]]
      ) +
      geom_segment(
        data = trajectory_data[["edges"]],
        aes(
          source_dim_1,
          source_dim_2,
          xend = target_dim_1,
          yend = target_dim_2
        ),
        size = 0.75,
        linetype = "solid",
        na.rm = TRUE
      ) +
      cerebro_export_theme()

    if (categorical) {
      colors_for_groups <- assignColorsToGroups(cells_df, color_variable)
      plot <- plot + scale_fill_manual(values = colors_for_groups)

      if (isTRUE(input[["trajectory_projection_group_labels"]])) {
        group_labels <- centerOfGroups(
          cells_df[, c("DR_1", "DR_2")],
          cells_df,
          2,
          color_variable
        )
        plot <- plot +
          geom_label(
            data = group_labels,
            mapping = aes(x_median, y_median, label = group),
            fill = "white",
            size = 4.5,
            color = "black",
            alpha = 0.5,
            fontface = "bold",
            label.size = 0,
            show.legend = FALSE
          )
      }
    } else {
      plot <- plot +
        scale_fill_distiller(
          palette = "Blues",
          direction = 1,
          guide = guide_colorbar(
            frame.colour = "black",
            ticks.colour = "black"
          )
        )
    }

    ggsave(file, plot, height = 8, width = 11, device = "pdf")
  }
)
