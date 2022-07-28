# plotly version

plot_timeseries_static <- function(aggfield,
                                   aggtype,
                                   changepoint_methods = "none") {

  timepointcolname <- names(aggfield$values)[1]
  # set up universal plot characteristics
  g <-
    ggplot2::ggplot(aggfield$values[, c(timepointcolname, aggtype), with = FALSE],
                    ggplot2::aes_string(timepointcolname, aggtype)) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_pretty(12),
      labels = scales::label_date_short(sep = " "),
      # breaks = "1 year", labels = scales::date_format("%Y")
      expand = c(0, 0)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    # no labels needed as info will be in the holding section
    ggplot2::labs(
      x = NULL,
      y = paste0(
        aggtype_friendlyname(aggtype, "long"),
        ifelse(
          aggfield$columnname == "[DUPLICATES]",
          "",
          paste0("\n(", aggfield$columnname, ")")
        )
      ),
      title = NULL
    )

  # if all values are NA, show a blank plot, otherwise plot the values
  if (!all(is.na(aggfield$values[[aggtype]]))) {
    g <- g + ggplot2::geom_point(na.rm = TRUE, shape = 4)

    # specify y axis scale
    maxval <- max(aggfield$values[[aggtype]], na.rm = TRUE)
    minval <- min(aggfield$values[[aggtype]], na.rm = TRUE)
    aggbreaks <-
      yscale_breaks(aggtype, maxval, minval, aggfield$fieldtype)
    g <- g + ggplot2::scale_y_continuous(breaks = aggbreaks,
                                         limits = c(min(minval, aggbreaks[1]),
                                                    max(maxval, aggbreaks[length(aggbreaks)]))
    )

    # NOTE: Changepoints functionality disabled until we find a method that works
    # # add changepoint lines if requested
    # if( changepoint_methods != "none"){
    # 	cpts <- aggfield$changepoints[[aggtype]]
    # 	for(k in 1:length(cpts)){
    # 		if( names(cpts[k]) %in% changepoint_methods || changepoint_methods == "all" ){
    # 			g <- g + ggplot2::geom_vline(xintercept=cpts[[k]]$changepoint_timepoints, colour = changepoint_colour(names(cpts[k])))
    # 		}
    # 	}
    # }
  }

  g <- plotly::ggplotly(g)

  g

}
