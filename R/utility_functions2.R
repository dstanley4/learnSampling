get_middle_percent <- function(sample_statistic, middle_percent = 80) {
     middle_prop <- round(middle_percent/100,2)
     start_prop <- (1 - middle_prop)/2
     end_prop <-  1 - start_prop
     sample_statistic = sort(sample_statistic)
     n = length(sample_statistic)
     xstart_n <- as.integer(n) * start_prop
     xstart_n <- round(xstart_n + 1)
     xstart_n <- as.integer(xstart_n)
     xend_n <- as.integer(n * end_prop)
     lower <- sample_statistic[xstart_n]
     upper <- sample_statistic[xend_n]
     output <- c(lower, upper)
     return(output)
}

get_text_points <- function(data, text, binwidth, breaks, max_chars_per_column) {

  maxx <- max(data$x) + abs(.0000001*max(data$x))
  minx <- min(data$x) - abs(.0000001*min(data$x))

  if (is.null(breaks)) {
      number_of_bins <- 20
      if (is.null(binwidth)) {
        #auto assign based on stat
        if (text == "p") {
          breaks <- seq(0,1, by = .05)
        } else if (text == "r" | text == "d") {
          binwidth <- .05
          if (minx <0) {
            minx <- ceiling(minx/binwidth)*binwidth
          } else {
            minx <- floor(minx/binwidth)*binwidth
          }
          if (maxx <0) {
            maxx <- floor(maxx/binwidth)*binwidth
          } else {
            maxx <- ceiling(maxx/binwidth)*binwidth
          }
          breaks <- seq(minx, maxx, by = binwidth)
        } else {
          binwidth <- (maxx-minx)/number_of_bins
          breaks <- seq(minx, maxx, by = binwidth)
        }

      } else {
        if (minx <0) {
          minx <- ceiling(minx/binwidth)*binwidth
        } else {
          minx <- floor(minx/binwidth)*binwidth
        }
        if (maxx <0) {
          maxx <- floor(maxx/binwidth)*binwidth
        } else {
          maxx <- ceiling(maxx/binwidth)*binwidth
        }
        breaks <- seq(minx, maxx, by = binwidth)
      }
  }

  # bin_center <- bin_centers(breaks)
  # bin_num    <- seq(1,(length(breaks)-1), by = 1)
  # bin_data   <- data.frame(bin_num, bin_center)

  # is_no_groups <- TRUE
  #
  # if ( length(unique(data$group)) > 1 ) {
  #   is_no_groups <- FALSE
  # }
  #
  # if ( length(unique(data$colour)) > 1 ) {
  #   is_no_groups <- FALSE
  # }

  if (is.null(data$colour)) {
  #if (is_no_groups) {
      plot_heights <- get_group_plot_heights(data$x, breaks, colour = NULL, group = data$group[1], PANEL = data$PANEL[1])
  } else {
    plot_heights <- get_multiple_group_plot_heights(data, breaks)
  }
  #print(plot_heights)

  max_ytop <- max(plot_heights$ytop)
  plot_heights$ytop_characters <- round((plot_heights$ytop/max_ytop) * max_chars_per_column)

  plot_points <- create_points_multiple_groups(plot_heights)
  text_out <- text
  if (text == "p") {
    text_out <- c("italic('p')")
  } else if (text == "r") {
    text_out <- c("italic('r')")
  } else if (text == "d") {
    text_out <- c("italic('d')")
  } else if (text == "M") {
    text_out <- c("italic('M')")
  }
  plot_points$label <- text_out


  #print("densisty check")
  # print(plot_heights)
  # #rescale so it fits under density curve
  maxy_plot_density <- max(plot_heights$maxdensity)
  plot_points$y <- ((plot_points$y/max(plot_points$y)) * maxy_plot_density)


  #remove label where y = 0
  y_is_zero <- plot_points$y <= 0
  plot_points$label[y_is_zero] <- ""



  plot_points
}


bin_centers <- function(bins) {
  loop <- 1:(length(bins)-1)
  output <- c()
  for (i in loop) {
    output[i] <- mean(c(bins[i], bins[i+1]))
  }
  output
}


get_group_plot_heights <- function(x_for_group, breaks, colour, group, PANEL) {

  #rangev <- range(x_for_group, na.rm = TRUE)
  #print(rangev)

  #xden <- ggplot2:::compute_density(x_for_group, w = NULL, from = rangev[1], to = rangev[2])
  xden <- ggplot2:::compute_density(x_for_group, w = NULL, from = min(breaks), to = max(breaks))
  #xden <- density(x_for_group)$y
  maxdensity <- max(xden$density)
  #print(sprintf("Max density: %1.2f", maxdensity))


  bin_center <- bin_centers(breaks)
  #bin_center <- breaks[1:(length(breaks)-1)] #not center left side
  bin_num    <- seq(1,(length(breaks)-1), by = 1)
  bin_data   <- data.frame(bin_num, bin_center, bin_count = 0)

  x_as_bin_num <- .bincode(x_for_group, breaks)
  num_per_bin <- table(x_as_bin_num)
  bins_used <- as.numeric(names(num_per_bin))
  bin_count <- as.numeric(num_per_bin)
  #bin_count <- bin_count/sum(bin_count) * 100 # making it bin percent
  # print("BIN COUNT")
  # print(bin_count)
  # print((bin_count/sum(bin_count)) * 100)
  bin_data$bin_count[bins_used] <- bin_count
  # print(bin_data)
  # xv <- bin_data$bin_center[bins_used]

  if (is.null(colour)) {
    #output <- data.frame(x = xv, ytop = bin_count, group = group, PANEL = PANEL, maxdensity)
    output <- data.frame(x = bin_data$bin_center, ytop = bin_data$bin_count, group = group, PANEL = PANEL, maxdensity)
  } else {
    #output <- data.frame(x = xv, ytop = bin_count, colour = colour, group = group, PANEL = PANEL, maxdensity)
    output <- data.frame(x = bin_data$bin_center, ytop = bin_data$bin_count, colour = colour, group = group, PANEL = PANEL, maxdensity)
  }

  #print("output")
  #print(output)
  output
}


get_multiple_group_plot_heights <- function(data, breaks) {

    group_colours <- unique(data$colour)

    cur_data <- data[data$colour == group_colours[1], ]
    plot_heights <- get_group_plot_heights(cur_data$x, breaks, colour = cur_data$colour[1], group = cur_data$group[1], PANEL = cur_data$PANEL[1])

    if (length(group_colours) > 1) {
      for (i in 2:length(group_colours)) {
        cur_data <- data[data$colour == group_colours[i], ]
        cur_plot_heights <- get_group_plot_heights(cur_data$x, breaks, colour = cur_data$colour[1], group = cur_data$group[1], PANEL = cur_data$PANEL[1])
        plot_heights <- rbind(plot_heights, cur_plot_heights)
      }
    }
  plot_heights
}


get_multiple_group_plot_heights_v2 <- function(data, breaks) {

  group_numbers <- unique(data$group)

  cur_data <- data[data$group == group_numbers[1], ]
  plot_heights <- get_group_plot_heights(cur_data$x, breaks, colour = "black", group = cur_data$group[1], PANEL = cur_data$PANEL[1])

  if (length(group_numbers) > 1) {
    for (i in 2:length(group_numbers)) {
      cur_data <- data[data$group == group_numbers[i], ]
      cur_plot_heights <- get_group_plot_heights(cur_data$x, breaks, colour = "black", group = cur_data$group[1], PANEL = cur_data$PANEL[1])
      plot_heights <- rbind(plot_heights, cur_plot_heights)
    }
  }
  plot_heights
}

create_points <- function(df) {
  num_row <- dim(df)[1]
  start_index <- 1
  ys_out <- rep(NA, sum(df$ytop_characters) + sum(df$ytop_characters == 0))
  xs_out <- rep(NA, sum(df$ytop_characters) + sum(df$ytop_characters == 0))

  for (i in 1:num_row) {
    cur_maxy <- df$ytop_characters[i]
    cur_x    <- df$x[i]

    dfout <- NULL
    #if (cur_maxy>0) {
      if (cur_maxy >0) {
        ys <- seq(1, cur_maxy, by = 1)
      } else {
        ys = 0
      }
      xs <- rep(cur_x, length(ys))
      end_index <- start_index + length(xs) - 1

      ys_out[start_index:end_index] <- ys
      xs_out[start_index:end_index] <- xs
      start_index <- end_index + 1
    #}
  }

  ys_out <- (ys_out/sum(ys_out)) * 100 * df$maxdensity[1]

  if (is.null(df$colour)) {
    dfout <- data.frame(x=xs_out, y=ys_out, group = df$group[1], PANEL = df$PANEL[1])
  } else {
    dfout <- data.frame(x=xs_out, y=ys_out, colour = df$colour[1], group = df$group[1], PANEL = df$PANEL[1])
  }

  dfout$y2 <- dfout$y/sum(dfout$y) * df$maxdensity[1]
  dfout
}


create_points_multiple_groups <- function(data) {

  if (is.null(data$colour)) {
    plot_points <- create_points(data)
  } else {
    group_colours <- unique(data$colour)
    cur_data <- data[data$colour == group_colours[1], ]
    plot_points <- create_points(cur_data)

    if (length(group_colours) > 1) {
      for (i in 2:length(group_colours)) {
        cur_data <- data[data$colour == group_colours[i], ]
        cur_plot_points <- create_points(cur_data)
        plot_points <- rbind(plot_points, cur_plot_points)
      }
    }
  }

  plot_points
}
