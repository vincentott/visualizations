

# Libaries ----
library(tidyverse)
library(stringr)
library(ggplot2)
library(scales)
library(cowplot)
library(magick)
library(grid)


filePath <- "/Users/vincent/Desktop/"


# Load data ----
rubiksRecords <- read.delim(paste0(filePath, "rubiksTimes.txt"), sep = "\t", header = FALSE)
colnames(rubiksRecords)[1] <- "date"
colnames(rubiksRecords)[2] <- "time"


# Reformat data ----
rubiksRecords <- rubiksRecords %>%
  mutate(
    time = ifelse(
      str_starts(time, "01."),
      str_remove(time, "^01\\."),  # Remove '01.' at the start
      time
    )
  ) %>%
  mutate(
    time = ifelse(
      str_ends(time, "\\.24"),
      str_remove(time, "\\.24$"),  # Remove '.24' at the end
      time
    )
  )

rubiksRecords[14, 2] <- "06.24"  # Actual time ends with ".24"

rubiksRecords <- rubiksRecords %>%
  mutate(time = as.numeric(time))


rubiksRecords$date <- as.Date(rubiksRecords$date, format = "%b %d, %Y")
rubiksRecords <- rubiksRecords[rev(1:nrow(rubiksRecords)), ]

# Remove first data point from 1982
rubiksRecords <- rubiksRecords[-1, ]

# Add padding value
rubiksRecords <- rbind(
  rubiksRecords,
  data.frame(date = as.Date("2024-10-12"), time = 3.13)
)


# Create Plot ----

lineSize <- 0.5


# Set time labels
shown_times <- c(
  14.76, 9.86,
  # 7.08,
  5.66,
  # 5.55,
  4.90, 3.47, 3.13
)
rubiksRecords$label  <- NA
rubiksRecords[36, 3] <- NA  # Remove padding value
rubiksRecords <- rubiksRecords %>% mutate(rowNumber = row_number())
rubiksRecords$label[rubiksRecords$rowNumber == 4]  <- "2004: < 15s"
rubiksRecords$label[rubiksRecords$rowNumber == 11] <- "2007: < 10s"
# rubiksRecords$label[rubiksRecords$rowNumber == 24] <- "2011: < 6s"
rubiksRecords$label[rubiksRecords$rowNumber == 27] <- "2015: < 5s"
rubiksRecords$label[rubiksRecords$rowNumber == 34] <- "2018: < 4s"


# Basic plot
recordsPlot <- ggplot(
    rubiksRecords,
    aes(x = date, y = time, color = time)
  ) +
  geom_hline(yintercept = 3.13,  linetype = "dotted", size = lineSize * 0.6, color = "black") +
  scale_color_gradient(high = "#bf37ff", low = "#4528dc") +
  geom_step(size = 1.5) +
  scale_y_continuous(
    limits = c(-0.02, 17.25),
    breaks = c(0, 2.5, 5, 7.5, 10, 12.5, 15),
    labels = function(x) ifelse(x %in% c(2.5, 7.5, 12.5), "", x),
    expand = c(0, 0)
  ) +
  geom_text(
    aes(label = label),
    hjust = -0.05,
    vjust = -0.5,
    size = 4.5
  ) +
  scale_x_date(
    limits = as.Date(c("2003-08-01", "2024-10-12")),
    breaks = seq(as.Date("2004-01-01"), as.Date("2024-01-01"), by = "2 years"),
    date_labels = "'%y",
    expand = c(0, 0)
  )


# # Add context
# recordsPlot <- recordsPlot +
#   
#   geom_hline(yintercept = 0.43,  linetype = "dotted", size = lineSize * 0.6, color = "black") +
#   annotate(
#     "text", x = as.Date("2004-01-01"), y = 0.43,
#     label = bquote("current record for smaller 2"^3 * " cube: 0.43s"),
#     vjust = -0.4, hjust = 0.025, size = 4, color = "darkgrey")


# Tidy theme
recordsPlot <- recordsPlot +
  
  theme_cowplot() +
  
  jaspGraphs::themeJaspRaw() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = lineSize * 0.6) +
  geom_segment(aes(x = as.Date("2003-08-01"), xend = as.Date("2003-08-01"), y = 0, yend = 15.25), 
               color = "black", linetype = "solid", size = lineSize * 0.6) + 

  labs(
    title = "Rubik's Cube Solved in Seconds",
    subtitle = "Record Times over the years",
    x = "Year",
    y = "Seconds",
    caption = "data & plot code available via vincentott.github.io"
  ) + 
  
  coord_fixed(ratio = 500, clip = "off") +
  annotate(
    "text", color = "#4528dc",
    label = "2023:",
    x = as.Date("2023-06-11"), y = 3.13, hjust = -0.1,
    vjust = -2,
    size = 4.5
  ) +
  annotate(
    "text", color = "#4528dc",
    label = "3.13s",
    x = as.Date("2023-06-11"), y = 3.13, hjust = -0.1,
    vjust = -0.5,
    size = 4.5
  ) +
  
  theme(
    plot.title = element_text(size = 17.5, vjust = -16, hjust = 1),
    plot.subtitle = element_text(size = 15, vjust = -18.5, hjust = 1),
    axis.text.x = element_text(size = 13),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 13),
    plot.margin = margin(t = -50, r = -150, b = 10, l = -200),
    plot.caption = element_text(size = 9)
  ) 

# Add picture
recordsPlotWithPicture <- ggdraw() +
  draw_plot(recordsPlot) +
  draw_image(
    paste0(filePath, "rubiksCube.png"),
    scale = 0.17, hjust = -0.225, vjust = -0.3
  )

# View
recordsPlotWithPicture

# Save
ggsave(
  filename = paste0(filePath, "rubiksTimes.pdf"),
  plot = recordsPlotWithPicture,
  width = 8, height = 8, units = 'in'
)
