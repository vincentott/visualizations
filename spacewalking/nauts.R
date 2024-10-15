
library(tidyverse)
library(ggplot2)
library(cowplot)

nauts <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv'
)

# Keep only those with some extravehicular activity
nauts <- nauts %>% select(c(number, sex, total_eva_hrs)) %>% filter(total_eva_hrs > 0)

# Remove duplicates
nauts <- nauts %>% 
  distinct(number, .keep_all = TRUE)

# Export for raincloud plot in JASP
write_csv(nauts, "/Users/vincent/Desktop/nauts.csv")

# Calculate spacewalking time for plot
timeShare <- nauts %>% group_by(sex) %>% summarize(hours = sum(total_eva_hrs)) %>% as.data.frame()
timeShare$percent <- ( timeShare$hours / sum(timeShare$hours) ) * 100

# How many days in total?
sum(timeShare$hours) / 24  # over 191

# Reorder sex levels and uppercase
timeShare$Sex <- factor(timeShare$sex, levels=c('male', 'female'))

timeSharePlot <- ggplot(timeShare, aes(fill=Sex, y=percent, x=factor(1))) + 
  geom_bar(position="fill", stat="identity", alpha = 0.66, width = 0.6) +
  jaspGraphs::themeJaspRaw() +
  coord_flip() +
  scale_fill_manual(values=c("#00A9E6", "#FB8B00")) +
  guides(fill = guide_legend(reverse=T, title = "", nrow = 1)) +
  labs(
    title    = "Over 191 days of Astronaut Spacewalking",
    subtitle = "6% by Female Astronauts (12 days)",
    caption  = "data from Tatsuya Corlett, Mariya Stavnichuk, & Svetlana V. Komarova (2020)"
  ) +
  theme(
    
    plot.title = element_text(hjust = 0.2, vjust = -12),
    plot.subtitle = element_text(hjust = 0.2, vjust = -12.75),
    
    legend.position = c(0.5, 0.225),
    
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    
    plot.margin = margin(t = 0, r = 5, b = 10, l = 5),

  )

# Add picture
timeSharePlotWithPicture <- ggdraw() +
  draw_plot(timeSharePlot) +
  draw_image(
    "/Users/vincent/Desktop/spacewalk.png",
    scale = 0.25, hjust = -0.325, vjust = -0.34
  )


timeSharePlotWithPicture

ggsave(
  filename = "/Users/vincent/Desktop/spacewalking1.pdf",
  plot = timeSharePlotWithPicture,
  width = 7.38, height = 5.97, units = 'in'
)