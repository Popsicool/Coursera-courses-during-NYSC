# List of Coursera Courses I took during my 12 months service year
# Author: Akinola Samson Oluwasegun
# April 2022
# This visualization is modelled after Jenn Schilling's work on Books he read in 2021

#### Load Libraries ####

library(here)
library(tidyverse)
library(extrafont)
library(ggtext)
library(janitor)
library(scales)
library(patchwork)
library(forcats)
library(ggfittext)
library(magick)
library(grid)
library(readxl)
library(gridExtra)
library(cowplot)
#### Load Data ####
Courses <- read_excel("C:/Users/hp/Desktop/NYSC_Course.xlsx")
course_edit <- Courses %>%
  mutate(Month_finished = factor(Month_finished,
                                 levels = c("August", "September",
                                            "October", "November", "December","January", "February", "March",
                                            "April")),

         label_color = ifelse(Major_skill_taught %in% c("R programming language", "Python", "Excel", "SQL", "Capstone project"),
                              "#000000", "#FFFFFF"),

         Skill = case_when(
          Major_skill_taught == "Intro to data analysis" ~ "•",
          Major_skill_taught == "Excel" ~ "ⴃ",
          Major_skill_taught == "Data Visualization" ~ "℅",
          Major_skill_taught == "SQL" ~ "¥",
          Major_skill_taught == "Presentation Skill" ~ "₿",
          Major_skill_taught == "R programming language" ~ "ⴔ",
          Major_skill_taught == "Statistics" ~ "Ω",
          Major_skill_taught == "Python" ~ "€",
          Major_skill_taught == "Capstone project" ~ "★")) %>% group_by(Month_finished) %>%
  mutate(run_total_minute = cumsum(Total_minutes_of_videos_watched),
         start = ifelse(row_number() == 1, 0 , lag(run_total_minute)),
         index = factor(row_number())) %>%
  ungroup()

symbol_legend_data2 <- tibble(
  type = c("Non Programming Skill", "Programming Skill"),

  x = c(0,1.3),

  y = c(0.3,0.3),


  label = c("•Intro to data analysis\n ℅Data Visualization\n ₿Presentation Skill\n ΩStatistics",
            "ⴃ Excel\n ¥SQL\n ⴔR programming language\n  €python\  ★Capstone project"))
#### Formatting ####
font <- "Franklin Gothic Book"
title_font <- "Bookman Old Style"
font_color <- "gray10"
bcolor <- "#DFDFDF"
theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),

  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),

  axis.title = element_text(size = 16, color = font_color),
  axis.text = element_text(size = 14, color = font_color),
  axis.ticks = element_line(color = font_color),

  axis.line = element_line(color = font_color),

  strip.text = element_text(size = 20, color = font_color, hjust = 0),

  legend.text = element_text(size = 10, color = font_color),
  legend.title = element_text(size = 10, color = font_color),

  plot.title.position = "plot",
  plot.title = element_markdown(size = 30, color = font_color, family = title_font),

  plot.subtitle = element_markdown(size = 20, color = font_color, family = title_font, lineheight = 1.2),

  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 10, color = font_color, hjust = 1)
)
Skill_pal <- c("#3A4B5B", # Google Data Analytics
               "#89B3A7", # Excel Skills for Data Analytics and Visualization
               "#E48F7B", # Presentation skills:Speechwriting and Storytelling
               "#A93343", # IBM Data Analyst
               "#2B409F", # Data Analysis and Presentation Skills: the PwC Approach
               "#698DA7", # No Specialization
               "#828284", # science fiction
               "#486344", # self help
               "#418472" )# Python for Everybody
course_stack <- ggplot(data = course_edit,
                     mapping = aes(x = Month_finished,
                                   y = Total_minutes_of_videos_watched,
                                   fill = Major_skill_taught,
                                   label = Course,
                                   group = fct_rev(index)))+

  geom_col(color = bcolor) +
  geom_text(mapping = aes(y = run_total_minute,
                          label = Skill,
                          color = label_color),
            nudge_y = 2,
            nudge_x = -0.1,
            vjust = 1,
            size = 4,
            lineheight = 100)+
geom_fit_text(mapping = aes(color = label_color),
                reflow = TRUE,
                position = "stack",
                family = font,
                lineheight = 0.8)+
  scale_fill_manual(values = Skill_pal) +
  scale_color_manual(values = course_edit$label_color %>% unique(.)) +
  guides(size = "none",
         fill = "none",
         color = "none") +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        axis.text = element_text(size = 16, color = font_color))+
  labs(caption = "Visualization Modelled after Jenn Schilling's work on Books he read in 2021")


course_stack
# Genre Bar + Legend
Course_legend_bar <- ggplot(data = course_edit,
                     mapping = aes(y = fct_rev(fct_infreq(Major_skill_taught)),
                                   fill = Major_skill_taught)) +
  geom_bar() +
  geom_bar_text(contrast = TRUE,
                stat = "count",
                mapping = aes(label = ..count..),
                family = font)+
  scale_fill_manual(values = Skill_pal) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  guides(fill = "none") +
  labs(title = "Focal point of the Courses") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        plot.title = element_markdown(size = 16, color = font_color, family = font))+
  theme(legend.position = "none")
Course_legend_bar

# Organization Bar + Legend
Org_legend_bar <- ggplot(data = course_edit,
                            mapping = aes(y=fct_rev(fct_infreq(Organization)),
                                          fill = specialization)) +
  geom_bar()+
  geom_bar_text(contrast = TRUE,
                stat = "count",
                mapping = aes( label = ..count..),
                family = font)+
  scale_fill_manual(values = Skill_pal) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  guides(fill = "none") +
  labs(title = "Courses by Organizations",
       caption = "@Samson_Akinola1") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        plot.title = element_markdown(size = 16, color = font_color, family = font))+
  annotate(geom = "text", x=0.7, y=0, size= 3.5, label= "Note: NRTSUEDF= National Research Tomsk State University E-Learning Development Fund")+
  plot.title = element_markdown(size = 16, color = font_color, family = font)+
  theme(legend.position = "none")
Org_legend_bar

# Symbol Legend
symbol_leg <- ggplot(data = symbol_legend_data2,
                        mapping = aes(y = y,
                                      x = x,
                                      label = label))+
  geom_text(hjust = 0,
            vjust = 1,
            lineheight = 0.8,
            size = 5,
            family = font,
            color = font_color) +
  scale_x_continuous(limits = c(0, 2.75)) +
  scale_y_continuous(limits = c(-0.5, 1)) +
  coord_cartesian(clip = "off",
                  expand = FALSE) +
  labs(title = "<i>Meaning of each Symbols:") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 100, b = 20, l = 20),
        plot.title = element_markdown(size = 16, color = font_color, family = font))+
  theme(legend.position = "none")

symbol_leg
# Put it together

right_side <- wrap_elements(full = symbol_leg) +
  wrap_elements(full = Course_legend_bar) +
  wrap_elements(full = Org_legend_bar) +
  plot_layout(ncol = 1,
              heights = c(1, 2, 1))

plot_grid(course_stack, right_side)
course_stack+ right_side+
  plot_layout(ncol= 2, widths = c(3,2))+
  plot_annotation(title = "<b>A Service Year of Learning</b>",
                  subtitle = "Coursera courses I took during my NYSC days(May 2021-April 2022).<br>
                  I watched 945 lecture Videos with a total of 5919 minutes,
                  and an average of 6 minutes per video " ,
                  caption = "Akinola Samson O. @Samson_Akinola1")

