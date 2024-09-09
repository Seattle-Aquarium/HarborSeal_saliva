## This script creates Progesterone plots for the harbor seal saliva manuscript


## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(here)
library(cowplot)
library(tidyverse)
library(readxl)

#check to see that here() leads to root directory or top-level folder
here()

#if incorrect, enter `set_here(path = "...")`
#restart RStudio
#check here() again to make sure you're in the right top-level folder

#clean environment
rm(list = ls())

#import data
data <- read_excel(here("data", "Progesterone data.xlsx"))

################################
# Stats
################################

#how many samples (n) per animal
data %>%
  count(ID)

#rename P column
colnames(data)[4] <- "Progesterone"

#summary stats 
data %>%
  group_by(ID) %>%
  summarise(min = min(Progesterone),
            mean = mean(Progesterone),
            max = max(Progesterone),
            sd = sd(Progesterone))

################################
# Individual plots
################################

#pull out f1 data
f1 <- data %>%
  filter(ID == "F1") 

#format date column
f1 <- f1 %>% 
  mutate(f1, Date = as.Date(Date, format = "%Y"))

#F1 plot
f1.plot <- ggplot(f1, aes(x = Date)) +
  geom_line(aes(y = Progesterone), lty = 1, alpha = 0.7) +
  geom_point(aes(y = Progesterone), shape = 16, alpha = 0.5) +
  labs(title = "F1 Profile", y = "Progesterone (pg/ml)") +
  theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 600)) +
  scale_x_date(date_labels = "%m/%Y") 

#add labels and arrows
f1.plot +
  annotate("text", x = as.Date("2012-09-5"), y = 500, label = "Implantation", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = as.Date("2012-09-5"), y = 475, xend = as.Date("2012-09-5"), yend = 350),
                               arrow = arrow(length = unit(0.5, "cm")), color = "red") +
  annotate("text", x = as.Date("2013-06-10"), y = 500, label = "Birth", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = as.Date("2013-06-10"), y = 475, xend = as.Date("2013-06-10"), yend = 350),
               arrow = arrow(length = unit(0.5, "cm")), color = "red")


#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(f1.plot)

ggsave(here("output", "F1 profile.png"))


##################################

#pull out f2 data
f2 <- data %>%
  filter(ID == "F2") 

#clean up date column
f2 <- f2 %>% 
  mutate(f2, Date = as.Date(Date, format = "%Y"))

#F2 plot
f2.plot <- ggplot(f2, aes(x = Date)) +
  geom_line(aes(y = Progesterone), lty = 1, alpha = 0.7) +
  geom_point(aes(y = Progesterone), shape = 16, alpha = 0.5) +
  labs(title = "F2 Profile", y = "Progesterone (pg/ml)") +
  theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 800)) +
  scale_x_date(date_labels = "%m/%Y") 

#add labels and arrows
f2.plot +
  annotate("text", x = as.Date("2014-07-30"), y = 700, label = "Implantation", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = as.Date("2014-07-30"), y = 675, xend = as.Date("2014-07-30"), yend = 600),
               arrow = arrow(length = unit(0.5, "cm")), color = "red") +
  annotate("text", x = as.Date("2015-05-31"), y = 700, label = "Birth", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = as.Date("2015-05-31"), y = 675, xend = as.Date("2015-05-31"), yend = 550),
               arrow = arrow(length = unit(0.5, "cm")), color = "red")


#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(f2.plot)

ggsave(here("output", "F2 profile.png"))

