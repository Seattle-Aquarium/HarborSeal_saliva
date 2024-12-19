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
data <- read_excel(here("input", "Progesterone data_birthdate.xlsx"))

################################
# Stats
################################

#how many samples (n) per animal
data %>%
  count(ID)

#rename P column
colnames(data)[4] <- "Progesterone"

#change character string to numeric
data$Progesterone <- as.numeric(data$Progesterone)



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

f1.implantation<- as.Date("2012-09-01")
f1.birth <- as.Date("2013-06-10")
f1.postbirth <- as.Date("2013-06-11")
f1.end <- as.Date("2013-11-04")

#F1 plot
f1.plot <- ggplot(f1, aes(x = Date)) +
  geom_line(aes(y = Progesterone), lty = 1, alpha = 0.7) +
  geom_point(aes(y = Progesterone), shape = 16, alpha = 0.5) +
  labs(title = "F1 Profile", y = "Progesterone (pg/ml)") +
  theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 600)) +
  scale_x_date(date_labels = "%m/%Y") +
  geom_segment(aes(x = f1.implantation, y = 184, xend = f1.birth, yend = 184)) +
  geom_segment(aes(x = f1.implantation, y = 184+89, xend = f1.birth, yend = 184+89), linetype = 2) +
  geom_segment(aes(x = f1.implantation, y = 184-89, xend = f1.birth, yend = 184-89), linetype = 2) +
  geom_segment(aes(x = f1.postbirth, y = 166, xend = f1.end, yend = 166)) +
  geom_segment(aes(x = f1.postbirth, y = 166+169, xend = f1.end, yend = 166+169), linetype = 2) +
  geom_segment(aes(x = f1.postbirth, y = 166-169, xend = f1.end, yend = 166-169), linetype = 2) 
  

  
#add labels and arrows
f1.plot <- f1.plot +
  annotate("text", x = as.Date("2012-09-5"), y = 500, label = "Implantation", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = as.Date("2012-09-5"), y = 475, xend = as.Date("2012-09-5"), yend = 350),
                               arrow = arrow(length = unit(0.5, "cm")), color = "grey10") +
  annotate("text", x = as.Date("2013-06-10"), y = 500, label = "Birth", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = as.Date("2013-06-10"), y = 475, xend = as.Date("2013-06-10"), yend = 350),
               arrow = arrow(length = unit(0.5, "cm")), color = "grey10")


#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(f1.plot)

ggsave(here("output", "F1 profile black with means.png"))


##################################

#pull out f2 data
f2 <- data %>%
  filter(ID == "F2") 

#clean up date column
f2 <- f2 %>% 
  mutate(f2, Date = as.Date(Date, format = "%Y"))

f2.implantation<- as.Date("2014-08-01")
f2.birth <- as.Date("2015-05-31")

#F2 plot
f2.plot <- ggplot(f2, aes(x = Date)) +
  geom_line(aes(y = Progesterone), lty = 1, alpha = 0.7) +
  geom_point(aes(y = Progesterone), shape = 16, alpha = 0.5) +
  labs(title = "F2 Profile", y = "Progesterone (pg/ml)") +
  theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 800)) +
  scale_x_date(date_labels = "%m/%Y") +
  geom_segment(aes(x = f2.implantation, y = 180, xend = f2.birth, yend = 180)) +
  geom_segment(aes(x = f2.implantation, y = 180+98, xend = f2.birth, yend = 180+98), linetype = 2) +
  geom_segment(aes(x = f2.implantation, y = 180-98, xend = f2.birth, yend = 180-90), linetype = 2) 

#add labels and arrows
f2.plot <- f2.plot +
  annotate("text", x = as.Date("2014-07-30"), y = 700, label = "Implantation", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = as.Date("2014-07-30"), y = 675, xend = as.Date("2014-07-30"), yend = 600),
               arrow = arrow(length = unit(0.5, "cm")), color = "grey10") +
  annotate("text", x = as.Date("2015-05-31"), y = 700, label = "Birth", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = as.Date("2015-05-31"), y = 675, xend = as.Date("2015-05-31"), yend = 550),
               arrow = arrow(length = unit(0.5, "cm")), color = "grey10")


#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(f2.plot)

ggsave(here("output", "F2 profile black with mean.png"))



####################
#### stats
####################

#f1 birth 6/10/2013 (healthy)
#f2 birth 5/31/2015 (stillborn)


######## comparison to plasma levels from 1981 paper #############

#range last 6 to 7 weeks of pregnancy
#f1 4/22/2013 - 6/10/2013

date1<- as.Date("2013-04-22")
date2<- as.Date("2013-06-10")
  
range <- f1 %>%
  filter(Date>date1 & Date<date2)

#f2 4/13/2015 - 5/31/2015
date3<- as.Date("2015-04-13")
date4<- as.Date("2015-05-31")

range2<- f2 %>%
  filter(Date>date3 & Date<date4)

#two days prior to birth
#f1 6/8/2013
#on 6/7 it was 170.0 pg/ml

#f2 5/29/2015
#no data

#five days following birth
#f1 6/15/2013
#7/12 50.1 pg/ml

#f2 6/5/2015
#no data

############# averages #############

#f1 only
#f1 estimated implantation 09/01/2012
#f1 birth 6/10/2013 (healthy)

f1.implantation<- as.Date("2012-09-01")
f1.birth <- as.Date("2013-06-10")

f1.preg<- f1 %>%
  filter(Date>f1.implantation & Date<f1.birth)

mean(f1.preg$Progesterone)
sd(f1.preg$Progesterone)

f1.nonpreg <- f1 %>%
  filter(Date>f1.birth)

mean(f1.nonpreg$Progesterone)
sd(f1.nonpreg$Progesterone)

t.test(f1.preg$Progesterone, f1.nonpreg$Progesterone)

#f2 only
#f2 estimated implantation 08/01/2014
#f2 birth 5/31/2015 (stillborn)

f2.implantation<- as.Date("2014-08-01")
f2.birth <- as.Date("2015-05-31")

f2.preg<- f2 %>%
  filter(Date>f2.implantation & Date<f2.birth)

mean(f2.preg$Progesterone)
sd(f2.preg$Progesterone)

#no samples for nonpreg analysis

#### compare f1 and f2

t.test(f1.preg$Progesterone, f2.preg$Progesterone)




