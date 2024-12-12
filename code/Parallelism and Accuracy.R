## This script creates parallelism and accuracy plots for the harbor seal saliva manuscript


## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(here)
library(cowplot)
library(tidyverse)
library(scales)

#check to see that here() leads to root directory or top-level folder
here()

#if incorrect, enter `set_here(path = "...")`
#restart RStudio
#check here() again to make sure you're in the right top-level folder

###############################
#Figure 
# Parallelism
###############################

#Testosterone

test.par <- data.frame("Standard Concentration" = c(7.81, 31.25, 125, 500, 2000, 8000, 32000), 
                       "%Bound Standards" = c(89.54, 85.99, 64.11, 34.11, 15.45, NA, NA),
                       "%Bound Dilutions" = c(90.7, 92.8, 88.6, 79.8, 75.2, 65.1, 54.1))

p.test.plot <- ggplot(test.par, aes(Standard.Concentration)) + 
  geom_line(aes(y=X.Bound.Standards), col = "black", size = 0.8) +
  geom_point(aes(y=X.Bound.Standards)) +
  geom_line(aes(y=X.Bound.Dilutions), col = "gray33", linetype = "dashed", size = 0.8) +
  geom_point(aes(y=X.Bound.Dilutions)) +
  labs (x = "Testosterone (ng/mL)", y = "Percent label bound") +
  theme_bw(base_size = 10) +
  scale_x_continuous(trans = "log10", labels = function(l) ifelse(l <= 9999, l, comma(l))) +
  scale_y_continuous(limits = c(0,100)) +
  annotate("text", label = "Standards", x = 1000, y = 14,
           size = 3, color = "black") +
  annotate("text", label = "1:1", x = 40000, y = 57,
           size = 3, color = "black") +
  annotate("text", label = "1:2", x = 10000, y = 68,
           size = 3, color = "black") +
  annotate("text", label = "1:4", x = 2000, y = 80,
           size = 3, color = "black") +
  annotate("text", label = "1:8", x = 600, y = 83,
           size = 3, color = "black") +
  annotate("text", label = "1:16", x = 175, y = 90,
           size = 3, color = "black") +
  annotate("text", label = "1:32", x = 40, y = 96,
           size = 3, color = "black") +
  annotate("text", label = "1:64", x = 10, y = 95,
           size = 3, color = "black") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey10", size = 0.8) +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Testosterone assay parallelism")

print(p.test.plot)

#stats
test.long <- test.par %>%
  drop_na %>%
  pivot_longer("X.Bound.Standards":"X.Bound.Dilutions",
    names_to = "Type",
    values_to = "Value"
  )

var.test(Value ~ Type, data = test.long)
#reject the null hypothesis
#two population variances are not equal



#Progesterone

prog.par <- data.frame("Standard Concentration" = c(15.62, 62.5, 125, 250, 500, 2000, 4000), 
                          "%Bound Standards" = c(97.56, 81.58, 59.27, 35.52, 17.43, 4.22, NA),
                          "%Bound Dilutions" = c(102.1, 93, 85, 70.5, 54.2, 35.3, 36.3))

p.prog.plot <- ggplot(prog.par, aes(Standard.Concentration)) + 
  geom_line(aes(y=X.Bound.Standards), col = "black", size = 0.8) +
  geom_point(aes(y=X.Bound.Standards)) +
  geom_line(aes(y=X.Bound.Dilutions), col = "gray33", linetype = "dashed", size = 0.8) +
  geom_point(aes(y=X.Bound.Dilutions)) +
  labs (x = "Progesterone (ng/mL)", y = "Percent label bound") +
  theme_bw(base_size = 10) +
  scale_x_continuous(trans = "log10") +
                     
                     #labels = c(10, 100, 1000, "10,000", "100,000", "1,000,000"), 
                     #breaks = c(10, 100, 1000, 10000, 100000, 1000000)) +
  scale_y_continuous(limits = c(0,120)) +
  annotate("text", label = "Standards", x = 2000, y = 10,
           size = 3, color = "black") +
  annotate("text", label = "1:1", x = 4000, y = 40,
           size = 3, color = "black") +
  annotate("text", label = "1:2", x = 2000, y = 40,
           size = 3, color = "black") +
  annotate("text", label = "1:4", x = 550, y = 59,
           size = 3, color = "black") +
  annotate("text", label = "1:8", x = 300, y = 73,
           size = 3, color = "black") +
  annotate("text", label = "1:16", x = 150, y = 89,
           size = 3, color = "black") +
  annotate("text", label = "1:32", x = 70, y = 97,
           size = 3, color = "black") +
  annotate("text", label = "1:64", x = 20, y = 105,
           size = 3, color = "black") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey10", size = 0.8) +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 5.3)) +
  ggtitle("Progesterone assay parallelism")


print(p.prog.plot)


#stats
prog.long <- prog.par %>%
  drop_na %>%
  pivot_longer("X.Bound.Standards":"X.Bound.Dilutions",
               names_to = "Type",
               values_to = "Value"
  )

var.test(Value ~ Type, data = prog.long)
#accept the null hypothesis
#two population variances are equal


#Estradiol

est.par <- data.frame("Standard Concentration" = c(29.3, 117.2, 468.8, 1875, 7500, 30000, 120000), 
                       "%Bound Standards" = c(92.55, 83.76, 64.71, 42.13, 25.06, 13.9, NA),
                       "%Bound Dilutions" = c(100, 100.1, 94.7, 90.3, 96.9, 90.1, 83.9))

p.est.plot <- ggplot(est.par, aes(Standard.Concentration)) + 
  geom_line(aes(y=X.Bound.Standards), col = "black", size = 0.8) +
  geom_point(aes(y=X.Bound.Standards)) +
  geom_line(aes(y=X.Bound.Dilutions), col = "gray33", linetype = "dashed", size = 0.8) +
  geom_point(aes(y=X.Bound.Dilutions)) +
  labs (x = "Estradiol (ng/mL)", y = "Percent label bound") +
  theme_bw(base_size = 10) +
  scale_x_continuous(trans = "log10", labels = c(10, 100, 1000, "10,000", "100,000", "1,000,000"), breaks = c(10, 100, 1000, 10000, 100000, 1000000)) +
  scale_y_continuous(limits = c(0,120)) +
  annotate("text", label = "Standards", x = 45000, y = 10,
           size = 3, color = "black") +
  annotate("text", label = "1:1", x = 110000, y = 90,
           size = 3, color = "black") +
  annotate("text", label = "1:2", x = 40000, y = 93,
           size = 3, color = "black") +
  annotate("text", label = "1:4", x = 8000, y = 100,
           size = 3, color = "black") +
  annotate("text", label = "1:8", x = 2000, y = 94,
           size = 3, color = "black") +
  annotate("text", label = "1:16", x = 600, y = 98,
           size = 3, color = "black") +
  annotate("text", label = "1:32", x = 125, y = 104,
           size = 3, color = "black") +
  annotate("text", label = "1:64", x = 40, y = 103,
           size = 3, color = "black") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey10", size = 0.8) +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 5.3)) +
  ggtitle("Estradiol assay parallelism")


print(p.est.plot)

#stats
est.long <- est.par %>%
  drop_na %>%
  pivot_longer("X.Bound.Standards":"X.Bound.Dilutions",
               names_to = "Type",
               values_to = "Value"
  )

var.test(Value ~ Type, data = est.long)
#reject the null hypothesis
#two population variances are not equal

 


#put parallelism plots together
p.plots <- plot_grid(p.test.plot, p.prog.plot, p.est.plot, labels = "AUTO")
print(p.plots)

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(p.plots)

ggsave(here("output", "Parallelism with labels black.png"))





###############################
#Figure
# Accuracy
###############################

#progesterone
pro.acc <- data.frame("Standard Expected" = c(1000, 250, 125, 62.5, 31.25, 7.81),
                       "Standard Observed" = c(1253.6, 540.8, 342, 216.2, 223.4, 164.6))


a.pro.plot <- ggplot(pro.acc) +
  geom_point(aes(x = Standard.Expected, y = Standard.Observed)) +
  labs (x = "Standard expected (ng/mL)", y = "Standard observed (ng/mL)") +
  theme_bw(base_size = 10) +
  # scale_x_continuous(trans = "log10") +
  # scale_y_continuous(limits = c(0,100)) +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_smooth(method = lm, se = FALSE, aes(x = Standard.Expected, y = Standard.Observed),
              colour = "grey30", linetype = "dashed")+
  ggtitle("Progesterone assay accuracy")

lm_eqn <- function(est.acc){
  m <- lm(Standard.Observed ~ Standard.Expected, pro.acc);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

a.pro.plot <- a.pro.plot + geom_text(x = 750, y = 400, label = lm_eqn(est.acc), parse = TRUE)

print(a.pro.plot)




# #put accuracy plots together
# a.plots <- plot_grid(a.cort.plot, a.cortico.plot, labels = "AUTO")
# print(a.plots)


#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(a.pro.plot)

ggsave(here("output", "Progesterone Accuracy.png"))




