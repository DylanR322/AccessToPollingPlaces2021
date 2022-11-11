#### PSCI 338
## Final Project: Analysis & Table Outputs
## Dylan Radley

# Analysis & Table Outputs
# # Dylan Radley PSCI 3338 Final Project

library(rio)
library(dplyr)
library(tidyr)
library(stargazer)
library(stringr)
library(ggplot2)
library(gridExtra)
library(plm)
library(htmlTable)

rm(list = ls())

setwd("~/Desktop/GitHub/psci-338-project/Data/CleanData/")
# NOTE: If running this code on a different computer or with different file structure, adjust your working directory accordingly.

# Load in the cleaned data!

pps <- import("2018pollingplacesRich.RData")

# Change the Working Directory so that anything being Exported goes into Visuals

setwd("~/Desktop/GitHub/psci-338-project/Visuals/")

# Pull out states in the south and midwest.

pps <- pps[pps$region == "South" | pps$region == "Midwest", ]

# Tweak one variable name

names(pps)[38] <- "pctBachelororhigher"

# Calculate polling places per ten thousand people. 

pps$pppTenTh <- pps$pppcapita * 10000


### Summary Statistics for variables ------
# This code will calculate the summary statistics and put them in tables! Most tables can be found in Visuals after being
# exported from here out of R using the export functions in RStudio.

names(pps)
pps.justvars <- pps[, c(44, 6, 8, 13, 38, 41, 43)] # isolate the variables being summarized.

# simple test table
stargazer(pps.justvars, type='text', summary.stat = c("n", "mean", "median", "p25", "p75", "sd", "min", "max"))

# same table, with labels added!
stargazer(pps.justvars, type='html', summary.stat = c("mean", "median", "p25", "p75", "sd", "min", "max"),
          title='Descriptive Statistics for Independent and Dependent Variables (n = 1163)', align = TRUE,
          covariate.labels = c("Polling Places per 10k Voting Age People (D)", 
                               "Partisan Lean (I)", 
                               "% White (I)",
                               "Unemployment Rate (C)",
                               "% Bachelor's Degree or Higher (C)",
                               "Population Density (C)"), digits = 3, out = "descstats.htm")

# Create a simple table that just counts how many counties are examined from each state.
statetable <- table(pps$state)
statetable <- as.data.frame(statetable)
names(statetable)[1] <- "State"
names(statetable)[2] <- ""

stargazer(statetable, type='html',
          title='# of Counties per State', summary = FALSE, rownames = FALSE, out = "statetable.htm")

# Create a simple table with the number of counties represented from each region.
regiontable <- table(pps$region)
regiontable <- as.data.frame(regiontable)
names(regiontable)[1] <- "Region"
names(regiontable)[2] <- ""

stargazer(regiontable, type='html',
          title='# Counties per Regions Examined', summary = FALSE, rownames = FALSE, out = "regiontable.htm")

# Create a table with the average polling places per ten thousand residents in each of the states examined
avgpppTenThstate <- summarize(group_by(pps, state),
                              avgpppTenTh = mean(pppTenTh))
names(avgpppTenThstate)[1] <- "State"
names(avgpppTenThstate)[2] <- "Average Polling Places per 10k Voting Age Residents"

stargazer(avgpppTenThstate, type='html',
          title='Average Polling Places per 10k Voting Age Residents', summary = FALSE, 
          rownames = FALSE, digits = 2, out = "stateavgtable.htm")

### Regressions ------

# Basic regressions for Hypotheses 1 and 2, with no controls. (Polling places and the pct white in a county, and partisan lean)
H1nocontrols <- lm(pppTenTh ~ pctWhite, data = pps)
summary(H1nocontrols)

H2nocontrols <- lm(pppTenTh ~ plean, data = pps)
summary(H2nocontrols)

# create a table of these regression outputs!
stargazer(H1nocontrols, H2nocontrols, type = "html",
          dep.var.labels = "Polling Places per 10k Voting-Age Residents",
          covariate.labels = c("% White", "Partisan Lean"), out = "modelnocontrols.htm", report = ('vc*p'))

# rerun the regressions together, and with controls added in!
H1and2controls <- lm(pppTenTh ~ pctWhite + plean + log(popdensity) + region + pctUnemploymentRate +
                   pctBachelororhigher, data = pps)
summary(H1and2controls)

# Fixed effects model, using states!
H1and2controlsstate <- plm(pppTenTh ~ pctWhite + plean + log(popdensity) + pctUnemploymentRate +
                       pctBachelororhigher, data = pps, index = "state", model = "within")
summary(H1and2controlsstate)

# Rerunning the H1and2 model, but normalize pppTenTh, so that effect sizes can be interpreted through the coefficients
interpretations <- lm(scale(pppTenTh) ~ pctWhite + plean + log(popdensity) + region + pctUnemploymentRate +
                             pctBachelororhigher, data = pps)
summary(interpretations)

# Make a normalized pppTenTh

pps$normalizedpppTenTh <- scale(pps$pppTenTh)

# Rerun the fixed effects model with this normalized pppTenTh, so that effect sizes can be interpreted.
interpretationsstate <- plm(normalizedpppTenTh ~ pctWhite + plean + log(popdensity) + pctUnemploymentRate +
                              pctBachelororhigher, data = pps, index = "state", model = "within")
summary(interpretationsstate)

# Create Tables of the regression outputs

stargazer(H1and2controls, type = "text") # test table

# Large table with labels, for the H1and2 Model, no fixed effects.
stargazer(H1and2controls, interpretations, type = "html",
          dep.var.labels = c("Polling Places per 10k Voting-Age Residents", 
                             "Normalized Polling Places per 10k (Effect Size)"),
          covariate.labels = c("% White", "Partisan Lean", "Logged Population Density", "South", "Unemployment Rate",
                               "% Bacehlor's Degree or Higher"), out = "model.htm", report = ('vc*p'))

# Create a table for the results of the fixed effects model and its interpretations. This is the one used in the paper. 

stargazer(H1and2controlsstate, interpretationsstate, type = "html",
          dep.var.labels = c("Polling Places per 10k Voting-Age Residents", 
                             "Normalized Polling Places per 10k (Effect Size)"),
          covariate.labels = c("% White", "Partisan Lean", "Logged Population Density", "Unemployment Rate",
                               "% Bachelor's Degree or Higher"), out = "modelstate.htm", report = ('vc*p'))


### Interactions ------

# Interaction model seeing how the relationship between race and polling place changes between regions
H3 <- lm(pppTenTh ~ pctWhite * region, pps)
summary(H3)

# Interaction model seeing how the relationship between partisanship and polling place changes between regions
H4 <- lm(pppTenTh ~ plean * region, pps)
summary(H4)

# Put these models into a table

stargazer(H3, H4, type = "text") # simple test table

# Larger table with labels!
stargazer(H3, H4, type = "html",
          dep.var.labels = "Polling Places per 10k Voting-Age Residents",
          covariate.labels = c("% White", "Partisan Lean", "South", "South and % White", "South and Partisan Lean"), 
          out = "interactions.htm", report = ('vc*p'))

### Graphs ------


# Graph of just the effects of race on polling places, no controls
whitegraph <- pps %>% ggplot(aes(x = pctWhite, y = pppTenTh)) + 
  geom_point(pch = 1, alpha = 0.5) + ggtitle("") + xlab("% White") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.2)+
  theme_bw()

# Graph of just the effects of partisanship on polling places, no controls
pleangraph <- pps %>% ggplot(aes(x = plean, y = pppTenTh)) + 
  geom_point(pch = 1, alpha = 0.5) + ggtitle("") + xlab("Partisan Lean") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.2)+
  theme_bw()

# Graph of just the effects of race on polling places, no controls, but separating by region
whitesepgraph <- pps %>% ggplot(aes(x = pctWhite, y = pppTenTh, color = region)) + 
  geom_point(pch = 1, alpha = 0.5) + ggtitle("") + xlab("% White") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.2)+
  theme_bw()

# Graph of just the effects of partisanship on polling places, no controls, but separating by region
pleansepgraph <- pps %>% ggplot(aes(x = plean, y = pppTenTh, color = region)) + 
  geom_point(pch = 1, alpha = 0.5) + ggtitle("") + xlab("Partisan Lean") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.2) +
  theme_bw()

# ones with the controls

# Graph of population density and polling places
pdensitygraph <- pps %>% ggplot(aes(x = log(popdensity), y = pppTenTh)) + 
  geom_point(pch = 1, alpha = 0.5) + ggtitle("") + xlab("Logged Population Density") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.2) +
  theme_bw()

# Graph of unemployment and polling places
unemploygraph <- pps %>% ggplot(aes(x = pctUnemploymentRate, y = pppTenTh)) + 
  geom_point(pch = 1, alpha = 0.5) + ggtitle("") + xlab("Unemployment Rate") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.2) +
  theme_bw()

# Graph of educational attainment and polling places
educgraph <- pps %>% ggplot(aes(x = pctBachelororhigher, y = pppTenTh)) + 
  geom_point(pch = 1, alpha = 0.5) + ggtitle("") + xlab("% Bachelor's Degree or Higher") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.2) +
  theme_bw()

# Boxplot of Polling places per ten thousand by region
regionboxplot <- pps %>% ggplot(aes(x = region, y = pppTenTh)) + 
  geom_boxplot() + ggtitle("") + xlab("Region") + ylab("Polling Places per 10k Voting-Age People") +
  theme_bw()

# Export the figures that will be used in the paper!

# The two graphs of the independent variables and the dependent variable
grid.arrange(whitegraph, pleangraph, ncol = 2)
indvargraphs <- arrangeGrob(whitegraph, pleangraph, ncol = 2)
ggsave(file="indvarsgraphs.png", indvargraphs, width = 13, height = 7.5, units = 'in')

# the two same graphs, but separated by region
grid.arrange(whitesepgraph, pleansepgraph, ncol = 2)
indvarreggraphs <- arrangeGrob(whitesepgraph, pleansepgraph, ncol = 2)
ggsave(file="indvarreggraphs.png", indvarreggraphs, width = 13, height = 7.5, units = 'in')

# Create graphs that look at each variable and polling places per ten thousand, but color by state

whitegraphstate <- pps %>% ggplot(aes(x = pctWhite, y = pppTenTh, color = state)) + 
  geom_point(pch = 1, alpha = 0.7) + ggtitle("") + xlab("% White") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.1) + ylim(0, 50) +
  theme_bw() + theme(legend.position = "none")

pleangraphstate <- pps %>% ggplot(aes(x = plean, y = pppTenTh, color = state)) + 
  geom_point(pch = 1, alpha = 0.7) + ggtitle("") + xlab("Partisan Lean") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.1) + ylim(0, 50) +
  theme_bw()

pdensitygraphstate <- pps %>% ggplot(aes(x = log(popdensity), y = pppTenTh, color = state)) + 
  geom_point(pch = 1, alpha = 0.7) + ggtitle("") + xlab("Logged Population Density") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.1) + ylim(0, 50) +
  theme_bw() + theme(legend.position = "none")

educgraphstate <- pps %>% ggplot(aes(x = pctBachelororhigher, y = pppTenTh, color = state)) + 
  geom_point(pch = 1, alpha = 0.7) + ggtitle("") + xlab("% With a Bachelor's or Higher") + ylab("Polling Places per 10k Voting-Age People") + 
  geom_smooth(method='lm', formula= y~x, alpha = 0.1) + ylim(0, 50) +
  theme_bw()

# combine the graphs together!
grid.arrange(whitegraphstate, pleangraphstate, pdensitygraphstate, educgraphstate,
             ncol = 2, nrow = 2)
stategraphs <- arrangeGrob(whitegraphstate, pleangraphstate, pdensitygraphstate, educgraphstate,
                            ncol = 2, nrow = 2)
ggsave(file="stategraphs.png", stategraphs, width = 13, height = 15, units = 'in')

# That's all!
