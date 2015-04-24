library(reshape2) # tidyr is nicer but your laptops have too-old an R version
library(lme4) # for mixed-effects models

dat9 =read.table("chapter_12_exercise_9.dat")
names(dat9) = c("LH-Grids", "RH-Grids", "LH-Brackets", "RH-Brackets") # name the conditions
dat9$Subject = 1:10 # add a column of subject numbers
# So what's our problem? The data aren't tidy!
  # We have several observations per row.
  # We want one observation per row.
# Let's first pound the data flat.
  # in reshape2 the function is melt(),
  # in tidyr the function is gather()
molten9 = melt(dat9, id.vars="Subject")
# Now our "variable" column really represents information we want in two columns.
  # One column for "hemisphere"
  # One column for grids/brackets.
# If we had tidyr we could use separate() to tear "variable" into two columns.
# Here we'll use grep().
molten9$hemisphere = NA
molten9$hemisphere[grep("LH", molten9$variable)] = "left"
molten9$hemisphere[grep("RH", molten9$variable)] = "right"
# and again for grids
molten9$grids = NA
molten9$grids[grep("Grids", molten9$variable)] = "grids"
molten9$grids[grep("Brackets", molten9$variable)] = "brackets"

# okay! So now it's finally tidy!
# Now we can run our model.
model1 = aov(value ~ grids*hemisphere + 
               Error(Subject/(hemisphere*grids)), 
             data=molten9)
summary(model1)

modelHLM = lmer(value ~ grids * hemisphere + (1|Subject), data=molten9)
summary(modelHLM)
Anova(modelHLM, type="III")

# d. 95% CI for condition effect just within the left hemisphere
  # filter the data just for hemisphere == left
molten.LH = subset(molten9, hemisphere == "left")
model2 = aov(value ~ grids + Error(Subject/grids), data=molten.LH)
summary(model2)


dat17=read.table("chapter_12_exercise_17.dat")
# Add column names
names(dat17) <- c("Day1", "Day2", "Day3", "Day4", "Group")
# Add column of subject numbers
dat17$Subject = 1:14
# This time when we melt, we want to keep "Group" as an ID variable
# That is, we don't want to break it across rows.
molten17 = melt(dat17, id.vars=c("Subject", "Group"))
molten17
# note what happens if we don't keep Group as an ID variable
melt(dat17, id.vars=c("Subject"))
# Now we can fit our ANOVA
molten17$Group = as.factor(molten17$Group)
model17=aov(value ~ Group*variable + Error(Subject/variable),
            dat = molten17)
summary(model17)


dat18=read.table("chapter_12_exercise_18.dat")
dat19=read.table("chapter_12_exercise_19.dat")

