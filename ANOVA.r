library(reshape2) # tidyr is nicer but your laptops have too-old an R version


dat9 =read.table("chapter_12_exercise_9.dat")
names(dat9) = c("LH-Grids", "RH-Grids", "LH-Brackets", "RH-Brackets") # name the conditions
dat9$Subject = 1:10 # add a column of subject numbers
# So what's our problem? The data aren't tidy!
  # We have several observations per row.
  # We want one observation per row.
# Let's first pound the data flat.
  # in reshape2 the function is melt(),
  # in tidyr the function is gather()
molten = melt(dat9, id.vars="Subject")
# Now our "variable" column really represents information we want in two columns.
  # One column for "hemisphere"
  # One column for grids/brackets.
# If we had tidyr we could use separate() to tear "variable" into two columns.
# Here we'll use grep().
molten$hemisphere = NA
molten$hemisphere[grep("LH", molten$variable)] = "left"
molten$hemisphere[grep("RH", molten$variable)] = "right"
# and again for grids
molten$grids = NA
molten$grids[grep("Grids", molten$variable)] = "grids"
molten$grids[grep("Brackets", molten$variable)] = "brackets"

# okay! So now it's finally tidy!
# Now we can run our model.
model1 = aov(value ~ grids*hemisphere + Error(Subject/(hemisphere*grids)), data=molten)
summary(model1)
# d. 95% CI for condition effect just within the left hemisphere
  # filter the data just for hemisphere == left
molten.LH = subset(molten, hemisphere == "left")
model2 = aov(value ~ grids + Error(Subject/grids), data=molten)
summary(model2)


dat17=read.table("chapter_12_exercise_17.dat")
dat18=read.table("chapter_12_exercise_18.dat")
dat19=read.table("chapter_12_exercise_19.dat")

