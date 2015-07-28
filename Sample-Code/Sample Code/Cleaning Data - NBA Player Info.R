### CLEANING ###

# Set the working directory
setwd("C:/Users/owner/Dropbox/Ph.D/Intro Data Analysis Course")

# Load the Excel file with the data and remove the non-needed first column
dat <- read.csv(file="2010-2011 NBA Player Data.csv", header=T)
dat <- dat[,-1]

# Save the number of players in the data
n.players <- dim(dat)[1]

# Most of the data has been loaded as factors so convert the proper
# variables to characters
dat$player <- as.character(dat$player)
dat$position <- as.character(dat$position)
dat$college <- as.character(dat$college)
dat$years.pro <- as.character(dat$years.pro)

# Format the dates of birth as dates, but first convert to characters.
# Be sure to specify the order of the dates in the as.Date() command.
dat$date.birth <- as.character(dat$date.birth)
dat$date.birth <- as.Date(dat$date.birth, "%d-%b-%y")

# Calculate age in days by subtracting date of birth from current date.
# Divide by 365.25 to get the date in years, then round down to the nearest
# integer using the floor() command.
age <- as.numeric(Sys.Date() - dat$date.birth)
age <- age/365.25
age <- floor(age)

# Create vectors to hold the new data we will get from cleaning the old.
first.name <- c()
last.name <- c()
feet <- c()
inches <- c()

# Loop through all the players.
for(i in 1:n.players)
{
  # Split the full names into first and last.
  # Be careful for players with only one name.
  names <- strsplit(dat$player[i], " ")
  first.name <- c(first.name, names[[1]][1])
  if(!is.na(names[[1]][2]))
  {
    last.name <- c(last.name, names[[1]][2])
  } else
  {
    last.name <- c(last.name, NA)
  }
  # NOTE: This does not fix all the names, which might have
  # extra or too few spaces in their names. You can write code
  # for each specific name to fix it, if you know their true names.
  
  # Some of the positions are misspelled and some of the combination positions are
  # listed in different orders. Correct the misspellings and make sure the combination
  # has the same order (ex: Changing forward-guard to guard-forward)
  if(dat$position[i] == "Gard")
  {
    dat$position[i] <- "Guard"
  } else if(dat$position[i] == "Forwar" | dat$position[i] == "Foward")
  {
    dat$position[i] <- "Forward"
  } else if(dat$position[i] == "Ceter-Forward" | dat$position[i] == "Center-Forward")
  {
    dat$position[i] <- "Forward-Center"
  } else if(dat$position[i] == "Forward-Guard")
  {
    dat$position[i] <- "Guard-Forward"
  }
  
  # Excel misunderstood the height values and assumed them to be dates.
  # For instance, it treated a height of 6'7" (6-7) as 7-Jun. Therefore,
  # if a player has a "Jun", they are at least 6 feet tall. "Jul" means 7
  # and "May" means 5. The other string is their inches above 5, 6, or 7
  # feet. Note that the strsplit() command cannot split on dashes so I
  # replaced the dashes with spaces.
  ht <- sub("-", " ", dat$height[i])
  ht <- strsplit(ht, " ")
  
  if(ht[[1]][1] == "Jun")
  {
    feet <- c(feet, 6)
    inches <- c(inches, ht[[1]][2])
  } else if(ht[[1]][2] == "Jun")
  {
    feet <- c(feet, 6)
    inches <- c(inches, ht[[1]][1])
  } else if(ht[[1]][1] == "Jul")
  {
    feet <- c(feet, 7)
    inches <- c(inches, ht[[1]][2])
  } else if(ht[[1]][2] == "Jul")
  {
    feet <- c(feet, 7)
    inches <- c(inches, ht[[1]][1])
  } else
  {
    feet <- c(feet, 5)
    inches <- c(inches, ht[[1]][1])
  }
}

# Convert feet and inches to integers
feet <- as.integer(feet)
inches <- as.integer(inches)

# Convert position back to a factor
dat$position <- as.factor(dat$position)

# Remove the original height variable (not necessary)
dat$height <- NULL

# Create the new data set by binding the new data as columns to
# the original data set.
new.dat <- cbind(dat, age, first.name, last.name, feet, inches)


### PROCESSING ###

# Define the upper and lower limits for quantiles
quantiles <- summary(dat$weight)
iqr <- as.numeric(quantiles[5] - quantiles[2])
upper.limit <- as.numeric(quantiles[5] + 1.5*iqr)
lower.limit <- as.numeric(quantiles[2] - 1.5*iqr)

# Create a vector to hold indices for observations that will be deleted.
deletions <- c()

# Loop through all the players
for(i in 1:n.players)
{
  # If a player's weight is missing or considered an outlier or if they have retired, they will be removed.
  # Therefore, we add their row index to the deletions vector.
  if(is.na(dat$weight[i]))
  {
    deletions <- c(deletions, i)
  } else if(dat$weight[i] < lower.limit | dat$weight[i] > upper.limit | dat$years.pro[i] == "R")
  {
    deletions <- c(deletions, i)
  }
  
  # We then loop through the parts of the lists that the original loop has not reached
  # to look for duplications. If the current player's (i) name matches another player's
  # (j) name, then we will add j's row index to the deletions vector.
  for(j in i:n.players)
  {
    if(dat$player[i] == dat$player[j] & i != j)
    {
      deletions <- c(deletions, j)
    }
  }
}

# The new data set will consist of the players not designated for deletion.
new.dat <- dat[-(deletions),]