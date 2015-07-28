# Load the necessary package to open the SAS data file.
library(foreign)

# Load the data file into R.
dat <- read.xport("demo_e.xpt")

# Save the names of the desired variables as strings.
variables <- c("SEQN", "RIDAGEMN", "RIDRETH1", "DMDEDUC2", "DMDMARTL")

# Create a new data set with only the desired variables and the ID for each subject.
new.dat <- dat[,variables]

# Save the number of subjects in the data set.
n.obs <- dim(new.dat)[1]

# Create matrices to hold the indicator for Hispanic ethnicity
# and the new education variables.
hispanic <- matrix(nrow=n.obs, ncol=1)
education <- matrix(nrow=n.obs, ncol=1)

# Create vector to hold indices for underage subjects that may
# be in illegal relationships and a vector to hold indices for
# subjects with age values considered outliers.
illegal.status <- c()
age.outliers <- c()

# Define cutoffs for outliers for age.
quantiles <- summary(new.dat$RIDAGEMN)
iqr <- as.numeric(quantiles[5] - quantiles[2])
upper.limit <- as.numeric(quantiles[5] + 1.5*iqr)
lower.limit <- as.numeric(quantiles[2] - 1.5*iqr)

# Loop through all subjects.
for(i in 1:n.obs)
{
  # Luckily, R recognizes the "." as missing values. However, it does not recognize
  # placeholders who refused to answer or did not know the correct value.
  # Therefore, we have to indicate that those are missing values too.
  if(!is.na(new.dat[i, "DMDEDUC2"]) & (new.dat[i, "DMDEDUC2"] == 7 | new.dat[i, "DMDEDUC2"] == 9))
  {
    new.dat[i, "DMDEDUC2"] <- NA
  }
  
  if(!is.na(new.dat[i, "DMDMARTL"]) & (new.dat[i, "DMDMARTL"] == 77 | new.dat[i, "DMDMARTL"] == 99))
  {
    new.dat[i, "DMDMARTL"] <- NA
  }
  
  # Define the new Hispanic variable as 1 for those who have Hispanic ethnicity and 0 for those that do not.
  if(!is.na(new.dat[i, "RIDRETH1"]) & (new.dat[i, "RIDRETH1"] == 1 | new.dat[i, "RIDRETH1"] == 2))
  {
    hispanic[i,] <- 1
  } else 
  {
    hispanic[i,] <- 0
  }
  
  # Combine the non-diploma obtaining categories in the new education variable. Otherwise, the new variable
  # is the same as the old education variable.
  if(!is.na(new.dat[i, "DMDEDUC2"]) & (new.dat[i, "DMDEDUC2"] == 1 | new.dat[i, "DMDEDUC2"] == 2))
  {
    education[i,] <- 1
  } else if(!is.na(new.dat[i, "DMDEDUC2"]))
  {
    education[i,] <- new.dat[i, "DMDEDUC2"] - 1
  }
  
  # Make sure that if a subject is less than 18 years of age that they are not in any inappropriate relationships.
  if(!is.na(new.dat[i, "RIDAGEMN"]) & !is.na(new.dat[i, "DMDMARTL"]) & (new.dat[i, "RIDAGEMN"] < 216 & new.dat[i, "DMDMARTL"] != 5))
  {
    illegal.status <- c(illegal.status, i)
  }
  
  # Assess if there are any age outliers in the model. If so, add the index for that observation into the defined vector.
  if(!is.na(new.dat[i, "RIDAGEMN"]) & (new.dat[i, "RIDAGEMN"] < lower.limit | new.dat[i, "RIDAGEMN"] > upper.limit))
  {
    age.outliers <- c(age.outliers, i)
  }
}

# BONUS: Define a new factor variable based on the new education variable
# with the properly defined levels.
education.f <- factor(education, labels = c("No Diploma", "HS Diploma", "Some College", "College Degree"))