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
hispanic <- matrix(nrow=n.obs)
education <- matrix(nrow=n.obs)

# Define cutoffs for outliers for age.
quantiles <- summary(new.dat$RIDAGEMN)
iqr <- as.numeric(quantiles[5] - quantiles[2])
upper.limit <- as.numeric(quantiles[5] + 1.5*iqr)
lower.limit <- as.numeric(quantiles[2] - 1.5*iqr)

# Alternate cleaning and defining strategy: use the indexing of matrices and vectors and include
# the logical condition in the index. Better in some cases than for loops and if statements.

# Luckily, R recognizes the "." as missing values. However, it does not recognize
# placeholders who refused to answer or did not know the correct value.
# Therefore, we have to indicate that those are missing values too.
new.dat[new.dat$DMDEDUC2==7 | new.dat$DMDEDUC2==9, "DMDEDUC2"] <- NA
new.dat[new.dat$DMDMARTL==77 | new.dat$DMDMARTL==99, "DMDMARTL"] <- NA

# Define the new Hispanic variable as 1 for those who have Hispanic ethnicity and 0 for those that do not.
hispanic[new.dat$RIDRETH1==1 | new.dat$RIDRETH1==2] <- 1
hispanic[new.dat$RIDRETH1==3 | new.dat$RIDRETH1==4 | new.dat$RIDRETH1==5] <- 0

# Combine the non-diploma obtaining categories in the new education variable. Otherwise, the new variable
# is the same as the old education variable.
education[new.dat$DMDEDUC2 == 1 | new.dat$DMDEDUC2 == 2] <- 1
education[new.dat$DMDEDUC2 == 3] <- 2
education[new.dat$DMDEDUC2 == 4] <- 3
education[new.dat$DMDEDUC2 == 5] <- 4

# Assess if there are any age outliers in the model.
which(new.dat$RIDAGEMN < lower.limit | new.dat$RIDAGEMN > upper.limit)

# Make sure that if a subject is less than 18 years of age that they are
# not in any inappropriate relationships.
which(new.dat$RIDAGEMN < 216 & new.dat$DMDMARTL != 5)

# BONUS: Define a new factor variable based on the new education variable
# with the properly defined levels.
education.f <- factor(education, labels = c("No Diploma", "HS Diploma", "Some College", "College Degree"))
