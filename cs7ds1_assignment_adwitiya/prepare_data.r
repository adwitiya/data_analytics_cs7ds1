# Load the dataset from file.

library(readxl, quietly=TRUE)

crs$dataset <- read_excel("/home/stackoverflow/Desktop/data analytics/Employeeattrition.xlsx", guess_max=1e4)

crs$dataset


# Build the train/validate/test datasets.

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- NULL
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("Age", "BusinessTravel", "DailyRate", "Department",
                   "DistanceFromHome", "Education", "EducationField",
                   "EmployeeNumber", "EnvironmentSatisfaction", "Gender",
                   "HourlyRate", "JobInvolvement", "JobLevel", "JobRole",
                   "JobSatisfaction", "MaritalStatus", "MonthlyIncome",
                   "MonthlyRate", "NumCompaniesWorked", "OverTime",
                   "PercentSalaryHike", "PerformanceRating",
                   "RelationshipSatisfaction", "StockOptionLevel",
                   "TotalWorkingYears", "TrainingTimesLastYear",
                   "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole",
                   "YearsSinceLastPromotion", "YearsWithCurrManager")

crs$numeric   <- c("Age", "DailyRate", "DistanceFromHome", "Education",
                   "EmployeeNumber", "EnvironmentSatisfaction", "HourlyRate",
                   "JobInvolvement", "JobLevel", "JobSatisfaction",
                   "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked",
                   "PercentSalaryHike", "PerformanceRating",
                   "RelationshipSatisfaction", "StockOptionLevel",
                   "TotalWorkingYears", "TrainingTimesLastYear",
                   "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole",
                   "YearsSinceLastPromotion", "YearsWithCurrManager")

crs$categoric <- c("BusinessTravel", "Department", "EducationField",
                   "Gender", "JobRole", "MaritalStatus", "OverTime")

crs$target    <- "Attrition"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("EmployeeCount", "Over18", "StandardHours")
crs$weights   <- NULL