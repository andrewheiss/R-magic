# Assuming data frame GPA has this structure:
# ID | Gender | Ethnicity | Recent GPA | GPA 1 term | GPA 2 term

# grep + matrix notation
gpa.columns = grep("GPA", colnames(GPA)) 
GPA.only <- GPA[,gpa.columns]

# subset() with explicit columns to not select (-)
GPA.only <- subset(GPA, select = -c(ID, Gender, Ethnicity))

# subset() with grep
GPA.only <- subset(GPA, select = grep("GPA", names(GPA)))