#Week 4. Data wrangling. Álvaro Germán Torres Mora

#2. Read the "Human development" and "Gender inequality" datas into R. Here are the links to the datasets:
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")
#3. Explore the datasets: see the structure and dimensions of the data. Create summaries of the variables
str(hd)
dim(hd)
str(gii)
dim(gii)
summary(hd)
summary(gii)
#4.Look at the meta files and rename the variables with (shorter) descriptive names
colnames(hd) <- c("HDIR", "CTRY", "HDI", "LIF.EXP.BIRTH", "EXPECT.EDUCATION", "MEANEDUCATION", "GNI.PC", "GNI-HDIR")
colnames(gii) <- c("GIIR", "CTRY", "GII", "MAT.MORT.RATIO", "ADOL.BIRTH.RATE", "REPR.PARLIAMENT", "POP.SEC.EDUC.FEM", "POP.SEC.EDUC.MAL", "LAB.FOR.PART.FEM", "LAB.FOR.PART.MAL")
str(hd)
str(gii)
#5.Mutate the "Gender inequality" data and create two new variables
gii <- mutate(gii, EDUCATION.RATIO = POP.SEC.EDUC.FEM / POP.SEC.EDUC.MAL) 
gii <- mutate(gii, LABOUR.RATIO = LAB.FOR.PART.FEM / LAB.FOR.PART.MAL) 
#6.Join together the two datasets using the variable Country as the identifier
join_by <- c("CTRY")
human <- inner_join(hd, gii, by = join_by)
str(human)
dim(human)
write.csv(human, file = "human.csv", row.names = FALSE)
