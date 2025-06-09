

df <- data.frame(row.names = c("Male IU/L", "", "Female IU/L"))
df$"Male >= 21" <- "48"
df$"Male 18-20" <- "37"
df$"Male >= 21"[2] <- "Female >= 21"
df$"Male 18-20"[2] <- "Female 18-20"
df$"Male >= 21"[3] <- "31"
df$"Male 18-20"[3] <- "30"
kable(df)
print(df)

df<-read.csv("LiverDamage_NHANES.csv")
noquote(t(head(as.data.frame(
  apply(df,2, function(x) gsub("\\s+", "", x))),10)))

summary(na.omit(df))


df <- data.frame(row.names = c("SEQN",
                               "LBXSATSI",
                               "Gender", "Age",
                               "CountryOB",
                               "DietHealth",
                               "MilkConumption",
                               "TakeoutMeals",
                               "ReadyMeals",
                               "FrozenMeals",
                               "Cigs100",
                               "DrinkEveryday",
                               "VigWork",
                               "ModWork",
                               "TravelBikeWalk",
                               "VigRec",
                               "ModRec",
                               "MinSedentary",
                               "YEARS",
                               "Liverdamage"   ))

df$"Description"[1]<- "Sequential Number (ID)"
df$"Description"[2]<- "Alanine Aminotransferase (ALT) (U/L), the test variable we used to label disease."
df$"Description"[3]<- "Gender of patient"
df$"Description"[4]<- "Age of patient"
df$"Description"[5]<- "Country of birth.(1:Born in 50 US states or Washington, DC, 2:Others, 77:Refused, Don’t Know)"
df$"Description"[6]<-"How healthy is the diet(1:Excellent, 2:Very Good, 3:Good, 4:Fair, 5: Poor, 7: Refused, 9: Don’t know)"
df$"Description"[7]<-"Past 30 day milk product consumption(0:Never,1:Rarely, 2:Sometimes, 3: Often, 4:Varied, 7:Refused, 9:Don’t Know)"
df$"Description"[8]<-"Meals from fast food or pizza place"
df$"Description"[9]<-"Ready-to-eat foods in past 30 days"
df$"Description"[10]<-"Frozen meals/pizza in past 30 days"
df$"Description"[11]<-"Smoked at least 100 cigarettes in life (1/Yes, 2:No, 7:Refused, 9:Don’t know): AvgAlc12Month:days have 4/5 drinks - past 12 monthes"
df$"Description"[12]<-"Ever have 4/5 or more drinks every day"
df$"Description"[13]<-"Vigorous work activity(1/Yes, 2:No, 7:Refused, 9:Don’t know)"
df$"Description"[14]<-"Moderate work activity(1/Yes, 2:No, 7:Refused, 9:Don’t know)"
df$"Description"[15]<-"Walk or bicycle(1/Yes, 2:No, 7:Refused, 9:Don’t know)"
df$"Description"[16]<-"Vigorous recreational activities((1/Yes, 2:No, 7:Refused, 9:Don’t know)"
df$"Description"[17]<-"Moderate recreational activities((1/Yes, 2:No, 7:Refused, 9:Don’t know)"
df$"Description"[18]<-"Minutes sedentary activity"
df$"Description"[19]<-"Dataset years used"
df$"Description"[20]<-"Our target Variables(1:Yes, 0:No)"

library(knitr)
print(kable(df))


df<-read.csv("LiverDamage_NHANES.csv")
kable(t(head(df)))


tinytex::install_tinytex()







df<-read.csv("LiverDamage_NHANES.csv")
smp_size <- floor(0.70 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

str(train)
0
table(train$LiverDamage)

str(test)












5604+2402


nums <- colnames(train)[unlist(lapply(train, is.numeric), use.names = FALSE)]

cat<-colnames(train)[!colnames(train) %in% nums]

MySummary <- function(x){
  return(c(
    "minimum" = min(x),
    "first quartile" =quantile(x, .25),
    "median" =median(x),
    "third quartile" =quantile(x, .75),
    "maximum" = max(x),
    "IRQ" = IQR(x),
    "sd" = sd(x),
    "skewness" =skewness(x), 
    "kurtosis" = kurtosis(x))
  )
}

library(knitr)
summary<-round(apply(train[nums], MySummary, MARGIN=2),0)
kable(t(summary))










3939+431
3262+374	
























