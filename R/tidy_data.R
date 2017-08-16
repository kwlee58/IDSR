## Tidy Data
mat <- matrix(c(0, 1, 5, 4), nrow = 2)
rownames(mat) <- c("Male", "Female")
colnames(mat) <- c("Yes", "No")
mat
as.data.frame(mat) 
str(as.data.frame(mat)) ## matrix 에서 data frame 으로 class만 바뀜
as.data.frame.matrix(mat)
str(as.data.frame.matrix(mat)) ## 위와 같은 효과 
as.data.frame.table(mat) ## tidy data 임. observation/variable 관계로 볼 것.
str(as.data.frame.table(mat))
mat.df <- as.data.frame.table(mat)
names(mat.df) <- c("Gender", "Pregnancy", "Counts")
mat.df
str(mat.df)
Counts <- c(mat)
N <- length(Counts)
Gender <- gl(2, 1, N, labels = rownames(mat))
Pregnancy <- gl(2, 2, N, labels = c("Yes", "No"))
mat.df2 <- data.frame(Gender, Pregnancy, Counts)
str(mat.df2)

library(tidyverse)
mat.tbl <- mat %>%
  as_tibble() %>%
  mutate(Gender = row.names(mat)) %>%
  gather(key = Pregnancy, value = Counts, -Gender) %>%
  mutate(Gender = factor(Gender, levels = c("Male", "Female")),
         Pregnancy = factor(Pregnancy, levels = c("Yes", "No")))
mat.tbl
str(mat.df)

## read_csv

library(readr)
pew <- read_csv("https://raw.githubusercontent.com/tidyverse/tidyr/master/vignettes/pew.csv")
View(pew)
str(pew)
write.csv(pew, file = "../data/pew.csv", row.names = FALSE)
pew.tbl <- pew %>%
  as_tibble() %>%
  gather(key = Income, value = Counts, -religion) %>%
  mutate(religion = factor(religion),
         Income = ordered(Income, levels = names(pew)[-1]))
pew.tbl
names(pew.tbl)[1] <- "Religion"
pew.tbl

library(reshape2)
pew.melt <- melt(pew, 
                 id = "religion", ## Columns to keep as is
                 variable.name = "Income_Level", ## Factor name (Key)
                 value.name = "Counts") ## Cell values              
pew.melt
str(pew.melt)
names(pew.melt)[1] <- "Religion"
head(pew.melt, n = 20)

weather <- read_csv("https://raw.githubusercontent.com/tidyverse/tidyr/master/vignettes/weather.csv")
View(weather)
str(weather)
write.csv(weather, file = "../data/weather.csv", row.names = FALSE) ## data 폴더에 저장 
weather <- weather[, -1]
# options(stringsAsFactors = FALSE) ## 작동하지 않음.
weather.melt <- melt(weather, 
                     id = c("year", "month", "element"), ## tidy 상태로 둠. 
                     variable.name = "day",
                     na.rm = TRUE)
str(weather.melt)
weather.melt$day <- as.character(weather.melt$day) ## Factor 를 character로 
weather.melt$day <- sapply(weather.melt$day, substring, 2, USE.NAMES = FALSE) ## 날짜만 추출 
weather.melt$day <- as.integer(weather.melt$day) ## 날짜의 속성을 글자에서 숫자로 
str(weather.melt)
weather.melt <- weather.melt[, c("year", "month", "day", "element", "value")]
head(weather.melt)
weather.melt.cast <- dcast(weather.melt, 
                           year + month + day ~ element, 
                           value.var = "value") ## tmax, tmin 을 변수로 분리 
weather.melt.cast
weather.melt.cast$Date <- apply(weather.melt.cast[, 1:3], ## 연, 월, 일을 연-월-일 하나로 
                                MARGIN = 1, 
                                FUN = paste, 
                                collapse = "-")
weather.melt.cast
weather.tidy <- weather.melt.cast[, c("Date", "tmax", "tmin")] ## 불필요한 변수 정리
weather.tidy
str(weather.tidy)
weather.tidy$Date <- as.Date(weather.tidy$Date)
weather.tidy[, 2:3] <- sapply(weather.tidy[, 2:3], as.numeric)
str(weather.tidy)
weather.tidy

## Titanic2

titanic2 <- read_csv("https://raw.githubusercontent.com/rstudio/Intro/master/data/titanic2.csv")
View(titanic2)
titanic2.tbl <- titanic2 %>%
  gather(key = "Gender", value = "Counts", c("male", "female")) %>%
  spread(key = fate, value = Counts, drop = TRUE) %>%
  mutate(Rate = round(survived / (perished + survived), 2))
titanic2.tbl

titanic2.melt <- melt(titanic2, 
                      id = c("class", "age", "fate"),
                      variable.name = "Gender",
                      value.name = "Counts")
titanic2.melt
titanic2.melt.cast <- dcast(titanic2.melt, class + age + Gender ~ fate,
                           value.var = "Counts")
titanic2.melt.cast$Rate <- round(titanic2.melt.cast$survived /
                                   (titanic2.melt.cast$survived +
                                      titanic2.melt.cast$perished), 2)
titanic2.melt.cast
