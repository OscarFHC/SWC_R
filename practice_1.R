# when: 20161018 @ 14:21
# who: OSCAR
# what: practice git add, git commit, git diff

library(RSQLite)
conn = dbConnect(SQLite(), dbname='D:/Courses/UM/2016Oct_SWCapnentry/survey.sqlite')

# disconnect from the data base

dbDisconnect(conn)
rm(conn)
  # this is a connection to a sqlite data base, "survey.sqlite"
tables = dbListTables(conn)
tables

class(tables)

surveys = dbGetQuery(conn, 'SELECT * FROM surveys')
head(surveys)
summary(surveys)

sp = dbGetQuery(conn, 'SELECT * FROM species')

surveys = dbGetQuery(conn, 'SELECT * FROM surveys 
                     JOIN species ON surveys.species_id = species.species_id 
                     JOIN plots ON surveys.plot_id = plots.plot_id;')

surveys = read.table(file='D:/Courses/UM/2016Oct_SWCapnentry/ecology.csv', sep=",", header=TRUE)
# class returs the type of data structure
# typeof returns the data type, the way R represent the content of the data

df <- data.frame(
  x1 = c(TRUE, FALSE, TRUE),
  x2 = c(1, 'red', 2))

class(surveys$year) # gives a "integer"
class(surveys[,'year']) # gives a "interger"
class(surveys['year']) # gives a "data frame"
class(surveys[['year']]) # gives a "integer"

### Factors: integers associated with character
spice = factor(c("low", "medium", "low", "high"))

spice.o = factor(c("low", "medium", "low", "high"), levels = c("low", "medium", "high"), ordered = TRUE)
spice.o2 = ordered(spice, levels = c("high", "medium", "low"))

lv=c()
for (i in 1:length(levels(surveys[,"taxa"]))){
  lv[i]=names(sort(summary(surveys[,"taxa"]), decreasing=TRUE))[i]
}

# sort(): put values in order
# order():

### tabulation
tabulation = table(ordered(surveys[,"taxa"], levels=lv))
barplot(tabulation)

t1 = table(surveys[,"year"], surveys[, "taxa"])
t2 = with(surveys, table(year, taxa))


### subset
A = surveys[with(surveys, which(year %in% seq.int(1980, 1990))),]
A1 = surveys %>% filter(year %in% seq.int(1980, 1990),
                        taxa=="Rodent")

surveys %>%
  filter(!is.na(weight),
         taxa=="Rodent") %>%
  group_by(species_id) %>%
  summarize(med_weight = median(weight)) %>% # med_weight can be used to change the colname of the output
  print(n=25)

surveys_complete = surveys %>%
  filter(!is.na(weight),
         species_id != "",
         !is.na(hindfoot_length),
         sex != "",
         taxa == "Rodent")

common_sp = surveys_complete %>%
  group_by(species_id) %>%
  tally() %>%
  filter(n >= 50)

common_surveys = surveys_complete %>%
  filter(species_id %in% common_sp$species_id)
  


