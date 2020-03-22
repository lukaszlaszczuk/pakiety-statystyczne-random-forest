library(dplyr)
library(ggplot2)
library(readr)
library(ggmosaic)
library(extdplyr)

df <- read_csv("data/bank.csv")
sum(is.na(df))  # no NULLs in data.frame

head(df)
View(df)
sapply(df, class)

# zamiana kolumn character na factor oraz y na factor
char_cols <- sapply(df, class) == "character"
df[, char_cols] <- as.data.frame(lapply(df[, char_cols], as.factor))
df$'y' <- as.factor(df$'y')
sapply(df, class)


table(df$y)/length(df$y)  # niezbalansowany zbiór
ggplot(df, mapping = aes(x=y, fill=y)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))

unknowns <- df %>% filter(poutcome == 'unknown') %>% select(pdays, previous, poutcome)
View(unknowns)
sum(unknowns$previous != 0)  # if poutcome unknown client wasn't previously contacted
sum(unknowns$pdays != -1)  # if poutcome unknown pdays is -1 -> poutcome, pdays and previous are highly correlated

table(df$previous)
df$previous <- cut(df$previous, breaks=c(-1,0,1,3,10,max(df$previous)), labels = c("no calls", "1 call", "2 or 3 calls","4 to 10 calls",'hot line'))
table(df$previous)  # create bins from "previous" variable


ggplot(data = df, aes(x=balance, col=y)) +
  geom_density()

df %>% 
  filter(balance<20000) %>% 
    ggplot(aes(x=y, y=balance)) +
      geom_boxplot()

ggplot(df, aes(x=duration, col=y, fill=y)) +
  geom_density(alpha=0.1)

age_cut <- cut(df$age, breaks=c(-1,29,39,49,59,100), labels = c("<30", "30-40", "40-50","50-60",'>60'))

df$age <- age_cut

sum(df$age == ">60")

df %>%
  pct_routine(age, y) %>% 
    ggplot(aes(x=age, y=pct, fill=y)) + geom_col()
  


df$balance <- log10(df$balance + (abs(min(df$balance))+1))

ggplot(data = df) +
  geom_mosaic(aes(x = product(y, job), fill = job))

x = factor(df$month, tolower(month.abb))
df$month <- x

ggplot(data = df) +
  geom_mosaic(aes(x = product(y, month), fill = month))

ggplot(data = df) +
  geom_mosaic(aes(x = product(poutcome, month), fill = month))


sub_count <- df %>% group_by(month, y) %>% count()

sub_count %>% filter(y==0) %>%
  ggplot(aes(x=month, y=n, group=y)) +
  geom_line() +
  scale_x_discrete(limits=tolower(month.abb))

sub_count %>% filter(y==1) %>%
  ggplot(aes(x=month, y=n, group=y)) +
  geom_line() +
  scale_x_discrete(limits=tolower(month.abb))


ggplot(sub_count, aes(x=month, y=n, color=y, group=y)) +
  geom_line() +
  geom_point(col = "black") +
  scale_x_discrete(limits=tolower(month.abb))


ggplot(data = df) +
  geom_mosaic(aes(x=product(y, previous), fill=previous))


ggplot(data = df) +
  geom_mosaic(aes(x=product(y, age), fill=age))

ggplot(data = df) +
  geom_mosaic(aes(x=product(month, poutcome), fill=poutcome))

ggplot(data = df) +
  geom_mosaic(aes(x=product(month, poutcome), fill=poutcome)) +
  facet_wrap(y~.)


ggplot(data = df) +
  geom_mosaic(aes(x=product(y, education), fill=education)) +
  facet_wrap(.~marital)

ggplot(data = df) +
  geom_mosaic(aes(x=product(y, poutcome), fill=poutcome))


ggplot(df, mapping = aes(x=education, group=marital)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~marital)

ggplot(data = df) +
  geom_mosaic(aes(x=product(marital, education), fill=education))
