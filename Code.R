library(tidyverse)
bonds <- read_csv("Rental Data/RentalBond_Lodgements_June_2022.csv")
bonds <- bonds %>%
  add_row(read_csv("Rental Data/RentalBond_Lodgements_May_2022.csv"), 
          .after=TRUE) %>%
  add_row(read_csv("Rental Data/Rental-bond-lodgement-data-April-2022.csv"), 
          .after=TRUE) %>%
  add_row(read_csv("Rental Data/Rental-bond-lodgement-data-March-2022.csv"), 
          .after=TRUE) %>%
  add_row(read_csv("Rental Data/Rental-bond-lodgements-data-February-2022.csv"), 
          .after=TRUE) %>%
  add_row(read_csv("Rental Data/Rental-bond-lodgements-data-January-2022.csv"), 
          .after=TRUE) %>%
  add_row(read_csv("Rental Data/Rental-bond-lodgement-data-year-2021.csv"), 
          .after=TRUE) %>%
  add_row(read_csv("Rental Data/RentalBond_Lodgements_Year_2020.csv"), 
          .after=TRUE) %>%
  add_row(read_csv("Rental Data/RentalBond_Lodgements_Year_2019.csv"), 
          .after=TRUE) %>%
  add_row(read_csv("Rental Data/RentalBond_Lodgements_Year_2018-Copy.csv"), 
          .after=TRUE) %>%
  add_row(read_csv("Rental Data/RentalBond_Lodgements_Year_2017.csv"), 
          .after=TRUE) %>%
  add_row(read_csv("Rental Data/RentalBond_Lodgements_Year_2016.csv"), 
          .after=TRUE)

glimpse(bonds)  

bonds <- bonds %>%
  rename(
    date = `Lodgement Date`,
    postcode = Postcode,
    type = `Dwelling Type`,
    bedrooms = Bedrooms,
    rent = `Weekly Rent`
  )

bonds <- bonds %>%
  mutate(type = ifelse(type == "U", NA, type)) %>%
  mutate(bedrooms = ifelse(bedrooms == "U", NA, bedrooms)) %>%
  mutate(rent = ifelse(rent == "U", NA, rent))

bonds <- bonds[complete.cases(bonds),]

bonds <- bonds %>%
  mutate(date = as.Date(date, "%d/%m/%Y")) %>%
  mutate(postcode = as.factor(postcode)) %>%
  mutate(type = as.factor(type)) %>%
  mutate(bedrooms = as.factor(bedrooms)) %>%
  mutate(rent = as.numeric(rent)) %>%
  arrange(date)


glimpse(bonds)  

kingsford <- filter(bonds, postcode=="2032" & type=="F" & bedrooms=="1")

library(lubridate)

kingsford <- kingsford %>% 
  mutate(date = lubridate::parse_date_time(date,"ymd"))

kingsford$date <- floor_date(kingsford$date, "month")

kingsford <- kingsford %>%
  group_by(date) %>%
  summarize(quantile = quantile(rent, probs=0.90))

ggplot(data = kingsford, aes(x=date, y=quantile)) +
  geom_line() +
  geom_smooth()

