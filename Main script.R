library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
library(RColorBrewer)
library(reshape2)
library(readxl)
library(leaflet)

# Read CSV file
data <- read.csv("GACTT_RESULTS_ANONYMIZED_V2.csv")

#DATA CLEANING

#Delete redundant columns and columns not to be used in analysis
data %>% 
  data <- data %>% select (-Where.do.you.typically.drink.coffee.)
  data <- data %>% select (-How.do.you.brew.coffee.at.home.)
  data <- data %>% select (-How.else.do.you.brew.coffee.at.home.)
  data <- data %>% select (-Where.else.do.you.purchase.coffee.)
  data <- data %>% select (-On.the.go..where.do.you.typically.purchase.coffee.)
  data <- data %>% select (-Please.specify.what.your.favorite.coffee.drink.is)
  data <- data %>% select (-What.else.do.you.add.to.your.coffee.)
  data <- data %>% select (-What.kind.of.dairy.do.you.add.)
  data <- data %>% select (-What.other.flavoring.do.you.use.)
  data <- data %>% select (-What.kind.of.sugar.or.sweetener.do.you.add.)
  data <- data %>% select (-What.kind.of.flavorings.do.you.add.)
  data <- data %>% select (-Other.reason.for.drinking.coffee)
  data <- data %>% select (-Do.you.usually.add.anything.to.your.coffee.)
  data <- data %>% select (-Why.do.you.drink.coffee.)
  data <- data %>% select (-Gender..please.specify.)
  data <- data %>% select (-Ethnicity.Race..please.specify.)
  
#Removing columns on coffee taste test
  data <- data %>% select (-Coffee.A...Bitterness)
  data <- data %>% select (-Coffee.A...Acidity)
  data <- data %>% select (-Coffee.A...Personal.Preference)
  data <- data %>% select (-Coffee.A...Notes)
  data <- data %>% select (-Coffee.B...Bitterness)
  data <- data %>% select (-Coffee.B...Acidity)
  data <- data %>% select (-Coffee.B...Personal.Preference)
  data <- data %>% select (-Coffee.B...Notes)
  data <- data %>% select (-Coffee.C...Bitterness)
  data <- data %>% select (-Coffee.C...Acidity)
  data <- data %>% select (-Coffee.C...Personal.Preference)
  data <- data %>% select (-Coffee.C...Notes)
  data <- data %>% select (-Coffee.D...Bitterness)
  data <- data %>% select (-Coffee.D...Acidity)
  data <- data %>% select (-Coffee.D...Personal.Preference)
  data <- data %>% select (-Coffee.D...Notes)
  data <- data %>% select (-Before.today.s.tasting..which.of.the.following.best.described.what.kind.of.coffee.you.like.)
  
  #Commands to rename column names based on column number after removing columns
  colnames(data)[1] <- "ID"
  colnames(data)[2] <- "age"
  colnames(data)[3] <- "cups_per_day"
  colnames(data)[4] <- "drink_at_home"
  colnames(data)[5] <- "drink_at_office"
  colnames(data)[6] <- "drink_on_the_go"
  colnames(data)[7] <- "drink_at_cafe"
  colnames(data)[8] <- "drink_none"
  colnames(data)[9] <- "po_at_home"
  colnames(data)[10] <- "fp_at_home"
  colnames(data)[11] <- "esp_at_home"
  colnames(data)[12] <- "brewmachine_at_home"
  colnames(data)[13] <- "podmachine_at_home"
  colnames(data)[14] <- "instant_at_home"
  colnames(data)[15] <- "b2c_at_home"
  colnames(data)[16] <- "cb_at_home"
  colnames(data)[17] <- "coffee_extract_at_home"
  colnames(data)[18] <- "other_at_home"
  colnames(data)[19] <- "buy_at_chain"
  colnames(data)[20] <- "buy_at_local_cafe"
  colnames(data)[21] <- "buy_at_drive_thru"
  colnames(data)[22] <- "buy_at_specialty"
  colnames(data)[23] <- "buy_at_supermarket"
  colnames(data)[24] <- "buy_other"
  colnames(data)[25] <- "fav_drink"
  colnames(data)[26] <- "add_none"
  colnames(data)[27] <- "add_milk"
  colnames(data)[28] <- "add_sugar"
  colnames(data)[29] <- "add_flavor"
  colnames(data)[30] <- "add_other"
  colnames(data)[31] <- "milk_whole"
  colnames(data)[32] <- "milk_skim"
  colnames(data)[33] <- "milk_half_half"
  colnames(data)[34] <- "milk_creamer"
  colnames(data)[35] <- "milk_flavoured_creamer"
  colnames(data)[36] <- "milk_oat"
  colnames(data)[37] <- "milk_almond"
  colnames(data)[38] <- "milk_soy"
  colnames(data)[39] <- "milk_other"
  colnames(data)[40] <- "sugar_granulated"
  colnames(data)[41] <- "sugar_artificial"
  colnames(data)[42] <- "sugar_honey"
  colnames(data)[43] <- "sugar_maple_syrup"
  colnames(data)[44] <- "sugar_stevia"
  colnames(data)[45] <- "sugar_agave_nectar"
  colnames(data)[46] <- "sugar_brown_sugar"
  colnames(data)[47] <- "sugar_raw_turbinado"
  colnames(data)[48] <- "flavoring_vanilla"
  colnames(data)[49] <- "flavoring_caramel"
  colnames(data)[50] <- "flavoring_hazelnut"
  colnames(data)[51] <- "flavoring_cinnamon_ground_stick"
  colnames(data)[52] <- "flavoring_peppermint"
  colnames(data)[53] <- "flavoring_other"
  colnames(data)[54] <- "strength_preference"
  colnames(data)[55] <- "roast_preference"
  colnames(data)[56] <- "caffeine_preference"
  colnames(data)[57] <- "rate_coffee_expertise"
  colnames(data)[58] <- "wfh_wfo"
  colnames(data)[59] <- "spend_coffee_per_month"
  colnames(data)[60] <- "drink_cuz_taste_good"
  colnames(data)[61] <- "drink_cuz_need_caffeine"
  colnames(data)[62] <- "drink_cuz_need_ritual"
  colnames(data)[63] <- "drink_cuz_bathroom"
  colnames(data)[64] <- "drink_cuz_other_reasons"
  colnames(data)[65] <- "like_coffee"
  colnames(data)[66] <- "know_where_your_coffee_comes_from"
  colnames(data)[67] <- "most_paid_for_cup"
  colnames(data)[68] <- "most_willing_to_pay"
  colnames(data)[69] <- "getting_value_for_money_at_cafe"
  colnames(data)[70] <- "spend_on_equipment_in_5_years"
  colnames(data)[71] <- "getting_value_on_equipment_spend"
  colnames(data)[72] <- "gender"
  colnames(data)[73] <- "education_level"
  colnames(data)[74] <- "ethnicity_race"
  colnames(data)[75] <- "employment_status"
  colnames(data)[76] <- "no_of_children"
  colnames(data)[77] <- "political_affiliation"
  
#START OF ANALYSIS
  
#Sorting roast preference by age in a new dataframe
roast_by_age <- data %>%
    select(age, roast_preference) %>%
    arrange(age) %>%
    count(age, roast_preference)

# Remove rows with blank values in any column
roast_by_age <- subset(roast_by_age, apply(roast_by_age, 1, function(x) all(nzchar(x))))

# Sort the roast_preference variable according to a specific order
roast_by_age <- roast_by_age %>%
  mutate(roast_preference = fct_relevel(roast_preference, "Light", "Medium", "Dark"))

# Filter out the roast_preference values that you want to exclude
roast_by_age_filtered <- roast_by_age %>%
  filter(!roast_preference %in% c("Blonde", "French", "Italian", "Nordic"))

# Create the stacked bar chart for roast preference vs age
stacked_bar_chart <- ggplot(roast_by_age_filtered, aes(x = roast_preference, y = n, fill = age)) +
  geom_col() +
  scale_fill_discrete(name = "Age") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Roast Preference", y = "Count", title = "Younger coffee drinkers prefer lighter roasts", 
       subtitle = "Consumers shy away from darker roasts, which have higher bitterness levels")

# Convert ggplot to an interactive plot
interactive_plot <- ggplotly(stacked_bar_chart)

# Sorting age and cups per day into a new dataframe
cups_by_age <- data %>%
  select(age, cups_per_day) %>%
  arrange(age) %>%
  count(age, cups_per_day)

# Define the custom order for the 'age' variable
custom_order <- c("<18 years old", "18-24 years old", "25-34 years old", "35-44 years old", 
                  "45-54 years old", "55-64 years old", ">65 years old")

# Convert 'age' to a factor with custom order
cups_by_age$age <- factor(cups_by_age$age, levels = custom_order)

# Heatmap of cups per day of coffee by age
heatmap_plot <- ggplot(cups_by_age, aes(x = age, y = cups_per_day, fill = n)) +
  geom_tile() +
  labs(title = "Younger coffee drinkers consume 1-2 cups per day", 
       subtitle = "Older consumers stick to fewer cups of coffee", 
       x = "Age", y = "Cups per Day", fill = "Count") +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
  print(heatmap_plot)

# Create a new data frame with only the relevant columns for coffee drinking locations
  location_data <- data[, c("drink_at_home", "drink_at_office", "drink_on_the_go", "drink_at_cafe")]
  
# Convert logical values to numeric (TRUE -> 1, FALSE -> 0)
  location_data_numeric <- as.data.frame(sapply(location_data, as.numeric))
  
# Melt the data frame to long format for easier plotting
  melted_data <- reshape2::melt(location_data_numeric)
  
# Filter out rows where value is 1
  melted_data <- melted_data[melted_data$value != 0, ]
  
# Rename the columns and remove NA
  melted_data$variable <- factor(melted_data$variable,
                                 levels = c("drink_at_home", "drink_at_office", 
                                            "drink_on_the_go", "drink_at_cafe"),
                                 labels = c("Drink at home", "Drink at work", 
                                            "Drink on the go", "Drink at a cafe"))
  melted_data <- na.omit(melted_data)
  
# Create a bar chart of where coffee is consumed
  bar_chart <- ggplot(melted_data, aes(x = variable, fill = factor(value))) +
    geom_bar(show.legend = FALSE) +
    labs(title = "Most consumers drink coffee at home and at work",
         subtitle = "Coffee drinkers do not consume coffee on the go as much", 
         x = "Location", y = "Respondents") +
    scale_fill_discrete() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  print(bar_chart)

# Create a new data frame with only the relevant columns for coffee brewing method at home
  brewing_data <- data[, c("po_at_home", "fp_at_home", "esp_at_home", 
                           "brewmachine_at_home", "podmachine_at_home","instant_at_home","b2c_at_home"
                           ,"cb_at_home","coffee_extract_at_home","other_at_home")]
  
# Convert logical values to numeric (TRUE -> 1, FALSE -> 0)
  brewing_data_numeric <- as.data.frame(sapply(brewing_data, as.numeric))
  
# Melt the data frame to long format for easier plotting
  melted_brewing_data <- reshape2::melt(brewing_data_numeric)
  
# Rename the columns and remove NA
  melted_brewing_data$variable <- factor(melted_brewing_data$variable,
                                  levels = c("po_at_home", "fp_at_home", "esp_at_home", 
                                            "brewmachine_at_home", "podmachine_at_home","instant_at_home","b2c_at_home"
                                            ,"cb_at_home","coffee_extract_at_home","other_at_home"),
                                 labels = c("Pour over", "French Press", "Espresso", "Brewing machine", "Pod machine",
                                            "Instant coffee","Bean to cup", "Cold brew","Coffee extract","Other"))
  melted_brewing_data <- na.omit(melted_brewing_data)
  filtered_brewing_data <- melted_brewing_data[melted_brewing_data$value == 1, ]
  
# Create a bar chart of brew methods at home
  bar_brewing_chart <- ggplot(filtered_brewing_data, aes(x = variable, fill = factor(value))) +
    geom_bar(show.legend = FALSE) +
    labs(title = "Pour over is the most popular brewing method at home",
         subtitle = "Very few prefer instant coffee, bean to cup machines and coffee extracts", 
         x = "Brewing method", y = "Respondents") +
    scale_fill_discrete() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  print(bar_brewing_chart)
  
#Analyzing where consumers buy their coffee
  purchase_data <- data[, c("buy_at_chain","buy_at_local_cafe","buy_at_drive_thru",
                            "buy_at_specialty","buy_at_supermarket","buy_other")]
  
# Convert logical values to numeric (TRUE -> 1, FALSE -> 0)
  purchase_data_numeric <- as.data.frame(sapply(purchase_data, as.numeric))
  
# Melt the data frame to long format for easier plotting
  melted_purchase_data <- reshape2::melt(purchase_data_numeric)
  
# Rename the columns and remove NA
  melted_purchase_data$variable <- factor(melted_purchase_data$variable,
                                         levels = c("buy_at_chain","buy_at_local_cafe","buy_at_drive_thru",
                                                    "buy_at_specialty","buy_at_supermarket","buy_other"),
                                         labels = c("Coffee chain","Local cafe","Drive thru","Specialty coffee shop",
                                                    "Supermarket","Other"))
  melted_purchase_data <- na.omit(melted_purchase_data)
  filtered_purchase_data <- melted_purchase_data[melted_purchase_data$value == 1, ]
  
# Calculate counts for each level of the variable
  level_counts <- table(filtered_purchase_data$variable)
  
# Sort the levels based on counts in descending order
  sorted_levels <- names(sort(level_counts, decreasing = TRUE))
  
# Create a bar chart using ggplot2 with a gradient of purple
  bar_purchase_chart <- ggplot(filtered_purchase_data, aes(x = factor(variable, levels = sorted_levels), fill = variable)) +
    geom_bar(show.legend = FALSE) +
    labs(title = "Most consumers buy at a specialty coffee shop or a local cafe",
         subtitle = "Very few purchase their coffee at a supermarket", 
         x = "Where consumers buy coffee", y = "Respondents") +
    scale_fill_manual(values = rev(purple_gradient)) +  # Reverse gradient for darkest color
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  print(bar_purchase_chart)