
# Define the libraries needed for this project
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dashboardthemes)
library(highcharter)
library(scales)
library(dplyr)

# Import the dataset to our program
car_dataset <- read.csv("Car_Dataset/data.csv")
raw_dataset <- read.csv("Car_Dataset/data.csv")

# Clean the dataset
car_dataset <- car_dataset %>%
  rename(Brands = Make,
         Driven.Wheels = Driven_Wheels,
         Vehicle.Type = Vehicle.Style,
         Highway.MpG = highway.MPG,
         City.MpG = city.mpg) %>% 
  # The numbers are conversion from Miles to Km and Gallon to Liter
  mutate(Highway.KmL = as.integer(round(Highway.MpG*(1.61/3.79))),
         City.KmL = as.integer(round(City.MpG*(1.61/3.79))),
         Engine.KW = round(Engine.HP*0.7457),
         Region = case_when(Brands == "BMW" ~ "Europe",
                            Brands == "Audi" ~ "Europe",
                            Brands == "FIAT" ~ "Europe",
                            Brands == "Mercedes-Benz" ~ "Europe",
                            Brands == "Chrysler" ~ "America",
                            Brands == "Nissan" ~ "Asia",
                            Brands == "Volvo" ~ "Europe",
                            Brands == "Mazda" ~ "Asia",
                            Brands == "Pontiac" ~ "America",
                            Brands == "Oldsmobile" ~ "America",
                            Brands == "Dodge" ~ "America",
                            Brands == "Acura" ~ "Asia",
                            Brands == "Infiniti" ~ "Asia",
                            Brands == "Mitsubishi" ~ "Asia",
                            Brands == "Porsche" ~ "Europe",
                            Brands == "Suzuki" ~ "Asia",
                            Brands == "Lamborghini" ~ "Europe",
                            Brands == "Rolls-Royce" ~ "Europe",
                            Brands == "Scion" ~ "America",
                            Brands == "Ferrari" ~ "Europe",
                            Brands == "Saab" ~ "Europe",
                            Brands == "Ford" ~ "America",
                            Brands == "Lincoln" ~ "America",
                            Brands == "Maserati" ~ "Europe",
                            Brands == "Genesis" ~ "Asia",
                            Brands == "Alfa Romeo" ~ "Europe",
                            Brands == "GMC" ~ "America",
                            Brands == "Cadillac" ~ "America",
                            Brands == "Subaru" ~ "Asia",
                            Brands == "Lexus" ~ "Asia",
                            Brands == "HUMMER" ~ "America",
                            Brands == "Toyota" ~ "Asia",
                            Brands == "Hyundai" ~ "Asia",
                            Brands == "Kia" ~ "Asia",
                            Brands == "Volkswagen" ~ "Europe",
                            Brands == "Aston Martin" ~ "Europe",
                            Brands == "Tesla" ~ "America",
                            Brands == "McLaren" ~ "Europe",
                            Brands == "Plymouth" ~ "America",
                            Brands == "Bentley" ~ "Europe",
                            Brands == "Spyker" ~ "Europe",
                            Brands == "Land Rover" ~ "Europe",
                            Brands == "Bugatti" ~ "Europe",
                            Brands == "Maybach" ~ "Europe",
                            Brands == "Honda" ~ "Asia",
                            Brands == "Chevrolet" ~ "America",
                            Brands == "Buick" ~ "America",
                            Brands == "Lotus" ~ "Europe"))