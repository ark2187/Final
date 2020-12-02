Code for final project
================
11/5/2020

## Reading in a tidying the data

Reading in the border crossing data:

  - Time information was removed since all times were 12am and thus were
    not useful
  - All day information was removed because all were on the 1st of the
    month for each month
  - Years before 2000 were removed
  - There is an Eastport (`port_name`) in Maine and Idaho. Create a
    port\_name\_state variable instead so there is no confusion
  - Kept state variable but removed port\_name– we might want to look at
    state alone
  - I removed 2019 because there is only data for Jan, Feb and March
  - A lot of values were equal to 0. I removed these rows since that
    means there were no border crossings at that time for those measure
    types.

<!-- end list -->

``` r
month_df <- tibble(month = 1:12, month_name = month.name)

border_crossing_df = read_csv(file = "./Border_Crossing_Entry_Data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = str_sub(date, end = -13), 
         location = str_replace(location, "POINT ", ""), 
         port_name_state = str_c(port_name, state, sep = "_")) %>% 
  separate(date, into = c("month", "day", "year"), sep = "/", convert = TRUE) %>% 
    left_join(month_df, by = "month") %>%
  select(-day, -port_name) %>% 
  separate(location, into = c("long", "lat"), sep = " ") %>% 
  mutate(long = str_replace(long, "\\(", ""), 
         lat = str_replace(lat, "\\)", ""), 
         long = as.numeric(long),
         lat =  as.numeric(lat), 
         vehicle = if_else(str_detect(measure, "Passengers") |
                             measure == "Pedestrians",
                           "Non-Vehicles", "Vehicles")) %>% 
  filter(year >= 2000, year < 2019, value != 0)
```

    ## Parsed with column specification:
    ## cols(
    ##   `Port Name` = col_character(),
    ##   State = col_character(),
    ##   `Port Code` = col_double(),
    ##   Border = col_character(),
    ##   Date = col_character(),
    ##   Measure = col_character(),
    ##   Value = col_double(),
    ##   Location = col_character()
    ## )

``` r
saveRDS(border_crossing_df, file = "border_crossing_df_clean.rds")
```