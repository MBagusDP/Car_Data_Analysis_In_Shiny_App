
shinyServer(function(input, output, session){
  
  # Store data from actionButton ###############################################################################
  counter <- reactiveValues(countervalue = 0)
  counter2 <- reactiveValues(button_view_1 = 0)
  counter3 <- reactiveValues(button_view_2 = 0)
  counter4 <- reactiveValues(sort_button = 0)
  counter5 <- reactiveValues(sort_button_2 = 0)
  counter6 <- reactiveValues(sort_button_3 = 0)
  counter7 <- reactiveValues(button_view_3 = 0)
  counter8 <- reactiveValues(sort_button_4 = 0)
  counter9 <- reactiveValues(sort_button_5 = 0)
  counter10 <- reactiveValues(switch_button = 0)
  counter11 <- reactiveValues(sort_button_6 = 0)
  counter12 <- reactiveValues(view_more_value = 0)
  counter13 <- reactiveValues(arr_value = 0)
  
  
  
  # Store data from switchInput ################################################################################
  switch <- reactiveValues(switch_asia = "")
  switch2 <- reactiveValues(switch_europe = "")
  switch3 <- reactiveValues(switch_usa = "")
  
  
  
  # Update data from dropdown inputs when submit is pressed ####################################################
  update <- reactiveValues(update_brand = NULL)
  update2 <- reactiveValues(update_category = NULL)
  update3 <- reactiveValues(update_model = NULL)
  update4 <- reactiveValues(update_year = NULL)
  update5 <- reactiveValues(update_HP = NULL)
  result <- reactiveValues(result_value = NULL)
  
  
  
  # Data tables and logos ######################################################################################
  output$dataframe <- renderDataTable({
    
    # Display clean data table
    car_dataset %>% 
      select(input$show_vars) %>% 
      datatable(class = "cell-border stripe",
                option = list(scrollX = T,
                              pageLength = 5,
                              lengthMenu = list(c(5, 10), c("5", "10"))))
    
  })
  
  output$dataframe_raw <- renderDataTable({
    
    # Display raw data table
    raw_dataset %>% 
      datatable(class = "cell-border stripe",
                option = list(scrollX = T,
                              lengthMenu = list(c(5, 10, 25, 50, -1), 
                                                c("5", "10", "25", "50", "All"))))
    
  })
  
  output$spec_text <- renderDataTable({
    
    # Create a condition
    if (is.null(update$update_brand) == T &
        is.null(update2$update_categoty) == T &
        is.null(update3$update_model) == T &
        is.null(update4$update_year) == T &
        is.null(update5$update_hp) == T) {
      
      brand = "Suzuki"
      category = "N/A"
      model = "Aerio"
      year_select = c(1990, 2017)
      hp = c(55, 1001)
      
    }
    else {
      
      if (is.null(update$update_brand) == T) {
        brand = "Suzuki"
      }
      else {
        brand = update$update_brand
      }
      
      if (is.null(update2$update_category) == T) {
        category = "N/A"
      }
      else {
        category = update2$update_category
      }
      
      if (is.null(update3$update_model) == T) {
        model = "Aerio"
      }
      else {
        model = update3$update_model
      }
      
      if (is.null(update4$update_year) == T) {
        year_select = c(1990, 2017)
      }
      else {
        year_select = update4$update_year
      }
      
      if (is.null(update5$update_hp) == T) {
        hp = "Aerio"
      }
      else {
        hp = update5$update_hp
      }
      
    }
    
    # Subset data base on conditions
    df_model <- car_dataset %>% 
      mutate(MSRP = number(MSRP, prefix = "$", big.mark = ",", accuracy = 1)) %>% 
      relocate(Engine.KW, .after = Engine.HP) %>%
      relocate(Highway.KmL, .after = Highway.MpG) %>%
      relocate(City.KmL, .after = City.MpG) %>%
      filter(Brands == brand,
             Model == model,
             Year >= year_select[1],
             Year <= year_select[2],
             Engine.HP >= hp[1],
             Engine.HP <= hp[2])
    
    # Subset data table with a condition (imperial or metric)
    if (counter10$switch_button == 0) {
      df_model <- df_model %>%
        select(-c(Brands, Engine.HP, Highway.MpG, City.MpG, Popularity, Region)) %>% 
        datatable(class = "cell-border stripe",
                  option = list(scrollX = T,
                                pageLength = 5,
                                lengthMenu = list(c(5, 10, -1), c("5", "10", "All"))),
                  colnames = c("Fuel Type" = "Engine.Fuel.Type",
                               "Number of Cylinders" = "Engine.Cylinders",
                               "Tranmission" = "Transmission.Type",
                               "Driven Wheels" = "Driven.Wheels",
                               "Doors" = "Number.of.Doors",
                               "Market Category" = "Market.Category",
                               "Size" = "Vehicle.Size",
                               "Type" = "Vehicle.Type",
                               "Manufacturer's Suggested Retail Price" = "MSRP",
                               "Highway (Km/L)" = "Highway.KmL",
                               "City (Km/L)" = "City.KmL",
                               "Engine Power (KW)" = "Engine.KW"))
    }
    else {
      df_model <- df_model %>%
        select(-c(Brands, Engine.KW, Highway.KmL, City.KmL, Popularity, Region)) %>% 
        datatable(class = "cell-border stripe",
                  option = list(scrollX = T,
                                pageLength = 5,
                                lengthMenu = list(c(5, 10, -1), c("5", "10", "All"))),
                  colnames = c("Fuel Type" = "Engine.Fuel.Type",
                               "Engine Power (HP)" = "Engine.HP",
                               "Number of Cylinders" = "Engine.Cylinders",
                               "Tranmission" = "Transmission.Type",
                               "Driven Wheels" = "Driven.Wheels",
                               "Doors" = "Number.of.Doors",
                               "Market Category" = "Market.Category",
                               "Size" = "Vehicle.Size",
                               "Type" = "Vehicle.Type",
                               "Highway (MpG)" = "Highway.MpG",
                               "City (MpG)" = "City.MpG",
                               "Manufacturer's Suggested Retail Price" = "MSRP"))
    }
    
    # Display data table
    df_model
    
  })
  
  output$image_logo <- renderUI({
    
    # Create a condition
    if (is.null(update$update_brand) == T) {
      
      img(src = "Suzuki.png", height = "100%", width = "100%", align = "center")
      
    }
    else {
      
      # Display image of a car brand logo when a brand name is selected
      img(src = gsub(" ", "", paste(update$update_brand, ".png")), 
          height = "100%", 
          width = "100%", 
          align = "right")
      
    }
    
  })
  
  
  
  # Infoboxes ##################################################################################################
  output$top_american_brand <- renderUI({
    
    # Display best American brand
    cd_top_america <- car_dataset %>% 
      filter(Region == "America") %>% 
      group_by(Brands) %>% 
      summarise(Freq = n()) %>% 
      ungroup() %>% 
      arrange(desc(Freq)) %>% 
      head(1)
    
    cd_top_america$Brands
    
  })
  
  output$top_american_freq <- renderUI({
    
    # Display the distribution percentage of the best American brand
    cd_top_america <- car_dataset %>%
      filter(Region == "America") %>%
      group_by(Brands) %>%
      summarise(Freq = (n()/nrow(car_dataset))*100) %>%
      ungroup() %>%
      arrange(desc(Freq)) %>%
      mutate(Freq = number(Freq, accuracy = 0.01,  suffix = "%")) %>%
      head(1)

    paste(cd_top_america$Freq, "Filled the market")
    
  })
  
  output$top_europe_brand <- renderUI({
    
    # Display best Europe brand
    cd_top_europe <- car_dataset %>% 
      filter(Region == "Europe") %>% 
      group_by(Brands) %>% 
      summarise(Freq = n()) %>% 
      ungroup() %>% 
      arrange(desc(Freq)) %>% 
      head(1)
    
    cd_top_europe$Brands
    
  })
  
  output$top_europe_freq <- renderUI({
    
    # Display the distribution percentage of the best Europe brand
    cd_top_europe<- car_dataset %>% 
      filter(Region == "Europe") %>% 
      group_by(Brands) %>% 
      summarise(Freq = (n()/nrow(car_dataset))*100) %>% 
      ungroup() %>% 
      arrange(desc(Freq)) %>% 
      mutate(Freq = number(Freq, accuracy = 0.01,  suffix = "%")) %>%
      head(1)
    
    paste(cd_top_europe$Freq, "Filled the market")
    
  })
  
  output$top_asia_brand <- renderUI({
    
    # Display best Asia brand
    cd_top_asia <- car_dataset %>% 
      filter(Region == "Asia") %>% 
      group_by(Brands) %>% 
      summarise(Freq = n()) %>% 
      ungroup() %>% 
      arrange(desc(Freq)) %>% 
      head(1)
    
    cd_top_asia$Brands
    
  })
  
  output$top_asia_freq <- renderUI({
    
    # Display the distribution percentage of the best Asia brand
    cd_top_asia <- car_dataset %>% 
      filter(Region == "Asia") %>% 
      group_by(Brands) %>% 
      summarise(Freq = (n()/nrow(car_dataset))*100) %>% 
      ungroup() %>% 
      arrange(desc(Freq)) %>% 
      mutate(Freq = number(Freq, accuracy = 0.01,  suffix = "%")) %>%
      head(1)
    
    paste(cd_top_asia$Freq, "Filled the market")
    
  })
  
  output$region_selected1 <- renderUI({
    
    # Display Region name
    DF1 <- car_dataset %>% 
      filter(Region == input$select_trend_1) 
    
    unique(DF1$Region)
    
  })
  
  output$price_region1 <- renderUI({
    
    # Display region average price
    DF3 <- car_dataset %>% 
      filter(Region == input$select_trend_1) %>% 
      mutate(MSRP = mean(MSRP),
             MSRP = number(MSRP, prefix = "$", big.mark = ",", accuracy = 1))
    
    unique(DF3$MSRP)
    
  })
  
  output$region_selected2 <- renderUI({
    
    # Display Region name
    DF2 <- car_dataset %>% 
      filter(Region == input$select_trend_2)
    
    unique(DF2$Region)
    
  })
  
  output$price_region2 <- renderUI({
    
    # Display region average price
    DF4 <- car_dataset %>% 
      filter(Region == input$select_trend_2) %>% 
      mutate(MSRP = mean(MSRP),
             MSRP = number(MSRP, prefix = "$", big.mark = ",", accuracy = 1))
    
    unique(DF4$MSRP)
    
  })
  
  
  
  # Select, Slider, and Button inputs ##########################################################################
  output$trend_select_1 <- renderUI({
  
    # Select region for trend chart
    selectInput(
      inputId = "select_trend_1",
      label = "Select Region",
      choices = unique(car_dataset$Region),
      selected = "Asia")
      
      
  })
  
  output$trend_select_2 <- renderUI({
    
    # Filter region which has been selected by trend_select_1
    df_trnd_2 <- car_dataset %>% 
      filter(!Region %in% input$select_trend_1)
    
    # Select region for trend chart
    selectInput(
      inputId = "select_trend_2",
      label = NULL,
      choices = unique(df_trnd_2$Region),
      selected = "America")
    
  })
  
  output$switches <- renderUI({
    
    # Switch input to select region
    if (input$switch_asia == F & input$switch_europe == F & input$switch_usa == F) {
      
      switch3$switch_usa = "America"
      switch2$switch_europe = "Europe"
      switch$switch_asia = "Asia"
      
    }
    else {
      
      if (input$switch_asia == T) {
        switch$switch_asia = "Asia"
      }
      else {
        switch$switch_asia = ""
      }
      
      if (input$switch_europe == T) {
        switch2$switch_europe = "Europe"
      }
      else {
        switch2$switch_europe = ""
      }
      
      if (input$switch_usa == T) {
        switch3$switch_usa = "America"
      }
      else {
        switch3$switch_usa = ""
      }
      
    }
    
    # Silder year which were dependent on region switch inputs
    uiOutput("slider_year")
    
  })
  
  output$slider_year <- renderUI({
    
    # Create condition when switch region is selected
    if (switch$switch_asia == "" & switch2$switch_europe == "" & switch3$switch_usa == "") {
      
      df_yr <- car_dataset
      
    }
    else {
      
      df_yr <- car_dataset %>% 
        filter(Region %in% c(switch$switch_asia, 
                             switch2$switch_europe, 
                             switch3$switch_usa))
      
    }
    
    
    column(width = 12,
           
           # Slider input to select the range of year for car brands
           sliderInput(inputId = "date_range",
                       label = "Select Year Range",
                       value = range(df_yr$Year),
                       min = min(unique(df_yr$Year)),
                       max = max(unique(df_yr$Year)),
                       step = 1,
                       sep = ""))
    
  })
  
  output$brands_spec_out <- renderUI({
    
    # Subset data to create specific selection based on switches and slider_year
    df_br_spec <- car_dataset %>% 
      filter(Region %in% c(switch$switch_asia, 
                           switch2$switch_europe, 
                           switch3$switch_usa),
             Year >= input$date_range[1],
             Year <= input$date_range[2])
    
    # Select input to select desired brand
    selectInput(inputId = "brands_spec",
                label = "Selectable Brand",
                choices = unique(df_br_spec$Brands),
                selected = "Suzuki")
    
  })
  
  output$categories <- renderUI({
    
    # Subset input to create specific selection based on brands_spec_out and slider_year
    df_cat <- car_dataset %>% 
      filter(Year >= input$date_range[1],
             Year <= input$date_range[2],
             Brands == input$brands_spec)
    
    # Select iput to select desired market category
    selectInput(inputId = "pick_category",
                label = "Choose Category",
                choices = unique(df_cat$Market.Category),
                selected = "N/A")


    
  })
  
  output$model_spec_out <- renderUI({
    
    # Subset input to create specific selection based on brands_spec_out, categories, and slider_year
    df_mod <- car_dataset %>% 
      filter(Brands == input$brands_spec,
             Market.Category == input$pick_category,
             Engine.HP >= input$horsepower[1],
             Engine.HP <= input$horsepower[2])
    
    # Select input to select desired model
    selectInput(inputId = "model_spec",
                label = "Selectable Model",
                choices = unique(df_mod$Model))
    
  })
  
  output$actbtn_submit <- renderUI({
    
    # Submit button to update conditions
    actionBttn(inputId = "submit",
               label = "Submit",
               style = "simple",
               color = "primary",
               size = "sm")
    
  })
  
  output$text_change_chart <- renderUI({
    
    # Text info
    p(strong("Switch View"))
    
  })
  
  output$change_chart <- renderUI({
    
    # Button to switch between 2 different displayed charts
    actionBttn(inputId = "button_switch_view_1",
               icon = icon("object-ungroup"),
               style = "fill",
               color = "primary",
               size = "lg")
    
  })
  
  output$slider_HP <- renderUI({
    
    df_hp <- car_dataset %>%
      filter(!is.na(Engine.HP),
             Brands == input$brands_spec,
             Market.Category == input$pick_category)
    
    # Slider input engine power
    sliderInput(inputId = "horsepower",
                label = "Selectable Engine Horsepower",
                value = c(min(unique(df_hp$Engine.HP)),
                          max(unique(df_hp$Engine.HP))),
                min = min(unique(df_hp$Engine.HP)),
                max = max(unique(df_hp$Engine.HP)),
                step = 1,
                sep = "")
    
  })
  
  output$text_change_chart_2 <- renderUI({
    
    # Text info
    p(strong("Switch View"))
    
  })
  
  output$change_chart_2 <- renderUI({
    
    # Button to switch between 2 different displayed charts
    actionBttn(inputId = "button_switch_view_2",
               icon = icon("object-ungroup"),
               style = "fill",
               color = "primary",
               size = "lg")
    
  })
  
  output$year_range <- renderUI({
    
    if (counter2$button_view_1 == 1) {
      
      # Slider input year
      sliderInput(inputId = "slider_year_spec",
                  label = "Select Year Range",
                  value = c(2011, 2014),
                  min = min(unique(car_dataset$Year)),
                  max = max(unique(car_dataset$Year)),
                  step = 1,
                  sep = "")
      
    }
    
  })
  
  output$text_change_chart_3 <- renderUI({
    
    # Text info
    p(strong("Switch View"))
    
  })
  
  output$change_chart_3 <- renderUI({
    
    # Button to switch between 2 different displayed charts
    actionBttn(inputId = "button_switch_view_3",
               icon = icon("object-ungroup"),
               style = "fill",
               color = "primary",
               size = "lg")
    
  })
  
  output$text_actbtn_sort <- renderUI({
    
    # Text info
    if(counter3$button_view_2 == 0) {
      
      p(strong("Rearange Chart"))
      
    }
    
  })
  
  output$actbtn_sort<- renderUI({
    
    if(counter3$button_view_2 == 0) {
      
      # Rearrange chart view (unarranged, descending, ascending)
      actionBttn(inputId = "sort_button",
                 icon = icon("layer-group"),
                 style = "simple",
                 color = "primary",
                 size = "lg")
      
    }
    
  })
  
  output$text_actbtn_sort_2 <- renderUI({
    
    # Text info
    if(counter3$button_view_2 == 1) {
      
      p(strong("Rearange Chart"))
      
    }
    
  })
  
  output$actbtn_sort_2<- renderUI({
    
    if(counter3$button_view_2 == 1) {
      
      # Rearrange chart view (unarranged, descending, ascending)
      actionBttn(inputId = "sort_button_2",
                 icon = icon("layer-group"),
                 style = "simple",
                 color = "primary",
                 size = "lg")
      
    }
    
  })
  
  output$text_actbtn_sort_3 <- renderUI({
    
    # Text info
    if(counter3$button_view_2 == 2) {
      
      p(strong("Rearange Chart"))
      
    }
    
  })
  
  output$actbtn_sort_3<- renderUI({
    
    if(counter3$button_view_2 == 2) {
      
      # Rearrange chart view (unarranged, descending, ascending)
      actionBttn(inputId = "sort_button_3",
                 icon = icon("layer-group"),
                 style = "simple",
                 color = "primary",
                 size = "lg")
      
    }
    
  })
  
  output$text_actbtn_sort_4 <- renderUI({
    
    # Text info
    if(counter7$button_view_3 == 1) {
      
      p(strong("Metric System / Imperial System"))
      
    }
    
  })
  
  output$actbtn_sort_4<- renderUI({
    
    if(counter7$button_view_3 == 1) {
      
      # Switch between metric and imperial unit
      actionBttn(inputId = "sort_button_4",
                 icon = icon("balance-scale"),
                 style = "simple",
                 color = "primary",
                 size = "lg")
      
    }
    
  })
  
  output$text_actbtn_sort_5 <- renderUI({
    
    # Text info
    if(counter7$button_view_3 == 0) {
      
      p(strong("Metric System / Imperial System"))
      
    }
    
  })
  
  output$actbtn_sort_5 <- renderUI({
    
    if(counter7$button_view_3 == 0) {
      
      # Switch between metric and imperial unit
      actionBttn(inputId = "sort_button_5",
                 icon = icon("balance-scale"),
                 style = "simple",
                 color = "primary",
                 size = "lg")
      
    }
    
  })
  
  output$switch_unit <- renderUI({
    
    # Button to change between metric and imperial unit
    actionBttn(inputId = "button_switch_unit",
               icon = icon("balance-scale"),
               style = "fill",
               color = "primary",
               size = "lg",
               block = T)
    
  })
  
  output$text_actbtn_sort_6 <- renderUI({
    
    # Text info
    if(counter2$button_view_1 == 0) {
      
      p(strong("Rearange Chart"))
      
    }
    
  })
  
  output$actbtn_sort_6 <- renderUI({
    
    if(counter2$button_view_1 == 0) {
      
      # Rearrange chart view (unarranged, descending, ascending)
      actionBttn(inputId = "sort_button_6",
                 icon = icon("layer-group"),
                 style = "simple",
                 color = "primary",
                 size = "lg")
      
    }
    
  })
  
  output$view_more <- renderUI({
    
    if (counter12$view_more_value == 0) {
      
      actionBttn(inputId = "button_detail_down",
                 label = "View More Details",
                 icon = icon("chevron-down"),
                 color = "primary",
                 style = "material-circle",
                 size = "sm")
      
    }
    else {
      
      actionBttn(inputId = "button_detail_down",
                 label = "View More Details",
                 icon = icon("chevron-up"),
                 color = "primary",
                 style = "material-circle",
                 size = "sm")
      
    }
    
  })
  
  output$text_arrange_button_details <- renderUI({
    
    # Text info
    if(counter12$view_more_value == 1) {
      
      p("Rearange Chart")
      
    }
    
  })
  
  output$arrange_button_details <- renderUI({
    
    # when  view_more button is pressed
    if (counter12$view_more_value == 1) {
      
      # Rearrange chart view (unarranged, descending, ascending)
      actionBttn(inputId = "arrange_details",
                 icon = icon("layer-group"),
                 style = "simple",
                 color = "primary",
                 size = "lg")
      
    }
    
  })
  
  
  
  # Detect a button press and do something #####################################################################
  observeEvent(input$plot_button, {

    # show different chart when value is 0 or 1
    counter$countervalue <- counter$countervalue + 1

    if (counter$countervalue > 1) {
      counter$countervalue = 0
    }
    
  })
  
  observeEvent(input$submit, {
    
    # Detect submit button click and close dropdown button
    session$sendCustomMessage("close_drop_down_list", "")

  })
  
  observeEvent(input$button_switch_view_1, {
    
    # show different chart when value is 0 or 1
    counter2$button_view_1 <- counter2$button_view_1 + 1
    
    if (counter2$button_view_1 > 1) {
      counter2$button_view_1 = 0
    }
    
  })
  
  observeEvent(input$button_switch_view_2, {
    
    # show different chart when value is 0, 1, and 2
    counter3$button_view_2 <- counter3$button_view_2 + 1
    
    if (counter3$button_view_2 > 2) {
      counter3$button_view_2 = 0
    }
    
  })
  
  observeEvent(input$button_switch_view_3, {
    
    # show different chart when value is 0 or 1
    counter7$button_view_3 <- counter7$button_view_3 + 1
    
    if (counter7$button_view_3 > 1) {
      counter7$button_view_3 = 0
    }
    
  })
  
  observeEvent(input$sort_button, {
    
    # sort chart when value is 0 (unsorted), 1 (descending), or 2 (ascending)
    counter4$sort_button <- counter4$sort_button + 1
    
    if (counter4$sort_button > 2) {
      counter4$sort_button = 0
    }
    
  })
  
  observeEvent(input$sort_button_2, {
    
    # sort chart when value is 0 (unsorted), 1 (descending), or 2 (ascending)
    counter5$sort_button_2 <- counter5$sort_button_2 + 1
    
    if (counter5$sort_button_2 > 2) {
      counter5$sort_button_2 = 0
    }
    
  })
  
  observeEvent(input$sort_button_3, {
    
    # sort chart when value is 0 (unsorted), 1 (descending), or 2 (ascending)
    counter6$sort_button_3 <- counter6$sort_button_3 + 1
    
    if (counter6$sort_button_3 > 1) {
      counter6$sort_button_3 = 0
    }
    
  })
  
  observeEvent(input$sort_button_4, {
    
    # Switch between metric (0) and imperial (1)
    counter8$sort_button_4 <- counter8$sort_button_4 + 1
    
    if (counter8$sort_button_4 > 1) {
      counter8$sort_button_4 = 0
    }
    
  })
  
  observeEvent(input$sort_button_5, {
    
    # Switch between metric (0) and imperial (1)
    counter9$sort_button_5 <- counter9$sort_button_5 + 1
    
    if (counter9$sort_button_5 > 1) {
      counter9$sort_button_5 = 0
    }
    
  })
  
  observeEvent(input$sort_button_6, {
    
    # sort chart when value is 0 (unsorted), 1 (descending), or 2 (ascending)
    counter11$sort_button_6 <- counter11$sort_button_6 + 1
    
    if (counter11$sort_button_6 > 2) {
      counter11$sort_button_6 = 0
    }
    
  })
  
  observeEvent(input$submit, {
    
    # Update value from inputs inside dropdown button
    update$update_brand = input$brands_spec
    update2update_category = input$pick_category
    update3$update_model = input$model_spec
    update4$update_year = input$date_range
    update5$update_hp = input$horsepower
    
  })
  
  observeEvent(input$submit, {

    # Send error message when data table shows zero result
    if (is.null(update$update_brand) == T &
        is.null(update2$update_categoty) == T &
        is.null(update3$update_model) == T &
        is.null(update4$update_year) == T &
        is.null(update5$update_hp) == T) {

      brand = "Suzuki"
      category = "N/A"
      model = "Aerio"
      year_select = c(1990, 2017)
      hp = c(55, 1001)

    }
    else {

      if (is.null(update$update_brand) == T) {
        brand = "Suzuki"
      }
      else {
        brand = update$update_brand
      }

      if (is.null(update2$update_category) == T) {
        category = "N/A"
      }
      else {
        category = update2$update_category
      }

      if (is.null(update3$update_model) == T) {
        model = "Aerio"
      }
      else {
        model = update3$update_model
      }

      if (is.null(update4$update_year) == T) {
        year_select = c(1990, 2017)
      }
      else {
        year_select = update4$update_year
      }

      if (is.null(update5$update_hp) == T) {
        hp = "Aerio"
      }
      else {
        hp = update5$update_hp
      }

    }

    df_car <- car_dataset %>% filter(
      Brands == brand,
      Model == model,
      Year >= year_select[1],
      Year <= year_select[2],
      Engine.HP >= hp[1],
      Engine.HP <= hp[2])

    if (nrow(df_car) < 1) {
      
      # Error message
      sendSweetAlert(
            session = session,
            title = "Warning !!",
            text = "Selected Category returns 0 result! Please try again!",
            type = "error"
          )
      
    }

  })
  
  observeEvent(input$button_switch_unit, {
    
    # metric (0), imperial (1)
    counter10$switch_button <- counter10$switch_button + 1
    
    if (counter10$switch_button > 1) {
      counter10$switch_button = 0
    }
    
  })
  
  observeEvent(input$button_detail_down, {
    
    # Values for displaying the detailed infomation
    counter12$view_more_value <- counter12$view_more_value + 1
    
    if (counter12$view_more_value > 1) {
      counter12$view_more_value = 0
    }
    
  })
  
  observeEvent(input$arrange_details, {
    
    # Values to rearrage chart in detailed page
    counter13$arr_value <- counter13$arr_value + 1
    
    if (counter13$arr_value > 2) {
      counter13$arr_value = 0
    }
    
  })
  
  
  
  # Data Visualization using Highcharter #######################################################################
  output$top_charts <- renderHighchart({

    if (counter$countervalue == 0) {
      
      # Most sold car brands bar chart
      car_dataset %>% 
        group_by(Brands) %>%
        summarise(Frequency = n(),
                  MSRP = mean(MSRP)) %>%
        ungroup() %>%
        mutate(MSRP = number(MSRP, prefix = "$", big.mark = ",")) %>%
        arrange(desc(Frequency)) %>% 
        head(input$slider_top1) %>% 
        hchart("bar", 
               hcaes(Brands, Frequency),
               pointWidth = 35 - input$slider_top1,
               tooltip = list(pointFormat = "Average Price: {point.MSRP}",
                              headerFormat = "")) %>%
        hc_colorAxis(minColor = "darkred",
                     maxColor = "gold") %>%
        hc_xAxis(title = list(text = "Brands"),
                 categories = hcaes(Brands),
                 crosshairs = T) %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
    else {
      
      # Most sold car brands treemap
      car_dataset %>%
        group_by(Brands) %>%
        summarise(Freq = n(),
                  MSRP = mean(MSRP)) %>%
        ungroup() %>%
        arrange(desc(Freq)) %>%
        mutate(MSRP = number(MSRP, prefix = "$", big.mark = ",")) %>%
        head(input$slider_top1) %>%
        hchart("treemap", hcaes(x = Brands,
                                value = Freq, color = Freq),
               tooltip = list(pointFormat = "Average Price: {point.MSRP}")) %>%
        hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$popular_charts <- renderHighchart({
    
    if (counter$countervalue == 0) {
      
      # Most popular car brands bar plot
      car_dataset %>% 
        group_by(Brands, Popularity) %>%
        summarise(Frequency = n(),
                  MSRP = mean(MSRP)) %>%
        ungroup() %>%
        mutate(MSRP = number(MSRP, prefix = "$", big.mark = ",")) %>%
        arrange(desc(Popularity)) %>%
        head(input$slider_top2) %>% 
        hchart("bar", 
               hcaes(Brands, Popularity),
               pointWidth = 35 - input$slider_top2,
               tooltip = list(pointFormat = "Average Price: {point.MSRP}",
                              headerFormat = "")) %>%
        hc_colorAxis(minColor = "darkred",
                     maxColor = "gold") %>%
        hc_xAxis(title = list(text = "Brands"),
                 categories = hcaes(Brands),
                 crosshairs = T) %>%
        hc_yAxis(title = list(text = "Popularity")) %>%
        hc_legend(enabled = F) %>% 
        hc_tooltip(crosshairs = T) %>%
        hc_add_theme(hc_theme_google())
      
    }
    
    else {
      
      # Most popular car brands treemap
      car_dataset %>%
        group_by(Brands, Popularity) %>% 
        summarise(Freq = n(),
                  MSRP = mean(MSRP)) %>% 
        ungroup() %>% 
        arrange(desc(Popularity)) %>% 
        mutate(MSRP = number(MSRP, prefix = "$", big.mark = ",")) %>% 
        head(input$slider_top2) %>% 
        hchart("treemap", hcaes(x = Brands, 
                                value = Freq, color = Freq),
               tooltip = list(pointFormat = "Average Price: {point.MSRP}")) %>% 
        hc_colorAxis(stops = color_stops(colors = viridis::inferno(input$slider_top2))) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$price_growth <- renderHighchart({
    
    # Total average manufactured car prices per year
    plot <- car_dataset %>%
      group_by(Year) %>%
      summarise(MSRP = round(mean(MSRP), digit = 2)) %>%
      ungroup() %>%
      mutate(MSRP_Text = number(MSRP, accuracy = 1, prefix = "$", big.mark = ","),
             special_note = case_when(Year == 2001 ~ "Ferrari, Lamboghini, Bentley, and Rolls-Royce \n start to produce their car in America",
                                      Year == 2008 ~ "Bugatti begin production in America",
                                      Year == 2013 ~ "Maybach stop their production in America",
                                      Year == 2015 ~ "Lotus stop their production in America for 2 year \n until they begin to produce their car again in 2017 \n and FIAT just started their production with the \n overall average price of $21,711 which drags \n down the total average price",
                                      T ~ ""))
    
    # Total average manufactured car prices per year by region
    plot_region <- car_dataset %>% 
      filter(Region == input$select_trend_1) %>% 
      group_by(Year, Region) %>% 
      summarise(MSRP = round(mean(MSRP), digit = 2)) %>% 
      ungroup() %>% 
      mutate(MSRP_Text = number(MSRP, accuracy = 1, prefix = "$", big.mark = ","))
    
    # Total average manufactured car prices per year by region
    plot_region_2 <- car_dataset %>% 
      filter(Region == input$select_trend_2) %>% 
      group_by(Year, Region) %>% 
      summarise(MSRP = round(mean(MSRP), digit = 2)) %>% 
      ungroup() %>% 
      mutate(MSRP_Text = number(MSRP, accuracy = 1, prefix = "$", big.mark = ","))
    
    
    # Plot highchart line
    hchart(plot, "line", hcaes(Year, MSRP),
           color = "darkgreen",
           tooltip = list(pointFormat = "<span style='color:{point.color}; font-size: 20px'>\u25CF </span><b>Average Total Car Trend<b/>
                                         <br>Average Price: <b>{point.MSRP_Text}<b/><br/>
                                         <br><i>{point.special_note}<i/><br/>",
                          headerFormat = "")) %>% 
      hc_add_series(plot_region, "line", hcaes(Year, MSRP),
                    color = "darkred",
                    marker = list(symbol = "triangle"),
                    dashStyle = "shortDot",
                    tooltip = list(pointFormat = '<span style="color:{point.color}; font-size: 13px">\u25B2 </span> <b>{point.Region}<b/><br/>
                                                  <br>Average Price: <b>{point.MSRP_Text}<b/><br/>',
                                   headerFormat = "")) %>% 
      hc_add_series(plot_region_2, "line", hcaes(Year, MSRP),
                    color = "midnightblue",
                    marker = list(symbol = "square"),
                    dashStyle = "shortDot",
                    tooltip = list(pointFormat = '<span style="color:{point.color}; font-size: 18px">\u25A0	</span> <b>{point.Region}<b/><br/>
                                                  <br>Average Price: <b>{point.MSRP_Text}<b/><br/>',
                                   headerFormat = "")) %>%
      hc_xAxis(categories = hcaes(Year)) %>%
      hc_yAxis(crosshairs = T,
               title = list(text = "Manufacturer's Suggested Retail Price"),
               labels = list(format = "${value:,.0f}")) %>% 
      hc_tooltip(shared = T) %>% 
      hc_legend(enabled = F) %>% 
      hc_add_theme(hc_theme_google())
    
  })
  
  output$distribution_each_region <- renderHighchart({
    
    # Price distriobution of selected region
    plot1 <- car_dataset %>%
      filter(Region == input$select_trend_1)
    
    # Price distriobution of selected region
    plot2 <- car_dataset %>%
      filter(Region == input$select_trend_2)
    
    # Plot density highchart
    highchart() %>%
      hc_add_series(density(plot1$MSRP),
                    type = "area",
                    color = "#ECAD09",
                    name = unique(plot1$Region)) %>%
      hc_add_series(density(plot2$MSRP),
                    type = "area",
                    color = "#98E2F7",
                    name = unique(plot2$Region)) %>%
      hc_xAxis(crosshair = T,
               title = list(text = "Manufacturer's Suggested Retail Price"),
               max = 250000) %>%
      hc_yAxis(title = list(text = "Distribution")) %>% 
      hc_tooltip(pointFormat = "Distribution: <b>({point.y:.6f}) * 100%<b/>
                                <br>Average price: <b>${point.x:,.0f}<b/><br/>",
                 headerFormat = "") %>% 
      hc_plotOptions(series = list(marker = F)) %>% 
      hc_legend(enabled = F) %>% 
      hc_add_theme(hc_theme_google())
    
  })
  
  output$detail_outputs <- renderUI({
    
    if (counter12$view_more_value == 1) {
      
      br()
      box(width = 14,
        
        fluidRow(
          
          column(width = 12, align = "right", uiOutput("text_arrange_button_details")),
          
          column(width = 12, align = "right", uiOutput("arrange_button_details")),
          
          column(width = 12, highchartOutput("cat_details")),
          
          column(width = 6, highchartOutput("vec_type_details")),
          
          column(width = 6, highchartOutput("fuel_details")),
          
          column(width = 12, highchartOutput("power_details")),
          
          column(width = 4, highchartOutput("gear_details")),
          
          column(width = 4, highchartOutput("cyl_details")),
          
          column(width = 4, highchartOutput("wheel_details"))
          
        )
        
      )
      
    }
    
  })
  
  output$cat_details <- renderHighchart({
    
    if (counter12$view_more_value == 1) {
      
      # Impact of the category to the car price
      plot <- car_dataset %>%
        filter(Market.Category != "N/A") %>%
        group_by(Market.Category) %>%
        summarise(MSRP = round(mean(MSRP), digit = 3),
                  Freq = n()) %>%
        ungroup()
      
      if (counter13$arr_value == 1) {
        plot <- plot %>% 
          arrange(desc(MSRP))
      }
      else if(counter13$arr_value == 2) {
        plot <- plot %>% 
          arrange(MSRP)
      }
      
      plot %>% 
        hchart("lollipop", hcaes(Market.Category, MSRP, color = MSRP),
               tooltip = list(pointFormat = "Price: $<b>{point.y:,.0f}</b>")) %>%
        hc_xAxis(title = list(text = "Market Category")) %>%
        hc_yAxis(labels = list(format = "${value:,.0f}"),
                 title = list(text = "Manufacturer's Suggested Retail Price")) %>%
        hc_title(text = "Impact of The Market Category to The Car Price") %>% 
        hc_legend(enabled = F) %>%
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$vec_type_details <- renderHighchart({
    
    if (counter12$view_more_value == 1) {
      
      # Impact of the vehicle type to the car price
      plot <- car_dataset %>% 
        filter(Market.Category != "N/A") %>% 
        group_by(Vehicle.Type) %>% 
        summarise(MSRP = round(mean(MSRP), digit = 3), 
                  Freq = n()) %>% 
        ungroup()
      
      if (counter13$arr_value == 1) {
        plot <- plot %>% 
          arrange(desc(MSRP))
      }
      else if(counter13$arr_value == 2) {
        plot <- plot %>% 
          arrange(MSRP)
      }
      
      plot %>% 
        hchart("lollipop", hcaes(Vehicle.Type, MSRP, color = MSRP),
               tooltip = list(pointFormat = "Price: $<b>{point.y:,.0f}</b>",
                              headerFormat = "Number of Cylinder: <b>{point.x}</b><br>")) %>% 
        hc_xAxis(title = list(text = "Transmission Type")) %>%
        hc_yAxis(labels = list(format = "${value:,.0f}"),
                 title = list(text = "Manufacturer's Suggested Retail Price")) %>% 
        hc_title(text = "Impact of The Vehichle Type to the Car Price") %>% 
        hc_plotOptions(lollipop = list(marker = list(radius = 7))) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$fuel_details <- renderHighchart({
    
    if (counter12$view_more_value == 1) {
      
      # Impact of engine type to the car price
      plot <- car_dataset %>% 
        filter(Market.Category != "N/A") %>% 
        group_by(Engine.Fuel.Type) %>% 
        summarise(MSRP = round(mean(MSRP), digit = 3), 
                  Freq = n()) %>% 
        ungroup()
      
      if (counter13$arr_value == 1) {
        plot <- plot %>% 
          arrange(desc(MSRP))
      }
      else if(counter13$arr_value == 2) {
        plot <- plot %>% 
          arrange(MSRP)
      }
      
      plot %>%  
        hchart("lollipop", hcaes(Engine.Fuel.Type, MSRP, color = MSRP),
               tooltip = list(pointFormat = "Price: $<b>{point.y:,.0f}</b>")) %>% 
        hc_xAxis(title = list(text = "Engine Type")) %>% 
        hc_yAxis(labels = list(format = "${value:,.0f}"),
                 title = list(text = "Manufacturer's Suggested Retail Price")) %>% 
        hc_title(text = "Impact of Engine Type to The Car Price") %>% 
        hc_plotOptions(lollipop = list(marker = list(radius = 9))) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$power_details <- renderHighchart({
    
    if (counter12$view_more_value == 1) {
      
      # Impact of the engine power to the car price
      plot <- car_dataset
      
      if (counter13$arr_value == 1) {
        plot <- car_dataset %>% 
          filter(Market.Category != "N/A",
                 Engine.KW < 300)
      }
      else if(counter13$arr_value == 2) {
        plot <- car_dataset %>% 
          filter(Market.Category != "N/A",
                 Engine.KW < 500)
      }
      
      plot %>% 
        group_by(Engine.KW, Brands) %>% 
        summarise(MSRP = round(mean(MSRP), digit = 3), 
                  Freq = n()) %>% 
        ungroup() %>% 
        hchart("scatter", hcaes(Engine.KW, MSRP, color = MSRP),
               tooltip = list(pointFormat = "<b>{point.Brands}</b>
                                       <br>Power: <b>{point.x}</b>KW</br>
                                       <br>Price: $<b>{point.y:,.0f}</b></br>",
                              headerFormat = "")) %>% 
        hc_colorAxis(maxColor = "cyan",
                     minColor = "navy") %>%
        hc_xAxis(title = list(text = "Engine Power (KW)")) %>%
        hc_yAxis(labels = list(format = "${value:,.0f}"),
                 title = list(text = "Manufacturer's Suggested Retail Price")) %>% 
        hc_title(text = "Impact of The Engine Power to The Car Price") %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$gear_details <- renderHighchart({
    
    if (counter12$view_more_value == 1) {
      
      # Impact of the transmission type to the car price
      plot <- car_dataset %>% 
        filter(Market.Category != "N/A",
               Transmission.Type != "UNKNOWN") %>%
        group_by(Transmission.Type) %>% 
        summarise(MSRP = round(mean(MSRP), digit = 3), 
                  Freq = n()) %>% 
        ungroup() %>% 
        mutate(Transmission.Type = replace(Transmission.Type, 
                                           Transmission.Type == "AUTOMATED_MANUAL", 
                                           "AUTOMATED MANUAL"),
               Transmission.Type = replace(Transmission.Type, 
                                           Transmission.Type == "DIRECT_DRIVE", 
                                           "DIRECT DRIVE"))
      
      if (counter13$arr_value == 1) {
        plot <- plot %>% 
          arrange(desc(MSRP))
      }
      else if(counter13$arr_value == 2) {
        plot <- plot %>% 
          arrange(MSRP)
      }
      
      plot %>%  
        hchart("lollipop", hcaes(Transmission.Type, MSRP, color = MSRP),
               tooltip = list(pointFormat = "Price: $<b>{point.y:,.0f}</b>")) %>% 
        hc_xAxis(title = list(text = "Transmission Type")) %>%
        hc_yAxis(labels = list(format = "${value:,.0f}"),
                 title = list(text = "Manufacturer's Suggested Retail Price")) %>% 
        hc_title(text = "Impact of The Transmission Type to The Car Price") %>% 
        hc_plotOptions(lollipop = list(marker = list(radius = 9))) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$cyl_details <- renderHighchart({
    
    if (counter12$view_more_value == 1) {
      
      # Impact of the engine cylinder number to the car price
      plot <- car_dataset %>% 
        filter(Market.Category != "N/A",
               !is.na(Engine.Cylinders),
               Engine.Cylinders != 0,
               Engine.Cylinders <= 12
        ) %>%
        group_by(Engine.Cylinders) %>% 
        summarise(MSRP = round(mean(MSRP), digit = 3), 
                  Freq = n()) %>% 
        ungroup()
      
      if (counter13$arr_value == 1) {
        plot <- plot %>% 
          arrange(desc(MSRP))
      }
      else if(counter13$arr_value == 2) {
        plot <- plot %>% 
          arrange(MSRP)
      }
      
      plot %>% 
        hchart("lollipop", hcaes(as.character(Engine.Cylinders), MSRP, color = MSRP),
               tooltip = list(pointFormat = "Price: $<b>{point.y:,.0f}</b>",
                              headerFormat = "Number of Cylinder: <b>{point.x}</b><br>")) %>% 
        hc_xAxis(title = list(text = "Engine Power (KW)")) %>%
        hc_yAxis(labels = list(format = "${value:,.0f}"),
                 title = list(text = "Manufacturer's Suggested Retail Price")) %>% 
        hc_title(text = "Impact of The Number of Engine Cylinder to The Car Price") %>% 
        hc_plotOptions(lollipop = list(marker = list(radius = 7))) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$wheel_details <- renderHighchart({
    
    if (counter12$view_more_value == 1) {
      
      # Impact of the driven wheels to the car price
      plot <- car_dataset %>% 
        filter(Market.Category != "N/A") %>% 
        group_by(Driven.Wheels) %>% 
        summarise(MSRP = round(mean(MSRP), digit = 3), 
                  Freq = n()) %>% 
        ungroup()
      
      if (counter13$arr_value == 1) {
        plot <- plot %>% 
          arrange(desc(MSRP))
      }
      else if(counter13$arr_value == 2) {
        plot <- plot %>% 
          arrange(MSRP)
      }
      
      plot %>% 
        hchart("lollipop", hcaes(Driven.Wheels, MSRP, color = MSRP),
               tooltip = list(pointFormat = "Price: $<b>{point.y:,.0f}</b>")) %>% 
        hc_xAxis(title = list(text = "Transmission Type")) %>%
        hc_yAxis(labels = list(format = "${value:,.0f}"),
                 title = list(text = "Manufacturer's Suggested Retail Price")) %>% 
        hc_title(text = "Impact of The Dirven Wheels to The car Price") %>% 
        hc_plotOptions(lollipop = list(marker = list(radius = 9))) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$spec_cat <- renderHighchart({
    
    # Set initial value of select input brand to Suzuki
    if (is.null(update$update_brand) == T) {
      brand_selected = "Suzuki"
    }
    else {
      brand_selected = update$update_brand
    }
    
    
    
    if (counter2$button_view_1 == 0) {
      
      # Most produced category throughout the year
      plot <- car_dataset %>%
        filter(Brands == brand_selected) %>%
        group_by(Market.Category) %>%
        summarise(Frequency = n(),
                  MSRP = mean(MSRP)) %>%
        ungroup() %>%
        mutate(MSRP = number(MSRP, prefix = "$", big.mark = ","))
      
      
      
      if (counter11$sort_button_6 == 0) {
        arranged_plot <- plot
      }
      else if (counter11$sort_button_6 == 1) {
        arranged_plot <- plot %>% 
          arrange(desc(Frequency))
      }
      else {
        arranged_plot <- plot %>% 
          arrange(Frequency)
      }
      
      
      
      # Plot bar highchart
      hchart(arranged_plot, "column", 
               hcaes(Market.Category, Frequency),
               tooltip = list(pointFormat = "Average Price: {point.MSRP}",
                              headerFormat = "")) %>%
        hc_title(text = "Most Produced Category") %>% 
        hc_plotOptions(column = list(colorByPoint = T)) %>% 
        hc_colors(color = list("#77DD77", "#779ECB", "#FE6B64", "#FDFD98", "#B29DD9",
                               "#98E2F7", "#F4B5D2", "#FFE096", "#CAEEBE", "#B64D3A",
                               "#572D15", "#CE9C6F", "#504E6D", "#679A7D", "#F3D67F",
                               "#454332", "#C4698C", "#FD6400", "#786AAB", "#DE905C",
                               "#7FA56E", "#5E4557", "#C13D35", "#EE5921", "#7C323D",
                               "#C4C263", "#575F0D", "#E06C3A", "#1D3044", "#3B3769")) %>%
        hc_xAxis(title = list(text = "Market Category"),
                 crosshairs = T) %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_plotOptions(series = list(marker = F)) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    else {
      
      # Most popular category for each year
      car_dataset %>%
        filter(Brands == brand_selected,
               Market.Category != "N/A",
               Year >= input$slider_year_spec[1],
               Year <= input$slider_year_spec[2]) %>%
        group_by(Year, Market.Category) %>%
        summarise(Frequency = n(),
                  MSRP = mean(MSRP)) %>%
        ungroup() %>%
        rename(Market_Category = Market.Category) %>% 
        mutate(MSRP = number(MSRP, prefix = "$", big.mark = ",")) %>%
        hchart("column", 
               hcaes(Year, Frequency, group = Market_Category),
               tooltip = list(pointFormat = "<br><b>{point.Market_Category}<b/><br/>
                                             Average Price: {point.MSRP}",
                              headerFormat = "")) %>%
        hc_title(text = "Most Produced Category By Year") %>% 
        hc_plotOptions(column = list(colorByCategory = T)) %>%
        hc_colors(color = list("#77DD77", "#779ECB", "#FE6B64", "#FDFD98", "#B29DD9",
                               "#98E2F7", "#F4B5D2", "#FFE096", "#CAEEBE", "#B64D3A",
                               "#572D15", "#CE9C6F", "#504E6D", "#679A7D", "#F3D67F",
                               "#454332", "#C4698C", "#FD6400", "#786AAB", "#DE905C",
                               "#7FA56E", "#5E4557", "#C13D35", "#EE5921", "#7C323D",
                               "#C4C263", "#575F0D", "#E06C3A", "#1D3044", "#3B3769")) %>%
        hc_xAxis(title = list(text = "Market Category"),
                 crosshairs = T) %>%
        hc_yAxis(title = list(text = "Popularity")) %>%
        hc_plotOptions(series = list(marker = F)) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$spec_model <- renderHighchart({
    
    if (counter3$button_view_2 == 0) {
      
      # Most produced vehicle type
      plot <- car_dataset %>%
        filter(Brands == "Suzuki") %>%
        group_by(Vehicle.Type) %>%
        summarise(MSRP = round(mean(MSRP), digit = 2),
                  Frequency = n()) %>%
        ungroup() %>%
        mutate(MSRP_Text = number(MSRP, accuracy = 1, prefix = "$", big.mark = ","))
      
      
      
      if (counter4$sort_button == 0) {
        arranged_plot <- plot
      }
      else if (counter4$sort_button == 1) {
        arranged_plot <- plot %>% 
          arrange(desc(Frequency))
      }
      else {
        arranged_plot <- plot %>% 
          arrange(Frequency)
      }
      
      
      
      hchart(arranged_plot, 
             "column", 
             hcaes(Vehicle.Type, Frequency),
             tooltip = list(pointFormat = "<br>Model: <b>{point.Model}<b/><br/>
                                            Average Price: <b>{point.MSRP_Text}<b/>",
                            headerFormat = "")) %>%
        hc_title(text = "Most produced vehicle type") %>% 
        hc_plotOptions(column = list(colorByPoint = T)) %>%
        hc_colors(color = list("#77DD77", "#779ECB", "#FE6B64", "#FDFD98", "#B29DD9",
                               "#98E2F7", "#F4B5D2", "#FFE096", "#CAEEBE", "#B64D3A",
                               "#572D15", "#CE9C6F", "#504E6D", "#679A7D", "#F3D67F",
                               "#454332", "#C4698C", "#FD6400", "#786AAB", "#DE905C",
                               "#7FA56E", "#5E4557", "#C13D35", "#EE5921", "#7C323D",
                               "#C4C263", "#575F0D", "#E06C3A", "#1D3044", "#3B3769")) %>%
        hc_yAxis(crosshairs = T,
                 title = list(text = "Frequency")) %>% 
        hc_plotOptions(series = list(marker = F)) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    else if(counter3$button_view_2 == 1) {
      
      # Most Produced Model
      plot2 <- car_dataset %>%
        filter(Brands == "Suzuki") %>%
        group_by(Model) %>%
        summarise(MSRP = round(mean(MSRP), digit = 2),
                  Frequency = n()) %>%
        ungroup() %>% 
        mutate(MSRP_Text = number(MSRP, accuracy = 1, prefix = "$", big.mark = ","))
      
      
      
      if (counter5$sort_button_2 == 0) {
        arranged_plot2 <- plot2
      }
      else if (counter5$sort_button_2 == 1) {
        arranged_plot2 <- plot2 %>% 
          arrange(desc(Frequency))
      }
      else {
        arranged_plot2 <- plot2 %>% 
          arrange(Frequency)
      }
      
      
      
      hchart(arranged_plot2, 
             "column", 
             hcaes(Model, Frequency),
             tooltip = list(pointFormat = "<br>Model: <b>{point.Model}<b/><br/>
                                           Average Price: <b>{point.MSRP_Text}<b/>",
                            headerFormat = "")) %>%
        hc_title(text = "Most Produced Model") %>% 
        hc_plotOptions(column = list(colorByPoint = T)) %>%
        hc_colors(color = list("#77DD77", "#779ECB", "#FE6B64", "#FDFD98", "#B29DD9",
                               "#98E2F7", "#F4B5D2", "#FFE096", "#CAEEBE", "#B64D3A",
                               "#572D15", "#CE9C6F", "#504E6D", "#679A7D", "#F3D67F",
                               "#454332", "#C4698C", "#FD6400", "#786AAB", "#DE905C",
                               "#7FA56E", "#5E4557", "#C13D35", "#EE5921", "#7C323D",
                               "#C4C263", "#575F0D", "#E06C3A", "#1D3044", "#3B3769")) %>%
        hc_yAxis(crosshairs = T,
                 title = list(text = "Frequency")) %>% 
        hc_plotOptions(series = list(marker = F)) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    else {
      
      # Most Produced Model each year
      if (counter6$sort_button_3 == 0) {
        value = "normal"
      }
      else {
        value = "percent"
      }
      
      car_dataset %>%
        filter(Brands == "Suzuki") %>%
        group_by(Model, Year) %>%
        summarise(MSRP = round(mean(MSRP), digit = 2),
                  Frequency = n()) %>%
        ungroup() %>%
        mutate(MSRP_Text = number(MSRP, accuracy = 1, prefix = "$", big.mark = ",")) %>%
        hchart("column", 
               hcaes(Model, Frequency, group = desc(Year)),
               tooltip = list(pointFormat = "<br>Model: <b>{point.Model}<b/><br/>
                                      <br>Year: <b>{point.Year}<b/><br/>
                                      Average Price: <b>{point.MSRP_Text}<b/>",
                              headerFormat = "")) %>%
        hc_title(text = "Amount of Model Produced each Year") %>% 
        hc_colors(color = list("#77DD77", "#779ECB", "#FE6B64", "#FDFD98", "#B29DD9",
                               "#98E2F7", "#F4B5D2", "#FFE096", "#CAEEBE", "#B64D3A",
                               "#572D15", "#CE9C6F", "#504E6D", "#679A7D", "#F3D67F",
                               "#454332", "#C4698C", "#FD6400", "#786AAB", "#DE905C",
                               "#7FA56E", "#5E4557", "#C13D35", "#EE5921", "#7C323D",
                               "#C4C263", "#575F0D", "#E06C3A", "#1D3044", "#3B3769")) %>%
        hc_xAxis(reversed = T) %>%
        hc_yAxis(crosshairs = T,
                 title = list(text = "Frequnecy")) %>% 
        hc_plotOptions(column = list(colorByPoint = T,
                                     stacking = value)) %>%
        hc_plotOptions(series = list(marker = F)) %>% 
        hc_legend(enabled = F) %>% 
        hc_add_theme(hc_theme_google())
      
    }
    
  })
  
  output$spec_hp <- renderHighchart({
    
    # Set initial value of select input brand to Suzuki
    if (is.null(update$update_brand) == T) {
      brand_selected = "Suzuki"
    }
    else {
      brand_selected = update$update_brand
    }
    
    if (counter7$button_view_3 == 0) {
      
      
      
      if (counter9$sort_button_5 == 0) {
        
        # Average engine power by Killowatt
        plot <- car_dataset %>%
          filter(!is.na(Engine.KW),
                 Brands == brand_selected) %>% 
          group_by(Brands, Engine.KW) %>% 
          summarise(Frequency = n()) %>% 
          ungroup()
        
        hchart(density(plot$Engine.KW),
               color = " deepskyblue",
               name = unique(plot$Brands)) %>%
          hc_title(text = "Average Produced Engine Kilowatt") %>% 
          hc_xAxis(crosshair = T,
                   title = list(text = "Engine Kilowatt")) %>%
          hc_yAxis(title = list(text = "Distribution Percentage")) %>% 
          hc_tooltip(pointFormat = "Distribution: <b>({point.y:.6f}) * 100%<b/>
                                  <br>Power Produced: <b>{point.x:.0f}KW<b/><br/>",
                     headerFormat = "") %>% 
          hc_plotOptions(series = list(marker = F)) %>% 
          hc_legend(enabled = F) %>% 
          hc_add_theme(hc_theme_google())
        
      }
      else {
        
        # Average engine power by Horsepower
        plot <- car_dataset %>%
          filter(!is.na(Engine.HP),
                 Brands == brand_selected) %>% 
          group_by(Brands, Engine.HP) %>% 
          summarise(Frequency = n()) %>% 
          ungroup()
        
        hchart(density(plot$Engine.HP),
               color = " deepskyblue",
               name = unique(plot$Brands)) %>%
          hc_title(text = "Average Produced Engine Horsepower") %>% 
          hc_xAxis(crosshair = T,
                   title = list(text = "Engine Horsepower")) %>%
          hc_yAxis(title = list(text = "Distribution Percentage")) %>% 
          hc_tooltip(pointFormat = "Distribution: <b>({point.y:.6f}) * 100%<b/>
                                  <br>Power Produced: <b>{point.x:.0f}HP<b/><br/>",
                     headerFormat = "") %>% 
          hc_plotOptions(series = list(marker = F)) %>% 
          hc_legend(enabled = F) %>% 
          hc_add_theme(hc_theme_google())
        
      }
      
      
      
    }
    else {
      
      # Average Highway and City Kilometer per Liter
      plot_HK <- car_dataset %>%
        filter(Brands == brand_selected) %>%
        group_by(Brands, Highway.KmL) %>%
        summarise(Frequency = n()) %>%
        ungroup()
      
      plot_CK <- car_dataset %>%
        filter(Brands == brand_selected) %>%
        group_by(Brands, City.KmL) %>%
        summarise(Frequency = n()) %>%
        ungroup()
      
      # Average Highway and City Miles per Gallons
      plot_HM <- car_dataset %>%
        filter(Brands == brand_selected) %>%
        group_by(Brands, Highway.MpG) %>%
        summarise(Frequency = n()) %>%
        ungroup()
      
      plot_CM <- car_dataset %>%
        filter(Brands == brand_selected) %>%
        group_by(Brands, City.MpG) %>%
        summarise(Frequency = n()) %>%
        ungroup()
      
      
      
      if (counter8$sort_button_4 == 0) {
        
        # Plot Kilometer per Liter
        hchart(density(plot_HK$Highway.KmL),
               color = "#32567F",
               name = unique(plot_HK$Brands),
               tooltip = list(pointFormat = "Distribution: <b>({point.y:.2f}) * 100%<b/>
                                                      <br>Average Highway Kilometer/Liter: <b>{point.x:.2f}Km/L<b/><br/>"),
               headerFormat = "") %>%
          hc_title(text = "Average Highway and City Kilometer per Hour") %>% 
          hc_add_series(density(plot_CK$City.KmL),
                        type = "area",
                        color = "#FBC123",
                        name = unique(plot_CK$Brands),
                        tooltip = list(pointFormat = "Distribution: <b>({point.y:.2f}) * 100%<b/>
                                                      <br>Average City Kilometer/Liter: <b>{point.x:.2f}Km/L<b/><br/>"),
                                       headerFormat = "") %>%
          hc_xAxis(crosshair = T,
                   title = list(text = "Kilometer per Liter")) %>%
          hc_yAxis(title = list(text = "Distribution Percentage")) %>% 
          hc_plotOptions(series = list(marker = F)) %>%
          hc_legend(enabled = F) %>%
          hc_add_theme(hc_theme_google())
        
      }
      else {
        
        # Plot Miles per Gallons
        hchart(density(plot_HM$Highway.MpG),
               color = "#32567F",
               name = unique(plot_HM$Brands),
               tooltip = list(pointFormat = "Distribution: <b>({point.y:.2f}) * 100%<b/>
                                                      <br>Average Highway Miles/Gallons: <b>{point.x:.2f}MpG<b/><br/>"),
               headerFormat = "") %>%
          hc_title(text = "Average Highway and City Miles per Gallons") %>% 
          hc_add_series(density(plot_CM$City.MpG),
                        type = "area",
                        color = "#FBC123",
                        name = unique(plot_CM$Brands),
                        tooltip = list(pointFormat = "Distribution: <b>({point.y:.2f}) * 100%<b/>
                                                      <br>Average City Miles/Gallons: <b>{point.x:.2f}MpG<b/><br/>"),
                        headerFormat = "") %>% 
          hc_xAxis(crosshair = T,
                   title = list(text = "Miles per Gallons")) %>%
          hc_yAxis(title = list(text = "Distribution Percentage")) %>% 
        hc_plotOptions(series = list(marker = F)) %>%
          hc_legend(enabled = F) %>%
          hc_add_theme(hc_theme_google())
        
      }
      
    }
    
  })
  
})