
# BEGIN PROGRAMMING USING SHINY UI
shinyUI(
    
    dashboardPage(
        
        # HEADER OF THE DASHBOARD
        
        # Declare title
        dashboardHeader(title = "Car Brands"),
        
        # SIDEBAR OF THE DASHBOARD
        dashboardSidebar(
            
            sidebarMenu(
                
                # Create tab menu
                menuItem(text="Car Brands in USA",
                         icon=icon("flag-usa"),
                         tabName="car_brands_usa"),
                menuItem(text="Car Spesifications",
                         icon=icon("chart-bar"),
                         tabName="car_spec"),
                menuItem(text="Data",
                         icon=icon("file-alt"),
                         tabName="data_tab"),
                menuItem(text="Code",
                         icon=icon("code"),
                         href="https://www.google.com")
                
            )
            
        ),
        
        # BODY OF THE DASHBOARD
        dashboardBody(
            
            # Choose a theme for this project from dashboarthemes package
            shinyDashboardThemes(theme = "blue_gradient"),
            
            # Declare items for the tabs
            tabItems(
                
                
                
                # Tab Car Brands in USA
                tabItem(
                    
                    # Orientation setup
                    align = "center",
                    
                    # Decleare which tab menu that the declared values below will be placed
                    tabName = "car_brands_usa",
                    
                    # Create a header title
                    h1("CAR BRANDS OF THE USA"),
                    
                    # Introduction text
                    br(),
                    br(),
                    h4(align = "left",
                       "Welcome to the first page of Car Brands of the United States of America!"),
                    
                    p(align = "left",
                      "In this page, you can find many informations regarding cars from 1990 all the way up to 2017. The information
                      which can be found in this page are the car maker which produced the highest number of cars, the most popular 
                      brands according to the popuarity poll of the people in the United States, the increase of car prices from time
                      to time, the comparison of regional price trend from time to time, and lastly the distribution of cars of each 
                      region to add a bit more sepecific information on this page. To get details regarding the specification of the 
                      car of your choice, please go the Car Spesifications tab"),
                    
                    # Header text
                    br(),
                    h3("Top Car Brands in United States of America"),
                    br(),
                    
                    
                    
                    # Placed displayed data into the same row
                    fluidRow(
                        
                        # Create a tab box
                        tabBox(
                            
                            # Declare title, Id, and the width of the tab box
                            title = "Top Car Brands",
                            id = "tabset1", 
                            width = 8,
                            
                            # Create the first panel
                            tabPanel("Most Sold",
                                     
                                     # Slider input to control the number of cars displayed in the top chart
                                     sliderInput(
                                         inputId="slider_top1",
                                         label="Select Range of the top Listed Countries",
                                         min=5,
                                         max=40,
                                         value=10),
                                     
                                     # Displayed the Top Chart data visualization
                                     highchartOutput("top_charts")),
                            
                            # Create the second panel
                            tabPanel("Most popular",
                                     
                                     # Slider input to control the number of cars displayed in the top popular chart
                                     sliderInput(
                                         inputId="slider_top2",
                                         label="Select Range of the top Listed Countries",
                                         min=5,
                                         max=40,
                                         value=10),
                                     
                                     # Displayed the Top popular Chart data visualization
                                     highchartOutput("popular_charts"))
                            
                        ),
                        
                        # Header text for infobox
                        h4("Top American Brand"),
                        
                        # Infobox for the Top American car brand
                        infoBox(uiOutput("top_american_brand"),
                                 uiOutput("top_american_freq"),
                                 icon = icon("globe-americas"),
                                 color = "yellow",
                                 fill = T),
                        
                        # Header text for infobox
                        h4("Top European Brand"),
                        
                        # Infobox for the Top Europe car brand
                        infoBox(uiOutput("top_europe_brand"),
                                 uiOutput("top_europe_freq"),
                                 icon = icon("globe-europe"),
                                 color = "aqua",
                                 fill = T),
                        
                        # Header text for infobox
                        h4("Top Asian Brand"),
                        
                        # Infobox for the Top Asia car brand
                        infoBox(uiOutput("top_asia_brand"),
                                 uiOutput("top_asia_freq"),
                                 icon = icon("globe-asia"),
                                 color = "red",
                                 fill = T),
                        
                        # Header text
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        h4("Change Chart Type:"),
                        
                        # Button to change the chart type
                        actionBttn(inputId = "plot_button",
                                   icon = icon("object-ungroup"),
                                   style = "fill",
                                   color = "primary",
                                   size = "lg")
                        
                    ),
                    
                    
                    
                    # Header text
                    br(),
                    h3("Average Car Price Trend by Year"),
                    br(),
                    
                    
                    
                    # Create a box
                    box(width = 12,
                        
                        # Display Price growth chart
                        column(width = 9,
                               highchartOutput("price_growth")),
                        
                        # Select button to select Region
                        column(width = 3,
                            
                               uiOutput("trend_select_1"),
                                
                               uiOutput("trend_select_2")
                            
                        )
                        
                    ),
                    
                    
                    # Placed displayed data into the same row
                    br(),
                    br(),
                    fluidRow(
                        
                        column(width = 4,
                               
                               # Infobox contain the average price of selected region 1
                               infoBox(width = 12,
                                       uiOutput("region_selected1"),
                                       uiOutput("price_region1"),
                                       icon = icon("car"),
                                       color = "yellow",
                                       fill = T),
                               
                               # Infobox contain the average price of selected region 2
                               infoBox(width = 12,
                                       uiOutput("region_selected2"),
                                       uiOutput("price_region2"),
                                       icon = icon("car"),
                                       color = "aqua",
                                       fill = T)
                               
                        ),
                        
                        # Header text
                        h4("Average Price Distribution"),
                        
                        # Create a box
                        box(width = 8,
                            
                            # Display density chart for the selected regions
                            highchartOutput("distribution_each_region")

                        )
                    
                    ),
                    
                    
                    
                    # Create buttons for displaying detailed information
                    fluidRow(

                        column(align = "left",
                               width = 12,
                               p("View More Details")),

                        column(align = "left",
                               width = 12,
                               uiOutput("view_more"))

                    ),
                    
                    # Present the output when the view_more button is pressed
                    br(),
                    uiOutput("detail_outputs")
                    
                ),
                
                
                # Tab Car Spesifications
                tabItem(
                    
                    # Orientation setup
                    align = "center",
                    
                    # Decleare which tab menu that the declared values below will be placed
                    tabName = "car_spec",
                    
                    # Header text
                    h1("THE CAR OF YOU CHOICE"),
                    
                    # Introduction text
                    br(),
                    br(),
                    h4(align = "left",
                       "Welcome the the second page of Car Brands of the United States of America!"),
                    
                    p(align = "left",
                      "In this page, you can obtain a more specific information regarding a car. You can select your desired region, year, brand,
                      market category, model, Engine Power, and price range of the car of your choice. You do not have to worry about having to have 
                      an image about the car that you want because This app will definately help you find your ideal car!"),
                    
                    
                    h3("Your Journey of Sheer Driving Pleasure Starts Here"),
                    br(),
                    
                    
                    
                    # Create a box
                    box(align = "left",
                        width = 12,
                        
                        
                        
                        # Placed displayed data into the same row
                        fluidRow(
                            
                            column(width = 4,
                                   
                                   # Create a Dropdown menu
                                   dropdown(align = "center",
                                            
                                            # Make the drop down menu able to close by itself by a click of submit button
                                            tags$head(
                                                tags$script("Shiny.addCustomMessageHandler('close_drop_down_list', function(x){
                                $('html').click();
                                });")
                                            ),
                                            
                                            # Declare Dropdown button id and style
                                            inputId = "drop_down_list",
                                            label = "List of Input",
                                            icon = icon("caret-down"),
                                            color = "primary",
                                            style = "fill",
                                            size = "sm",
                                            
                                            # text
                                            p(strong("Filter Region")),
                                            
                                            
                                            
                                            # Placed displayed data into the same row
                                            fluidRow(
                                                
                                                # Switch button for the region of Asia
                                                column(width = 4, align = "center", p("Asia"), switchInput("switch_asia")),
                                                
                                                # Switch button for the region of Europe
                                                column(width = 4, align = "center", p("Europe"), switchInput("switch_europe")),
                                                
                                                # Switch button for the region of America
                                                column(width = 4, align = "center", p("America"), switchInput("switch_usa")),
                                                
                                                # Make switch input control the selection of select input brands
                                                uiOutput("switches")
                                                
                                            ),
                                            
                                            
                                            # Placed displayed data into the same row
                                            fluidRow(
                                                
                                                # Select input brands
                                                column(width = 6, uiOutput("brands_spec_out")),
                                                
                                                # select input market categories
                                                column(width = 6, uiOutput("categories"))
                                                
                                            ),
                                            
                                            # Slider input for Horsepower
                                            uiOutput("slider_HP"),
                                            
                                            # Select input for model
                                            uiOutput("model_spec_out"),
                                            
                                            # Submit button
                                            uiOutput("actbtn_submit")
                                            
                                   ),
                                   
                                   # Display logo of the selected brand
                                   br(),
                                   br(),
                                   uiOutput("image_logo")
                                   
                            ),
                            
                            column(width = 8,
                                   
                                   # Create a tab box
                                   tabBox(width = 12,
                                          
                                          title = "Car of Your Choice",
                                          
                                          # Create the first panel
                                          tabPanel("Market Category", 
                                                   
                                                   # Display the most produced categories of the selected brand
                                                   highchartOutput("spec_cat"),
                                                   
                                                   
                                                   
                                                   # Placed displayed data into the same row
                                                   fluidRow(
                                                       
                                                       column(width =10, 
                                                              
                                                              # Slider input for year range
                                                              uiOutput("year_range"),
                                                              
                                                              # Arrangement button
                                                              uiOutput("text_actbtn_sort_6"),
                                                              uiOutput("actbtn_sort_6")),
                                                       
                                                       column(width = 2,
                                                              
                                                              # Button to change the chart type
                                                              uiOutput("text_change_chart"), 
                                                              uiOutput("change_chart"))
                                                       
                                                   )),
                                          
                                          # Create the second panel
                                          tabPanel("Vehicle Type",
                                                   
                                                   # Display the most produced model of the selected brand
                                                   highchartOutput("spec_model"),
                                                   
                                                   
                                                   
                                                   # Placed displayed data into the same row
                                                   fluidRow(
                                                       
                                                       column(width = 10, 
                                                              
                                                              # Arrangement buttons
                                                              uiOutput("text_actbtn_sort"),
                                                              uiOutput("actbtn_sort"),
                                                              uiOutput("text_actbtn_sort_2"),
                                                              uiOutput("actbtn_sort_2"),
                                                              uiOutput("text_actbtn_sort_3"),
                                                              uiOutput("actbtn_sort_3")),
                                                       
                                                       column(width = 2,
                                                              
                                                              # Button to change the chart type
                                                              uiOutput("text_change_chart_2"), 
                                                              uiOutput("change_chart_2"))
                                                       
                                                   )),
                                          
                                          # Create the third panel
                                          tabPanel("Engine Specification",
                                                   
                                                   # Display Horsepower data distribution
                                                   highchartOutput("spec_hp"),
                                                   
                                                   
                                                   
                                                   # Placed displayed data into the same row
                                                   fluidRow(
                                                       
                                                       column(width = 10,
                                                              
                                                              # Arrangement buttons
                                                              uiOutput("text_actbtn_sort_4"),
                                                              uiOutput("actbtn_sort_4"),
                                                              uiOutput("text_actbtn_sort_5"),
                                                              uiOutput("actbtn_sort_5")),
                                                       
                                                       column(width = 2, 
                                                              
                                                              # Button to change the chart type
                                                              uiOutput("text_change_chart_3"), 
                                                              uiOutput("change_chart_3"))
                                                       
                                                   ))
                                          
                                   )
                                   
                            )
                            
                        )
                        

                    ),
                    
                    
                    # Header text
                    h2("Detailed Specifications"),
                    br(),
                    
                    # Create a box
                    box(width = 14,
                        
                        # Header text
                        h5(strong("Metric / Imperial")),
                        
                        # Button to change between metric and imperial unit
                        uiOutput("switch_unit"),
                        
                        # Data table output from the selected model
                        br(),
                        br(),
                        dataTableOutput("spec_text")
                        
                    ),
                    
                ),
                
                
                
                # Tab Data
                tabItem(
                    
                    # Orientation setup
                    align = "center",
                    
                    # Decleare which tab menu that the declared values below will be placed
                    tabName = "data_tab",
                    
                    # Introduction text
                    h2("Listed Car Data by Brands in United States of America"),
                    
                    br(),
                    br(),
                    p(align = "left",
                      "This application powered by Shiny app, was a result of analizing the data of the car listed in USA from a couple of different
                      sources accoding to the data constuctor of the car brands in United States of America. If you are interested in the detail
                      regarding the data of the previously mentioned data name, you can view the clean data, which has been processed before I begin
                      to process the data to make the app, below."),
                    
                    
                    
                    # Placed displayed data into the same row
                    br(),
                    fluidRow(
                        
                        column(
                            align = "left",
                            width = 2,
                            
                            # Check box to select columns to be displayed
                            box(checkboxGroupInput("show_vars", 
                                                   "Select Columns", 
                                                   names(car_dataset), 
                                                   selected = c("Brands", "Model", "Year")),
                                width = "100%")),
                        
                        column(
                            width = 10,
                            
                            # Data table of the car_dataset
                            box(dataTableOutput("dataframe"),
                                width = "100%"))
                        
                    ),
                    
                    # Header text
                    br(),
                    br(), 
                    h4(aling = "left",
                      "if you are interested in seing the raw data, check out the data table below:"),
                    
                    # Data table of the raw_dataset
                    br(),
                    br(),
                    dataTableOutput("dataframe_raw")
                )
                
            )
            
        )
        
    )
    
)
column(width = 2, switchInput("a"))