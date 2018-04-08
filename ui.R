library(leaflet)

# Choices for drop-downs
# vars <- c(
#     "你政大?" = "superzip",
#     "你台大" = "centile",
#     "我青椒" = "college",
#     "我寫歌" = "income",
#     "中興啦" = "adultpop"
# )

# vars2 <- c(
#     "你政?" = "superzip",
#     "你大" = "centile",
#     "我青" = "college",
#     "我寫" = "income",
#     "中啦" = "adultpop"
# )

navbarPage("統研8+9", id="NCCU_Stat",
           # tabPanel("map map map",
           #          div(class="outer",
           #              
           #              tags$head(
           #                  # Include our custom CSS
           #                  includeCSS("styles.css"),
           #                  includeScript("gomap.js")
           #              ),
           #              
           #              # If not using custom CSS, set height of leafletOutput to a number instead of percent
           #              leafletOutput("map", width="100%", height="100%"),
           #              
           #              # Shiny versions prior to 0.11 should use class = "modal" instead.
           #              # modal
           #              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
           #                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
           #                            width = 330, height = "auto",
           #                            
           #                            h2("等等的邊緣人選單啦!!"),
           #                            
           #                            selectInput("color", "Color", vars),
           #                            selectInput("size", "Size", vars2)
           #                            # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
           #                            #                  # Only prompt for threshold when coloring or sizing by superzip
           #                            #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
           #                            # ),
           #                            
           #                            # plotOutput("histCentile", height = 200),
           #                            # plotOutput("scatterCollegeIncome", height = 250)
           #              ),
           #              
           #              tags$div(id="cite",
           #                       'Data compiled for ', tags$em('Catch Potential Convience'), ' by 統研8+9 (NCCU_Stat, 2017).'
           #              )
           #          )
           # ),
           
           
           tabPanel("Data filter",
                    fluidRow(
                        column(3,
                               dateInput("date", label = h3("Date input"), value = "")
                        ),
                        column(3,
                               selectInput("hour", label = h3("Hour select"), 
                                           choices = list("00"=00,"01"=01,"02"=02,"03"=03,"04"=04,
                                                          "05"=05,"06"=06,"07"=07,"08"=08,"09"=09,
                                                          "10"=10,"11"=11,"12"=12,"13"=13,
                                                          "14"=14,"15"=15,"16"=16,"17"=17,
                                                          "18"=18,"19"=19,"20"=20,"21"=21,
                                                          "22"=22,"23"=23), 
                                           selected = 1)
                        ),
                        column(3,
                               # column後面第一個參數只能放1~12
                               # Copy the line below to make a slider range 
                               sliderInput("min", label = h3("Min Range"), min = 0, 
                                           max = 59, value = c(30, 40))#value為起始值
                        )
                        # column(2, verbatimTextOutput("valuedate")),
                        # column(2, verbatimTextOutput("valuehour")),
                        # column(2, verbatimTextOutput("valuemin"))

                    ),
                    hr(),
                    DT::dataTableOutput("table1")
           ),
           
           tabPanel("license trace",
                    fluidRow(
                        column(3,
                               textInput("license_eng", label = h3("license_eng"), value = "")
                        ),
                        column(3,
                               textInput("license_num", label = h3("license_num"), value = "")
                        )
                        # column(2, verbatimTextOutput("value_eng")),
                        # column(2, verbatimTextOutput("value_num"))
                    ),
                    hr(),
                    DT::dataTableOutput("table2")
           ),
           
           tabPanel('Map', #點擊後直接切換
                    fluidRow(
                        column(3,
                               dateInput("date2", label = h3("Date input"), value = "")
                        ),
                        column(3,
                               selectInput("hour2", label = h3("Hour select"), 
                                           choices = list("00"=00,"01"=01,"02"=02,"03"=03,"04"=04,
                                                          "05"=05,"06"=06,"07"=07,"08"=08,"09"=09,
                                                          "10"=10,"11"=11,"12"=12,"13"=13,
                                                          "14"=14,"15"=15,"16"=16,"17"=17,
                                                          "18"=18,"19"=19,"20"=20,"21"=21,
                                                          "22"=22,"23"=23), 
                                           selected = 1)
                        ),
                        column(3,
                               # column後面第一個參數只能放1~12
                               # Copy the line below to make a slider range 
                               sliderInput("min2", label = h3("Min Range"), min = 0, 
                                           max = 59, value = c(30, 40))#value為起始值
                        )
                        # column(2, verbatimTextOutput("valuedate")),
                        # column(2, verbatimTextOutput("valuehour")),
                        # column(2, verbatimTextOutput("valuemin"))
                        
                    ),
                    h1('加油站位置'),
                    #sliderInput("slider1", label = "Slider", min = 3, max = 10, value = 7),
                    leafletOutput("map") ##plotOutput 叫出output圖
           ) 
           
)
