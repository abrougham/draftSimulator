library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shiny)
library(DT)
library(ggplot2)
library(shinyalert)
library(tableHTML)
library(reshape)
library(shinythemes)
library(tidyverse)
library(data.table)
library(readxl)
library(shinydashboard)
library(plotly)
library(fmsb)
library(shinyWidgets)
library(shinycssloaders)
library(lubridate)
library(devtools)
library(dashboardthemes)
library(fresh)
library(sortable)
library(shinyjs)
library(htmlwidgets)
library(stringi)
library(sjmisc)

# Test string 

# Prelims / Data #

Teams_Data         <- read_excel("Teams_Data.xlsx")
Draftee_Data       <- read_excel("Draftee_Data.xlsx")
Draft_Order        <- read_excel("Draft_Order_2020.xlsx") %>% mutate( Round = as.numeric(gsub(x = Round , pattern = "Round " , replacement = "")))
Pick_Points        <- read_excel("Draft_Order_2020.xlsx") %>% mutate( Round = as.numeric(gsub(x = Round , pattern = "Round " , replacement = ""))) %>% select(Round,Pick,Points,FuturePts) 
Selected_Table     <- data.frame("Pick" = numeric() , "Team" = character() , "Player" = character())
Draft_Order_Future <- read_excel("Draft_Order_2021.xlsx") %>% mutate( Round = as.numeric(gsub(x = Round , pattern = "Round " , replacement = "")))
LogTable           <- data.frame("No" = numeric() , "Type" = character() , "Details" = character())
RoundImg           <- read_excel("RoundImg.xlsx")

twitterTimeline <- function(href, ...) {
  tagList(
    tags$a(class = "twitter-timeline", href = href, ...),
    tags$script("twttr.widgets.load()")
  )
}

bsModalNoClose <-function(...) {
  b = bsModal(...)
  b[[2]]$`data-backdrop` = "static"
  b[[2]]$`data-keyboard` = "false"
  return(b)
}

customTheme <- shinyDashboardThemeDIY(
  ######
  
  ### general
  appFontFamily = ""
  ,appFontColor = "#262626"
  ,primaryFontColor = "#0F0F0F"
  ,infoFontColor = "#0F0F0F"
  ,successFontColor = "#0F0F0F"
  ,warningFontColor = "#0F0F0F"
  ,dangerFontColor = "#0F0F0F"
  ,bodyBackColor = "#EEEEEE"
  
  ### header
  ,logoBackColor = "#0A285E"
  
  ,headerButtonBackColor = "#0A285E"
  ,headerButtonIconColor = "#FD1300"
  ,headerButtonBackColorHover = "#163C91"
  ,headerButtonIconColorHover = "#FD1300"
  
  ,headerBackColor = "#0A285E"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "#0A285E"
  ,sidebarPadding = "0" # here
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#28E041"
  
  ,sidebarSearchBackColor = "#0A285E"
  ,sidebarSearchIconColor = "#FD1300"
  ,sidebarSearchBorderColor = "#FD1300"
  
  ,sidebarTabTextColor = "#D6D6D6"
  ,sidebarTabTextSize = "14"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"
  
  ,sidebarTabBackColorSelected = "#163C91"
  ,sidebarTabTextColorSelected = "#FFFFFF"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "#163C91"
  ,sidebarTabTextColorHover = "#D6D6D6"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#FD1300"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "#FFFFFF"
  ,boxBorderRadius = "5"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#E1E1E1"
  ,boxPrimaryColor = "#5F9BD5"
  ,boxInfoColor = "#B4B4B4"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#ED7D31"
  ,boxDangerColor = "#E84C22"
  
  ,tabBoxTabColor = "#F8F8F8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#646464"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#F8F8F8"
  ,tabBoxHighlightColor = "#C8C8C8"
  ,tabBoxBorderRadius = "5"
  
  ### inputs
  ,buttonBackColor = "#D7D7D7"
  ,buttonTextColor = "#2D2D2D"
  ,buttonBorderColor = "#969696"
  ,buttonBorderRadius = "5"
  
  ,buttonBackColorHover = "#BEBEBE"
  ,buttonTextColorHover = "#000000"
  ,buttonBorderColorHover = "#969696"
  
  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#767676"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#F5F5F5"
  ,textboxBorderColorSelect = "#6C6C6C"
  
  ### tables
  ,tableBackColor = "#F8F8F8"
  ,tableBorderColor = "#EEEEEE"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)

######

ui = dashboardPagePlus(collapse_sidebar = TRUE, useShinyjs(),
                       
                       header = dashboardHeaderPlus(
                         #####
                         # - To get settings modal to appear
                         tags$li(actionBttn("settingsModal", label = "Settings", icon = icon("gears"), style = "minimal", size = 'sm'),class = "dropdown")
                         #####
                       ),
                       
                       sidebar = dashboardSidebar(collapsed = TRUE,
                                                  #####  
                                                  sidebarMenu(
                                                    menuItem("War Room",        tabName = "WarRoom_tab",        icon = icon("fighter-jet")),
                                                    menuItem("List Analysis",   tabName = "ListAnalysis_tab",   icon = icon("group")), 
                                                    menuItem("Developer",       tabName = "Developer_tab",      icon = icon("code")))
                                                  #####
                       ), 
                       
                       body = dashboardBody(
                         
                         # Custom theme
                         customTheme,
                         
                         # - AFL style font
                         use_googlefont("Titillium Web"),
                         use_theme(create_theme(bs_vars_font(family_sans_serif = "Titillium Web"))),
                         
                         # - Logo in broser window
                         tags$head(tags$link(rel="shortcut icon", href= "https://i.ibb.co/C2KMp08/Sim-Logo.png")),
                         
                         # - Footer text & alignment
                         div(class = "sticky_footer", HTML(paste0("Version 1.2.9","&nbsp;","&nbsp;","&nbsp;","&nbsp;","&nbsp;","&nbsp;")), align = 'right'), 
                         
                         # - Link to CSS stylesheet
                         tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
                                   tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")), 
                         
                         
                         
                         ###############################
                         #       Settings Modal        #
                         ###############################
                         #####
                         bsModalNoClose(id = "Sett_modal", trigger = "settingsModal", "" ,  # Empty string is there for the header, just leave it. Trust me.
                                        
                                        
                                        fluidRow(
                                          tabBox(width = 12,id = "settingsTab", title = tagList(icon("gear"), "Sim Settings"),
                                                 
                                                 tabPanel(tagList(icon("hourglass-o"), "Timer Settings"),
                                                          
                                                          fluidRow(
                                                            br(),
                                                            materialSwitch(inputId = "autoDraft" , label = h5(tags$b("Autodraft ON/OFF"),br()), value = TRUE, status = "danger"),
                                                            h6(tags$b("ON:"),HTML('&nbsp;'),"If time expires & no selection is made, the next best available player will be autodrafted by the CPU.",br(),
                                                               tags$b("OFF:"),"If time expires & no selection is made, user is forced to make a selection.",br(),
                                                               HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;'),"*** no bid matching will be considered if", tags$b("ON")),
                                                            br()
                                                          ),
                                                          fluidRow(
                                                            column(5,
                                                                   sliderTextInput(inputId = "pickTime",label = h5(tags$b("Pick Time Limit")),grid = T, force_edges = T, choices = c("30 sec.", "1 min.", "2 min.", "3 min.", "4 min.", "5 min."), selected = "2 min."),
                                                                   h6("How much time user has to make each pick.",br(),
                                                                      "*** takes effect at the end of the current pick.")
                                                            ),
                                                            column(1),
                                                            column(5,
                                                                   sliderTextInput(inputId = "extraTime",label = h5(tags$b("Extra Time Given")),grid = T, force_edges = T, choices = c("20 Secs", "30 Secs", "1 min.", "2 min.")),
                                                                   h6("How much extra time is added when more time is called for.")
                                                            )
                                                          )
                                                          
                                                          
                                                          
                                                 ),
                                                 
                                                 tabPanel(tagList(icon("commenting-o"), "Trade Logic"),
                                                          
                                                          materialSwitch(inputId = "diffOnOff", status = "danger", label = fluidRow(column(12,HTML('&nbsp;','&nbsp;','&nbsp;',"Turn on trade logic & set difficulty - or leave it off.")))),
                                                          conditionalPanel(condition = "input.diffOnOff == true",
                                                                           
                                                                           socialBox(width = 12,
                                                                                     comments = tagList(
                                                                                       boxComment(
                                                                                         src = "https://freesvg.org/img/Donald-Trump-Head.png", 
                                                                                         title = HTML('&nbsp;','&nbsp;',"Don. (Easy)"),
                                                                                         date = fluidRow(column(12,offset = 9,materialSwitch(inputId = "easyDiff",status = 'info', value = F))),
                                                                                         fluidRow(column(12,HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','"This has been the worst trade deal in the history of trade deals.. maybe ever"')))
                                                                                       ),
                                                                                       boxComment(
                                                                                         src = "",
                                                                                         title = HTML('&nbsp;','&nbsp;'," (Medium)"),
                                                                                         date = fluidRow(column(12,offset = 9,materialSwitch(inputId = "medDiff",status = 'info', value = F))),
                                                                                         fluidRow(column(12,HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','"Im standing infront of a burning house, offering you fire insurance on it. '))),
                                                                                       ),
                                                                                       boxComment(
                                                                                         src = "https://cdn.vox-cdn.com/thumbor/qh7MvpLFRBgRDWI3ArTVBkgoOJQ=/143x0:555x275/1400x1400/filters:focal(143x0:555x275):format(jpeg)/cdn.vox-cdn.com/uploads/chorus_image/image/49145567/jonah-hill-plays-peter-brand-in-moneyball.0.0.jpg",
                                                                                         title = HTML('&nbsp;','&nbsp;',"Moneyball. (Hard)"),
                                                                                         date = fluidRow(column(12,offset = 9,materialSwitch(inputId = "hardDiff",status = 'info',value = F))),
                                                                                         fluidRow(column(12,HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','"When you get the answer youre looking for - hang up."'))),
                                                                                       ) # box comment
                                                                                     ) # taglist
                                                                           ) # socialbox
                                                          ) # conditional panel
                                                 ),
                                                 
                                                 tabPanel(tagList(icon("desktop"), "Social Feed"),
                                                          HTML(paste0("Choose which social media stream appears LIVE (in 'Social Feed' tab)",br(),
                                                                      "See what the guru's & twitter community have to say as the picks are called.")),
                                                          
                                                          socialBox(width = 12,
                                                                    comments = tagList(
                                                                      boxComment(
                                                                        src = "https://pbs.twimg.com/profile_images/919784515932794880/UocDziZa_400x400.jpg",
                                                                        title = HTML('&nbsp;','&nbsp;',"AFL.com.au"), 
                                                                        date = fluidRow(column(12,offset = 9,materialSwitch(inputId = "aflSwitch",status = 'info', value = T))),
                                                                        fluidRow(column(12,HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;',"AFL Media Twitter (@AFLcomau)")))
                                                                      ),
                                                                      boxComment(
                                                                        src = "https://pbs.twimg.com/profile_images/759905188199084032/mMM3Rr0w_400x400.jpg",
                                                                        title = HTML('&nbsp;','&nbsp;',"FoxFooty"), 
                                                                        date = fluidRow(column(12,offset = 9,materialSwitch(inputId = "foxSwitch",status = 'info', value = F))),
                                                                        fluidRow(column(12,HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;',"Network (@FOXFOOTY)")))
                                                                      ),
                                                                      boxComment(
                                                                        src = "https://pbs.twimg.com/profile_images/1085053691046985728/c3mWosEe_400x400.jpg",
                                                                        title = HTML('&nbsp;','&nbsp;',"Draft Central"), 
                                                                        date = fluidRow(column(12,offset = 9,materialSwitch(inputId = "draftctlSwitch",status = 'info', value = F))),
                                                                        fluidRow(column(12,HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;',"Platform (@DraftCentralAus)")))
                                                                      ),
                                                                      boxComment(
                                                                        src = "https://pbs.twimg.com/profile_images/1246728570031882240/5Eo82Z7e_400x400.jpg",
                                                                        title = HTML('&nbsp;','&nbsp;',"Cal Twomey"),
                                                                        date = fluidRow(column(12,offset = 9,materialSwitch(inputId = "twomeySwitch",status = 'info', value = F))),
                                                                        fluidRow(column(12,HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;',"Journalist (@CalTwomey)"))),                                                        
                                                                      ),
                                                                      boxComment(
                                                                        src = "https://files.whooshkaa.com/podcasts/podcast_6554/podcast_media/98eced-am-6558-trends-podcast-cover-fa_1_.jpg",
                                                                        title = HTML('&nbsp;','&nbsp;',"Marc McGowan"),
                                                                        date = fluidRow(column(12,offset = 9,materialSwitch(inputId = "mcgowanSwitch",status = 'info',value = F))),
                                                                        fluidRow(column(12,HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;',"Journalist (@ByMarcMcGowan)"))),
                                                                      ),
                                                                      boxComment(
                                                                        src = "https://pbs.twimg.com/profile_images/1009637773672787968/ujJQAgjl_400x400.jpg",
                                                                        title = HTML('&nbsp;','&nbsp;',"Lystics AFL"),
                                                                        date = fluidRow(column(12,offset = 9,materialSwitch(inputId = "lysticsSwitch",status = 'info',value = F))),
                                                                        fluidRow(column(12,HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;','&nbsp;',"Podcast (@LysticsAFL)"))),
                                                                      )
                                                                    ) # taglist
                                                          ) # socialbox
                                                 ) # tabpanel
                                          ) # tabbox
                                        ), # fluidrow
                                        
                                        fluidRow(
                                          column(12, align = "right", actionBttn(inputId = "closeSettings",label = "Accept Changes",color = 'primary',size = 'sm')))
                         ),
                         #####
                         
                         ###############################
                         #         Trade Modal         #
                         ###############################
                         #####
                         bsModal(id = "trade_modal", trigger = "trade_btn", size = 'large'  , "",
                                 
                                 column(12,offset = 7 ,selectizeInput(inputId = "tradeWith_btn" , label = "Select Team To Trade Current Pick With" , choices = c("",setdiff( c(unique(Teams_Data$Team)) , "Adelaide" )) , selected = c("",setdiff( c(unique(Teams_Data$Team)) , "Adelaide" ))[2]  ),
                                        
                                        br()),
                                 
                                 fluidRow(
                                   
                                   column(2, uiOutput("TradeModal_Picking_img") , align = 'left') , #,style = "text-align: right; float:right" 
                                   column(2, uiOutput("Picking") , align = 'right'), 
                                   
                                   column(4), 
                                   
                                   column(2, uiOutput("TradePartner") , align = 'left'), 
                                   column(2, uiOutput("TradeModal_Partner_img") ,align = 'right') , 
                                   
                                 ),
                                 
                                 column(12, #align = 'center',
                                        
                                        column(5, # -- COLUMN 1
                                               
                                               fluidRow( # LHS Row 1
                                                 column(width = 5, offset = 1, br(), textOutput("CurrentPick")), 
                                                 column(width = 5, offset = 1, selectizeInput(inputId = "Pick_2_A", label = "2nd Pick", choices = ""  , options = list(placeholder = 'Select Pick'))) ,
                                               ), 
                                               
                                               fluidRow( # LHS Row 2 
                                                 column(width = 5, offset = 1, selectizeInput(inputId = "Pick_3_A", label = "3rd Pick", choices = ""  , options = list(placeholder = 'Select Pick'))) ,
                                                 column(width = 5, offset = 1, selectizeInput(inputId = "Pick_4_A", label = "4th Pick", choices = ""  , options = list(placeholder = 'Select Pick'))) ,
                                               ),
                                               
                                        ),
                                        
                                        column(2, img(src = "https://i.ibb.co/nkSKk66/straight-vertical-line-png-8.png", width = '7%'), align = 'center') , 
                                        
                                        
                                        column(5, # -- COLUMN 2
                                               
                                               fluidRow(# RHS Row 1 
                                                 column(width = 5, selectizeInput(inputId = "Pick_1_B", label = "1st Pick", choices = ""  , options = list(placeholder = 'Select Pick'))) ,
                                                 column(width = 5, offset = 1,selectizeInput(inputId = "Pick_2_B", label = "2nd Pick", choices = ""  , options = list(placeholder = 'Select Pick'))) ,
                                               ),
                                               
                                               fluidRow( # RHS Row 2 
                                                 column(width = 5, selectizeInput(inputId = "Pick_3_B", label = "3rd Pick", choices = ""  , options = list(placeholder = 'Select Pick'))) ,
                                                 column(width = 5, offset = 1, selectizeInput(inputId = "Pick_4_B", label = "4th Pick", choices = ""  , options = list(placeholder = 'Select Pick'))) ,
                                               ),
                                               
                                        )
                                        
                                 ), # close big column (12)
                                 
                                 fluidRow(
                                   
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   
                                 ),
                                 
                                 uiOutput("TradeInterest"),
                                 
                                 fluidRow(
                                   column(12, align = 'right', actionBttn(inputId = 'submitTrade_btn', label = 'Submit Trade', icon("sync-alt", lib = "font-awesome") , style = 'unite' , size = 'sm',color = 'primary' ))
                                 )
                                 
                         ), # close bsModal
                         #####
                         
                         tabItems(tabItem(tabName = "WarRoom_tab" ,
                                          
                                          fluidRow(
                                            column(4,
                                                   uiOutput("CurrentlyPicking"),
                                            ),
                                            
                                            column(8,
                                                   tabItem(tabName = "topStillAvailable",
                                                           
                                                           verticalTabsetPanel(color = "#163C91", selected = "Best Still Available", menuSide = "right",
                                                                               
                                                                               verticalTabPanel(title = "Draft Board" , icon = icon("list"), 
                                                                                                
                                                                                                tabBox(width = 12,id = "settingsTab", 
                                                                                                       
                                                                                                       tabPanel(title = "2020", 
                                                                                                                dataTableOutput("DraftBoard")),
                                                                                                       
                                                                                                       tabPanel(title = "2021", 
                                                                                                                dataTableOutput("DraftBoard_Future")),
                                                                                                       
                                                                                                       tabPanel(title = "Event Log", 
                                                                                                                dataTableOutput("Transactions")
                                                                                                       )),
                                                                                                
                                                                                                style = "height:480px; overflow-y: auto; padding-left: 0px"),
                                                                               
                                                                               verticalTabPanel(title = "Best Still Available", icon = icon("sort-by-attributes-alt",lib = "glyphicon"), 
                                                                                                uiOutput("Top5")),
                                                                               
                                                                               verticalTabPanel(title =  "Social Feed", icon = icon(name = "twitter",lib = "font-awesome"), 
                                                                                                uiOutput("tweet"))
                                                                               
                                                           )
                                                   )
                                            )
                                          ),
                                          
                                          fluidRow(
                                            tabItem(tabName = "pickBoxes",
                                                    div(style = "overflow-x:scroll;",
                                                        lapply(2:19, function(i) {
                                                          div(class = "same-row",
                                                              uiOutput(paste0("userBox_", i)))
                                                        })))),
                                          
                         ) ,  # tabItem (War Room)
                         
                         
                         tabItem(tabName = "ListAnalysis_tab" , 
                                 
                                 
                                 
                         ), # #tabItem (Draft Board)
                         
                         tabItem(tabName = "Developer_tab" , 
                                 
                                 uiOutput("DeveloperBox"),
                                 uiOutput("DevText")
                                 
                         )
                         
                         ) # tabItem(s)
                       ), # dashboard body 
                       
                       title = "AFL Draft Simulator" , 
                       
) # close ui






server <- function(input, output, session) {
  
  ###############################
  #        App Launch Vars      #
  ###############################  
  
  # Counter (All powerful - controls basically everything)
  counter  <- reactiveValues(countervalue = 1)
  
  # Trade Logic Default Value (101 = Logic OFF - all trades will accept)
  thresh <- reactiveValues(hold = -101) 
  
  # These vars control the MaterialSwitch() (used to make sure the switch that on remains on when user re-opens tab)
  #####
  # Difficulty Settings
  onoffVar <- reactiveValues(status = FALSE)
  easyVar  <- reactiveValues(status = FALSE)
  medVar   <- reactiveValues(status = FALSE)
  hardVar  <- reactiveValues(status = FALSE)
  # Twitter users
  afl      <- reactiveValues(status = FALSE)
  fox      <- reactiveValues(status = FALSE)
  draftctl <- reactiveValues(status = FALSE)
  twomey   <- reactiveValues(status = FALSE)
  mcgowan  <- reactiveValues(status = FALSE)
  lystics  <- reactiveValues(status = FALSE)
  #####
  
  # Variable for pick time length (set to 120 as default, because the default selection is '2 min.')
  ## - 120 only hardcoded here as a default setting upon opening app - dynamic from there out
  pick  <- reactiveValues(time = 120)
  
  # Variable for updating the sliderInput inside close button and keeping the slider at the time the user set
  pickvar <- reactiveValues(val = "2 min.")
  
  # Variable for extra time length (set to 20 as default)
  ## - 20 only hardcoded here as a default setting upon opening app - dynamic from there out
  moreTime  <- reactiveValues(time = 20)
  
  # Variable for updating the sliderInput inside close button and keeping the slider at the time the user set
  pickvar <- reactiveValues(val = "2 min.")
  
  # Base time - when user changes pick time input and time expires, this is the time it resets to (changes with IF/ELSE's) - pickVar$val decrements so cant use
  ## - 120 only hardcoded here as a default setting upon opening app - dynamic from there out
  base <- reactiveValues(time = 120)
  
  # This is for the Twitter URL - set AFL.com.au as default
  twitter <- reactiveValues(URL = "https://twitter.com/aflcomau")
  
  # - Open the bsModal() trade model when *trade_btn* is pushed (super important)
  observeEvent(input$trade_btn, {
    toggleModal(session, "trade_modal", "open")
  })
  
  # tmp storage table
  Selected_Table_tmp <- data.frame("Pick" = numeric() , "Team" = character() , "Player" = character())
  
  # Display the team currently on the clock (Upon launching app) 
  output$OnTheClock_Team <- renderText({HTML(paste0("<b>","On The Clock: ","</b>", Draft_Order[counter$countervalue,3]))}) 
  
  # Switches (these control the functionality of making sure never more than one switch is on at a time)
  #####
  
  onclick("easyDiff", {
    updateMaterialSwitch(session, "medDiff", value = F)
    updateMaterialSwitch(session, "hardDiff", value = F)
    updateMaterialSwitch(session, "diffOnOff", value = T)
  })
  
  onclick("medDiff",{
    updateMaterialSwitch(session, "easyDiff", value = F)
    updateMaterialSwitch(session, "hardDiff", value = F)
    updateMaterialSwitch(session, "diffOnOff", value = T)
  })
  
  onclick("hardDiff",{
    updateMaterialSwitch(session, "easyDiff", value = F)
    updateMaterialSwitch(session, "medDiff", value = F)
    updateMaterialSwitch(session, "diffOnOff", value = T)
  })
  
  onclick("diffOnOff",{
    updateMaterialSwitch(session, "easyDiff", value = F)
    updateMaterialSwitch(session, "medDiff" , value = F)
    updateMaterialSwitch(session, "hardDiff", value = F)
  })
  
  ### - Twitter - ###
  
  # Switches - Twitter
  onclick("aflSwitch", { # aflSwitch foxSwitch draftctlSwitch twomeySwitch mcgowanSwitch lysticsSwitch
    updateMaterialSwitch(session, "foxSwitch", value = F)
    updateMaterialSwitch(session, "draftctlSwitch", value = F)
    updateMaterialSwitch(session, "twomeySwitch", value = F)
    updateMaterialSwitch(session, "mcgowanSwitch", value = F)
    updateMaterialSwitch(session, "lysticsSwitch", value = F)
  })
  onclick("foxSwitch",{
    updateMaterialSwitch(session, "aflSwitch", value = F)
    updateMaterialSwitch(session, "draftctlSwitch", value = F)
    updateMaterialSwitch(session, "twomeySwitch", value = F)
    updateMaterialSwitch(session, "mcgowanSwitch", value = F)
    updateMaterialSwitch(session, "lysticsSwitch", value = F)
  })
  onclick("draftctlSwitch",{
    updateMaterialSwitch(session, "foxSwitch", value = F)
    updateMaterialSwitch(session, "aflSwitch", value = F)
    updateMaterialSwitch(session, "twomeySwitch", value = F)
    updateMaterialSwitch(session, "mcgowanSwitch", value = F)
    updateMaterialSwitch(session, "lysticsSwitch", value = F)
  })
  onclick("twomeySwitch", {
    updateMaterialSwitch(session, "foxSwitch", value = F)
    updateMaterialSwitch(session, "aflSwitch", value = F)
    updateMaterialSwitch(session, "draftctlSwitch", value = F)
    updateMaterialSwitch(session, "mcgowanSwitch", value = F)
    updateMaterialSwitch(session, "lysticsSwitch", value = F)
  })
  onclick("mcgowanSwitch",{
    updateMaterialSwitch(session, "foxSwitch", value = F)
    updateMaterialSwitch(session, "aflSwitch", value = F)
    updateMaterialSwitch(session, "twomeySwitch", value = F)
    updateMaterialSwitch(session, "draftctlSwitch", value = F)
    updateMaterialSwitch(session, "lysticsSwitch", value = F)
  })
  onclick("lysticsSwitch",{
    updateMaterialSwitch(session, "foxSwitch", value = F)
    updateMaterialSwitch(session, "aflSwitch", value = F)
    updateMaterialSwitch(session, "twomeySwitch", value = F)
    updateMaterialSwitch(session, "mcgowanSwitch", value = F)
    updateMaterialSwitch(session, "draftctlSwitch", value = F)
  })
  #####
  
  # - Modal that appears upon launching app
  ##### 
  showModal(modalDialog(
    size = 'l',
    easyClose = F , 
    
    fluidRow(
      
      column(12, align="center",
             div(style="display: inline-block;",img(src = "https://websites.sportstg.com/pics/00/01/61/66/1616625_1_O.jpg" , height=110, width=200)),
             div(style="display: inline-block;",img(src = "https://i.ibb.co/KyWFW0w/Screen-Shot-2020-09-27-at-12-16-01-pm.png", height=205 , width=320)),
             div(style="display: inline-block;",img(src = "https://1000logos.net/wp-content/uploads/2018/07/AFL-Logo.png" , height=115, width=200))
      ),
      
      fluidRow(
        column(12,align = 'center',
               HTML(paste0(h1(tags$b("Welcome to the 2020 AFL Draft Simulator")), "\n",
                           br(),
                           br(),
                           column(12 , align = 'left',                   
                                  h4(HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;',paste0("• Simulate the entire 2020 draft yourself including bids & trades."))) , "\n",
                                  h4(HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;',paste0("• Head to  ", tagList(icon("gears"), "Settings  "), "in the top right corner to adjust the simulation options."))) , "\n" , 
                                  h4(HTML('&nbsp;','&nbsp;','&nbsp;','&nbsp;',paste0("• Keep track of the draft, which players still remain & the live twitter reactions to each pick as it falls."))) , "\n" ,
                                  br(),
                           )
               )
               )
        )
      ),
      
      
    ),
    
    footer = actionBttn(inputId = "closeDrafted",label = "Close",color = 'primary',size = 'sm')))
  #####
  
  # - Modal for settings
  #####
  observeEvent(input$closeSettings, { 
    
    # This closes the bsModal Settings modal when close button in that modal is pushed
    toggleModal(session, modalId = "Sett_modal", toggle = "close")   
    
    # This if else determines whats actually been turned On/Off by the user when they click the close button (closeSettings) & exit (easyClose = F so cant bypass)
    # - Also ensures when open back up the settings, the switches are on/off as they left them!  
    
    # - Pick Time IF/ELSE
    if (input$pickTime == "30 sec."){
      pickvar$val <- "30 sec."
    } else if (input$pickTime == "1 min.") {
      pickvar$val <- "1 min."
    } else if (input$pickTime == "2 min.") {
      pickvar$val <- "2 min."
    } else if (input$pickTime == "3 min.") {
      pickvar$val <- "3 min."
    } else if (input$pickTime == "4 min.") {
      pickvar$val <- "4 min."
    } else {
      pickvar$val <- "5 min."
    }
    
    # - Trade Logic IF/ELSE
    #####
    
    ### - these control the range of possible outcomes when user plays with ON/OFF & Difficulty switches
    # - (outcomes: ON w/ Easy, Med or Hard Selected, ON with nothing selected (revert to default), OFF// )
    
    # If the trade button logic ON:
    if (input$diffOnOff == T & input$easyDiff == T) {
      
      onoffVar$status <- TRUE
      
      easyVar$status <- TRUE
      medVar$status  <- FALSE
      hardVar$status <- FALSE
      thresh$hold <- -5
      
    } else if (input$diffOnOff == T & input$medDiff == T) { 
      
      onoffVar$status <- TRUE
      
      easyVar$status <- FALSE
      medVar$status  <- TRUE
      hardVar$status <- FALSE
      thresh$hold <- 10
      
    } else if (input$diffOnOff == T & input$hardDiff == T) {
      
      onoffVar$status <- TRUE
      
      easyVar$status <- FALSE
      medVar$status  <- FALSE
      hardVar$status <- TRUE
      thresh$hold <- 25
      
    } else if (input$diffOnOff == F) {
      
      onoffVar$status <- FALSE
      
      easyVar$status  <- FALSE
      medVar$status   <- FALSE
      hardVar$status  <- FALSE
      thresh$hold <- -101
      
      # If the trade button logic ON but nothing selected:   
    } else if  (input$diffOnOff == T & input$easyDiff == F & input$medDiff == F & input$hardDiff == F) {
      
      onoffVar$status <- FALSE
      
      easyVar$status  <- FALSE
      medVar$status   <- FALSE
      hardVar$status  <- FALSE
      thresh$hold <- -101
      
      updateMaterialSwitch(session, "easyDiff", value = F)
      updateMaterialSwitch(session, "medDiff", value = F)
      updateMaterialSwitch(session, "hardDiff", value = F)
      updateMaterialSwitch(session, "diffOnOff", value = F)
      
    }
    
    #####
    
    # - Twitter Choice IF/ELSE
    #####
    
    if (input$aflSwitch == T) {
      afl$status      <- T
      fox$status      <- FALSE
      draftctl$status <- FALSE
      twomey$status   <- FALSE
      mcgowan$status  <- FALSE
      lystics$status  <- FALSE
      twitter$URL <- "https://twitter.com/aflcomau"
    } else if (input$foxSwitch == T) {
      afl$status      <- FALSE
      fox$status      <- T
      draftctl$status <- FALSE
      twomey$status   <- FALSE
      mcgowan$status  <- FALSE
      lystics$status  <- FALSE
      twitter$URL <- "https://twitter.com/FOXFOOTY"
    } else if (input$draftctlSwitch == T) { 
      afl$status      <- FALSE
      fox$status      <- FALSE
      draftctl$status <- T
      twomey$status   <- FALSE
      mcgowan$status  <- FALSE
      lystics$status  <- FALSE
      twitter$URL <- "https://twitter.com/DraftCentralAus"
    } else if (input$twomeySwitch == T) { 
      afl$status      <- FALSE
      fox$status      <- FALSE
      draftctl$status <- FALSE
      twomey$status   <- T
      mcgowan$status  <- FALSE
      lystics$status  <- FALSE
      twitter$URL <- "https://twitter.com/CalTwomey"
    } else if (input$mcgowanSwitch == T) { 
      afl$status      <- FALSE
      fox$status      <- FALSE
      draftctl$status <- FALSE
      twomey$status   <- FALSE
      mcgowan$status  <- T
      lystics$status  <- FALSE
      twitter$URL <- "https://twitter.com/ByMarcMcGowan"
    } else { # (Lystics AFL)
      afl$status      <- FALSE
      fox$status      <- FALSE
      draftctl$status <- FALSE
      twomey$status   <- FALSE
      mcgowan$status  <- FALSE
      lystics$status  <- T
      twitter$URL <- "https://twitter.com/LysticsAFL"
    }   
    
    #####
    
    removeModal()
  })
  #####
  
  # - For Trade Options Modal (1 here, 1 in trade_btn section)
  ##### 
  # Display Trade Partner in trade modal
  output$TradePartner <- reactive({ input$tradeWith_btn })
  output$Picking <- reactive({ Draft_Order[counter$countervalue,3] %>% pull() })
  
  output$TradeModal_Picking_img <- renderUI({
    img(src = Teams_Data %>% filter(Team == Draft_Order[counter$countervalue,3] %>% pull()) %>% pull(TeamLogo), width = '100%')
  })
  
  output$TradeModal_Partner_img <- renderUI({
    img(src = Teams_Data %>% filter(Team == input$tradeWith_btn) %>% pull(TeamLogo), width = '100%')
  })
  #####
  
  # Next 20 Picks widgetUserBox()'s *upon launch*
  ##### 
  
  output$CurrentlyPicking <- renderUI({
    
    widgetUserBox(width = 12, height = 'auto',
                  collapsible = F,
                  type = NULL,
                  boxToolSize = 'xs' ,
                  src = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue,3])) %>% select(Circle) %>% pull() ,
                  background = T,
                  backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue,3])) %>% select(BannerURL_2) %>% pull() ,
                  column(12,align = 'center',
                         br(),
                         br(),
                         pickerInput('Player_Select',   choices = sort(Draftee_Data %>% pull(Player)) , choicesOpt = list(subtext = Draftee_Data %>% arrange(Player) %>% mutate(Paste = ifelse(!is.na(Ties) , paste0(" (",Ties,")") , paste0(""))) %>% pull(Paste)) , options = list(title = 'Select Player To Draft Here' , `live-search` = TRUE) ),
                         
                         HTML(paste0(h2("Pick: ",counter$countervalue))) ,
                         HTML(paste0(h2(tags$b(tags$u("On The Clock:"))," ",Draft_Order[counter$countervalue,3] %>% pull()))),
                         tags$br(),
                         fluidRow(
                           column(1,offset = 4,  
                                  textOutput("timeleft") , 
                           ),
                           column(6,
                                  dropdown(width = '260px', 
                                           
                                           tags$h4("Timer Controls:"),
                                           fluidRow(
                                             actionBttn('start','Start', style = 'bordered', color = 'primary', size = 'sm'),
                                             actionBttn('stop' ,'Pause', style = 'bordered', color = 'primary', size = 'sm'),
                                             actionBttn('reset','Reset', style = 'bordered', color = 'primary', size = 'sm'),
                                             br(),
                                             h6("These can be adjusted via",tagList(icon("hourglass-o")),"in Settings"),
                                           ),
                                           style = "unite", icon = icon("gear"), size = 'xs', up = T, 
                                           status = "primary",
                                           animate = animateOptions(
                                             exit = animations$fading_exits$fadeOutRightBig
                                           )
                                  )
                           )  
                         ),
                         
                         tags$br(),
                         column(width = 6,  actionBttn('draft_btn', 'DRAFT', icon("crosshairs", lib = "font-awesome"), style = "unite", color = 'primary', size = 'm', block = T )),
                         column(width = 6,  actionBttn('pass_btn' , 'PASS' , icon("hand-paper", lib = "font-awesome"), style = "unite", color = 'primary', size = 'm', block = T )),
                         column(width = 12, actionBttn('trade_btn', 'TRADE', icon("handshake" , lib = "font-awesome"), style = 'unite', color = 'primary', size = 'm', block = T)),
                         
                  ),
                  
    ) # close userbox
  })
  
  output$userBox_2 <- renderUI({
    widgetUserBox( # 2
      title = paste0("Pick ", counter$countervalue+1,".") , 
      subtitle = paste0(Draft_Order[1+1,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+1] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+1,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+1,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+1,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  }) 
  
  output$userBox_3 <- renderUI({
    widgetUserBox( # 3
      title = paste0("Pick ", counter$countervalue+2,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+2,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+2] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+2,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F, 
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+2,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+2,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_4 <- renderUI({
    widgetUserBox( # 4
      title = paste0("Pick ", counter$countervalue+3,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+3,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+3] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+3,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+3,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+3,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_5 <- renderUI({
    widgetUserBox( # 5
      title = paste0("Pick ", counter$countervalue+4,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+4,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+4] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+4,3])) %>% select(BannerURL) %>% pull() , 
      color = 'green',
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+4,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+4,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    ) 
  })
  
  output$userBox_6 <- renderUI({
    widgetUserBox( # 6
      title = paste0("Pick ", counter$countervalue+5,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+5,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+5] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+5,3])) %>% select(BannerURL) %>% pull() , 
      color = 'green',
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+5,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+5,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_7 <- renderUI({
    widgetUserBox( # 7
      title = paste0("Pick ", counter$countervalue+6,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+6,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+6] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+6,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+6,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+6,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )  
  }) 
  
  output$userBox_8 <- renderUI({
    widgetUserBox( # 8
      title = paste0("Pick ", counter$countervalue+7,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+7,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+7] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+7,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+7,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+7,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_9 <- renderUI({
    widgetUserBox( # 9
      title = paste0("Pick ", counter$countervalue+8,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+8,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+8] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+8,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+8,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+8,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_10 <- renderUI({
    widgetUserBox( # 10
      title = paste0("Pick ", counter$countervalue+9,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+9,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+9] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+9,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+9,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+9,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_11 <- renderUI({
    widgetUserBox( # 11
      title = paste0("Pick ", counter$countervalue+10,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+10,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+10] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+10,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+10,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+10,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_12 <- renderUI({
    widgetUserBox( # 12
      title = paste0("Pick ", counter$countervalue+11,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+11,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+11] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+11,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+11,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+11,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })  
  
  output$userBox_13 <- renderUI({
    widgetUserBox( # 13
      title = paste0("Pick ", counter$countervalue+12,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+12,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+12] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+12,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+12,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+12,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_14 <- renderUI({
    widgetUserBox( # 14
      title = paste0("Pick ", counter$countervalue+13,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+13,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+13] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+13,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+13,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+13,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_15 <- renderUI({  
    widgetUserBox( # 15
      title = paste0("Pick ", counter$countervalue+14,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+14,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+14] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+14,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+14,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+14,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_16 <- renderUI({
    widgetUserBox( # 16
      title = paste0("Pick ", counter$countervalue+15,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+15,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+15] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+15,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+15,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+15,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_17 <- renderUI({  
    widgetUserBox( # 17
      title = paste0("Pick ", counter$countervalue+16,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+16,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+16] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+16,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+16,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+16,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_18 <- renderUI({  
    widgetUserBox( # 18
      title = paste0("Pick ", counter$countervalue+17,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+17,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+17] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+17,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+17,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+17,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_19 <- renderUI({  
    widgetUserBox( # 19
      title = paste0("Pick ", counter$countervalue+18,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+18,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+18] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+18,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+18,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+18,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$userBox_20 <- renderUI({  
    widgetUserBox( # 20
      title = paste0("Pick ", counter$countervalue+19,".") , 
      subtitle = paste0(Draft_Order[counter$countervalue+19,3]) ,
      type = 2,
      width = 12,
      src = paste0(Draft_Order$Round[counter$countervalue+19] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
      background = T,
      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+19,3])) %>% select(BannerURL) %>% pull() , 
      closable = F,
      collapsible = F,
      footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+19,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+19,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
    )
  })
  
  output$DeveloperBox <- renderUI({
    widgetUserBox(
      title = "Aaron Brougham" , 
      subtitle = tags$div(tags$b("Developer"), tags$br(), tags$br(), tags$br(), paste0("Data person. Negroni drinker. Draft enthusiast.")) ,
      type = 2,
      width = 5,
      src = "https://media-exp1.licdn.com/dms/image/C5603AQGpJc9CFcQZWA/profile-displayphoto-shrink_400_400/0?e=1605744000&v=beta&t=_2-qHz20bzMrg3QGG_h3GeEE3sE1z08HpqkUrnDjxgM" ,
      color = 'danger',
      closable = F,
      collapsible = F,
      socialButton(
        url = "https://twitter.com/abrougham1",
        type = 'twitter'
      ),
      socialButton(
        url = "https://www.linkedin.com/in/aaronbrougham/",
        type = 'linkedin'
      ),
      socialButton(
        url = "http://github.com",
        type = "github"
      )
    )
  })
  
  
  output$DevText <- renderUI({
    
    box(width = 9,
        tags$div(h3("Dev Notes")),
        tags$b(h6("(Version 1.2.9)")),
        tags$br(),
        
        tags$div("Bug Fixes:"),
        tags$code("•"),
        tags$br(),
        #tags$br(),
        tags$code("•"),
        tags$br(),
        #tags$br(),
        tags$code("•") ,
        tags$br(),
        #tags$br(),
        tags$code("•"),
        tags$br(),
        tags$br(),
        
        tags$div("Updates:"),
        tags$code("•"),
        tags$br(),
        tags$br(),
        tags$code("•"),
        tags$br(),
        tags$br(),
        tags$code("•"),
        tags$br(),
        tags$br(),
        tags$code("•"),
        tags$br(),
    )
    
  })
  
  
  #####
  
  # Top Still Available boxes
  #####
  
  Draftee_Data <- Draftee_Data %>% arrange(Projected)
  Top_P1 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[1] 
  Top_P2 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[2]
  Top_P3 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[3] 
  Top_P4 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[4] 
  Top_P5 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[5]
  Top_P6 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[6] 
  
  output$Top5 <- renderUI({
    
    productList(
      
      productListItem( # Top: 1 
        src = Draftee_Data %>% filter(Player == Top_P1) %>% pull(Image),
        productTitle = tags$a(href="https://www.afl.com.au/draft/prospects/jamarra-ugle-hagan", tags$b(Top_P1), target="_blank"),
        Draftee_Data %>% filter(Player == Top_P1) %>% pull(Position), br(),
        Draftee_Data %>% filter(Player == Top_P1) %>% pull(Club), br(),
        productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
        priceColor   = "success",
      ),
      
      productListItem( # Top: 2 
        src = Draftee_Data %>% filter(Player == Top_P2) %>% pull(Image),
        productTitle = Top_P2,
        Draftee_Data %>% filter(Player == Top_P2) %>% pull(Position), br(),
        Draftee_Data %>% filter(Player == Top_P2) %>% pull(Club), br(),
        productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
        priceColor   = "success",
      ),
      
      productListItem( # Top: 3 
        src = Draftee_Data %>% filter(Player == Top_P3) %>% pull(Image),
        productTitle = Top_P3,
        Draftee_Data %>% filter(Player == Top_P3) %>% pull(Position), br(),
        Draftee_Data %>% filter(Player == Top_P3) %>% pull(Club), br(),
        productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
        priceColor   = "success",
      ),
      
      productListItem( # Top: 4 
        src = Draftee_Data %>% filter(Player == Top_P4) %>% pull(Image),
        productTitle = Top_P4,
        Draftee_Data %>% filter(Player == Top_P4) %>% pull(Position), br(),
        Draftee_Data %>% filter(Player == Top_P4) %>% pull(Club), br(),
        productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
        priceColor   = "success",
      ),
      
      productListItem( # Top: 5 
        src = Draftee_Data %>% filter(Player == Top_P5) %>% pull(Image),
        productTitle = Top_P5,
        Draftee_Data %>% filter(Player == Top_P5) %>% pull(Position), br(),
        Draftee_Data %>% filter(Player == Top_P5) %>% pull(Club), br(),
        productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
        priceColor   = "success",
      ),
      
      productListItem( # Top: 5 
        src = Draftee_Data %>% filter(Player == Top_P6) %>% pull(Image),
        productTitle = Top_P6,
        Draftee_Data %>% filter(Player == Top_P6) %>% pull(Position), br(),
        Draftee_Data %>% filter(Player == Top_P6) %>% pull(Club), br(),
        productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
        priceColor   = "success",
      ),
    ) # productList()
    
  })
  #####
  
  # - Output Draftboard Table (Upon launching app)
  #####    
  output$DraftBoard <- DT::renderDataTable({ 
    
    datatable(data = Draft_Order %>% inner_join(. , Teams_Data[c(1,5)] , by = c("Actual_Pick" = "Team")) %>% 
                mutate(Team = paste0(Actual_Pick,"<img src=","'",TeamLogo,"'"," ", "height='40'style=float:right></img>")) %>% select(Round,Pick,Team,Points) %>% left_join(.,Selected_Table[c(1,3)], by = c("Pick"="Pick")) %>% select(Round,Pick,Team,Player,Points) ,  
              escape = F , 
              # one of these below options makes Club name go over one line only! Important!
              class = "row-bordered hover stripe nowrap order-column" , 
              rownames = FALSE,
              options  = list(dom = "t",
                              columnDefs = list(list(className = 'dt-center', targets = "_all")),
                              paging = F,
                              searching = F,
                              scrollX = F, # 'background-color', '#002651'; 'border-bottom-left-radius','5px'; 
                              info = F, 
                              headerCallback = JS(
                                "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(3).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(4).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(3).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(4).css('color', 'white')
}"))) %>%
      formatStyle(
        "Pick" ,
        color = styleEqual(counter$countervalue,"white") ,
        backgroundColor = styleEqual(counter$countervalue,"#163C91"))
    
    
    
  }) # close renderDataTable()
  #####
  
  # - Output *FUTURE* Draftboard Table (Upon launching app)
  #####    
  output$DraftBoard_Future <- DT::renderDataTable({ 
    
    datatable(data = Draft_Order_Future %>% inner_join(. , Teams_Data[c(1,5)] , by = c("Actual_Pick" = "Team")) %>% 
                mutate(Team = paste0(Actual_Pick,"<img src=","'",TeamLogo,"'"," ", "height='40'style=float:right></img>")) %>% select(Round,Pick,Team,Points) ,
              escape = F , 
              # one of these below options makes Club name go over one line only! Important!
              class = "row-bordered hover stripe nowrap order-column" , 
              rownames = FALSE,
              options  = list(dom = "t",
                              columnDefs = list(list(className = 'dt-center', targets = "_all")),
                              paging = F,
                              searching = F,
                              scrollX = F, # 'background-color', '#002651'; 'border-bottom-left-radius','5px'; 
                              info = F, 
                              headerCallback = JS(
                                "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(3).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(4).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(3).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(4).css('color', 'white')}"))) 
    
    
    
  }) # close renderDataTable()
  #####
  
  
  #           #
  ############# - End of app launching stuff    
  #           #
  
  ###############################
  #        Twitter Feed         #
  ###############################
  ##### 
  output$tweet <- renderUI({
    
    # Generate twitter feed (May need to repeat this black of code to get it to re-run at different points??)
    column(width=12, box(width = 12,twitterTimeline(twitter$URL)),style = "height:475px; overflow-y: auto;")
    
  })
  #####
  
  ###############################
  #         Pass Button         #
  ###############################
  observeEvent(input$pass_btn, {
    
    # - Pick Time IF/ELSE *pass button*
    if (input$pickTime == "30 sec."){
      pick$time <- 30
      base$time <- 30
      pickvar$val <- "30 sec."
    } else if (input$pickTime == "1 min.") {
      pick$time <- 60   
      base$time <- 60
      pickvar$val <- "1 min."
    } else if (input$pickTime == "2 min.") {
      pick$time <- 120
      base$time <- 120
      pickvar$val <- "2 min."
    } else if (input$pickTime == "3 min.") {
      pick$time <- 180  
      base$time <- 180
      pickvar$val <- "3 min."
    } else if (input$pickTime == "4 min.") {
      pick$time <- 240  
      base$time <- 240
      pickvar$val <- "4 min."
    } else {
      pick$time <- 300
      base$time <- 300
      pickvar$val <- "5 min."
    }
    
    
    
    showModal(modalDialog(
      easyClose = T , 
      fluidRow(      
        column(12, align="center",
               div(style="display: inline-block;",img(src = "https://i.ibb.co/J5y4ptV/Screen-Shot-2020-09-30-at-10-59-27-pm.png", height=230, width=340))),
        br(),
        br(),
        br(),
        br(),
        
      ),
      
      
      h3(paste0(Draft_Order[counter$countervalue,3]," have passed on Pick ",counter$countervalue,".")) ,
      br(),
      paste0(Draft_Order[counter$countervalue+1,3]," are now on the clock with Pick ",counter$countervalue+1,"."),
      
      footer = actionBttn(inputId = "closePassed",label = "Close",color = 'primary',size = 'sm')
      
    ))
    
    # Building the table containing the selected players one row at a time
    Selected_Table_tmp <- data.frame("Pick" = numeric() , "Team" = character() , "Player" = character())
    Selected_Table_tmp[1,1] <- counter$countervalue
    Selected_Table_tmp[1,2] <- Draft_Order[counter$countervalue,3] %>% pull() 
    Selected_Table_tmp[1,3] <- "PASS"
    
    # Binding tmp table (Selected_Table_tmp) to master (Selected_Table)
    Selected_Table_tmp <- as.data.frame(Selected_Table_tmp)
    Selected_Table <<- as.data.frame(rbind(Selected_Table,Selected_Table_tmp))
    Selected_Table <<- na.omit(Selected_Table)
    
    # - Output Draftboard Table *pass_btn*      
    #####    
    output$DraftBoard <- DT::renderDataTable({ 
      
      datatable(data = Draft_Order %>% inner_join(. , Teams_Data[c(1,5)] , by = c("Actual_Pick" = "Team")) %>% 
                  mutate(Team = paste0(Actual_Pick,"<img src=","'",TeamLogo,"'"," ", "height='40'style=float:right></img>")) %>% select(Round,Pick,Team,Points) %>% left_join(.,Selected_Table[c(1,3)], by = c("Pick"="Pick")) %>% select(Round,Pick,Team,Player,Points) ,  
                escape = F , 
                # one of these below options makes Club name go over one line only! Important!
                class = "row-bordered hover stripe nowrap order-column" , 
                rownames = FALSE,
                options  = list(dom = "t",
                                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                paging = F,
                                searching = F,
                                scrollX = F, # 'background-color', '#002651'; 'border-bottom-left-radius','5px'; 
                                info = F, 
                                headerCallback = JS(
                                  "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(3).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(4).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(3).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(4).css('color', 'white')
}"))) %>%
        formatStyle(
          "Pick" ,
          color = styleEqual(counter$countervalue,"white") ,
          backgroundColor = styleEqual(counter$countervalue,"#163C91"))
      
      
      
    }) # close renderDataTable()
    #####
    
    pick$time <- base$time
    active(TRUE)
    
    counter$countervalue <- counter$countervalue + 1
    
  })
  # - (end) Pass Button
  
  ################################
  #          Draft Button        #
  ################################
  observeEvent(input$draft_btn, {
    
    # Condition to flag modal if no player is selected
    if ( input$Player_Select == "" ) {
      
      showModal(modalDialog(title = HTML(paste("<b>","Error: ","</b>","you haven't selected a player.")),
                            footer = actionBttn(inputId = "errorClose",label = "My bad, fam.",color = 'primary',size = 'sm'),
                            br(),
                            
                            img(src="https://thumbs.gfycat.com/UnfinishedImpeccableCollardlizard-size_restricted.gif",),
                            
                            br(),
                            br(),
                            easyClose = T
      ))
      
      # TIED PLAYERS - condition to eval. if player is tied to a team ()
    } else if ( Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Ties) %in% Teams_Data$Team == T  &  Draft_Order[counter$countervalue,3] %>% pull() != Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Ties) ) {
      
      # Bid Options Modal 
      #####
      showModal(modalDialog(
        easyClose = F , 
        
        fluidRow(      
          
          column(12, align="center",
                 div(style="display: inline-block;",img(src = Teams_Data %>% filter(Team == Draft_Order[counter$countervalue,3] %>% pull()) %>% pull(TeamLogo), height=115, width=140)),
                 div(style="display: inline-block;",img(src = "https://i.ibb.co/wzpkSyn/Bid-Pending-Graphic.png", height=175, width=250)),
                 div(style="display: inline-block;",img(src = Teams_Data %>% filter(Team == Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Ties)) %>% pull(TeamLogo), height=115, width=140)))
        ),
        
        br(),
        br(),
        
        HTML(paste0(h4(tags$u(Draft_Order[counter$countervalue,3] %>% pull())," have bid on ", tags$b(input$Player_Select), " with Pick ", counter$countervalue,".")) ,
             paste0(h4(input$Player_Select ," is tied to ", tags$u(Draftee_Data %>% filter(Player == input$Player_Select ) %>% pull(Ties)), " " , Draftee_Data %>% filter(Player == input$Player_Select ) %>% pull(Category) ,".")),
             paste0(h4(" Would you like to match the bid for ", input$Player_Select  , " as " , Draftee_Data %>% filter(Player == input$Player_Select ) %>% pull(Ties)," ?"))) ,
        
        br(),
        br(),
        footer = tagList(actionBttn(inputId = "yesMatch",label = "Match Bid",color = 'success',size = 'sm') , actionBttn(inputId = "noMatch", label = "Dont Match", color = 'danger',size = 'sm'))))
      #####
      
      # - +1 on counter not necessary here - do it in the yesMatch or noMatch buttons  
      
      # UN-TIED PLAYERS - condition to eval. if player isnt tied to a team. *OR* If the team player is tied to is the team that chose them
    } else if ( is.na( Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Ties) ) == T | Draft_Order[counter$countervalue,3] %>% pull() == Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Ties) ) {
      
      # Building the table containing the selected players one row at a time
      Selected_Table_tmp[1,1] <- counter$countervalue
      Selected_Table_tmp[1,2] <- Draft_Order[counter$countervalue,3] %>% pull() 
      Selected_Table_tmp[1,3] <- input$Player_Select
      
      # Binding tmp table (Selected_Table_tmp) to master (Selected_Table)
      Selected_Table_tmp <- as.data.frame(Selected_Table_tmp)
      Selected_Table <<- as.data.frame(rbind(Selected_Table,Selected_Table_tmp))
      Selected_Table <<- na.omit(Selected_Table)
      
      # This is the updated player pool that doesnt include the players that have been drafted
      UpdatedPool <- reactive({ sort(c(setdiff(Draftee_Data$Player , Selected_Table$Player))) })
      
      # This is updating the select input dropdown with the updated player pool (UpdatedPool())
      updatePickerInput(session , inputId = "Player_Select" , choices = UpdatedPool() , choicesOpt = list(subtext = UpdatedPool() %>% as.data.frame() %>% inner_join(. , Draftee_Data[c(1,9)] , by = c("." = "Player")) %>% mutate(Paste = ifelse(!is.na(Ties) , paste0("(",Ties,")") , paste0(""))) %>% pull(Paste)) , selected = "" ) 
      
      # - Output Draftboard Table (subject to being moved) *still inside draft_btn*      
      #####    
      output$DraftBoard <- DT::renderDataTable({ 
        
        datatable(data = Draft_Order %>% inner_join(. , Teams_Data[c(1,5)] , by = c("Actual_Pick" = "Team")) %>% 
                    mutate(Team = paste0(Actual_Pick,"<img src=","'",TeamLogo,"'"," ", "height='40'style=float:right></img>")) %>% select(Round,Pick,Team,Points) %>% left_join(.,Selected_Table[c(1,3)], by = c("Pick"="Pick")) %>% select(Round,Pick,Team,Player,Points) ,  
                  escape = F , 
                  # one of these below options makes Club name go over one line only! Important!
                  class = "row-bordered hover stripe nowrap order-column" , 
                  rownames = FALSE,
                  options  = list(dom = "t",
                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                  paging = F,
                                  searching = F,
                                  scrollX = F, # 'background-color', '#002651'; 'border-bottom-left-radius','5px'; 
                                  info = F, 
                                  headerCallback = JS(
                                    "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(3).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(4).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(3).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(4).css('color', 'white')
}"))) %>%
          formatStyle(
            "Pick" ,
            color = styleEqual(counter$countervalue,"white") ,
            backgroundColor = styleEqual(counter$countervalue,"#163C91"))
        
      }) # close renderDataTable()
      #####
      
      # Top Still Available boxes
      #####
      
      Draftee_Data <- Draftee_Data %>% arrange(Projected)
      Top_P1 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[1] 
      Top_P2 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[2]
      Top_P3 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[3] 
      Top_P4 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[4] 
      Top_P5 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[5]
      Top_P6 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[6] 
      
      output$Top5 <- renderUI({
        
        productList(
          
          productListItem( # Top: 1 
            src = Draftee_Data %>% filter(Player == Top_P1) %>% pull(Image),
            productTitle = Top_P1,
            Draftee_Data %>% filter(Player == Top_P1) %>% pull(Position), br(),
            Draftee_Data %>% filter(Player == Top_P1) %>% pull(Club), br(),
            productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
            priceColor   = "success",
          ),
          
          productListItem( # Top: 2 
            src = Draftee_Data %>% filter(Player == Top_P2) %>% pull(Image),
            productTitle = Top_P2,
            Draftee_Data %>% filter(Player == Top_P2) %>% pull(Position), br(),
            Draftee_Data %>% filter(Player == Top_P2) %>% pull(Club), br(),
            productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
            priceColor   = "success",
          ),
          
          productListItem( # Top: 3 
            src = Draftee_Data %>% filter(Player == Top_P3) %>% pull(Image),
            productTitle = Top_P3,
            Draftee_Data %>% filter(Player == Top_P3) %>% pull(Position), br(),
            Draftee_Data %>% filter(Player == Top_P3) %>% pull(Club), br(),
            productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
            priceColor   = "success",
          ),
          
          productListItem( # Top: 4 
            src = Draftee_Data %>% filter(Player == Top_P4) %>% pull(Image),
            productTitle = Top_P4,
            Draftee_Data %>% filter(Player == Top_P4) %>% pull(Position), br(),
            Draftee_Data %>% filter(Player == Top_P4) %>% pull(Club), br(),
            productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
            priceColor   = "success",
          ),
          
          productListItem( # Top: 5 
            src = Draftee_Data %>% filter(Player == Top_P5) %>% pull(Image),
            productTitle = Top_P5,
            Draftee_Data %>% filter(Player == Top_P5) %>% pull(Position), br(),
            Draftee_Data %>% filter(Player == Top_P5) %>% pull(Club), br(),
            productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
            priceColor   = "success",
          ),
          
          productListItem( # Top: 5 
            src = Draftee_Data %>% filter(Player == Top_P6) %>% pull(Image),
            productTitle = Top_P6,
            Draftee_Data %>% filter(Player == Top_P6) %>% pull(Position), br(),
            Draftee_Data %>% filter(Player == Top_P6) %>% pull(Club), br(),
            productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
            priceColor   = "success",
          ),
        ) # productList()
        
      })
      #####
      
      # Modal for when player is DRAFTED normally
      #####
      showModal(modalDialog(
        
        easyClose = T , 
        
        fluidRow(
          
          column(12, align="center",
                 div(style="display: inline-block;",img(src = "https://i.ibb.co/R4Zcnzq/Will-Phillips.jpg" , height=100, width=100)),
                 div(style="display: inline-block;",img(src = "https://i.ibb.co/xH8cbBn/Screen-Shot-2020-09-22-at-10-05-39-pm.png", height=175 , width=260)),
                 div(style="display: inline-block;",img(src = Teams_Data %>% filter(Team == Draft_Order[counter$countervalue,3] %>% pull() ) %>% pull(TeamLogo) , height=115, width=150))
          ),
          
          fluidRow(
            column(12,align = 'center',
                   HTML(paste0(h1(tags$b("PICK ",counter$countervalue," - ",Draft_Order[counter$countervalue,3] %>% pull())), "\n",
                               h2(input$Player_Select) , "\n",
                               #br(),
                               h4(paste0(Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Club))) , "\n",
                               h4(paste0(Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Position))) , "\n" , 
                               h4(paste0(Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Height), " | " , Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Weight))) , "\n" 
                   ))
            )
          )
        ),
        
        
        
        footer = actionBttn(inputId = "closeDrafted",label = "Close",color = 'primary',size = 'sm')))
      
      #####
      
      # - Pick Time IF/ELSE  *draft normal*
      if (input$pickTime == "30 sec."){
        pick$time <- 30
        base$time <- 30
        pickvar$val <- "30 sec."
      } else if (input$pickTime == "1 min.") {
        pick$time <- 60   
        base$time <- 60
        pickvar$val <- "1 min."
      } else if (input$pickTime == "2 min.") {
        pick$time <- 120
        base$time <- 120
        pickvar$val <- "2 min."
      } else if (input$pickTime == "3 min.") {
        pick$time <- 180  
        base$time <- 180
        pickvar$val <- "3 min."
      } else if (input$pickTime == "4 min.") {
        pick$time <- 240  
        base$time <- 240
        pickvar$val <- "4 min."
      } else {
        pick$time <- 300
        base$time <- 300
        pickvar$val <- "5 min."
      }
      
      pick$time <- base$time
      active(TRUE)
      
      # - +1 on counter IS necessary here - dont have modals extending from this to do it in like with tied players
      counter$countervalue <- counter$countervalue+1
      
    } 
    
  }) 
  # - (end) Draft Button
  
  # If yesMatch
  observeEvent(input$yesMatch, {
    
    # Draft the player whose been bid on
    #####
    Selected_Table_tmp[1,1] <- counter$countervalue
    Selected_Table_tmp[1,2] <- Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Ties) 
    Selected_Table_tmp[1,3] <- input$Player_Select
    
    Selected_Table_tmp <- as.data.frame(Selected_Table_tmp)
    Selected_Table <<- as.data.frame(rbind(Selected_Table,Selected_Table_tmp))
    Selected_Table <<- na.omit(Selected_Table)
    #####
    
    # Log the trade in LogTable
    Log_tmp <- data.frame("No" = numeric() , "Type" = character() , "Details" = character())
    Log_tmp[1,1] <- nrow(LogTable) + 1
    Log_tmp[1,2] <- "Bid"
    Log_tmp[1,3] <- paste0(Draft_Order[counter$countervalue,3] %>% pull(), " bid on ", input$Player_Select, " at Pick ",counter$countervalue, br(),
                           "The bid was matched by ", Draftee_Data %>% filter(Player == input$Player_Select ) %>% pull(Ties))
    
    LogTable <<- as.data.frame(rbind(LogTable,Log_tmp))
    
    
    # Variables
    round1_bid      <- ifelse(Draft_Order$Round[counter$countervalue] == 1 , TRUE , FALSE) # This will have to change to be dynamic with picks that are in 1st round - but this will do for now
    tiedTeam        <- Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Ties)
    tiedTeamsPicks  <- Draft_Order %>% filter(Actual_Pick == tiedTeam , Pick >= counter$countervalue) %>% select(Round,Pick,Actual_Pick,Points) 
    pointsRemaining <- ifelse(round1_bid == T , round(Pick_Points$Points[counter$countervalue]-(20/100*Pick_Points$Points[counter$countervalue])) , (Pick_Points$Points[counter$countervalue] - 197) )
    enoughPoints    <- ifelse( tiedTeamsPicks %>% filter(Points>0) %>% summarise(PointsTotal = sum(Points)) %>% pull(PointsTotal) > pointsRemaining , TRUE , FALSE )
    picksUsed       <- data.frame()
    
    if (enoughPoints == TRUE){
      
      for (i in 1:(nrow(tiedTeamsPicks)+1)) { # +1 because errors for team like Fremantle who only hold 2 picks - the +1 just returns NA which doesnt do anything
        
        if(pointsRemaining > 0) {
          
          pointsRemaining <- pointsRemaining - tiedTeamsPicks$Points[i]
          
          picksUsed <- rbind(picksUsed , tiedTeamsPicks$Pick[i])
          
          
        } else {
          
          picksUsed <- picksUsed %>% rename("Pick" = 1)
          # Filter out the picks that have been used in the bid here
          Draft_Order <<- Draft_Order %>% filter( !(Draft_Order$Pick %in% picksUsed$Pick) ) %>% mutate(Pick = row_number())
          
          # This line tells us which pick the bidding team will slide in at if/when they go over the required no. of points
          excessPick <- which.min(abs(Pick_Points$Points-abs(pointsRemaining)))
          
          Draft_Order <<- Draft_Order %>%
            # This awards the pick to the team whove matched the bid
            add_row(.before = counter$countervalue , 
                    Round = Draft_Order$Round[counter$countervalue],
                    Pick = counter$countervalue,
                    Actual_Pick = tiedTeam , 
                    Points = Pick_Points$Points[1+counter$countervalue]) %>% 
            mutate(Pick = row_number()) %>%
            inner_join(. , Pick_Points , by = c("Pick" = "Pick")) %>%
            select(-contains(".x")) %>%
            rename("Points" = "Points.y" , 
                   "Round" = "Round.y") %>%
            select(Round,Pick,Actual_Pick,Points) %>%
            # This awards the surplus pick
            add_row(.before = which(Draft_Order$Pick == excessPick) ,
                    Round = Draft_Order$Round[excessPick-1],
                    Pick = excessPick,
                    Actual_Pick = tiedTeam ,
                    Points = Pick_Points$Points[excessPick]) %>% 
            mutate(Pick = row_number()) %>%
            inner_join(. , Pick_Points , by = c("Pick" = "Pick")) %>%
            select(-contains(".x")) %>%
            rename("Points" = "Points.y" , 
                   "Round" = "Round.y") %>%
            select(Round,Pick,Actual_Pick,Points)
          
          for (j in 1:nrow(picksUsed)){
            
            Draft_Order <<- Draft_Order %>%
              # This adds the picks that have been used to match the bid to the end of the draft order
              add_row(.after = nrow(Draft_Order) , 
                      Round = Draft_Order$Round[nrow(Draft_Order)-1],
                      Pick = 0,
                      Actual_Pick = tiedTeam , 
                      Points = 0) %>%
              mutate(Pick = row_number())
          }
          
          break
          
        } # end else
        
      } # end for
      
    } else {
      
      pointsDeficit <- pointsRemaining - tiedTeamsPicks %>% summarise(Points = sum(Points)) %>% pull()
      
      Draft_Order <<- Draft_Order %>% filter( !(Draft_Order$Pick %in% c(tiedTeamsPicks %>% filter(Points > 0) %>% pull(Pick)))) %>% mutate(Pick = row_number())
      
      # So award the pick to the team that matches the bid - this part stays the same
      
      Draft_Order <<- Draft_Order %>%
        # This awards the pick to the team whove matched the bid
        add_row(.before = counter$countervalue , 
                Round = Draft_Order$Round[counter$countervalue],
                Pick = counter$countervalue,
                Actual_Pick = tiedTeam , 
                Points = Pick_Points$Points[1+counter$countervalue]) %>%
        mutate(Pick = row_number()) %>%
        inner_join(. , Pick_Points[c(2,3)] , by = c("Pick" = "Pick") ) %>%
        rename("Points"= "Points.y") %>% 
        select(Round,Pick,Actual_Pick,Points)
      
      
      # Send all the picks with points to the back of the draft
      for (k in 1:nrow(tiedTeamsPicks %>% filter(Points > 0))){
        
        Draft_Order <<- Draft_Order %>%
          # This adds the picks that have been used to match the bid to the end of the draft order
          add_row(.after = nrow(Draft_Order) , 
                  Round = Draft_Order$Round[nrow(Draft_Order)-1],
                  Pick = 0,
                  Actual_Pick = tiedTeam , 
                  Points = 0) %>%
          mutate(Pick = row_number())
      }
      
      # Take off points deficit from next years pick (note might not have a pick in this round so would have to make dynamic and take it from the teams next available pick)
      PickIndex <- min(which(Draft_Order_Future$Actual_Pick == tiedTeam ))
      
      # Find which pick theyll drop to after deficit
      DropToPick <- which.min(abs(Pick_Points$Points - (Pick_Points$Points[PickIndex] - pointsDeficit)))
      
      # Minus the deficit
      Draft_Order_Future$Points[PickIndex] <<- Draft_Order_Future$Points[PickIndex] - pointsDeficit
      Draft_Order_Future <<- Draft_Order_Future %>% arrange(desc(Points)) %>% mutate(Pick = row_number()) %>% inner_join(. , Pick_Points, by = c("Pick" = "Pick")) %>% select(Round.y, Pick, Actual_Pick, Points.y ) %>% rename("Points" = "Points.y", "Round" = "Round.y")
      
    }  # end enoughpoints
    
    # This is the updated player pool that doesnt include the players that have been drafted
    UpdatedPool <- reactive({ sort(c(setdiff(Draftee_Data$Player , Selected_Table$Player))) })
    
    # This is updating the select input dropdown with the updated player pool (UpdatedPool())
    updatePickerInput(session , inputId = "Player_Select" , choices = UpdatedPool() , choicesOpt = list(subtext = UpdatedPool() %>% as.data.frame() %>% inner_join(. , Draftee_Data[c(1,9)] , by = c("." = "Player")) %>% mutate(Paste = ifelse(!is.na(Ties) , paste0("(",Ties,")") , paste0(""))) %>% pull(Paste)) , selected = "" ) 
    
    # - Output Draftboard Table *inside yesMatch*      
    #####    
    output$DraftBoard <- DT::renderDataTable({ 
      
      datatable(data = Draft_Order %>% inner_join(. , Teams_Data[c(1,5)] , by = c("Actual_Pick" = "Team")) %>% 
                  mutate(Team = paste0(Actual_Pick,"<img src=","'",TeamLogo,"'"," ", "height='40'style=float:right></img>")) %>% select(Round,Pick,Team,Points) %>% left_join(.,Selected_Table[c(1,3)], by = c("Pick"="Pick")) %>% select(Round,Pick,Team,Player,Points) ,  
                escape = F , 
                # one of these below options makes Club name go over one line only! Important!
                class = "row-bordered hover stripe nowrap order-column" , 
                rownames = FALSE,
                options  = list(dom = "t",
                                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                paging = F,
                                searching = F,
                                scrollX = F, # 'background-color', '#002651'; 'border-bottom-left-radius','5px'; 
                                info = F, 
                                headerCallback = JS(
                                  "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(3).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(4).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(3).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(4).css('color', 'white')
}"))) %>%
        formatStyle(
          "Pick" ,
          color = styleEqual(counter$countervalue,"white") ,
          backgroundColor = styleEqual(counter$countervalue,"#163C91"))
      
      
      
    }) # close renderDataTable()
    #####
    
    # - Output LogTable *inside yesMatch*
    #####
    output$Transactions <- DT::renderDataTable({ 
      
      datatable(data = LogTable,
                escape = F , 
                # one of these below options makes Club name go over one line only! Important!
                class = "row-bordered hover stripe nowrap order-column" , 
                rownames = FALSE,
                options  = list(dom = "t",
                                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                paging = F,
                                searching = F,
                                scrollX = F, 
                                info = F,
                                headerCallback = JS(
                                  "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    
}"))) %>%
        formatStyle('Details', `text-align` = 'left')
      
    })
    #####
    
    # Top 5 Available boxes
    #####
    
    Draftee_Data <- Draftee_Data %>% arrange(Projected)
    Top_P1 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[1] 
    Top_P2 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[2]
    Top_P3 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[3] 
    Top_P4 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[4] 
    Top_P5 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[5]
    Top_P6 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[6] 
    
    output$Top5 <- renderUI({
      
      productList(
        
        productListItem( # Top: 1 
          src = Draftee_Data %>% filter(Player == Top_P1) %>% pull(Image),
          productTitle = Top_P1,
          Draftee_Data %>% filter(Player == Top_P1) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P1) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
        
        productListItem( # Top: 2 
          src = Draftee_Data %>% filter(Player == Top_P2) %>% pull(Image),
          productTitle = Top_P2,
          Draftee_Data %>% filter(Player == Top_P2) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P2) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
        
        productListItem( # Top: 3 
          src = Draftee_Data %>% filter(Player == Top_P3) %>% pull(Image),
          productTitle = Top_P3,
          Draftee_Data %>% filter(Player == Top_P3) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P3) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
        
        productListItem( # Top: 4 
          src = Draftee_Data %>% filter(Player == Top_P4) %>% pull(Image),
          productTitle = Top_P4,
          Draftee_Data %>% filter(Player == Top_P4) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P4) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
        
        productListItem( # Top: 5 
          src = Draftee_Data %>% filter(Player == Top_P5) %>% pull(Image),
          productTitle = Top_P5,
          Draftee_Data %>% filter(Player == Top_P5) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P5) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
        
        productListItem( # Top: 5 
          src = Draftee_Data %>% filter(Player == Top_P6) %>% pull(Image),
          productTitle = Top_P6,
          Draftee_Data %>% filter(Player == Top_P6) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P6) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
      ) # productList()
      
    })
    #####
    
    # - Summary Modal (1 modal for both enough points & not enough outcomes) 
    #####
    showModal(modalDialog(
      easyClose = T,
      footer = tagList(actionBttn(inputId = "closeYes",label = "Close",color = 'primary',size = 'sm')) ,
      
      column(12, align="center",
             div(style="display: inline-block;",img(src = "https://resources.afl.com.au/photo-resources/2020/03/17/e8f96772-bae4-4bde-9a4e-12458fe158ab/TR060320MK0440.jpg?width=536&height=536" , height=100, width=100)),
             div(style="display: inline-block;",img(src = "https://i.ibb.co/M8QNbsR/Bid-Matched-Graphic.png", height=175 , width=260)),
             div(style="display: inline-block;",img(src = Teams_Data %>% filter(Team == tiedTeam) %>% pull(TeamLogo) , height=115, width=150)),
             
             br(),
             br(),
             br(),
      ),
      
      if (enoughPoints == T){
        
        HTML(paste0(h4(tiedTeam, tags$b(paste0(" give up: ")),  "pick(s): "  , paste(unlist(picksUsed),collapse = ", ")) , 
                    paste0(h4(tiedTeam, tags$b(paste0(" recieve: ")) , input$Player_Select, " & pick(s): ", excessPick, ", " , paste(unlist(Draft_Order[(nrow(Draft_Order)-length(picksUsed)):nrow(Draft_Order) , 2] %>% pull() ), collapse = ", ")))))
        
        
      } else {
        
        HTML(  
          paste0(Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Ties)," didnt have enough points with picks in this years draft to match the bid.") ,
          "<br/>",     
          "<br/>",     
          paste0("<b>", "<u>" ,pointsDeficit,"</u>","</b>", " points have been deducted from their first selection in 2021 (Projected at Pick ",PickIndex,") "),
          "<br/>",
          paste0("This deficit would drop them from Pick ",PickIndex," to Pick ",DropToPick, " next year.")
        )
        
      }
      
    ))
    #####
    
    # - Pick Time IF/ELSE *bid matched*
    if (input$pickTime == "30 sec."){
      pick$time <- 30
      base$time <- 30
      pickvar$val <- "30 sec."
    } else if (input$pickTime == "1 min.") {
      pick$time <- 60   
      base$time <- 60
      pickvar$val <- "1 min."
    } else if (input$pickTime == "2 min.") {
      pick$time <- 120
      base$time <- 120
      pickvar$val <- "2 min."
    } else if (input$pickTime == "3 min.") {
      pick$time <- 180  
      base$time <- 180
      pickvar$val <- "3 min."
    } else if (input$pickTime == "4 min.") {
      pick$time <- 240  
      base$time <- 240
      pickvar$val <- "4 min."
    } else {
      pick$time <- 300
      base$time <- 300
      pickvar$val <- "5 min."
    }
    
    pick$time <- base$time
    active(TRUE)
    
    counter$countervalue <- counter$countervalue+1
    
  })
  # --- yesMatch button closed above
  
  # If noMatch
  observeEvent(input$noMatch, {
    
    # Building the table containing the selected players one row at a time
    Selected_Table_tmp[1,1] <- counter$countervalue
    Selected_Table_tmp[1,2] <- Draft_Order[counter$countervalue,3] %>% pull() 
    Selected_Table_tmp[1,3] <- input$Player_Select
    
    # Binding tmp table (Selected_Table_tmp) to master (Selected_Table)
    Selected_Table_tmp <- as.data.frame(Selected_Table_tmp)
    Selected_Table <<- as.data.frame(rbind(Selected_Table,Selected_Table_tmp))
    Selected_Table <<- na.omit(Selected_Table)
    
    # Log the trade in LogTable
    Log_tmp <- data.frame("No" = numeric() , "Type" = character() , "Details" = character())
    Log_tmp[1,1] <- nrow(LogTable) + 1
    Log_tmp[1,2] <- "Bid"
    Log_tmp[1,3] <- paste0(Draft_Order[counter$countervalue,3] %>% pull(), " bid on ", input$Player_Select, " at Pick ",counter$countervalue, br(),
                           "The bid was NOT matched by ", Draftee_Data %>% filter(Player == input$Player_Select ) %>% pull(Ties))
    
    LogTable <<- as.data.frame(rbind(LogTable,Log_tmp))
    
    
    # This is the updated player pool that doesnt include the players that have been drafted
    UpdatedPool <- reactive({ sort(c(setdiff(Draftee_Data$Player , Selected_Table$Player))) })
    
    # This is updating the select input dropdown with the updated player pool (UpdatedPool())
    updatePickerInput(session , inputId = "Player_Select" , choices = UpdatedPool() , choicesOpt = list(subtext = UpdatedPool() %>% as.data.frame() %>% inner_join(. , Draftee_Data[c(1,9)] , by = c("." = "Player")) %>% mutate(Paste = ifelse(!is.na(Ties) , paste0("(",Ties,")") , paste0(""))) %>% pull(Paste)) , selected = "" ) 
    
    # - Output Draftboard Table (subject to being moved) *inside noMatch*      
    #####    
    output$DraftBoard <- DT::renderDataTable({ 
      
      datatable(data = Draft_Order %>% inner_join(. , Teams_Data[c(1,5)] , by = c("Actual_Pick" = "Team")) %>% 
                  mutate(Team = paste0(Actual_Pick,"<img src=","'",TeamLogo,"'"," ", "height='40'style=float:right></img>")) %>% select(Round,Pick,Team,Points) %>% left_join(.,Selected_Table[c(1,3)], by = c("Pick"="Pick")) %>% select(Round,Pick,Team,Player,Points) ,  
                escape = F , 
                # one of these below options makes Club name go over one line only! Important!
                class = "row-bordered hover stripe nowrap order-column" , 
                rownames = FALSE,
                options  = list(dom = "t",
                                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                paging = F,
                                searching = F,
                                scrollX = F, # 'background-color', '#002651'; 'border-bottom-left-radius','5px'; 
                                info = F, 
                                headerCallback = JS(
                                  "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(3).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(4).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(3).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(4).css('color', 'white')
}"))) %>%
        formatStyle(
          "Pick" ,
          color = styleEqual(counter$countervalue,"white") ,
          backgroundColor = styleEqual(counter$countervalue,"#163C91"))
      
      
      
    }) # close renderDataTable()
    #####
    
    # - Output LogTable *inside noMatch*
    #####
    output$Transactions <- DT::renderDataTable({ 
      
      datatable(data = LogTable,
                escape = F , 
                # one of these below options makes Club name go over one line only! Important!
                class = "row-bordered hover stripe nowrap order-column" , 
                rownames = FALSE,
                options  = list(dom = "t",
                                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                paging = F,
                                searching = F,
                                scrollX = F, 
                                info = F,
                                headerCallback = JS(
                                  "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    
}"))) %>%
        formatStyle('Details', `text-align` = 'left')
      
    })
    #####
    
    # Top Still Available boxes
    #####
    
    Draftee_Data <- Draftee_Data %>% arrange(Projected)
    Top_P1 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[1] 
    Top_P2 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[2]
    Top_P3 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[3] 
    Top_P4 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[4] 
    Top_P5 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[5]
    Top_P6 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[6] 
    
    output$Top5 <- renderUI({
      
      productList(
        
        productListItem( # Top: 1 
          src = Draftee_Data %>% filter(Player == Top_P1) %>% pull(Image),
          productTitle = Top_P1,
          Draftee_Data %>% filter(Player == Top_P1) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P1) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
        
        productListItem( # Top: 2 
          src = Draftee_Data %>% filter(Player == Top_P2) %>% pull(Image),
          productTitle = Top_P2,
          Draftee_Data %>% filter(Player == Top_P2) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P2) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
        
        productListItem( # Top: 3 
          src = Draftee_Data %>% filter(Player == Top_P3) %>% pull(Image),
          productTitle = Top_P3,
          Draftee_Data %>% filter(Player == Top_P3) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P3) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
        
        productListItem( # Top: 4 
          src = Draftee_Data %>% filter(Player == Top_P4) %>% pull(Image),
          productTitle = Top_P4,
          Draftee_Data %>% filter(Player == Top_P4) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P4) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
        
        productListItem( # Top: 5 
          src = Draftee_Data %>% filter(Player == Top_P5) %>% pull(Image),
          productTitle = Top_P5,
          Draftee_Data %>% filter(Player == Top_P5) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P5) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
        
        productListItem( # Top: 5 
          src = Draftee_Data %>% filter(Player == Top_P6) %>% pull(Image),
          productTitle = Top_P6,
          Draftee_Data %>% filter(Player == Top_P6) %>% pull(Position), br(),
          Draftee_Data %>% filter(Player == Top_P6) %>% pull(Club), br(),
          productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
          priceColor   = "success",
        ),
      ) # productList()
      
    })
    #####
    
    showModal(modalDialog(
      easyClose = T,
      footer = tagList(actionBttn(inputId = "closeNo",label = "Close",color = 'primary',size = 'sm')),
      
      column(12, align="center",
             div(style="display: inline-block;",img(src = "https://resources.afl.com.au/photo-resources/2020/03/17/e8f96772-bae4-4bde-9a4e-12458fe158ab/TR060320MK0440.jpg?width=536&height=536" , height=100, width=100)),
             div(style="display: inline-block;",img(src = "https://i.ibb.co/kcs0PFB/Bid-Not-Matched-Graphic.png", height=175 , width=260)),
             div(style="display: inline-block;",img(src = Teams_Data %>% filter(Team == Draft_Order[counter$countervalue,3] %>% pull()) %>% pull(TeamLogo) , height=115, width=150)),
             
             br(),
             br(),
             br()
             
      ),
      
      HTML(paste0(h4(tags$u(Draftee_Data %>% filter(Player == input$Player_Select) %>% pull(Ties))," have chosen not to match the bid for ", tags$b(input$Player_Select))) , "\n",
           paste0(h4(tags$b(input$Player_Select)," has been drafted by ", tags$u(Draft_Order[counter$countervalue,3] %>% pull()), " with Pick ", counter$countervalue,".")))
      
    ))
    
    # - Pick Time IF/ELSE *no match*
    if (input$pickTime == "30 sec."){
      pick$time <- 30
      base$time <- 30
      pickvar$val <- "30 sec."
    } else if (input$pickTime == "1 min.") {
      pick$time <- 60   
      base$time <- 60
      pickvar$val <- "1 min."
    } else if (input$pickTime == "2 min.") {
      pick$time <- 120
      base$time <- 120
      pickvar$val <- "2 min."
    } else if (input$pickTime == "3 min.") {
      pick$time <- 180  
      base$time <- 180
      pickvar$val <- "3 min."
    } else if (input$pickTime == "4 min.") {
      pick$time <- 240  
      base$time <- 240
      pickvar$val <- "4 min."
    } else {
      pick$time <- 300
      base$time <- 300
      pickvar$val <- "5 min."
    }
    
    pick$time <- base$time
    active(TRUE)
    
    counter$countervalue <- counter$countervalue+1
    
  })
  # --- noMatch button closed above
  
  #######################################
  #  Variables for Trade Modal (in UI)  #
  #######################################
  #####
  observe({
    
    trade_Teams           <<- unique(Teams_Data$Team)
    teamwithPick          <<- as.character(Draft_Order %>% filter(Pick == counter$countervalue) %>% pull(Actual_Pick)) 
    teamtradeWith         <<- input$tradeWith_btn
    Team_choices          <<- c("",setdiff(c(trade_Teams) , teamwithPick ))
    output$CurrentPick    <- renderText({ paste0("Pick: ",counter$countervalue) })
    
    Picks_A <- setdiff(c(Draft_Order %>% filter(Actual_Pick == teamwithPick) %>% pull(Pick)) , c(Selected_Table$Pick)) 
    Picks_B <- setdiff(c(Draft_Order %>% filter(Actual_Pick == teamtradeWith) %>% pull(Pick)) , c(Selected_Table$Pick)) 
    
    # - LHS of trade modal
    #####
    TeamA_2 <- as.character( input$Pick_2_A )
    TeamA_3 <- as.character( input$Pick_3_A )
    TeamA_4 <- as.character( input$Pick_4_A )
    
    A2_choices <- c("",setdiff(Picks_A,c(counter$countervalue,TeamA_3,TeamA_4)))
    A3_choices <- c("",setdiff(Picks_A,c(counter$countervalue,TeamA_2,TeamA_4)))
    A4_choices <- c("",setdiff(Picks_A,c(counter$countervalue,TeamA_2,TeamA_3)))
    
    A2_choices_future <- c("",setdiff(Draft_Order_Future %>% filter(Actual_Pick == teamwithPick) %>% mutate(Suffix = ifelse(Round == 1, "st",ifelse(Round == 2, "nd",ifelse(Round == 3, "rd", ifelse(Round >= 4, "th", "")))),  Fut_Pick = paste0("Fut. ",Round,Suffix," (",Pick,")")) %>% pull(Fut_Pick), c(TeamA_3,TeamA_4)))
    A3_choices_future <- c("",setdiff(Draft_Order_Future %>% filter(Actual_Pick == teamwithPick) %>% mutate(Suffix = ifelse(Round == 1, "st",ifelse(Round == 2, "nd",ifelse(Round == 3, "rd", ifelse(Round >= 4, "th", "")))),  Fut_Pick = paste0("Fut. ",Round,Suffix," (",Pick,")")) %>% pull(Fut_Pick), c(TeamA_2,TeamA_4)))
    A4_choices_future <- c("",setdiff(Draft_Order_Future %>% filter(Actual_Pick == teamwithPick) %>% mutate(Suffix = ifelse(Round == 1, "st",ifelse(Round == 2, "nd",ifelse(Round == 3, "rd", ifelse(Round >= 4, "th", "")))),  Fut_Pick = paste0("Fut. ",Round,Suffix," (",Pick,")")) %>% pull(Fut_Pick), c(TeamA_2,TeamA_3)))
    
    updateSelectizeInput(session, inputId = "Pick_2_A", label = "2nd Pick", selected = TeamA_2 , choices = list(Current = A2_choices , Future = A2_choices_future ))
    updateSelectizeInput(session, inputId = "Pick_3_A", label = "3rd Pick", selected = TeamA_3 , choices = list(Current = A3_choices , Future = A3_choices_future ))
    updateSelectizeInput(session, inputId = "Pick_4_A", label = "4th Pick", selected = TeamA_4 , choices = list(Current = A4_choices , Future = A4_choices_future ))
    
    teamwithPickPicks  <<- c(counter$countervalue, TeamA_2, TeamA_3, TeamA_4)
    
    
    # IF future picks are involved, sum TeamB_DVI (*** may be a better way to do this)
    if( str_contains(teamwithPickPicks , "Fut.") == T ) {
      
      TeamA_Futures <- teamwithPickPicks %>% str_subset(pattern = "Fut. ")
      
      TeamA_DVI <- Draft_Order_Future %>%
        slice(which(Draft_Order_Future$Actual_Pick == teamwithPick & Draft_Order_Future$Round %in% gsub(x = gsub("([0-9]+).*$", "\\1", TeamA_Futures), pattern = "Fut. " ,  replacement = "") )) %>%
        summarise(FutTot = sum(FuturePts)) %>% pull(FutTot) +
        sum(na.omit(c(as.numeric(Pick_Points$Points[counter$countervalue]),
                      as.numeric(Pick_Points$Points[as.numeric(input$Pick_2_A)]),
                      as.numeric(Pick_Points$Points[as.numeric(input$Pick_3_A)]),
                      as.numeric(Pick_Points$Points[as.numeric(input$Pick_4_A)]))) + 200) # The +200 is just an additional kicker
      
    } else {
      
      TeamA_DVI <- sum(na.omit(c(as.numeric(Pick_Points$Points[counter$countervalue]),
                                 as.numeric(Pick_Points$Points[as.numeric(input$Pick_2_A)]),
                                 as.numeric(Pick_Points$Points[as.numeric(input$Pick_3_A)]),
                                 as.numeric(Pick_Points$Points[as.numeric(input$Pick_4_A)]))) + 200)
      
    }
    
    #####
    
    # - RHS of trade modal
    #####
    TeamB_1 <- as.character( input$Pick_1_B ) 
    TeamB_2 <- as.character( input$Pick_2_B )
    TeamB_3 <- as.character( input$Pick_3_B )
    TeamB_4 <- as.character( input$Pick_4_B )
    
    B1_choices <- c("",setdiff(Picks_B,c(TeamB_2,TeamB_3,TeamB_4)))
    B2_choices <- c("",setdiff(Picks_B,c(TeamB_1,TeamB_3,TeamB_4)))
    B3_choices <- c("",setdiff(Picks_B,c(TeamB_1,TeamB_2,TeamB_4)))
    B4_choices <- c("",setdiff(Picks_B,c(TeamB_1,TeamB_2,TeamB_3)))
    
    B1_choices_future <- c("",setdiff(Draft_Order_Future %>% filter(Actual_Pick == teamtradeWith) %>% mutate(Suffix = ifelse(Round == 1, "st",ifelse(Round == 2, "nd",ifelse(Round == 3, "rd", ifelse(Round >= 4, "th", "")))),  Fut_Pick = paste0("Fut. ",Round,Suffix," (",Pick,")")) %>% pull(Fut_Pick), c(TeamB_2,TeamB_3,TeamB_4)))        
    B2_choices_future <- c("",setdiff(Draft_Order_Future %>% filter(Actual_Pick == teamtradeWith) %>% mutate(Suffix = ifelse(Round == 1, "st",ifelse(Round == 2, "nd",ifelse(Round == 3, "rd", ifelse(Round >= 4, "th", "")))),  Fut_Pick = paste0("Fut. ",Round,Suffix," (",Pick,")")) %>% pull(Fut_Pick), c(TeamB_1,TeamB_3,TeamB_4)))
    B3_choices_future <- c("",setdiff(Draft_Order_Future %>% filter(Actual_Pick == teamtradeWith) %>% mutate(Suffix = ifelse(Round == 1, "st",ifelse(Round == 2, "nd",ifelse(Round == 3, "rd", ifelse(Round >= 4, "th", "")))),  Fut_Pick = paste0("Fut. ",Round,Suffix," (",Pick,")")) %>% pull(Fut_Pick), c(TeamB_1,TeamB_2,TeamB_4)))
    B4_choices_future <- c("",setdiff(Draft_Order_Future %>% filter(Actual_Pick == teamtradeWith) %>% mutate(Suffix = ifelse(Round == 1, "st",ifelse(Round == 2, "nd",ifelse(Round == 3, "rd", ifelse(Round >= 4, "th", "")))),  Fut_Pick = paste0("Fut. ",Round,Suffix," (",Pick,")")) %>% pull(Fut_Pick), c(TeamB_1,TeamB_2,TeamB_3)))
    
    updateSelectizeInput(session, inputId = "Pick_1_B", label = "1st Pick", selected = TeamB_1 , choices = list(Current = B1_choices , Future = B1_choices_future ))  
    updateSelectizeInput(session, inputId = "Pick_2_B", label = "2nd Pick", selected = TeamB_2 , choices = list(Current = B2_choices , Future = B2_choices_future ))
    updateSelectizeInput(session, inputId = "Pick_3_B", label = "3rd Pick", selected = TeamB_3 , choices = list(Current = B3_choices , Future = B3_choices_future ))
    updateSelectizeInput(session, inputId = "Pick_4_B", label = "4th Pick", selected = TeamB_4 , choices = list(Current = B4_choices , Future = B4_choices_future ))
    
    teamtradeWithPicks <<- c(TeamB_1, TeamB_2, TeamB_3, TeamB_4) # "6","Fut. 1st (1)","Fut. 2nd (19)","") #
    
    # print(teamtradeWithPicks)
    
    # IF future picks are involved, sum TeamB_DVI (*** may be a better way to do this)
    if( str_contains(teamtradeWithPicks , "Fut.") == T ) {
      
      TeamB_Futures <- teamtradeWithPicks %>% str_subset(pattern = "Fut. ")
      
      TeamB_DVI <- Draft_Order_Future %>%
        slice(which(Draft_Order_Future$Actual_Pick == teamtradeWith & Draft_Order_Future$Round %in% gsub(x = gsub("([0-9]+).*$", "\\1", TeamB_Futures),pattern = "Fut. " ,  replacement = "") )) %>%
        summarise(FutTot = sum(FuturePts)) %>% pull(FutTot) +
        sum(na.omit(c(as.numeric(Pick_Points$Points[as.numeric(input$Pick_1_B)]),
                      as.numeric(Pick_Points$Points[as.numeric(input$Pick_2_B)]),
                      as.numeric(Pick_Points$Points[as.numeric(input$Pick_3_B)]),
                      as.numeric(Pick_Points$Points[as.numeric(input$Pick_4_B)]))))
      
    } else {
      
      TeamB_DVI <- sum(na.omit(c(as.numeric(Pick_Points$Points[as.numeric(input$Pick_1_B)]),
                                 as.numeric(Pick_Points$Points[as.numeric(input$Pick_2_B)]),
                                 as.numeric(Pick_Points$Points[as.numeric(input$Pick_3_B)]),
                                 as.numeric(Pick_Points$Points[as.numeric(input$Pick_4_B)]))))
    }
    #####
    
    
    #####
    
    # - Trade Modal Interest Bar & Variables
    #####
    
    interestLevel  <<-  ( ((TeamB_DVI/TeamA_DVI)*100) - thresh$hold )
    
    interestColour <<- ifelse(interestLevel <= 40, "danger" , 
                              ifelse(interestLevel > 40 & interestLevel <= 80, "warning" , 
                                     ifelse(interestLevel >80 , "success" , "info")))
    
    #print(paste0("Team A :",TeamA_DVI,"\n",
    #             "Team B :",TeamB_DVI))
    
    print(paste0("Normal: ", ((TeamB_DVI/TeamA_DVI)*100)))
    print(paste0("Adjust.: ", ( ((TeamB_DVI/TeamA_DVI)*100) - thresh$hold )))
    
    output$TradeInterest <- renderUI({
      fluidRow(align = 'center',
               column(8,progressBar(id = "tradeInterest", title = "Trade Interest:", value = interestLevel , status = interestColour, striped = TRUE) , offset = 2),
               br(),
               br(),
               br(),
               hr()
      )
    })
    #####
    
    
    
  })  
  #####
  
  ################################
  #    Submit Trade Responses    #
  ################################
  observeEvent(input$submitTrade_btn, {
    
    if( interestLevel > 90 ) {
      
      # Close the trade options modal 
      toggleModal(session, modalId = "trade_modal", toggle = "close")   
      
      pick$time <- base$time
      active(TRUE)
      
      showModal(modalDialog(
        easyClose = T , 
        
        fluidRow(      
          
          column(12, align="center",
                 div(style="display: inline-block;",img(src = "https://i.ibb.co/3rBqLQL/Screen-Shot-2020-09-27-at-1-10-31-pm.png", height=230, width=325))),
          
          br(),
          br(),
          br(),
          br(),
          
        ),
        
        fluidRow(
          
          column(5, h3(teamwithPick," Trade:"), align = 'left') , 
          column(2),
          column(5, h3(teamtradeWith, " Trade:"), align = 'right') , 
          
        ),
        
        fluidRow(
          
          column(5, h4("Pick(s): ",paste0(stri_remove_empty(teamwithPickPicks), sep="", collapse=", ")), align = 'left') , 
          column(2),
          column(5, h4("Pick(s): ",paste0(stri_remove_empty(teamtradeWithPicks), sep="", collapse=", ")), align = 'right') , 
          
        ),
        
        footer = actionBttn(inputId = "closeAccepted",label = "Close",color = 'primary',size = 'sm')
        
      ))
      
      
      # Log the trade in LogTable
      Log_tmp <- data.frame("No" = numeric() , "Type" = character() , "Details" = character())
      Log_tmp[1,1] <- nrow(LogTable) + 1
      Log_tmp[1,2] <- "Trade"
      Log_tmp[1,3] <- paste0(teamwithPick, " traded Pick(s): ", paste0(stri_remove_empty(teamwithPickPicks), sep="", collapse=", "), br(), "FOR", br(),  teamtradeWith, " Pick(s): ", paste0(stri_remove_empty(teamtradeWithPicks), sep="", collapse=", "))
      
      LogTable <<- as.data.frame(rbind(LogTable,Log_tmp))
      
      
      ### - The actual logic for swapping the picks - ###
      
      # ---- *Strip out Current & Future Picks & convert to numeric* ---- #
      
      # - Team with the pick
      # Future Picks: (need to do this first because teamtradeWithPicks var. changes below)
      teamWithPicksPicks_Future <- as.numeric(as.data.frame(teamwithPickPicks) %>% filter(grepl(x = teamwithPickPicks , pattern = "Fut.")) %>% mutate(Pick = sub("^.*?\\((.*)\\)[^)]*$", "\\1", teamwithPickPicks)) %>% pull(Pick))
      # Current Picks: 
      teamwithPickPicks <- as.numeric(teamwithPickPicks[which(teamwithPickPicks != "")] %>% as.data.frame() %>% filter(!grepl(x = . , pattern = "Fut.")) %>% pull(.))
      
      # Team trading with
      # Future Picks: (need to do this first because teamtradeWithPicks var. changes below)
      teamtradeWithPicks_Future <- as.numeric(as.data.frame(teamtradeWithPicks) %>% filter(grepl(x = teamtradeWithPicks , pattern = "Fut.")) %>% mutate(Pick = sub("^.*?\\((.*)\\)[^)]*$", "\\1", teamtradeWithPicks)) %>% pull(Pick))
      # Current Picks: 
      teamtradeWithPicks <- as.numeric(teamtradeWithPicks[which(teamtradeWithPicks != "")] %>% as.data.frame() %>% filter(!grepl(x = . , pattern = "Fut.")) %>% pull(.))
      
      
      
      ### - For-loop to swap picks around in Draft_Order DF - ###
      
      # Team B -- > Team A
      for (i in 1:length(teamtradeWithPicks)) {
        
        Draft_Order$Actual_Pick[teamtradeWithPicks[i]] <<- teamwithPick
        
      }
      
      # Team A --- > Team B
      for (i in 1:length(teamwithPickPicks)) {
        
        Draft_Order$Actual_Pick[teamwithPickPicks[i]] <<- teamtradeWith
        
      }
      
      
      # TEAM A FUTURES - This ifelse will trigger if future picks have actually been involved in the trade
      if( length(teamWithPicksPicks_Future != 0)) {
        
        # Swap the future picks around 
        for (j in 1:length(teamWithPicksPicks_Future)) {
          
          Draft_Order_Future$Actual_Pick[teamWithPicksPicks_Future[j]] <<- teamtradeWith
          
        }
        
      }
      
      
      # TEAM B FUTURES -  This ifelse will trigger if future picks have actually been involved in the trade
      if( length(teamtradeWithPicks_Future != 0)) {
        
        # Swap the future picks around 
        for (j in 1:length(teamtradeWithPicks_Future)) {
          
          Draft_Order_Future$Actual_Pick[teamtradeWithPicks_Future[j]] <<- teamwithPick
          
        }
        
      }
      
      
      
      
      
      output$Transactions <- DT::renderDataTable({ 
        
        datatable(data = LogTable,
                  escape = F , 
                  # one of these below options makes Club name go over one line only! Important!
                  class = "row-bordered hover stripe nowrap order-column" , 
                  rownames = FALSE,
                  options  = list(dom = "t",
                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                  paging = F,
                                  searching = F,
                                  scrollX = F, 
                                  info = F,
                                  headerCallback = JS(
                                    "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    
}"))) %>%
          formatStyle('Details', `text-align` = 'left')
        
      })
      
      
      teamwithPick <<- as.character(Draft_Order %>% filter(Pick == counter$countervalue) %>% pull(Actual_Pick)) 
      Team_choices <<- c("",setdiff(c(trade_Teams) , teamwithPick ))
      updateSelectInput(session, inputId = "tradeWith_btn", label = "Select Team To Trade Current Pick With" , choices = Team_choices, selected = Team_choices[2])  
      
      
      # Next 20 Picks widgetUserBox()'s *still inside submitTrade_btn*      
      #####  
      
      output$CurrentlyPicking <- renderUI({
        
        widgetUserBox(width = 12, height = 'auto',
                      collapsible = F,
                      type = NULL,
                      boxToolSize = 'xs' ,
                      src = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue,3])) %>% select(Circle) %>% pull() ,
                      background = T,
                      backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue,3])) %>% select(BannerURL_2) %>% pull() ,
                      column(12,align = 'center',
                             br(),
                             br(),
                             pickerInput('Player_Select',   choices = sort(Draftee_Data %>% pull(Player)) , choicesOpt = list(subtext = Draftee_Data %>% arrange(Player) %>% mutate(Paste = ifelse(!is.na(Ties) , paste0(" (",Ties,")") , paste0(""))) %>% pull(Paste)) , options = list(title = 'Select Player To Draft Here' , `live-search` = TRUE) ),
                             
                             HTML(paste0(h2("Pick: ",counter$countervalue))) ,
                             HTML(paste0(h2(tags$b(tags$u("On The Clock:"))," ",Draft_Order[counter$countervalue,3] %>% pull()))),
                             tags$br(),
                             fluidRow(
                               column(1,offset = 4,  
                                      textOutput("timeleft") , 
                               ),
                               column(6,
                                      dropdown(width = '260px', 
                                               
                                               tags$h4("Timer Controls:"),
                                               fluidRow(
                                                 actionBttn('start','Start', style = 'bordered', color = 'primary', size = 'sm'),
                                                 actionBttn('stop' ,'Pause', style = 'bordered', color = 'primary', size = 'sm'),
                                                 actionBttn('reset','Reset', style = 'bordered', color = 'primary', size = 'sm'),
                                                 br(),
                                                 h6("These can be adjusted via",tagList(icon("hourglass-o")),"in Settings"),
                                               ),
                                               style = "unite", icon = icon("gear"), size = 'xs', up = T, 
                                               status = "primary",
                                               animate = animateOptions(
                                                 exit = animations$fading_exits$fadeOutRightBig
                                               )
                                      )
                               )  
                             ),
                             
                             tags$br(),
                             column(width = 6,  actionBttn('draft_btn', 'DRAFT', icon("crosshairs", lib = "font-awesome"), style = "unite", color = 'primary', size = 'm', block = T )),
                             column(width = 6,  actionBttn('pass_btn' , 'PASS' , icon("hand-paper", lib = "font-awesome"), style = "unite", color = 'primary', size = 'm', block = T )),
                             column(width = 12, actionBttn('trade_btn', 'TRADE', icon("handshake" , lib = "font-awesome"), style = 'unite', color = 'primary', size = 'm', block = T)),
                             
                      ),
                      
        ) # close userbox
      })
      
      output$userBox_2 <- renderUI({
        widgetUserBox( # 2
          title = paste0("Pick ", counter$countervalue+1,".") , 
          subtitle = paste0(Draft_Order[1+1,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+1] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+1,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+1,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+1,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      }) 
      
      output$userBox_3 <- renderUI({
        widgetUserBox( # 3
          title = paste0("Pick ", counter$countervalue+2,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+2,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+2] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+2,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F, 
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+2,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+2,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_4 <- renderUI({
        widgetUserBox( # 4
          title = paste0("Pick ", counter$countervalue+3,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+3,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+3] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+3,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+3,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+3,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_5 <- renderUI({
        widgetUserBox( # 5
          title = paste0("Pick ", counter$countervalue+4,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+4,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+4] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+4,3])) %>% select(BannerURL) %>% pull() , 
          color = 'green',
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+4,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+4,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        ) 
      })
      
      output$userBox_6 <- renderUI({
        widgetUserBox( # 6
          title = paste0("Pick ", counter$countervalue+5,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+5,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+5] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+5,3])) %>% select(BannerURL) %>% pull() , 
          color = 'green',
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+5,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+5,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_7 <- renderUI({
        widgetUserBox( # 7
          title = paste0("Pick ", counter$countervalue+6,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+6,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+6] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+6,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+6,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+6,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )  
      }) 
      
      output$userBox_8 <- renderUI({
        widgetUserBox( # 8
          title = paste0("Pick ", counter$countervalue+7,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+7,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+7] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+7,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+7,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+7,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_9 <- renderUI({
        widgetUserBox( # 9
          title = paste0("Pick ", counter$countervalue+8,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+8,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+8] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+8,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+8,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+8,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_10 <- renderUI({
        widgetUserBox( # 10
          title = paste0("Pick ", counter$countervalue+9,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+9,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+9] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+9,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+9,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+9,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_11 <- renderUI({
        widgetUserBox( # 11
          title = paste0("Pick ", counter$countervalue+10,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+10,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+10] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+10,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+10,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+10,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_12 <- renderUI({
        widgetUserBox( # 12
          title = paste0("Pick ", counter$countervalue+11,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+11,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+11] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+11,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+11,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+11,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })  
      
      output$userBox_13 <- renderUI({
        widgetUserBox( # 13
          title = paste0("Pick ", counter$countervalue+12,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+12,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+12] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+12,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+12,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+12,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_14 <- renderUI({
        widgetUserBox( # 14
          title = paste0("Pick ", counter$countervalue+13,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+13,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+13] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+13,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+13,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+13,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_15 <- renderUI({  
        widgetUserBox( # 15
          title = paste0("Pick ", counter$countervalue+14,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+14,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+14] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+14,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+14,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+14,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_16 <- renderUI({
        widgetUserBox( # 16
          title = paste0("Pick ", counter$countervalue+15,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+15,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+15] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+15,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+15,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+15,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_17 <- renderUI({  
        widgetUserBox( # 17
          title = paste0("Pick ", counter$countervalue+16,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+16,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+16] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+16,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+16,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+16,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_18 <- renderUI({  
        widgetUserBox( # 18
          title = paste0("Pick ", counter$countervalue+17,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+17,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+17] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+17,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+17,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+17,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_19 <- renderUI({  
        widgetUserBox( # 19
          title = paste0("Pick ", counter$countervalue+18,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+18,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+18] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+18,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+18,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+18,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      
      output$userBox_20 <- renderUI({  
        widgetUserBox( # 20
          title = paste0("Pick ", counter$countervalue+19,".") , 
          subtitle = paste0(Draft_Order[counter$countervalue+19,3]) ,
          type = 2,
          width = 12,
          src = paste0(Draft_Order$Round[counter$countervalue+19] %>% as.data.frame() %>% rename("Rnd" = 1) %>% inner_join(.,RoundImg , by = c("Rnd" = "Round")) %>% pull(RndImg) ),
          background = T,
          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+19,3])) %>% select(BannerURL) %>% pull() , 
          closable = F,
          collapsible = F,
          footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+19,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+19,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
        )
      })
      #####
      
      # - Output Draftboard Table *still inside submitTrade_btn*      
      #####  
      output$DraftBoard <- DT::renderDataTable({ 
        
        datatable(data = Draft_Order %>% inner_join(. , Teams_Data[c(1,5)] , by = c("Actual_Pick" = "Team")) %>% 
                    mutate(Team = paste0(Actual_Pick,"<img src=","'",TeamLogo,"'"," ", "height='40'style=float:right></img>")) %>% select(Round,Pick,Team,Points) %>% left_join(.,Selected_Table[c(1,3)], by = c("Pick"="Pick")) %>% select(Round,Pick,Team,Player,Points) ,  
                  escape = F , 
                  # one of these below options makes Club name go over one line only! Important!
                  class = "row-bordered hover stripe nowrap order-column" , 
                  rownames = FALSE,
                  options  = list(dom = "t",
                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                  paging = F,
                                  searching = F,
                                  scrollX = F, # 'background-color', '#002651'; 'border-bottom-left-radius','5px'; 
                                  info = F, 
                                  headerCallback = JS(
                                    "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(3).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(4).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(3).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(4).css('color', 'white')
}"))) %>%
          formatStyle(
            "Pick" ,
            color = styleEqual(counter$countervalue,"white") ,
            backgroundColor = styleEqual(counter$countervalue,"#163C91"))
        
      }) # close renderDataTable()
      #####
      
      # - Output *FUTURE* Draftboard Table *still inside submitTrade_btn*      
      #####    
      output$DraftBoard_Future <- DT::renderDataTable({ 
        
        datatable(data = Draft_Order_Future %>% inner_join(. , Teams_Data[c(1,5)] , by = c("Actual_Pick" = "Team")) %>% 
                    mutate(Team = paste0(Actual_Pick,"<img src=","'",TeamLogo,"'"," ", "height='40'style=float:right></img>")) %>% select(Round,Pick,Team,Points) ,
                  escape = F , 
                  # one of these below options makes Club name go over one line only! Important!
                  class = "row-bordered hover stripe nowrap order-column" , 
                  rownames = FALSE,
                  options  = list(dom = "t",
                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                  paging = F,
                                  searching = F,
                                  scrollX = F, # 'background-color', '#002651'; 'border-bottom-left-radius','5px'; 
                                  info = F, 
                                  headerCallback = JS(
                                    "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(3).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(4).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(3).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(4).css('color', 'white')}"))) 
        
        
        
      }) # close renderDataTable()
      #####
      
      # - For Trade Options Modal (1 here, 1 in trade_btn section)
      ##### 
      
      # Display Trade Partner in trade modal
      output$TradePartner <- reactive({ input$tradeWith_btn })
      output$Picking <- reactive({ Draft_Order[counter$countervalue,3] %>% pull() })
      
      output$TradeModal_Picking_img <- renderUI({
        img(src = Teams_Data %>% filter(Team == Draft_Order[counter$countervalue,3] %>% pull()) %>% pull(TeamLogo), width = '100%')
      })
      
      output$TradeModal_Partner_img <- renderUI({
        img(src = Teams_Data %>% filter(Team == input$tradeWith_btn) %>% pull(TeamLogo), width = '100%')
      })
      #####
      
      # - Pick Time IF/ELSE *trade accepted*
      if (input$pickTime == "30 sec."){
        pick$time <- 30
        base$time <- 30
        pickvar$val <- "30 sec."
      } else if (input$pickTime == "1 min.") {
        pick$time <- 60   
        base$time <- 60
        pickvar$val <- "1 min."
      } else if (input$pickTime == "2 min.") {
        pick$time <- 120
        base$time <- 120
        pickvar$val <- "2 min."
      } else if (input$pickTime == "3 min.") {
        pick$time <- 180  
        base$time <- 180
        pickvar$val <- "3 min."
      } else if (input$pickTime == "4 min.") {
        pick$time <- 240  
        base$time <- 240
        pickvar$val <- "4 min."
      } else {
        pick$time <- 300
        base$time <- 300
        pickvar$val <- "5 min."
      }
      
      
    } else {
      
      showModal(modalDialog(
        easyClose = T , 
        footer = actionBttn(inputId = "closeRejected",label = "Close",color = 'primary',size = 'sm'),
        
        fluidRow(      
          
          column(12, align="center",
                 div(style="display: inline-block;",img(src = "https://i.ibb.co/dWgm75X/Screen-Shot-2020-09-27-at-1-05-46-pm.png", height=300, width=450))),
          
          
          
        ), 
        
        fluidRow(align = 'center',
                 HTML(paste0(h4(ifelse(interestColour == "danger" , paste0(teamwithPick," have no interest in that trade.") ,
                                       ifelse(interestColour == "warning", paste0(teamwithPick," are somewhat interested in a trade but need more value."), 
                                              paste0(teamwithPick," are interested but need more value."))))))
                 
        )
        
        
      ))
      
      
    }
    
  })
  # - (end) Submit Trade Button
  
  ###############################
  #     Timer Functionality     #
  ###############################
  #####    
  
  active <- reactiveVal(FALSE)
  
  # Output the time left.
  output$timeleft <- renderText({
    paste0(sprintf('%02d:%02d', minute(seconds_to_period(pick$time)), second(seconds_to_period(pick$time))))
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    
    invalidateLater(1000, session)
    
    isolate({
      if(active())
      {
        pick$time <- pick$time-1 # this is what decrements the time by 1
        
        if(pick$time == 0 | pick$time < 0)
        {
          active(FALSE)
          
          # Pick the autodrafted player
          Selected_Table_tmp <- data.frame("Pick" = numeric() , "Team" = character() , "Player" = character())
          Selected_Table_tmp[1,1] <- counter$countervalue
          Selected_Table_tmp[1,2] <- Draft_Order[counter$countervalue,3] %>% pull() 
          Selected_Table_tmp[1,3] <- setdiff(Draftee_Data %>% arrange(Projected) %>% pull(Player),Selected_Table$Player)[1]
          
          # Binding tmp table (Selected_Table_tmp) to master (Selected_Table)
          Selected_Table_tmp <- as.data.frame(Selected_Table_tmp)
          Selected_Table <<- as.data.frame(rbind(Selected_Table,Selected_Table_tmp))
          Selected_Table <<- na.omit(Selected_Table)
          
          # This is the updated player pool that doesnt include the players that have been drafted
          UpdatedPool <- reactive({ sort(c(setdiff(Draftee_Data$Player , Selected_Table$Player))) })
          
          # This is updating the select input dropdown with the updated player pool (UpdatedPool())
          updatePickerInput(session , inputId = "Player_Select" , choices = UpdatedPool() , choicesOpt = list(subtext = UpdatedPool() %>% as.data.frame() %>% inner_join(. , Draftee_Data[c(1,9)] , by = c("." = "Player")) %>% mutate(Paste = ifelse(!is.na(Ties) , paste0("(",Ties,")") , paste0(""))) %>% pull(Paste)) , selected = "" ) 
          
          # - Output Draftboard Table (subject to being moved) - update after autodraft
          #####    
          output$DraftBoard <- DT::renderDataTable({ 
            
            datatable(data = Draft_Order %>% inner_join(. , Teams_Data[c(1,5)] , by = c("Actual_Pick" = "Team")) %>% 
                        mutate(Team = paste0(Actual_Pick,"<img src=","'",TeamLogo,"'"," ", "height='40'style=float:right></img>")) %>% select(Round,Pick,Team,Points) %>% left_join(.,Selected_Table[c(1,3)], by = c("Pick"="Pick")) %>% select(Round,Pick,Team,Player,Points) ,  
                      escape = F , 
                      # one of these below options makes Club name go over one line only! Important!
                      class = "row-bordered hover stripe nowrap order-column" , 
                      rownames = FALSE,
                      options  = list(dom = "t",
                                      columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                      paging = F,
                                      searching = F,
                                      scrollX = F, 
                                      info = F, 
                                      headerCallback = JS(
                                        "function( thead, data, start, end, display ) {
                                
                                                    $('th', thead).css('border-bottom', '2px solid #FD1300')
                                
                                                    $(thead).closest('thead').find('th').eq(0).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(1).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(2).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(3).css('background-color', '#163C91')
                                                    $(thead).closest('thead').find('th').eq(4).css('background-color', '#163C91')
                                                    
                                                    $(thead).closest('thead').find('th').eq(0).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(1).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(2).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(3).css('color', 'white')
                                                    $(thead).closest('thead').find('th').eq(4).css('color', 'white')
}"))) %>%
              formatStyle(
                "Pick" ,
                color = styleEqual(counter$countervalue,"white") ,
                backgroundColor = styleEqual(counter$countervalue,"#163C91"))
            
            
            
          }) # close renderDataTable()
          #####
          
          # Top 5 Available boxes
          #####
          
          Draftee_Data <- Draftee_Data %>% arrange(Projected)
          Top_P1 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[1] 
          Top_P2 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[2]
          Top_P3 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[3] 
          Top_P4 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[4] 
          Top_P5 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[5]
          Top_P6 <- setdiff(Draftee_Data$Player , Selected_Table$Player)[6] 
          
          output$Top5 <- renderUI({
            
            productList(
              
              productListItem( # Top: 1 
                src = Draftee_Data %>% filter(Player == Top_P1) %>% pull(Image),
                productTitle = Top_P1,
                Draftee_Data %>% filter(Player == Top_P1) %>% pull(Position), br(),
                Draftee_Data %>% filter(Player == Top_P1) %>% pull(Club), br(),
                productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
                priceColor   = "success",
              ),
              
              productListItem( # Top: 2 
                src = Draftee_Data %>% filter(Player == Top_P2) %>% pull(Image),
                productTitle = Top_P2,
                Draftee_Data %>% filter(Player == Top_P2) %>% pull(Position), br(),
                Draftee_Data %>% filter(Player == Top_P2) %>% pull(Club), br(),
                productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
                priceColor   = "success",
              ),
              
              productListItem( # Top: 3 
                src = Draftee_Data %>% filter(Player == Top_P3) %>% pull(Image),
                productTitle = Top_P3,
                Draftee_Data %>% filter(Player == Top_P3) %>% pull(Position), br(),
                Draftee_Data %>% filter(Player == Top_P3) %>% pull(Club), br(),
                productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
                priceColor   = "success",
              ),
              
              productListItem( # Top: 4 
                src = Draftee_Data %>% filter(Player == Top_P4) %>% pull(Image),
                productTitle = Top_P4,
                Draftee_Data %>% filter(Player == Top_P4) %>% pull(Position), br(),
                Draftee_Data %>% filter(Player == Top_P4) %>% pull(Club), br(),
                productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
                priceColor   = "success",
              ),
              
              productListItem( # Top: 5 
                src = Draftee_Data %>% filter(Player == Top_P5) %>% pull(Image),
                productTitle = Top_P5,
                Draftee_Data %>% filter(Player == Top_P5) %>% pull(Position), br(),
                Draftee_Data %>% filter(Player == Top_P5) %>% pull(Club), br(),
                productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
                priceColor   = "success",
              ),
              
              productListItem( # Top: 5 
                src = Draftee_Data %>% filter(Player == Top_P6) %>% pull(Image),
                productTitle = Top_P6,
                Draftee_Data %>% filter(Player == Top_P6) %>% pull(Position), br(),
                Draftee_Data %>% filter(Player == Top_P6) %>% pull(Club), br(),
                productPrice = paste0("Draft Range: " , setdiff(Draftee_Data$Player , Selected_Table$Player) %>% as.data.frame() %>% rename("Player" = 1) %>% inner_join(. , Draftee_Data[,c("Player","Projected","Range")] , by = c("Player" = "Player")) %>% top_n(wt = Projected , n = -1) %>% select(Range)  %>% pull() ) ,
                priceColor   = "success",
              ),
            ) # productList()
            
          })
          #####
          
          showModal(modalDialog(
            easyClose = T , 
            fluidRow(      
              column(12, align="center",
                     div(style="display: inline-block;",img(src = "https://i.ibb.co/qg7pkMb/Screen-Shot-2020-09-30-at-11-31-48-pm.png", height=230, width=340)))
            ),
            fluidRow(      
              column(12,
                     h4("Time has expired, the CPU has autodrafted the next best available player: ",tags$u(Selected_Table_tmp[1,3])),
                     br(),
                     h4(Draft_Order[counter$countervalue+1,3] %>% pull(), "are now on the clock with Pick ",counter$countervalue+1,"."))
            ),
            footer = actionBttn(inputId = "closeExpired",label = "Close",color = 'primary',size = 'sm')
          )
          )
          
          
          pick$time <- base$time
          active(TRUE)
          counter$countervalue <- counter$countervalue + 1
          
          # Next 20 Picks widgetUserBox()'s *inside timer functionality*
          #####  
          
          output$CurrentlyPicking <- renderUI({
            
            widgetUserBox(width = 12, height = 'auto',
                          collapsible = F,
                          type = NULL,
                          boxToolSize = 'xs' ,
                          src = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue,3])) %>% select(Circle) %>% pull() ,
                          background = T,
                          backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue,3])) %>% select(BannerURL_2) %>% pull() ,
                          column(12,align = 'center',
                                 br(),
                                 br(),
                                 pickerInput('Player_Select',   choices = sort(Draftee_Data %>% pull(Player)) , choicesOpt = list(subtext = Draftee_Data %>% arrange(Player) %>% mutate(Paste = ifelse(!is.na(Ties) , paste0(" (",Ties,")") , paste0(""))) %>% pull(Paste)) , options = list(title = 'Select Player To Draft Here' , `live-search` = TRUE) ),
                                 
                                 HTML(paste0(h2("Pick: ",counter$countervalue))) ,
                                 HTML(paste0(h2(tags$b(tags$u("On The Clock:"))," ",Draft_Order[counter$countervalue,3] %>% pull()))),
                                 tags$br(),
                                 fluidRow(
                                   column(1,offset = 4,  
                                          textOutput("timeleft") , 
                                   ),
                                   column(6,
                                          dropdown(width = '260px', 
                                                   
                                                   tags$h4("Timer Controls:"),
                                                   fluidRow(
                                                     actionBttn('start','Start', style = 'bordered', color = 'primary', size = 'sm'),
                                                     actionBttn('stop' ,'Pause', style = 'bordered', color = 'primary', size = 'sm'),
                                                     actionBttn('reset','Reset', style = 'bordered', color = 'primary', size = 'sm'),
                                                     br(),
                                                     h6("These can be adjusted via",tagList(icon("hourglass-o")),"in Settings"),
                                                   ),
                                                   style = "unite", icon = icon("gear"), size = 'xs', up = T, 
                                                   status = "primary",
                                                   animate = animateOptions(
                                                     exit = animations$fading_exits$fadeOutRightBig
                                                   )
                                          )
                                   )  
                                 ),
                                 
                                 tags$br(),
                                 column(width = 6,  actionBttn('draft_btn', 'DRAFT', icon("crosshairs", lib = "font-awesome"), style = "unite", color = 'primary', size = 'm', block = T )),
                                 column(width = 6,  actionBttn('pass_btn' , 'PASS' , icon("hand-paper", lib = "font-awesome"), style = "unite", color = 'primary', size = 'm', block = T )),
                                 column(width = 12, actionBttn('trade_btn', 'TRADE', icon("handshake" , lib = "font-awesome"), style = 'unite', color = 'primary', size = 'm', block = T)),
                                 
                          ),
                          
            ) # close userbox
          })
          
          output$userBox_2 <- renderUI({
            widgetUserBox( # 2
              title = paste0("Pick ", counter$countervalue+1,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+1,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+1]),
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+1,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+1,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+1,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          }) 
          
          output$userBox_3 <- renderUI({
            widgetUserBox( # 3
              title = paste0("Pick ", counter$countervalue+2,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+2,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+2]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+2,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F, 
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+2,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+2,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_4 <- renderUI({
            widgetUserBox( # 4
              title = paste0("Pick ", counter$countervalue+3,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+3,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+3]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+3,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+3,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+3,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_5 <- renderUI({
            widgetUserBox( # 5
              title = paste0("Pick ", counter$countervalue+4,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+4,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+4]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+4,3])) %>% select(BannerURL) %>% pull() , 
              color = 'green',
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+4,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+4,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            ) 
          })
          
          output$userBox_6 <- renderUI({
            widgetUserBox( # 6
              title = paste0("Pick ", counter$countervalue+5,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+5,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+5]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+5,3])) %>% select(BannerURL) %>% pull() , 
              color = 'green',
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+5,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+5,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_7 <- renderUI({
            widgetUserBox( # 7
              title = paste0("Pick ", counter$countervalue+6,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+6,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+6]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+6,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+6,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+6,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )  
          }) 
          
          output$userBox_8 <- renderUI({
            widgetUserBox( # 8
              title = paste0("Pick ", counter$countervalue+7,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+7,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+7]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+7,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+7,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+7,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_9 <- renderUI({
            widgetUserBox( # 9
              title = paste0("Pick ", counter$countervalue+8,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+8,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+8]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+8,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+8,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+8,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_10 <- renderUI({
            widgetUserBox( # 10
              title = paste0("Pick ", counter$countervalue+9,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+9,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+9]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+9,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+9,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+9,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_11 <- renderUI({
            widgetUserBox( # 11
              title = paste0("Pick ", counter$countervalue+10,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+10,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+10]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+10,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+10,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+10,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_12 <- renderUI({
            widgetUserBox( # 12
              title = paste0("Pick ", counter$countervalue+11,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+11,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+11]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+11,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+11,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+11,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })  
          
          output$userBox_13 <- renderUI({
            widgetUserBox( # 13
              title = paste0("Pick ", counter$countervalue+12,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+12,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+12]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+12,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+12,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+12,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_14 <- renderUI({
            widgetUserBox( # 14
              title = paste0("Pick ", counter$countervalue+13,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+13,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+13]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+13,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+13,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+13,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_15 <- renderUI({  
            widgetUserBox( # 15
              title = paste0("Pick ", counter$countervalue+14,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+14,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+14]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+14,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+14,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+14,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_16 <- renderUI({
            widgetUserBox( # 16
              title = paste0("Pick ", counter$countervalue+15,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+15,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+15]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+15,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+15,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+15,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_17 <- renderUI({  
            widgetUserBox( # 17
              title = paste0("Pick ", counter$countervalue+16,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+16,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+16]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+16,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+16,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+16,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_18 <- renderUI({  
            widgetUserBox( # 18
              title = paste0("Pick ", counter$countervalue+17,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+17,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+17]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+17,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+17,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+17,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_19 <- renderUI({  
            widgetUserBox( # 19
              title = paste0("Pick ", counter$countervalue+18,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+18,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+18]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+18,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+18,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+18,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          
          output$userBox_20 <- renderUI({  
            widgetUserBox( # 20
              title = paste0("Pick ", counter$countervalue+19,".") , 
              subtitle = paste0(Draft_Order[counter$countervalue+19,3]) ,
              type = 2,
              width = 12,
              src = paste0(Draft_Order$RndImg[counter$countervalue+19]) , 
              background = T,
              backgroundUrl = Teams_Data %>% filter(Team == paste0(Draft_Order[counter$countervalue+19,3])) %>% select(BannerURL) %>% pull() , 
              closable = F,
              collapsible = F,
              footer = HTML(paste0("Picks: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+19,3] %>% pull()) %>% pull(Pick) , sep="", collapse=", "),HTML('&nbsp;'),HTML('&nbsp;')," | ",HTML('&nbsp;'),HTML('&nbsp;'),"DVI Capital: ",paste0(Draft_Order %>% filter(Actual_Pick == Draft_Order[counter$countervalue+19,3] %>% pull()) %>% summarise(pts = sum(Points)) %>% pull())))
            )
          })
          #####
          
          # - Pick Time IF/ELSE *inside timer incase of time out*
          if (input$pickTime == "30 sec."){
            pick$time <- 30
            base$time <- 30
            pickvar$val <- "30 sec."
          } else if (input$pickTime == "1 min.") {
            pick$time <- 60   
            base$time <- 60
            pickvar$val <- "1 min."
          } else if (input$pickTime == "2 min.") {
            pick$time <- 120
            base$time <- 120
            pickvar$val <- "2 min."
          } else if (input$pickTime == "3 min.") {
            pick$time <- 180  
            base$time <- 180
            pickvar$val <- "3 min."
          } else if (input$pickTime == "4 min.") {
            pick$time <- 240  
            base$time <- 240
            pickvar$val <- "4 min."
          } else {
            pick$time <- 300
            base$time <- 300
            pickvar$val <- "5 min."
          }
          
        } 
      }
    })
  })
  
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop,  {active(FALSE)})
  observeEvent(input$reset, {pick$time <- pick$time + 900})
  
  #####
  # - (end) Timer
  
  ###############################
  #         Modal Closes        #
  ###############################
  #####
  # closes the modal that pops up when you launch the app
  observeEvent(input$startModalClose, { 
    removeModal()
  })
  
  # closes the modal that pops up when you pass on a pick
  observeEvent(input$closePassed, { 
    removeModal()
  })
  
  # closes the second modal that pops up after you hit Match Bid
  observeEvent(input$closeYes, { 
    removeModal()
  })
  
  # closes the second modal that pops up after you hit No Match to the bid 
  observeEvent(input$closeNo, { 
    removeModal()
  })
  
  # closes the modal that pops up when you havent selected a player
  observeEvent(input$errorClose, { 
    removeModal()
  })
  
  # closes the modal that pops up when you draft a player regularly
  observeEvent(input$closeDrafted, { 
    removeModal()
  })
  
  # closes modal that pops up when trade is accepted
  observeEvent(input$closeAccepted, { 
    removeModal()
  })
  
  # closes modal that pops up when trade is rejected
  observeEvent(input$closeRejected, { 
    removeModal()
  })
  
  # closes the modal that pops up when time expires on the clock 
  observeEvent(input$closeExpired, { 
    removeModal()
  })
  
  #####
  
} # close server


shinyApp(ui = ui, server = server)