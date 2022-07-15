
# patch::patch_function(streamgraph:::streamgraph_html, "list", shiny::tagList, replace_it = T, auto_assign_patched_function = T)

options(scipen = 15)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

# data load from
# https://www.rbi.org.in/Scripts/Statistics.aspx

# graph libs
library(dygraphs)
library(streamgraph)
library(plotly)
library(tidycells)

# own libs
source("Lib/load_data_from_local.R")
source("Lib/read_data.R")
source("Lib/progress_of_data_read.R")

ui <- dashboardPage(
  title = "Payment System Indicators",
  
  header = dashboardHeader(
    title = tags$div(
      conditionalPanel(
        condition = "input.main_sidebar == true",
        tags$img(src='RBILogo.webp', height='30px', align = "left"),
        h4("Payment System Indicators")
      ),
      conditionalPanel(
        condition = "input.main_sidebar == false",
        tags$img(src='RBILogo.webp', height='20px', align = "left", style="opacity:0.5;"),
        h6("PSS", style="opacity:0.5;")
      )
    ),
    controlbarIcon = icon("cogs")
  ),
  
  sidebar = dashboardSidebar(
    id = "main_sidebar",
    sidebarMenu(
      id = "sidebarMenu_tab",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Data Upload", tabName = "data_upload", icon = icon("th"))
    )
  ),
  
  body = dashboardBody(
    useShinyjs(),
    shinyjs::extendShinyjs(
      text = "shinyjs.reloadMe = function() { document.location.reload(); window.location.reload(); history.go(-1); }",
      functions = c("reloadMe")
    ),
    tabItems(
      # dashboard tab content
      tabItem(
        tabName = "dashboard",
        # info box
        fluidRow(
          infoBoxOutput("infobox1"),
          infoBoxOutput("infobox2"),
          infoBoxOutput("infobox3")
        ),
        
        # Control UI
        uiOutput("main_panel_control"),
        # dashboard area
        fluidRow(
          column(
            6,
            div(
              dygraphOutput("dyseries"),
              style="background-color: white; height: 400px;"
            )
          ),
          column(
            6,
            div(
              streamgraphOutput("stseries"),
              style="background-color: white;height: 400px;"
            )
            
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            uiOutput("left_lower_chart"), 
          ),
          column(
            6,
            plotlyOutput("pltbar")
          )
        )
      ),
      
      # data_upload tab content
      tabItem(
        tabName = "data_upload",
        # info box
        fluidRow(
          infoBoxOutput("du_infobox1"),
          infoBoxOutput("du_infobox2"),
          infoBoxOutput("du_infobox3")
        ),
        fluidRow(
          box(
            title = "Data Upload",
            width = 6, 
            fileInput(
              inputId = "upload_file_1", 
              label = div(
                "Choose Data File", 
                actionButton(
                  inputId = "upload_file_1_help", 
                  icon = icon("question"), 
                  label = NULL
                )
              ),
              multiple = FALSE,
              accept = c(
                "application/vnd.ms-excel",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
            )
          ),
          box(
            title = "Data Processing",
            width = 6,
            id = "data_proc_pane",
            shinyWidgets::progressBar(
              id = "dp_prog",
              title = "Data Processing Progress" ,
              value = 0, 
              display_pct = TRUE, 
              striped = TRUE
            ),
            actionBttn(
              inputId = "reload_app", 
              label = "Reload", 
              icon = icon("retweet"), 
              color = "danger", 
              style = "fill"
            )
          )
        )
      )
    )
  ),
  
  controlbar = dashboardControlbar(
    id = "right_controlbar",
    controlbarMenu(
      controlbarItem(
        "Chart Control",
        uiOutput("right_controlbar_control") 
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  control_place <- reactiveVal("panel")
  
  observe({
    req(input$gr_var)
    tar <- arg_info$Gr_Var$name == input$gr_var
    tar <- arg_info$Gr_Var$val[tar]
    tar <- unique(dat[[tar]])
    
    updateSelectizeInput(
      session,
      inputId = "gr_var_fine",
      label = NULL, 
      choices = tar,
      selected = tar
    )
  })
  
  output$main_panel_control <- renderUI({
    
    
    if(control_place()=="panel"){
      tagList(
        fluidRow(
          column(
            2,
            selectInput(
              inputId = "time_freq",
              label = "Time Frequency", 
              choices = arg_info$Time_Var$name,
              multiple = FALSE
            )
          ),
          column(
            2,
            selectInput(
              inputId = "gr_var",
              label = "Analyze on", 
              choices = arg_info$Gr_Var$name,
              multiple = FALSE
            )
          ),
          column(
            2,
            tags$label("Further Subset"),
            tags$input(id="show_gr_var_fine0", type="checkbox"),
            conditionalPanel(
              condition = "input.show_gr_var_fine0 == true", 
              selectizeInput(
                inputId = "gr_var_fine",
                label = NULL,
                choices = unique(dat[[arg_info$Gr_Var$val[1]]]),
                selected = unique(dat[[arg_info$Gr_Var$val[1]]]),
                multiple = TRUE
              )
            )
          ),
          column(
            2,
            selectInput(
              inputId = "var_type",
              label = "Focus on", 
              choices = arg_info$Var_Type$name,
              multiple = FALSE
            )
          ),
          column(
            2,
            selectInput(
              inputId = "agg_fn",
              label = "Summarizing Function", 
              choices = arg_info$Aggregator$name,
              multiple = FALSE
            )
          ),
          column(
            2,
            tags$label(
              "Shift Control", 
              a("for further controls", style = "font-size: 10px;")),
            div(
              materialSwitch(
                inputId = "shift_ctrl_pg", 
                label = "to Right Side",
                value = FALSE,
                status = "primary"
              )
            )
          )
        )
      )
    }else{
      updateControlbar("right_controlbar")
      NULL
    }
  })
  
  observe({
    if(isTRUE(input$shift_ctrl_bar)){
      control_place("panel")
    }
  })
  
  observe({
    if(isTRUE(input$shift_ctrl_pg)){
      control_place("controlbar")
    }
  })
  
  output$right_controlbar_control <- renderUI({
    
    req(input$shift_ctrl_pg)
    
    if(control_place()=="controlbar"){
      tagList(
        selectInput(
          inputId = "time_freq",
          label = "Time Frequency",
          choices = arg_info$Time_Var$name,
          multiple = FALSE
        ),
        selectInput(
          inputId = "gr_var",
          label = "Analyze on",
          choices = arg_info$Gr_Var$name,
          multiple = FALSE
        ),
        tags$label("Further Subset"),
        tags$input(id="show_gr_var_fine", type="checkbox", checked="checked"),
        conditionalPanel(
          condition = "input.show_gr_var_fine == true",
          selectizeInput(
            inputId = "gr_var_fine",
            label = NULL,
            choices = unique(dat[[arg_info$Gr_Var$val[1]]]),
            selected = unique(dat[[arg_info$Gr_Var$val[1]]]),
            multiple = TRUE
          )
        ),
        selectInput(
          inputId = "var_type",
          label = "Focus on",
          choices = arg_info$Var_Type$name,
          multiple = FALSE
        ),
        selectInput(
          inputId = "agg_fn",
          label = "Summarizing Function",
          choices = arg_info$Aggregator$name,
          multiple = FALSE
        ),
        sliderInput(
          inputId = "ma_window", 
          label = "Moving Average Window", 
          value = 1, 
          min = 1, 
          max = 12,
          round = TRUE,
          step = 1
        ),
        
        conditionalPanel(
          condition = "input.time_freq != 'Quarter'",
          radioGroupButtons(
            inputId = "LL_chart_sw",
            label = "Left Lower Chart-type",
            choices = c("Pie","Growth"),
            justified = TRUE,
            checkIcon = list(
              yes = icon(
                "ok", 
                lib = "glyphicon"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.LL_chart_sw == 'Growth' || input.time_freq == 'Quarter'",
          selectInput(
            inputId = "xox_in",
            label = "Growth Variant",
            choices = arg_info$xox_map$vals[
              arg_info$xox_map$name == arg_info$Time_Var$name[1]
            ][[1]]
          )
        ),
        tags$label("Shift Control"),
        div(
          materialSwitch(
            inputId = "shift_ctrl_bar",
            label = "to Main Panel",
            value = FALSE,
            status = "primary"
          )
        )
      )
    }else{
      updateControlbar("right_controlbar")
      NULL
    }
  })
  
  
  #left lower chart
  
  output$left_lower_chart <- renderUI({
    if(isTRUE(input$LL_chart_sw == "Growth")|isTRUE(input$time_freq=="Quarter")){
      div(
        dygraphOutput("growth_chart"),
        style="background-color: white; height: 400px;"
      )
    }else{
      plotlyOutput("pltpie")
    }
  })
  
  # growth UI update
  observe({
    input$time_freq
    req(input$LL_chart_sw)
    if(input$LL_chart_sw == "Growth" | input$time_freq == "Quarter"){
      
      updateSelectInput(
        session = session, 
        inputId = "xox_in", 
        label = "Growth Variant",
        choices = arg_info$xox_map$vals[
          arg_info$xox_map$name == interpret(input$time_freq, arg_info$Time_Var)
        ][[1]]
      )
    }
  })
  
  ##############
  # load data
  disp_dat <- reactiveVal()
  
  observe({
    
    req(input$var_type)
    
    disp_dat(
      get_data(
        # Var_Type_in = interpret(input$var_type, arg_info$Var_Type),
        Time_Var = interpret(input$time_freq, arg_info$Time_Var), 
        Aggregator = interpret(input$agg_fn, arg_info$Aggregator), 
        Gr_Var = interpret(input$gr_var, arg_info$Gr_Var),
        Gr_Var_Fine = input$gr_var_fine
      )
    )
  })
  
  
  ma_win <- reactiveVal(1)
  
  observe({
    
    req(input$ma_window)
    
    mt <- as.integer(input$ma_window)
    mt <- mt %>% max(1) %>% min(15)
    
    ma_win(mt)
  })
  
  output$dyseries <- renderDygraph({
    req(input$var_type)
    req(input$time_freq)
    req(input$agg_fn)
    req(input$gr_var)
    
    if(isTRUE(length(input$gr_var_fine)>0)){
      disp_dat()$get_flat_dat(
        Var= interpret(input$var_type, arg_info$Var_Type), 
        in_ts = TRUE, 
        wd = ma_win()
      ) %>% 
        dygraph() %>% 
        dyRangeSelector()
    }else{
      NULL
    }
  })
  
  output$stseries <- renderStreamgraph({
    req(input$var_type)
    req(input$dyseries_date_window)
    
    disp_dat()$get_ma_dat(wd = ma_win()) %>% 
      filter(Var_Type == interpret(input$var_type, arg_info$Var_Type)) %>% 
      filter(Time_Var >= (input$dyseries_date_window %>% min %>% as.Date)) %>% 
      filter(Time_Var <= (input$dyseries_date_window %>% max %>% as.Date)) %>%
      streamgraph(key = Gr_Var, value = Val, date = Time_Var) %>% 
      sg_fill_tableau() 
    
  })
  
  output$pltpie <- renderPlotly({
    req(input$var_type)
    disp_dat()$get_monthly_pie_dat(
      Var = interpret(input$var_type, arg_info$Var_Type),
      wd = ma_win()
    ) %>%  
      select(-Month) %>% 
      rename(Month = Month_Fct) %>% 
      plot_ly(values=~Val,labels=~factor(Gr_Var), frame=~Month, type = "pie") %>%
      config(showLink  = F, displayModeBar = F)
  })
  
  
  output$growth_chart <- renderDygraph({
    req(input$var_type)
    req(input$time_freq)
    req(input$agg_fn)
    req(input$gr_var)
    
    if(isTRUE(length(input$gr_var_fine)>0)){
      
      disp_dat()$get_xox_dat(
        Var = interpret(input$var_type, arg_info$Var_Type), 
        in_ts = TRUE, 
        wd = ma_win(), 
        xox = input$xox_in
      ) %>% 
        dygraph() %>% 
        dyRangeSelector()
    }else{
      NULL
    }
  })
  
  output$pltbar <- renderPlotly({
    req(input$var_type)
    req(input$dyseries_date_window)
    
    disp_dat()$data %>% 
      filter(Time_Var >= (input$dyseries_date_window %>% min %>% as.Date)) %>% 
      filter(Time_Var <= (input$dyseries_date_window %>% max %>% as.Date)) %>%
      filter(Var_Type == interpret(input$var_type, arg_info$Var_Type)) %>% 
      group_by(Gr_Var) %>% 
      summarise(Val = sum(Val, na.rm = TRUE), .groups = "drop") %>% 
      plot_ly(x= ~Gr_Var, y=~Val, color=~Gr_Var, type= "bar") %>% 
      layout(showlegend = FALSE, 
             xaxis = list(title = ""),
             yaxis = list(title = input$var_type)) %>% 
      config(showLink  = F, displayModeBar = F)
  })
  
  # infobox fills
  
  output$infobox1 <- renderInfoBox({
    req(input$dyseries_date_window)
    
    infoBox(
      title = "Date Range", 
      value = paste0(
        input$dyseries_date_window %>% min %>% as.Date %>% format("%d-%b-%Y"),
        " to ", 
        input$dyseries_date_window %>% max %>% as.Date %>% format("%d-%b-%Y")
      ), 
      icon = icon("calendar"),
      color = "aqua", 
      fill = TRUE
    )
  })
  
  output$infobox2 <- renderInfoBox({
    
    req(input$dyseries_date_window)
    
    dpart_l <- disp_dat()$data %>% 
      filter(Time_Var >= (input$dyseries_date_window %>% min %>% as.Date)) %>% 
      filter(Time_Var <= (input$dyseries_date_window %>% max %>% as.Date))
    
    dpart_l <- dpart_l %>% filter(Var_Type == "Vol")
    
    infoBox(
      title = "Total Volumne", 
      value = paste0(
        round(sum(dpart_l$Val, na.rm = TRUE)),
        " Lakhs"
      ), 
      icon = icon("coins"),
      color = "olive", 
      fill = TRUE
    )
    
  })
  
  output$infobox3 <- renderInfoBox({
    
    req(input$dyseries_date_window)
    req(disp_dat)
    
    dpart_l <- disp_dat()$data %>% 
      filter(Time_Var >= (input$dyseries_date_window %>% min %>% as.Date)) %>% 
      filter(Time_Var <= (input$dyseries_date_window %>% max %>% as.Date))
    
    dpart_l <- dpart_l %>% filter(Var_Type == "Val")
    
    infoBox(
      title = "Total Value", 
      value = paste0(
        round(sum(dpart_l$Val, na.rm = TRUE)),
        " Crores"
      ), 
      icon = icon("rupee-sign"),
      color = "orange", 
      fill = TRUE
    )
    
  })
  
  
  ############# Data Load Module #############
  
  file_uploaded <- reactiveVal(FALSE)
  rbg_h <- reactiveVal()
  
  observe({
    req(input$upload_file_1)
    
    tryCatch(
      {
        rbg_h(read_data(input$upload_file_1$datapath))
        file_uploaded(TRUE)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
  })
  
  observeEvent(input$upload_file_1_help, {
    showModal(modalDialog(
      title = "From Where to get the Data?",
      div(
        "Download the data (in xls/xlsx format) from",
        a(href="https://www.rbi.org.in/Scripts/Statistics.aspx", 
          "www.rbi.org.in"),
        " > Statistics > Settlement Data of Payment Systems",
        tags$img(src='help.png', width='100%', align = "center")
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observe({
    if(file_uploaded()){
      shinyjs::show(id = "data_proc_pane", anim = TRUE)
      shinyjs::disable(id = "upload_file_1")
    }else{
      shinyjs::hide(id = "data_proc_pane")
      shinyjs::hide(id = "reload_app")
    }
  })
  
  # progress bar
  observe({
    
    input$sidebarMenu_tab
    
    if(file_uploaded() & 
       isTRUE(input$sidebarMenu_tab=="data_upload") & 
       isTRUE(try(rbg_h()$is_alive(), silent = TRUE))){
      
      pp <- progress_of_data_read()
      
      updateProgressBar(
        session = session, 
        id = "dp_prog", 
        value = pp$pct, 
        title = pp$desc
      )
      
      invalidateLater(500)
      
    }
    
    if(isFALSE(try(rbg_h()$is_alive(), silent = TRUE))){
      
      pp <- progress_of_data_read()
      
      updateProgressBar(
        session = session, 
        id = "dp_prog", 
        value = pp$pct, 
        title = pp$desc
      )
      
      if(pp$pct == 100){
        shinyjs::show(id = "reload_app", anim = TRUE)
      }
    }
    
    
  })
  
  observeEvent(input$reload_app,{
    envl<- new.env()
    source("Lib/load_data_from_local.R", local = envl)
    dat <<- envl$dat
    shinyjs::js$reloadMe()
  })
  
  
  # existing file info
  output$du_infobox1 <- renderInfoBox({
    
    now_dat <- list.files("Data/Cache/", pattern = ".xlsx", full.names = TRUE)
    
    if(length(now_dat)>0){
      sheets <-  tidyxl::xlsx_sheet_names(now_dat)
      
      latest_month <- sheets %>% 
        lubridate::my() %>% 
        max() %>% 
        format("%b %Y")
      
      from_month <- sheets %>% 
        lubridate::my() %>% 
        min() %>% 
        format("%b %Y")
      
      tag <- paste0(from_month, " to ", latest_month)
    }else{
      tag <- "No xls/xlsx Present"
    }
    
    infoBox(
      title = "Present Data", 
      value = tag, 
      icon = icon("chart-bar"), 
      color = "light-blue", 
      width = 4,
      fill = FALSE
    )
  })
  
  output$du_infobox2 <- renderInfoBox({
    now_dat_meta <- readRDS("Data/Local/meta")
    infoBox(
      title = "Present Data Type", 
      value = now_dat_meta$type, 
      icon = icon("chart-pie"), 
      color = "teal", 
      width = 4,
      fill = FALSE
    )
  })
  
  output$du_infobox3 <- renderInfoBox({
    now_dat_meta <- readRDS("Data/Local/meta")
    infoBox(
      title = "Present Data Update Date", 
      value = format(now_dat_meta$last_update,"%d %b %Y"), 
      icon = icon("calendar-day"), 
      color = "orange", 
      width = 4,
      fill = FALSE
    )
  })
  
  
}

shinyApp(ui, server)