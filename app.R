#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' in RStudio.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyglide)
library(rmarkdown)
library(readxl)
library(httr)

##########################
# data
##########################

GET("https://github.com/zoidy/RepoAssessmentHSS/raw/main/rubric.xlsx", write_disk("rubric.xlsx", overwrite = TRUE))
data <- read_excel("rubric.xlsx")

# Group the Activities that belong together:
# Generate a list where each item is a list of row indices that belong to the same group
# as defined by the first column. The indices are in the same order as they appear in the 
# input sheet. (Assisted by Gemini)
#
# Example:
#     row_indices_list[2] is a list with elements [4,5]
# This means that the second group of rows (grouped according to the value of the first column)
# are rows 4 and 5. In other words, rows 4 and 5 have the same value in the first column.
row_indices_list <- split(
  seq_len(nrow(data)), 
  factor(data$`Activity`, levels = unique(data$`Activity`))
)


##########################
# UI - variables and funcs
##########################
numqs <<- 0
maxscore <<- 0

controls <- tags$div(
    tags$div(class="my-control prev-screen"),
    tags$div(class="my-control next-screen")
)

css <- "
    .my-control {
      font-size: 0em;
    }

    .pretty { /* https://github.com/dreamRs/shinyWidgets/issues/478 */
          white-space: normal;
          margin-bottom: 5px;
    }
    .pretty .state label {
      line-height: 1.5em;
      margin-top: -4px;
    }
    .pretty .state label::after, .pretty .state label::before {
      top: -2px;
    }
    
    .link_button {
        -webkit-border-radius: 4px;
        -moz-border-radius: 4px;
        border-radius: 4px;
        border: solid 1px #20538D;
        text-shadow: 0 -1px 0 rgba(0, 0, 0, 0.4);
        -webkit-box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.4), 0 1px 1px rgba(0, 0, 0, 0.2);
        -moz-box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.4), 0 1px 1px rgba(0, 0, 0, 0.2);
        box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.4), 0 1px 1px rgba(0, 0, 0, 0.2);
        background: #4479BA;
        color: #FFF;
        padding: 8px 12px;
        text-decoration: none;
    }
    
    .control-label {
      font-weight: unset
    }
    
    max-width: 800px;
  "

build_header <- function(num, theme = TRUE, bigpicture = TRUE) {
    list(
        if (theme) h2("Theme:",data$`Activity`[num]) else span(),
        if (bigpicture) h3(data$`Description`[num]) else span(),
        br()
    )
}

build_question <- function(num) {
    numqs <<- numqs + 1
    choicenames <- c(if(is.na(data$`Score 0`)[num]) {} else data$`Score 0`[num],
                     if(is.na(data$`Score 1`)[num]) {} else data$`Score 1`[num], 
                     if(is.na(data$`Score 2`)[num]) {} else data$`Score 2`[num], 
                     if(is.na(data$`Score 3`)[num]) {} else data$`Score 3`[num])
    choicevalues <- c(if(is.na(data$`Score 0`[num])) {} 
                        else paste("0", data$`Metric Weight`[num], choicenames[1], sep="||"),
                      if(is.na(data$`Score 1`[num])) {} 
                        else paste("1", data$`Metric Weight`[num], choicenames[2], sep="||"),
                      if(is.na(data$`Score 2`[num])) {} 
                        else paste("2", data$`Metric Weight`[num], choicenames[3], sep="||"),
                      if(is.na(data$`Score 3`[num])) {} 
                        else paste("3", data$`Metric Weight`[num], choicenames[4], sep="||"))
    maxchoice <- tail(choicevalues, 1)
    maxchoicevals <- head(strsplit(maxchoice,"\\|{2}")[[1]], 2)
    maxchoicevals <- lapply(maxchoicevals, as.numeric)
    maxscore <<- maxscore + prod(unlist(maxchoicevals))
    list(
        h4("Question",num),
        p(data$Value[num]),
        p(em(data$`Advice`[num])),
        br(),
        radioButtons(
          inputId = paste0("Q",num,"_","metric"),
          label = if(is.na(data$`Metric`[num])) {NULL} else data$`Metric`[num],
          width = "100%",
          choiceNames = choicenames,
          choiceValues = choicevalues
        ),
        textAreaInput(
          inputId = paste0("Q",num,"_","notes"),
          label = "Notes"
        ),
        br(),
        conditionalPanel(
          # Hidden inputs to store values that will be retrieved for the report
          condition = "false",
          textInput(
            inputId = paste0("Q",num,"_","activity"),
            value = data$Activity[num],
            label = NULL
          ),
          textInput(
            inputId = paste0("Q",num,"_","value"),
            value = data$Value[num],
            label = NULL
          ),
          textInput(
            inputId = paste0("Q",num,"_","metriclabel"),
            value = data$Metric[num],
            label = NULL
          ),
          textAreaInput(
            inputId = paste0("Q",num,"_","metricchoices"),
            value = paste(if(is.na(data$`Score 0`)[num]) {} else {paste(data$`Score 0`[num], "(0)")},
                          if(is.na(data$`Score 1`)[num]) {} else {paste(data$`Score 1`[num], "(1)")}, 
                          if(is.na(data$`Score 2`)[num]) {} else {paste(data$`Score 2`[num], "(2)")}, 
                          if(is.na(data$`Score 3`)[num]) {} else {paste(data$`Score 3`[num], "(3)")},
                          sep = "\n"),
            label = NULL
          )
        ) 
    )
}


build_nav <- function(first = FALSE, last = FALSE) {
    #avoid the built-in shinyglide buttons. They appear too far down
    #so build our own
    nav <- div(`data-glide-el`="controls")
    
    if(!first && !last) {
      first <- TRUE
      last <- TRUE
    }
    
    if(last)
      nav <- tagAppendChildren(nav,
              a(`data-glide-dir`="<", href="#", "Back", class="link_button"),
              span("  ")
             )
                        
    if(first)
      nav <- tagAppendChild(nav,
              a(`data-glide-dir`=">", href="#", "Next", class="link_button")
             )
    
    nav <- tagAppendChildren(p(br()),nav,br())
}

##########################
# UI - shiny
##########################
instructions_screen <- list(
  h1("Instructions"),
  p("The rubric is built to aid in assessing a data repository service based on a set of criteria defined in rubric.xlsx.
          The rubric is organized into *Activities* associated with a data repository service.
          Each Activity encompases one or more *Values* that are to be assessed
          and each value has one or more *Metrics*. Each Metric has a variable score assigned to it. 
          After completing the assessment, a final score is provided along with an option to download a report.
          The score can be used to benchmark a repository service over time.
      "),
  p("See the ", a(href="https://github.com/zoidy/RepoAssessmentHSS","GitHub repo"), 
    "for the source code, rubric, and customization instructions."),
  p(),
  build_nav(first = T)
)
screens <- list(instructions_screen)

for(i in seq(from=2, to=length(row_indices_list)+1, length.out=length(row_indices_list))){
  # build the questions screens from the data by taking the questions from the indices of the grouped rows
  screens[[i]] <- list(
       build_header(row_indices_list[[i-1]][1]),
       lapply(row_indices_list[[i-1]], build_question),
       build_nav()
     )
}

results_screen <- list(
  h1("Results"),
  h2("Your weighted score:",textOutput("s1", inline = T), "out of", maxscore),
  p("Whether this is an acceptable score or not is up to you. The number sets a benchmark for self-improvement, not a point of comparison to others."),
  br(),
  downloadButton("report", "Download report"),
  build_nav(last = T)
)
screens[[length(screens)+1]] <- results_screen

ui <- fluidPage(
    tags$head(
        tags$style(css)
    ),
    
    titlePanel("Values-based Assessment of Institutional Data Repository Services"),
    glide(
      next_label = icon("chevron-right", lib="glyphicon"),
      previous_label = icon("chevron-left", lib="glyphicon"),
      loading_label = icon("hourglass", lib="glyphicon"),
      swipe = TRUE,
      keyboard = FALSE, #keyboard TRUE changes the radio button selection when navigating
      custom_controls = controls,
      lapply(screens, screen)
    )
)


##########################
# Server
##########################
server <- function(input, output, session) {
    
    result <- reactiveVal(0)

    output$s1 <- renderText({
        c(result())
    })
    
    event_triggers <- reactive({
        #Listen to events from all question inputs
        #https://stackoverflow.com/a/41961038
        #https://community.rstudio.com/t/looping-through-shiny-inputs-by-name-using-get-not-working-why/24145/2
        l <- list()
        for(i in seq(numqs))
            l <- append(l, input[[paste0("Q",i,"_metric")]])
        l
    })
    
    observeEvent(event_triggers(),{
        # Calculate the score on every event and update the reactive variable for display
        score <- 0
        for(i in seq_len(numqs)) {
            # split by '||'. The last element contains the choice text so exclude it before numeric conversion.
            selectedchoiceval <- strsplit(input[[paste0("Q",i,"_metric")]],"\\|{2}")
            q_vals <- lapply(selectedchoiceval[[1]][-length(selectedchoiceval[[1]])], as.numeric)
            score <- score + prod(unlist(q_vals))
            print(paste(i,prod(q_vals[[1]])))
        }
        print(paste("score:", score))
        result(score)
    })
    
    
    type = "byActivity"
    output$report <- downloadHandler(
      # https://shiny.posit.co/r/articles/build/generating-reports/
      # For html output, change this to "report.html"
      filename = paste0("report_", type, ".pdf"),
      content = function(file) {
        
        reportparams <- list()
        
        #by activity
        for(i in seq_len(numqs)){
          question_activity <- input[[paste0("Q",i,"_activity")]]
          if(length(reportparams[[question_activity]]) < 1){
            reportparams[[question_activity]] <- list()
          }
          reportparams[[question_activity]] <- append(
              reportparams[[question_activity]],
              list(list(value = input[[paste0("Q",i,"_value")]],
                   metriclabel = input[[paste0("Q",i,"_metriclabel")]],
                   metricchoice = tail(strsplit(input[[paste0("Q",i,"_metric")]],"\\|{2}")[[1]],1),
                   metricweight = strsplit(input[[paste0("Q",i,"_metric")]],"\\|{2}")[[1]][2],
                   metricchoices = input[[paste0("Q",i,"_metricchoices")]],
                   notes = input[[paste0("Q",i,"_notes")]]))
            )
          
        }
        
        
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), paste0("report_", type, ".rmd"))
        file.copy(paste0("report_", type, ".rmd"), tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(n = input[["Q1_activity"]])
        params <- list(score = result(), maxscore = maxscore, n = reportparams)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
