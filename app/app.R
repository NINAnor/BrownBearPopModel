suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(shinycssloaders)
  library(data.table)
  library(tibble)
  library(dplyr)
  library(httr)
  library(maditr)
  library(scales)
  library(tidyr)
  library(stringr)
  library(shinybusy)
  library(MESS)
  library(htmltools)
  library(plotly)
  library(popbio)
  library(shinyvalidate)
  library(readr)
  library(shinyBS)
  library(shinyscreenshot)
})

options(shiny.sanitize.errors = TRUE)

source('R/run_bear.R')
harvest_age<-readRDS("data/harvest_age.rds")

# error supression CSS
tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

# Define UI for application
ui <- navbarPage("Beskattningsmodell för honbjörnar V03.2025", id = "tabs",
                 tabPanel("Hem",
                          htmltools::includeMarkdown("www/front_matter.md")
                 ),
                 tabPanel("Resurser",
                          fluidRow(
                            column(12,
                                   wellPanel(
                                     tabsetPanel(
                                       tabPanel(title = 'Instruktionsvideo',
                                                h2("Video från onlinewebinar"),
                                                br(),
                                                tags$iframe(src="https://player.vimeo.com/video/720966569?h=fb308a3e7b", style="display:block; width:80%; height:80vh;", frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture")
                                       ),
                                       tabPanel(title = 'Artiklar',
                                                h2('Relevanta artiklar'),
                                                br(),
                                                htmltools::includeMarkdown("www/papers.md")
                                       ),
                                       tabPanel(title='Presentation',
                                                h2(),
                                                br(),
                                                tags$iframe(src="https://mfr.osf.io/render?url=https://osf.io/vzdtc/?direct%26mode=render%26action=download%26mode=render", style="display:block; width:80%; height:80vh;", frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture")
                                       )
                                     ))))),

                 #
                 tabPanel("Modelldata",
                          fluidRow(column(8, align="left",
                                          '1: Populationsstorlek (95% konfidensintervall):',
                                          br(),br(),
                                          numericInput("lowRange", "Undre:", 100, min = 0, max = 1000),
                                          uiOutput("high"),
                                          br(), br(),
                                          '2: Lasta upp avskjutningsdata här: ',
                                          radioButtons(
                                            "zeroRem",
                                            label = h4("Är det fler än 0 skjutna honor i området?"),
                                            choices = list(
                                              "Ja" = "Yes",
                                              "Nej" = "No"
                                            ),
                                            selected = "Yes"
                                          ),
                                          bsTooltip("zeroRem",
                                                    title = "If there are no shot female bears (zero) in your region then no data need to be uploaded - the model just needs to know the other inputs",
                                                    placement = "right",
                                                    trigger = "hover"
                                          ),
                                          conditionalPanel("input.zeroRem == 'Yes'",

                                                           # Input: Select a file ----
                                                           fileInput("harvestdata", "Välj indatafil (.CSV)",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
                                                           wellPanel(tabsetPanel(
                                                             tabPanel(title = 'Data', htmlOutput("uploaded_attributes"),
                                                                      tableOutput("data_summary")),
                                                             tabPanel(title = 'Uppladdad datatabell', htmlOutput("uploaded_summary"),
                                                                      tableOutput("view"))
                                                           )),
                                                           br(), br(),
                                                           '3:Senaste år för inventering: ',
                                                           uiOutput("census")),
                                          conditionalPanel("input.zeroRem == 'No'",
                                                           numericInput("census_yr1", "inventeringsår", 2020, min=2010, max= lubridate::year(Sys.Date()))
                                          ),

                                          br(), br(),
                                          '4: Detta år?:',
                                          numericInput("this_yr", "detta år", lubridate::year(Sys.Date()), min=2000),#, max= lubridate::year(Sys.Date())),
                                          br(), br(),
                                          '5: Antal år för prognosen:',
                                          numericInput("forecast", "år", 5, min = 1, max = 5),
                                          br(), br(),
                                          '6: Avskjutning av honor per år i ditt område:',
                                          checkboxInput("multiple_harvests", "Ange jaktuttag per år?", value = FALSE),
                                          uiOutput("female_harvest_inputs")
                          ))
                 ),
                 tabPanel("Kör modellen",
                          actionButton("run_model", "Kör modellen"),
                          br(),br(),
                          selectInput("iters", "Mode",
                                      c("demonstration"="50",
                                        "full model run (can take 10 minutes to run)"="5000"),
                                      selected = "5000"),
                          bsTooltip("iters",
                                    title = "For a quicker model run (less robust) use the demonstration version. A full model run will take a while to complete - go and make yourself a coffee while you wait!",
                                    placement = "right",
                                    trigger = "hover"
                          ),
                          #add_busy_spinner(spin = "cube-grid"),
                          add_busy_spinner(spin = "radar", margins = c(10, 20)),
                          #shinycssloaders::withSpinner(
                          plotlyOutput("plot1")
                          #)
                          ,
                          br(),br(),
                          dataTableOutput("tableID")

                          ,
                          br(),br(),
                          actionButton("go", "Ta en 'screenshot'")
                 ),
                 tabPanel("Hjälp",
                          br(),br(),
                          'Om du behöver hjälp vänligen kontakta oss på',
                          HTML('<a href="mailto:bearmodel@nina.no?">bearmodel@nina.no</a>')
                 )
)

# Define server logic
server <- function(input, output, session) {
  data_internal <- reactiveValues(
    raw=NULL,
    reshape=NULL
  )
  removals <- reactiveValues(
    raw=NULL
  )
  output$high <- renderUI({
    numericInput("HighRange", "Övre:", input$lowRange+1, min = input$lowRange+1, max = 1000)
  })

  # renderUI to allow multiple quotas

  output$female_harvest_inputs <- renderUI({
    req(input$forecast)

    num_years <- min(5, input$forecast)

    if (isTRUE(input$multiple_harvests)) {
      # If user wants to input per year
      lapply(1:num_years, function(i) {
        numericInput(paste0("female_harvest_", i),
                     paste("År", i, "Honbjörnar"),
                     value = 25, min = 0, max = 1000)
      })
    } else {
      # Single input for all years
      numericInput("female_harvest_all",
                   "Jaktuttag (gäller alla år)",
                   value = 25, min = 0, max = 1000)
    }
  })

  observeEvent(input$harvestdata, {
    data_internal$raw<- read_delim(
      file = input$harvestdata$datapath,
      delim = ";",
      escape_double = FALSE,
      trim_ws = TRUE)

    output$data_summary <- renderPrint({
      if(!is.null(data_internal$raw)){
        cat(paste0(
          "Du har laddat upp en dataset med ", nrow(data_internal$raw),
          " rader och ", ncol(data_internal$raw),
          " kolumner. Om detta inte var vad du förväntade dig kan du behöva",
          " strukturera om filen och försöka igen.<br>",
          "<br> Identifierade kolumnnamn:<br>",
          paste(colnames(data_internal$raw), collapse = "<br>"),
          "<br> Identifierat inventeringsår: ",
          min(data_internal$raw[1])
        ))
      }
    })

    output$view<-renderTable({
      if(!is.null(data_internal$raw)){
        data_internal$raw
      }
    })

    output$census<-renderUI({
      numericInput("census_yr", "census year", min(data_internal$raw[1]), min=2000, max= lubridate::year(Sys.Date()))
    })



  })





  observeEvent(input$run_model, {
    if(input$zeroRem=='Yes'){
      data_internal$reshape<<-data_internal$raw%>%
        rename(År = 1) |>
        rename(Alder=2) |>
        mutate(Alder=pmin(Alder,19)) |>
        group_by(År,Alder)  |>
        rowwise() |>
        mutate(Alder=
                 replace_na(
                   Alder,
                   sample(c(0:19),1,
                          replace =TRUE,
                          prob=harvest_age$Andel))) |>
        summarise(Antall=n()) |>
        ungroup() |>
        complete(Alder=0:19, nesting(År=(input$census_yr+1):input$this_yr-1), fill=list(Antall=0))  |>
        group_by(År, Alder) |>
        summarise(Antall=sum(Antall)) |>
        pivot_wider(names_from=År, values_from=Antall) |>
        select(-Alder)  |>
        as.matrix()
      if (isTRUE(input$multiple_harvests)) {
        female_harvest_values <- sapply(1:min(5, input$forecast), function(i) {
          input[[paste0("female_harvest_", i)]]
        })
      } else {
        female_harvest_values <- rep(input$female_harvest_all, min(5, input$forecast))
      }

      data=run_bear(lowest=input$lowRange, highest=input$HighRange,years_since=as.numeric(input$this_yr-input$census_yr),years_to_forecast=as.numeric(input$forecast),
                    female_harvest=female_harvest_values, removals=data_internal$reshape, nsim=as.numeric(input$iters))
      N_bear_tibble=as_tibble(data$post)

      N_bear_tibble1=as_tibble(N_bear_tibble[,1:as.numeric(input$this_yr-input$census_yr)])

      N_bear_tibble2=as_tibble(N_bear_tibble[, c(as.numeric((input$this_yr-input$census_yr)+1):as.numeric((input$this_yr-input$census_yr)+input$forecast))])


      N_bear_tibble1=N_bear_tibble1%>%
        pivot_longer(cols=everything(),names_to = "year", values_to="bears") %>%
        mutate(year=gsub("V", "", year)) %>%
        mutate(year=as.numeric(year)+input$census_yr-1) %>%
        mutate(label="Rekonstruerat bestånd")

      N_bear_tibble2=N_bear_tibble2%>%
        pivot_longer(cols=everything(),names_to = "year", values_to="bears") %>%
        mutate(year=gsub("V", "", year)) %>%
        mutate(year=as.numeric(year)+input$census_yr-1)%>%
        mutate(label="Prognos")

      N_bear_tibble3<-reactive(bind_rows(N_bear_tibble1, N_bear_tibble2))
      output$plot1=renderPlotly({
        if (isTRUE(input$multiple_harvests)) {
          harvest_values <- vapply(
            1:min(5, input$years_to_forecast),
            function(i) {
              val <- input[[paste0("female_harvest_", i)]]
              if (!is.null(val) && val != "") as.numeric(val) else NA_real_
            },
            numeric(1)
          )
        } else {
          # Single harvest value
          val <- input$female_harvest_all
          if (!is.null(val) && val != "") {
            cat("Harvest single value: ", as.numeric(input$female_harvest_all), "\n")
            harvest_values <- rep(as.numeric(val), min(5, input$years_to_forecast))
          } else {
            harvest_values <- rep(NA_real_, min(5, input$years_to_forecast))
          }
        }

        harvest_values <- na.omit(harvest_values)

        if (length(harvest_values) == 0) {
          harvest_text <- "inga värden"
        } else if (length(unique(harvest_values)) == 1) {
          harvest_text <- as.character(harvest_values[1])
        } else {
          harvest_text <- paste(
            paste(harvest_values[-length(harvest_values)], collapse = ", "),
            "och", harvest_values[length(harvest_values)]
          )
        }



        plot=N_bear_tibble3()%>%
          ggplot(aes(as.integer(year), bears, group=year, fill=label))+
          geom_violin(col="grey",alpha=0.6)+
          expand_limits(y=0) +
          ylab("Female bears") +
          xlab("Year") +
          theme_classic() +
          stat_summary(fun=median, geom="point", size=1, color="white")+
          scale_x_continuous(breaks = input$census_yr:(input$this_yr+input$forecast+1))+
          theme(legend.title= element_blank())+
          scale_fill_brewer(palette="Dark2")+
          ggtitle(paste0("Rekonstruktion av hondjurspopulationen från ", input$census_yr,
                         " Inventering och prognos från ",input$this_yr, " med årligt jaktuttag på ", harvest_text, " honor"))


        plotly::ggplotly(plot,tooltip="text")%>%
          style(hoverinfo = 'none')
      })
      output$tableID<-renderDataTable({
        # Get harvest values depending on toggle
        if (isTRUE(input$multiple_harvests)) {
          # Multiple harvest inputs (e.g. female_harvest_1, female_harvest_2, ...)
          harvest_values <- vapply(
            1:min(5, input$years_to_forecast),
            function(i) {
              val <- input[[paste0("female_harvest_", i)]]
              if (!is.null(val) && !is.na(val)) as.numeric(val) else NA_real_
            },
            numeric(1)
          )
        } else {
          # Single harvest value applied to all forecast years
          harvest_value_all <- input$female_harvest_all
          if (!is.null(harvest_value_all) && !is.na(harvest_value_all)) {
            harvest_values <- rep(as.numeric(harvest_value_all), min(5, input$years_to_forecast))
          } else {
            harvest_values <- rep(NA_real_, min(5, input$years_to_forecast))
          }
        }

        # Remove NAs
        harvest_values <- na.omit(harvest_values)

        # Check if all values are the same
        if (length(unique(harvest_values)) == 1) {
          # If all values are the same, return just that value
          harvest_text <- as.character(harvest_values[1])
        } else {
          # If values are different, format them with "og" before the last value
          harvest_text <- paste(
            paste(harvest_values[-length(harvest_values)], collapse = ", "),
            "og", harvest_values[length(harvest_values)],
            sep = " "
          )
        }
        tab=N_bear_tibble3() %>%
          group_by(year) %>%
          summarise(mean.bears = mean(bears, na.rm = TRUE),
                    hdi=ggdist::hdci(bears)) %>%
          mutate("År"=year) %>%
          mutate("Medelvärde antal honor"= mean.bears,

                 "Undre konfidensintervall"= round(hdi[,1],2),

                 "Övre konfidensintervall"= round(hdi[,2],2) ) %>%
          select("År","Medelvärde antal honor", "Undre konfidensintervall",
                 "Övre konfidensintervall")

        datatable(tab,
                  caption = paste0("Rekonstruktion av hondjurspopulationen från ", input$census_yr,
                                   " Inventering och prognos från ",input$this_yr, " med årligt jaktuttag på ", harvest_text, " honor"),
                  extensions = 'Buttons',

                  options = list(
                    paging = TRUE,
                    pageLength = 20,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('pdf', 'copy', 'csv', 'excel'
                    )
                  ),

                  class = "display"
        )
      })

    }else{
      removals$raw<-matrix(0, ncol=input$this_yr-input$census_yr1, nrow=20)
      if (isTRUE(input$multiple_harvests)) {
        female_harvest_values <- sapply(1:min(5, input$forecast), function(i) {
          input[[paste0("female_harvest_", i)]]
        })
      } else {
        female_harvest_values <- rep(input$female_harvest_all, min(5, input$forecast))
      }
      data=run_bear(lowest=input$lowRange, highest=input$HighRange,years_since=as.numeric(input$this_yr-input$census_yr1),years_to_forecast=as.numeric(input$forecast),
                    female_harvest=female_harvest_values, removals=removals$raw, nsim=as.numeric(input$iters))
      N_bear_tibble=as_tibble(data$post)
      N_bear_tibble1=as_tibble(N_bear_tibble[,1:as.numeric(input$this_yr-input$census_yr1+1)])
      N_bear_tibble2=as_tibble(N_bear_tibble[, c(as.numeric((input$this_yr-input$census_yr1)+2):as.numeric((input$this_yr-input$census_yr1)+input$forecast+1))])


      N_bear_tibble1=N_bear_tibble1%>%
        pivot_longer(cols=everything(),names_to = "year", values_to="bears") %>%
        mutate(year=gsub("V", "", year)) %>%
        mutate(year=as.numeric(year)+input$census_yr1-1) %>%
        mutate(label="reconstruction")

      N_bear_tibble2=N_bear_tibble2%>%
        pivot_longer(cols=everything(),names_to = "year", values_to="bears") %>%
        mutate(year=gsub("V", "", year)) %>%
        mutate(year=as.numeric(year)+input$census_yr1-1)%>%
        mutate(label="forecast")


      N_bear_tibble3<-reactive(bind_rows(N_bear_tibble1, N_bear_tibble2))
      output$plot1=renderPlotly({
        if (isTRUE(input$multiple_harvests)) {
          harvest_values <- vapply(
            1:min(5, input$years_to_forecast),
            function(i) {
              val <- input[[paste0("female_harvest_", i)]]
              if (!is.null(val) && val != "") as.numeric(val) else NA_real_
            },
            numeric(1)
          )
        } else {
          # Single harvest value
          val <- input$female_harvest_all
          if (!is.null(val) && val != "") {
            cat("Harvest single value: ", as.numeric(input$female_harvest_all), "\n")
            harvest_values <- rep(as.numeric(val), min(5, input$years_to_forecast))
          } else {
            harvest_values <- rep(NA_real_, min(5, input$years_to_forecast))
          }
        }

        harvest_values <- na.omit(harvest_values)

        if (length(harvest_values) == 0) {
          harvest_text <- "inga värden"
        } else if (length(unique(harvest_values)) == 1) {
          harvest_text <- as.character(harvest_values[1])
        } else {
          harvest_text <- paste(
            paste(harvest_values[-length(harvest_values)], collapse = ", "),
            "och", harvest_values[length(harvest_values)]
          )
        }
        plot=N_bear_tibble3() %>%
          ggplot(aes(as.integer(year), bears, group=year, fill=label))+
          geom_violin(col="grey",alpha=0.6)+
          expand_limits(y=0) +
          ylab("Female bears") +
          xlab("Year") +
          theme_classic() +
          stat_summary(fun=median, geom="point", size=1, color="white")+
          scale_x_continuous(breaks = input$census_yr1:(input$this_yr+input$forecast+1))+
          theme(legend.title= element_blank())+
          scale_fill_brewer(palette="Dark2")+
          ggtitle(paste0("Rekonstruktion av hondjurspopulationen från ", input$census_yr1,
                         " Inventering och prognos från ",input$this_yr, " med årligt jaktuttag på ", harvest_text, " honor"))

        plotly::ggplotly(plot,tooltip="text")%>%
          style(hoverinfo = 'none')
      })

      output$tableID <- renderDataTable({
        if (isTRUE(input$multiple_harvests)) {
          harvest_values <- vapply(
            1:min(5, input$years_to_forecast),
            function(i) {
              val <- input[[paste0("female_harvest_", i)]]
              if (!is.null(val) && val != "") as.numeric(val) else NA_real_
            },
            numeric(1)
          )
        } else {
          # Single harvest value
          val <- input$female_harvest_all
          if (!is.null(val) && val != "") {
            cat("Harvest single value: ", as.numeric(input$female_harvest_all), "\n")
            harvest_values <- rep(as.numeric(val), min(5, input$years_to_forecast))
          } else {
            harvest_values <- rep(NA_real_, min(5, input$years_to_forecast))
          }
        }

        harvest_values <- na.omit(harvest_values)

        if (length(harvest_values) == 0) {
          harvest_text <- "inga värden"
        } else if (length(unique(harvest_values)) == 1) {
          harvest_text <- as.character(harvest_values[1])
        } else {
          harvest_text <- paste(
            paste(harvest_values[-length(harvest_values)], collapse = ", "),
            "och", harvest_values[length(harvest_values)]
          )
        }
        tab <- N_bear_tibble3() %>%
          group_by(year) %>%
          summarise(
            mean.bears = mean(bears, na.rm = TRUE),
            lower = tryCatch(ggdist::hdci(bears)[1], error = function(e) NA),
            upper = tryCatch(ggdist::hdci(bears)[2], error = function(e) NA),
            .groups = "drop"
          ) %>%
          mutate("År" = year,
                 "Medelvärde antal honor" = mean.bears,
                 "Undre konfidensintervall" = round(lower, 2),
                 "Övre konfidensintervall" = round(upper, 2)) %>%
          select("År", "Medelvärde antal honor", "Undre konfidensintervall", "Övre konfidensintervall")
        datatable(tab,
                  caption = paste0(
                    "Rekonstruktion av hondjurspopulationen från ", input$census_yr1,
                    " Inventering och prognos från ", input$this_yr,
                    " med årligt jaktuttag på ", harvest_text, " honor"
                  ),
                  extensions = 'Buttons',
                  options = list(
                    paging = TRUE,
                    pageLength = 20,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  rownames = FALSE,
                  class = "display"
        )
      })
    }



    observeEvent(input$go, {
      screenshot()
    })
  })
}


# Run the application
shinyApp(ui = ui, server = server)
