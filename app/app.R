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
ui <- navbarPage("Beskattningsmodell för honbjörnar V04.2023", id = "tabs",
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
                                          numericInput("female_harvest", "Honbjörnar", 25, min = 0, max = 1000)

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
                 tabPanel("Hjålp",
                          br(),br(),
                          'Om du behöver hjälp vänligen kontakta oss på',
                          HTML('<a href="mailto:bearmodel@nina.no?">bearmodel@nina.no</a>')
                 )
)

# Define server logic required to draw a histogram
# Define server logic required to draw a histogram
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



  observeEvent(input$harvestdata, {
    data_internal$raw<- read_delim(
      file = input$harvestdata$datapath,
      delim = ";",
      escape_double = FALSE,
      trim_ws = TRUE)

    output$data_summary <- renderPrint({
      if(!is.null(data_internal$raw)){
        cat(paste0(
          "You've uploaded a dataset containing ", nrow(data_internal$raw),
          " rows and ", ncol(data_internal$raw),
          " columns. If this is not what you expected, you might want to",
          " restructure the file and try again.<br>",
          "<br> Detected column names as follows:<br>",
          paste(colnames(data_internal$raw), collapse = "<br>"),
          "<br> Detected census year: ",
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


      data=run_bear(lowest=input$lowRange, highest=input$HighRange,years_since=as.numeric(input$this_yr-input$census_yr),years_to_forecast=as.numeric(input$forecast),
                    female_harvest=input$female_harvest, removals=data_internal$reshape, nsim=as.numeric(input$iters))
      N_bear_tibble=as_tibble(data)

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
                         " Inventering och prognos från ",input$this_yr, " med årligt jaktuttag på ", input$female_harvest, " honor"))


        plotly::ggplotly(plot,tooltip="text")%>%
          style(hoverinfo = 'none')

      })
      output$tableID<-renderDataTable({
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
                                   " Inventering och prognos från ",input$this_yr, " med årligt jaktuttag på ", input$female_harvest, " honor"),

                  extensions = 'Buttons',

                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('pdf'
                                # 'copy', 'csv', 'excel'
                    )
                  ),

                  class = "display"
        )
      })

    }else{
      removals$raw<-matrix(0, ncol=input$this_yr-input$census_yr1, nrow=20)
      data=run_bear(lowest=input$lowRange, highest=input$HighRange,years_since=as.numeric(input$this_yr-input$census_yr1),years_to_forecast=as.numeric(input$forecast),
                    female_harvest=input$female_harvest, removals=removals$raw, nsim=as.numeric(input$iters))
      N_bear_tibble=as_tibble(data)

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
                         " Inventering och prognos från ",input$this_yr, " med årligt jaktuttag på ", input$female_harvest, " honor"))

        plotly::ggplotly(plot,tooltip="text")%>%
          style(hoverinfo = 'none')
      })
      output$tableID<-renderDataTable({
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
                  caption = paste0("Rekonstruktion av hondjurspopulationen från ", input$census_yr1,
                                   " Inventering och prognos från ",input$this_yr, " med årligt jaktuttag på ", input$female_harvest, " honor"),
                  extensions = 'Buttons',

                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',

                    buttons = c(#'copy', 'csv', 'excel',
                      'pdf')
                  ),

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
