library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(esquisse)
library(dplyr)
library(DT)

whole_data <- data.table::fread(file = "multiIndex_variablesDict.csv",
                                sep=",",
                                header = TRUE,
                                drop = c("categoryValues", "batch_group"),
                                na.strings = c("NA", "")) %>%
   select(simplified_varName, everything())
study_names <- whole_data$level_0 %>% unique()
not_all_na <- function(x) {!all(is.na(x))}


{
   #### UI HEADER ####
   DBheader <- dashboardHeader(
      title="BDCatalyst Variables Browser"
   )

   DBsidebar <- dashboardSidebar(
      filterDF_UI("filtering")
   )

   #### UI BODY ####
   DBbody <- dashboardBody(
      fluidRow(      pickerInput(
         inputId = "subset",
         label = "Study Subset:",
         choices = study_names,
         selected = study_names,
         multiple = T,
         options = list('actions-box' = T),
         inline = TRUE)
         ),
      fluidRow(
         column(width = 12,
            box(
               width = NULL,
               div(style = 'overflow-x: scroll',
               DT::dataTableOutput(outputId = "table")),
               progressBar(
                  id = "pbar", value = 100,
                  total = 100, display_pct = TRUE
               ),
               # tags$p("Code dplyr:"),
               # verbatimTextOutput(outputId = "code_dplyr"),
               # tags$p("Expression:"),
               # verbatimTextOutput(outputId = "code"),
               # tags$p("Filtered data:"),
               # verbatimTextOutput(outputId = "res_str")
            )
         )
      )
   )



   #### GENERATING UI ####

   ui <- dashboardPage(
      skin="yellow",
      DBheader,
      DBsidebar,
      DBbody
   )
}

server <- function(input, output, session) {

   data <- reactive({
      whole_data %>% filter(level_0 %in% input$subset) %>%
         select_if(not_all_na)
   })

   res_filter <- callModule(
      module = filterDF,
      id = "filtering",
      data_table = data,
      data_name = reactive("Filtered Variable names")
   )

   observeEvent(res_filter$data_filtered(), {
      updateProgressBar(
         session = session, id = "pbar",
         value = nrow(res_filter$data_filtered()), total = nrow(data())
      )
   })

   output$table <- DT::renderDT({
      res_filter$data_filtered() %>%
         DT::datatable(caption = "You can rearrange columns order by drag and drop",
                       escape = FALSE, filter = 'top', rownames = FALSE,
                       extensions = list('ColReorder' = NULL, 'RowReorder' = NULL,
                                         'Buttons' = NULL),
                       options = list(dom = 'BRrltpi',
                                      lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
                                      pageLength = 50,
                                      ColReorder = TRUE,
                                      rowReorder = TRUE,
                                      buttons = list(I('colvis'), list(
                                                      extend = "collection",
                                                     buttons = c('copy', 'csv', 'pdf'),
                                                     text = "Export"
                                                     ))
                       )) %>% DT::formatStyle(
                          "simplified_varName",
                          backgroundColor = 'lightgreen')

   }, options = list(pageLength = 100))


   output$code_dplyr <- renderPrint({
      res_filter$code$dplyr
   })
   output$code <- renderPrint({
      res_filter$code$expr
   })

   output$res_str <- renderPrint({
      str(res_filter$data_filtered())
   })

}

shinyApp(ui, server)
