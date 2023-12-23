source("./functions/calculateDMY.R")
source("./functions/pers_reactable.R")
source("./functions/update_selection.R")

serverProcess <- function(id, Data){
  moduleServer(id,function(input, output, session){
    js$disableTab('Monthly');js$disableTab("Yearly")
    
    #UI DINAMIC----------------------------------
    escolhas2 <- reactive({
      return(colnames(Data())[-1])
    })
    
    output$diariosum <- renderUI({
      selectInput(NS(id,"diariosum"), "Sum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[c(1,5)]
      )
    })
    
    output$diariomean <- renderUI({
      selectInput(NS(id,"diariomean"), "Mean:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[-c(1,5)]
      )
    })
    output$mensalsum <- renderUI({
      selectInput(NS(id,"mensalsum"), "Sum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[c(1)]
      )
    })
    
    output$mensalmean <- renderUI({
      selectInput(NS(id,"mensalmean"), "Mean:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[-c(1)]
      )
    })
    
    output$anualsum <- renderUI({
      selectInput(NS(id,"anualsum"), "Sum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[c(1)]
      )
    })
    
    output$anualmean <- renderUI({
      selectInput(NS(id,"anualmean"), "Mean:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[-c(1)]
      )
    })
    
    #Processamento------------------
    #Gerar tabelas
    #Tabela diaria----------------------------
    #manter selecao e remover opcoes ja selecionadas entre os seletores
    pre_select_d <- reactive({
      pre_select_fun(a=input$diariosum,b=input$diariomean)
    })
    
    observeEvent(input$diariosum, {
      updateSelectInput(session, "diariomean",
                        choices = setdiff(escolhas2(), input$diariosum),
                        selected = pre_select_d()$b
      )
    })
    
    observeEvent(input$diariomean, {
      updateSelectInput(session, "diariosum",
                        choices = setdiff(escolhas2(), input$diariomean),
                        selected = pre_select_d()$a
      )
    })
    
    TableDiario <- eventReactive(input$diariobtn,{
      show_modal_spinner(text = "Processing...",
                         spin = "half-circle",
                         color = "black")
      
      tryCatch(
        {
          df <-
            dplyr::mutate(Data(),!!rlang::sym(colnames(Data())[1]) := as.Date(!!rlang::sym(colnames(Data())[1])))
          
          df <-
            calculateDMY(
              data = df,
              col_date = colnames(df)[1],
              col_sum = input$diariosum,
              col_mean = input$diariomean
            )
          
          js$enableTab('Monthly')
          
          remove_modal_spinner()
          return(df)
        },
        error = function(e) {
          remove_modal_spinner()
          stop(safeError(e))
        },
        warning =function(e){
          remove_modal_spinner()
          stop(safeError(e))
        },
        finally = function(e){
          remove_modal_spinner()
          stop(safeError(e))
        }
      )
      
      
    })
    
    output$table_diario <- renderReactable({
      pers_reactable(mutate(TableDiario(),
                            Data = as.character(Data)))
    })
    
    #Tabela mensal-------------------------------
    #manter selecao e remover opcoes ja selecionadas entre os seletores
    pre_select_m <- reactive({
      pre_select_fun(a=input$mensalsum,b=input$mensalmean)
    })
    
    observeEvent(input$mensalsum, {
      updateSelectInput(session, "mensalmean",
                        choices = setdiff(escolhas2(), input$mensalsum),
                        selected = pre_select_m()$b
      )
    })
    
    observeEvent(input$mensalmean, {
      updateSelectInput(session, "mensalsum",
                        choices = setdiff(escolhas2(), input$mensalmean),
                        selected = pre_select_m()$a
      )
    })
    
    TableMensal <- eventReactive(input$mensalbtn,{
      show_modal_spinner(text = "Processing...",
                         spin = "half-circle",
                         color = "black")
      tryCatch(
        {
          
          df <-
            calculateDMY(
              data = TableDiario(),
              col_date = colnames(TableDiario())[1],
              col_sum = input$mensalsum,
              col_mean = input$mensalmean,
              type = "Monthly"
            )
          
          js$enableTab('Yearly')
          remove_modal_spinner()
          return(df)
        },
        error = function(e) {
          remove_modal_spinner()
          stop(safeError(e))
        },
        warning =function(e){
          remove_modal_spinner()
          stop(safeError(e))
        },
        finally = function(e){
          remove_modal_spinner()
          stop(safeError(e))
        }
      )
      
      
    })
    
    output$table_mensal <- renderReactable({
      pers_reactable(TableMensal())
    })
    
    #Tabela Anual------------------------------
    #manter selecao e remover opcoes ja selecionadas entre os seletores
    pre_select_a <- reactive({
      pre_select_fun(a=input$anualsum,b=input$mensalmean)
    })
    
    observeEvent(input$anualsum, {
      updateSelectInput(session, "anualmean",
                        choices = setdiff(escolhas2(), input$anualsum),
                        selected = pre_select_a()$b
      )
    })
    
    observeEvent(input$anualmean, {
      updateSelectInput(session, "anualsum",
                        choices = setdiff(escolhas2(), input$anualmean),
                        selected = pre_select_a()$a
      )
    })
    
    TableAnual <- eventReactive(input$anualbtn,{
      show_modal_spinner(text = "Processing...",
                         spin = "half-circle",
                         color = "black")
      tryCatch(
        {
          df <-
            calculateDMY(
              data = TableMensal(),
              col_date = colnames(TableMensal())[1],
              col_sum = input$anualsum,
              col_mean = input$anualmean,
              type = "Yearly"
            )
          remove_modal_spinner()
          return(df)
        },
        error = function(e) {
          remove_modal_spinner()
          stop(safeError(e))
        },
        warning =function(e){
          remove_modal_spinner()
          stop(safeError(e))
        },
        finally = function(e){
          remove_modal_spinner()
          stop(safeError(e))
        }
      )
      
    })
    
    output$table_anual <- renderReactable({
      pers_reactable(TableAnual())
    })
    
    #Download-------
    observeEvent(input$DownBD, {
      shinyalert(html = TRUE, text = tagList(
        radioButtons(
          NS(id,"sepDownBD"), "Separador",
          choices = c(Comma = ",",
                      Semicolon = ";",
                      Tab = "\t"),
          selected = ";"),
        radioButtons(NS(id,"decDownBD"), "Decimal points",
                     choices = c(Comma = ",",
                                 Point = "."),
                     selected = "."),
        actionButton(NS(id,"DownBD_alert"),"OK")
      ),
      showConfirmButton = FALSE,
      size = "xs")
    })
    
    observe({
      req(input$sepDownBD)
      if(input$decDownBD == "," & input$sepDownBD==","){
        updateRadioButtons(
          session,
          inputId = "decDownBD",
          choices = c(Comma = ",",
                      Point = "."),
          selected = "."
        )
      }
    })
    
    observeEvent(input$DownBD_alert,{
      runjs("
        var separador = $('input[name=\"processtabs-sepDownBD\"]:checked').val();
        var decimal = $('input[name=\"processtabs-decDownBD\"]:checked').val();
        Reactable.downloadDataCSV('processtabs-table_diario',
        'file.csv',
        { sep: separador,dec: decimal });
        ")
    })
    
    
    observeEvent(input$DownBM, {
      shinyalert(html = TRUE, text = tagList(
        radioButtons(
          NS(id,"sepDownBM"), "Separador",
          choices = c(Comma = ",",
                      Semicolon = ";",
                      Tab = "\t"),
          selected = ";"),
        radioButtons(NS(id,"decDownBM"), "Decimal points",
                     choices = c(Comma = ",",
                                 Point = "."),
                     selected = "."),
        actionButton(NS(id,"DownBM_alert"),"OK")
      ),
      showConfirmButton = FALSE,
      size = "xs")
    })
    
    observe({
      req(input$sepDownBM)
      if(input$decDownBM == "," & input$sepDownBM==","){
        updateRadioButtons(
          session,
          inputId = "decDownBM",
          choices = c(Comma = ",",
                      Point = "."),
          selected = "."
        )
      }
    })
    
    observeEvent(input$DownBM_alert,{
      runjs("
        var separador = $('input[name=\"processtabs-sepDownBM\"]:checked').val();
        var decimal = $('input[name=\"processtabs-decDownBM\"]:checked').val();
        Reactable.downloadDataCSV('processtabs-table_mensal',
        'file.csv',
        { sep: separador,dec: decimal });
        ")
    })
    
    observeEvent(input$DownBA, {
      shinyalert(html = TRUE, text = tagList(
        radioButtons(
          NS(id,"sepDownBA"), "Separador",
          choices = c(Comma = ",",
                      Semicolon = ";",
                      Tab = "\t"),
          selected = ";"),
        radioButtons(NS(id,"decDownBA"), "Decimal points",
                     choices = c(Comma = ",",
                                 Point = "."),
                     selected = "."),
        actionButton(NS(id,"DownBA_alert"),"OK")
      ),
      showConfirmButton = FALSE,
      size = "xs")
    })
    
    observe({
      req(input$sepDownBA)
      if(input$decDownBA == "," & input$sepDownBA==","){
        updateRadioButtons(
          session,
          inputId = "decDownBA",
          choices = c(Comma = ",",
                      Point = "."),
          selected = "."
        )
      }
    })
    
    
    observeEvent(input$DownBA_alert,{
      runjs("
        var separador = $('input[name=\"processtabs-sepDownBA\"]:checked').val();
        var decimal = $('input[name=\"processtabs-decDownBA\"]:checked').val();
        Reactable.downloadDataCSV('processtabs-table_anual',
        'file.csv',
        { sep: separador,dec: decimal });
        ")
    })
    return(list(TableDiario,TableMensal,TableAnual))
  })
}
