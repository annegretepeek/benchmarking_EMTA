#Company benchmarking app
#Annegrete Peek

library(shiny)
library(dplyr)
library(zoo)
library(plotly)
library(DT)
library(ggplot2)
library(arrow)

DATA_DIR <- "data"
ACTIVE_COMPANIES_PATH <- file.path(DATA_DIR, "active_companies.parquet")

if (!file.exists(ACTIVE_COMPANIES_PATH)) {
  stop("Missing active companies file: ", ACTIVE_COMPANIES_PATH)
}

active_companies <- arrow::read_parquet(ACTIVE_COMPANIES_PATH) %>%
  mutate(
    registrikood = as.character(registrikood),
    compare_label = as.character(compare_label)
  )

if (!"company_bucket" %in% names(active_companies)) {
  stop("active_companies.parquet must contain compare_label.")
}

if (!"compare_label" %in% names(active_companies)) {
  stop("active_companies.parquet must contain compare_label.")
}

company_choices <- active_companies %>%
  arrange(compare_label) %>%
  select(registrikood, compare_label) %>%
  distinct() %>%
  { stats::setNames(.$registrikood, .$compare_label) }

registrikood_to_url <- function(registrikoodid) {
  paste(registrikoodid, collapse = ",")
}

parandus <- function(string){
  string <- gsub("<ff>FFFFC3<ff>FFFF9C", "Ü", string)
  string <- gsub("<ff>FFFFC3<ff>FFFF95", "Õ", string)
  string <- gsub("<ff>FFFFC3<ff>FFFF84", "Ä", string)
  string <- gsub("<ff>FFFFC3<ff>FFFF96", "Ö", string)
  string <- gsub("<ff>FFFFC5<ff>FFFFA0", "Š", string)
  string <- gsub("<ff>FFFFC5<ff>FFFFBD", "Ž", string)
  return(string)
}

kymne_aste = function(tunnus){
  max_vaartus = max(abs(tunnus[abs(tunnus)<Inf]), na.rm = TRUE)
  if (max_vaartus > 1e+09) {
    div = 1e+09
    label = "(miljardites)"
  } else { 
    if (max_vaartus > 1e+06) {
      div = 1e+06
      label = "(miljonites)"
    } else {
      if (max_vaartus > 1000) {
        div = 1000
        label = "(tuhandetes)"
      } else {
        
        div = 1
        label = ""
      }
    }}
  return(list(div = div, label = label)) 
}

safe_div <- function(x, y, multiplier = 1) {
  dplyr::if_else(!is.na(y) & y != 0, x / y * multiplier, NA_real_)
}

read_selected_companies <- function(selected_registrikoodid) {
  selected_registrikoodid <- as.character(selected_registrikoodid)
  
  if (length(selected_registrikoodid) == 0 || all(is.na(selected_registrikoodid)) || all(selected_registrikoodid == "")) {
    return(tibble::tibble())
  }
  
  selected_active <- active_companies %>%
    filter(registrikood %in% selected_registrikoodid)
  
  if (nrow(selected_active) == 0) {
    return(tibble::tibble())
  }
  
  selected_buckets <- active_companies |>
    filter(registrikood %in% selected_registrikoodid) |>
    pull(company_bucket) |>
    unique()
  
  bucket_paths <- file.path(
    DATA_DIR,
    "emta_compare",
    paste0("company_bucket=", selected_buckets)
  )
  
  bucket_paths <- bucket_paths[dir.exists(bucket_paths)]
  
  if (length(bucket_paths) == 0) {
    stop(
      "No matching bucket folders found. Checked: ",
      paste(
        file.path(DATA_DIR, paste0("company_bucket=", selected_buckets)),
        collapse = ", "
      )
    )
  }
  
  raw_data <- arrow::open_dataset(file.path(bucket_paths, "part-0.parquet")) |>
    filter(registrikood %in% selected_registrikoodid) |>
    collect()
  
  raw_data %>%
    mutate(
      registrikood = as.character(registrikood),
      aasta = as.integer(aasta),
      kvartal = as.integer(kvartal),
      aeg = zoo::as.yearqtr(paste(aasta, kvartal), format = "%Y %q"),
      rmaksud = as.numeric(rmaksud),
      toomaksud = as.numeric(toomaksud),
      kaive = as.numeric(kaive),
      tootajad = as.numeric(tootajad),
      kaive_tootaja = safe_div(kaive, tootajad),
      tmaksud_tootaja = safe_div(toomaksud, tootajad),
      rmaksud_kaive = safe_div(rmaksud, kaive, 100),
      tmaksud_kaive = safe_div(toomaksud, kaive, 100),
      tegevusvaldkond = dplyr::coalesce(emtak2tekst, emtak, emtak2)
    ) %>%
    arrange(nimi, aeg)
}

server <- function(input, output, session)  {
  
  firma <- reactiveVal(NULL)
  
  observe({
    url_params <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(url_params$registrikood)) {
      firma_url <- strsplit(url_params$registrikood, ",")[[1]]
      
      invalid <- setdiff(firma_url, active_companies$registrikood)
      
      if (length(invalid) > 0) {
        showNotification(
          paste(
            "Registrikood ei ole aktiivsete ettevõtete nimekirjas:",
            paste(invalid, collapse = ", ")
          ),
          type = "warning",
          duration = 10
        )
      }
      
      firma(intersect(firma_url, active_companies$registrikood))
    }
  })
  
  observe({
    updateSelectizeInput(
      session,
      "Firma",
      choices = c("Vali ettevõtted" = "", company_choices),
      selected = firma(),
      server = TRUE
    )
    
    if (!is.null(firma()) && length(firma()) > 0) {
      isolate({
        shinyjs::click("go")
      })
    }
  })
  
  observe({
    updateTextInput(
      session,
      "share_link",
      value = share_url()
    )
  })
  
  andmed <- eventReactive(input$go, {
    read_selected_companies(input$Firma)
     
  })
  
  output$tabel <- DT::renderDataTable({
    tabel_andmed <- andmed() %>%
      arrange(desc(aeg)) %>%
      select(
        Nimi = nimi,
        Liik = liik,
        Tegevusvaldkond = tegevusvaldkond,
        Maakond = maakond,
        Asukoht = aadress
      ) %>%
      group_by(Nimi) %>%
      slice(1) %>%
      ungroup()
    
    DT::datatable(
      tabel_andmed,
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = length(unique(andmed()$nimi)),
        autoWidth = FALSE,
        columnDefs = list(list(width = "400px", targets = "_all"))
      ),
      rownames = FALSE
    )
  })
  
  output$k2ive1 <- renderPlotly({
    skaala = kymne_aste(andmed()$kaive)
    p <- ggplot(andmed(), aes(aeg, kaive/skaala$div, color = nimi, group = nimi, text = paste0(nimi, "\n", aeg, "\nKäive: ", prettyNum(kaive, big.mark = " "), " €"))) + geom_line() + geom_point(size = 0.8) + theme_bw() + 
        labs(x = "", y = paste0("€ ", skaala$label)) + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0) 
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$tootajad1 <- renderPlotly({
    skaala = kymne_aste(andmed()$tootajad)
    p <- ggplot(andmed(), aes(aeg, tootajad/skaala$div, color = nimi, group = nimi, text = paste0(nimi, "\n", aeg, "\nTöötajate arv: ", prettyNum(tootajad, big.mark = " ")))) + geom_line() + geom_point(size = 0.8) + theme_bw() +
      labs(x = "", y = paste0(" ", skaala$label)) + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0)
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$k2ive2 <- renderPlotly({
    skaala = kymne_aste(andmed()$kaive_tootaja)
    p <- ggplot(andmed(), aes(aeg, kaive_tootaja/skaala$div, color = nimi, group = nimi, text = paste0(nimi, "\n", aeg, "\nKäive töötaja kohta: ", prettyNum(kaive_tootaja, big.mark = " "), " €"))) + geom_line() + geom_point(size = 0.8) + 
      theme_bw() + labs(x = "", y = paste0("€ ", skaala$label)) + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0)
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$tootajad2 <- renderPlotly({
    skaala = kymne_aste(andmed()$tmaksud_tootaja)
    p <- ggplot(andmed(), aes(aeg, tmaksud_tootaja/skaala$div, color = nimi, group = nimi, text = paste0(nimi, "\n", aeg, "\nTööjõumaksud töötaja kohta: ", prettyNum(tmaksud_tootaja, big.mark = " "), " €"))) + geom_line() + 
      geom_point(size = 0.8) + theme_bw() + labs(x = "", y = paste0("€ ", skaala$label)) + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0)
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$maksud1 <- renderPlotly({
    p <- ggplot(andmed(), aes(aeg, rmaksud_kaive, color = nimi, group = nimi, text = paste0(nimi, "\n", aeg, "\nRiiklikud maksud käibest: ", rmaksud_kaive, "%"))) + geom_line() + geom_point(size = 0.8) + theme_bw() + 
      labs(x = "", y = "%") + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0)
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$maksud2 <- renderPlotly({
    p <- ggplot(andmed(), aes(aeg, tmaksud_kaive, color = nimi, group = nimi, text = paste0(nimi, "\n", aeg, "\nTööjõumaksud käibest: ", tmaksud_kaive, "%"))) + geom_line() + geom_point(size = 0.8) + theme_bw() +
      labs(x = "", y = "%") + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0)
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  share_url <- reactive({
    
    req(input$Firma)
    
    base_url <- sub(
      "\\?.*$",
      "",
      session$clientData$url_href
    )
    
    paste0(
      base_url,
      "?registrikood=",
      URLencode(registrikood_to_url(input$Firma))
    )
  })

}

ui <- function(request) {
  fluidPage(
    tags$title("Ettevõtete ajaloo võrdlus - Annegrete Molloka"),
    mainPanel(width = 12,
              fluidRow(
                column(8, selectizeInput(width = '100%', "Firma", "", choices = c("Vali ettevõtted" = ""), multiple = TRUE)),
                column(4, br(), actionButton(width = '100%', "go", "Tee joonised"))
              ),
              conditionalPanel(condition = "input.go != 0",
                               fluidRow(
                                 column(12, DT::dataTableOutput("tabel"))
                               ),
                               fluidRow(
                                 column(6,
                                        h3("Käive*"),
                                        plotlyOutput("k2ive1", height = 250)),
                                 column(6,
                                        h3("Töötajate arv**"),
                                        plotlyOutput("tootajad1", height = 250))
                               ),
                               fluidRow(
                                 column(6,
                                        h3("Käive töötaja kohta"),
                                        plotlyOutput("k2ive2", height = 250)),
                                 column(6,
                                        h3("Tööjõumaksud töötaja kohta"),
                                        plotlyOutput("tootajad2", height = 250))
                               ),
                               fluidRow(
                                 column(6,
                                        h3("Riiklikud maksud käibest (%)"),
                                        plotlyOutput("maksud1", height = 250)),
                                 column(6,
                                        h3("Tööjõumaksud käibest (%)"),
                                        plotlyOutput("maksud2", height = 250))
                               ),
                               br(),
                               textInput("share_link", "Link jagamiseks", value = ""
                               ),
                               br(),
                               br(),
                               tags$div(class = "header", checked = NA,
                                        tags$p("Tabel on tehtud EMTA väljastatud kvartaalsete andmete pealt, mis asuvad",
                                               tags$a(href = "https://emta.ee/eraklient/amet-uudised-ja-kontakt/uudised-pressiinfo-statistika/statistika-ja-avaandmed#tasutud-maksud", "siin.", target = "_blank"), " I kvartal (Q1) on detsember-veebruar, II kvartal (Q2) on märts-mai, III kvartal (Q3) on juuni-august, IV kvartal (Q4) on september-november."), 
                                        tags$p("*Deklareeritud käibena avaldatakse käibedeklaratsioonide ridade 1, 2 ja 3 summa."),
                                        tags$p("**Töötajate arv on möödunud kvartali viimase kuupäeva seisuga töötamise registrisse kantud kehtiva kandega tööd tegevate isikute arv, tööjõumaksud on kvartali jooksul kassapõhiselt tasutud summa. Seega ei ole töötajate arv ja tööjõumaksud kvartalis üks ühele võrreldavad.")
                               ))
    ))
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
