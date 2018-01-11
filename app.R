#Company benchmarking app
#Annegrete Peek

library(shiny)
library(dplyr)
library(zoo)
library(plotly)
library(DT)

load("bench_data.RDa")

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
  max_vaartus = max(tunnus, na.rm = TRUE)
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

server <- function(input, output, session)  {
  
  firma <- NULL
  
  onRestore(function(url) {
    firma <<- parandus(url$input$Firma)
  })
  
  observe(
    updateSelectizeInput(session, "Firma", choices = c("Vali ettevõtted" = "", dt$Nimi), selected = firma, server = TRUE)
  )
  
  andmed <- eventReactive(input$go, {
    andmed <- dt %>% filter(Nimi %in% input$Firma)
  })
  
  output$tabel <- DT::renderDataTable({
    tabel_andmed <- andmed() %>% arrange(desc(aeg)) %>%  select(Nimi, Liik, Käibemaksukohuslane = KMK, Tegevusvaldkond, Maakond) %>% group_by(Nimi) %>% slice(1)
    DT::datatable(tabel_andmed, options = list(dom = 't',
                                             ordering = FALSE,
                                             pageLength = length(unique(andmed()$Nimi)),
                                             autoWidth = FALSE,
                                             columnDefs = list(list(width = "400px", targets = "_all"))),
                  rownames = FALSE)
  })
  
  output$k2ive1 <- renderPlotly({
    skaala = kymne_aste(andmed()$kaive)
    p <- ggplot(andmed(), aes(aeg, kaive/skaala$div, color = Nimi, group = Nimi, text = paste0(Nimi, "\n", aeg, "\nKäive: ", prettyNum(kaive, big.mark = " "), " €"))) + geom_line() + geom_point(size = 0.8) + theme_bw() + 
        labs(x = "", y = paste0("€ ", skaala$label)) + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0) 
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = F)
  })
  
  output$tootajad1 <- renderPlotly({
    skaala = kymne_aste(andmed()$tootajad)
    p <- ggplot(andmed(), aes(aeg, tootajad/skaala$div, color = Nimi, group = Nimi, text = paste0(Nimi, "\n", aeg, "\nTöötajate arv: ", prettyNum(tootajad, big.mark = " ")))) + geom_line() + geom_point(size = 0.8) + theme_bw() +
      labs(x = "", y = paste0(" ", skaala$label)) + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0)
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = F)
  })
  
  output$k2ive2 <- renderPlotly({
    skaala = kymne_aste(andmed()$kaive_tootaja)
    p <- ggplot(andmed(), aes(aeg, kaive_tootaja/skaala$div, color = Nimi, group = Nimi, text = paste0(Nimi, "\n", aeg, "\nKäive töötaja kohta: ", prettyNum(kaive_tootaja, big.mark = " "), " €"))) + geom_line() + geom_point(size = 0.8) + 
      theme_bw() + labs(x = "", y = paste0("€ ", skaala$label)) + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0)
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = F)
  })
  
  output$tootajad2 <- renderPlotly({
    skaala = kymne_aste(andmed()$tmaksud_tootaja)
    p <- ggplot(andmed(), aes(aeg, tmaksud_tootaja/skaala$div, color = Nimi, group = Nimi, text = paste0(Nimi, "\n", aeg, "\nTööjõumaksud töötaja kohta: ", prettyNum(tmaksud_tootaja, big.mark = " "), " €"))) + geom_line() + 
      geom_point(size = 0.8) + theme_bw() + labs(x = "", y = paste0("€ ", skaala$label)) + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0)
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = F)
  })
  
  output$maksud1 <- renderPlotly({
    p <- ggplot(andmed(), aes(aeg, rmaksud_kaive, color = Nimi, group = Nimi, text = paste0(Nimi, "\n", aeg, "\nRiiklikud maksud käibest: ", rmaksud_kaive, "%"))) + geom_line() + geom_point(size = 0.8) + theme_bw() + 
      labs(x = "", y = "%") + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0)
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = F)
  })
  
  output$maksud2 <- renderPlotly({
    p <- ggplot(andmed(), aes(aeg, tmaksud_kaive, color = Nimi, group = Nimi, text = paste0(Nimi, "\n", aeg, "\nTööjõumaksud käibest: ", tmaksud_kaive, "%"))) + geom_line() + geom_point(size = 0.8) + theme_bw() +
      labs(x = "", y = "%") + guides(color = guide_legend(title = NULL)) + scale_x_yearqtr(format = "%Y Q%q") + expand_limits(y = 0)
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = F)
  })

  setBookmarkExclude(c("tabel_cell_clicked", "tabel_rows_all", "tabel_rows_current", "tabel_rows_selected", "tabel_search", "tabel_state", ".clientValue-default-plotlyCrosstalkOpts",
                       ".clientValue-plotly_hover-A", "go"))
}

ui <- function(request) {
  fluidPage(#theme = "bootstrap.css",
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
                               bookmarkButton(label = "Link jagamiseks", title = "Salvesta see vaade ja saa jagamiseks vaate URL."),
                               br(),
                               br(),
                               tags$div(class = "header", checked = NA,
                                        tags$p("Tabel on tehtud EMTA väljastatud kvartaalsete andmete pealt, mis asuvad",
                                               tags$a(href = "http://www.emta.ee/et/kontaktid-ja-ametist/maksulaekumine-statistika/tasutud-maksud-kaive-ja-tootajate-arv", "siin.", target = "_blank"), " I kvartal (Q1) on detsember-veebruar, II kvartal (Q2) on märts-mai, III kvartal (Q3) on juuni-august, IV kvartal (Q4) on september-november."), 
                                        tags$p("*Deklareeritud käibena avaldatakse käibedeklaratsioonide ridade 1, 2 ja 3 summa."),
                                        tags$p("**Töötajate arv on möödunud kvartali viimase kuupäeva seisuga töötamise registrisse kantud kehtiva kandega tööd tegevate isikute arv, tööjõumaksud on kvartali jooksul kassapõhiselt tasutud summa. Seega ei ole töötajate arv ja tööjõumaksud kvartalis üks ühele võrreldavad.")
                               ))
    ))
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
