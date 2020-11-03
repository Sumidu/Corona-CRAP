## app.R ##
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(gghighlight)
library(zoo)
library(slider)
library(glue)
#library(hrbrthemes)
library(ggdark)
library(dashboardthemes)
library(patchwork)
library(DT)



if (file.exists("local.local")) {
    source(here::here("CoronaWatch/data.R"))
    source(here::here("CoronaWatch/aachen.R"))
} else {
    source(here::here("data.R"))
    source(here::here("aachen.R"))
}
raw <- dataRKI()

#raw <- read_csv("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data") %>% 
#    mutate(Meldedatum = as_date(Meldedatum)) %>% 
#    mutate(Refdatum = as_date(Refdatum))


aachen_data <- dataAachen()






# read population data ----
de_DE <- locale("de", decimal_mark = ",")

parse_chr_xls <- function(x) {
    str_remove_all(x, " ") %>% parse_number(locale = de_DE)
}

population <- read_csv2(here::here("kreise.csv")) %>% 
    mutate_at(c("Flaeche", "Gesamt", "Maenner", "Frauen", "Dichte"), parse_chr_xls) 



if (FALSE) {
    # missing LKS?
    raw %>% select(IdLandkreis, Landkreis) %>% unique() %>% anti_join(
    read_csv2("CoronaWatch/kreise.csv") %>% select(IdLandkreis) %>% unique())
}



opt_landkreis <- raw$Landkreis %>% unique()
opt_start <- raw$Meldedatum %>% min()
opt_end <- raw$Meldedatum %>% max()
meldestand <- max(raw$Datenstand)
total_days <- as.numeric(opt_end - opt_start)
all_dates <- seq.Date(opt_start, opt_end, by = "day")

opt_kommunen <- aachen_data %>% pull(Kommune) %>% unique()

map_data <- read_rds(here::here("germany_shape.rds")) %>% mutate(id = factor(id))

# TEST RANGE ----

raw %>% 
    #filter(Landkreis == "StadtRegion Aachen") %>% 
    group_by(Meldedatum, Altersgruppe, Landkreis) %>% 
    summarise(AnzahlFall = sum(AnzahlFall)) %>% 
    mutate(Altersgruppe = factor(Altersgruppe, 
                                 levels = c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+", "unbekannt"))) -> 
    temp
    
temp %>% ungroup() %>% 
    complete(Meldedatum = seq.Date(opt_start, opt_end, by = "day"), 
             Altersgruppe, 
             Landkreis,
             fill = list(AnzahlFall = 0)) 

raw$Meldedatum %>% unique()





ui <- dashboardPage(
    dashboardHeader(title = "Corona Dashboard für Aachen"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Verlauf StädteRegion Aachen", tabName = "aachen", icon = icon("map")),
            menuItem("Die letzten Tage", tabName = "tabLastDays", icon = icon("chart-bar")),
            menuItem("Gesamter Verlauf", tabName = "tabAllDays", icon = icon("chart-area")),
            menuItem("Karte", tabName = "map", icon = icon("map")),
            menuItem("Verlauf nach Altersgruppen", tabName = "ageGroups", icon = icon("chart-bar"))
            #,bookmarkButton()
        )
    ),
    dashboardBody(
        shinyDashboardThemes(
            theme = "grey_dark"
        ),
        tabItems(
            # tab:Aachen ----
            tabItem(tabName = "aachen",
                    fluidRow(
                        box(width = 9, title = "Aktive Fälle in Zahlen",  collapsible = TRUE,
                            selectizeInput(inputId = "kommune_select", label = "Kommune auswählen", 
                                           choices = opt_kommunen,
                                           select = opt_kommunen[1]),
                            valueBoxOutput("active_cases"),
                            valueBoxOutput("active_cases_per_100k"),
                            valueBoxOutput("relative_risk"),
                        ),
                        box(width = 3, title = "Riskoberechnung Kontakte", collapsible = TRUE, collapsed = TRUE,
                            sliderInput("contact_count",label = "Anzahl Kontakte", min = 1, max = 1000, value = 50)),
                        box(width = 3, title = "Riskoberechnung Infektionsgeschehen", collapsible = TRUE, collapsed = TRUE,
                            sliderInput("as_bias", label = "Unterschätzungsfaktor Infektionen", min = 1, max = 20, value = 5)
                            
                        ),
                    box(width = 12, title = "Lokale Daten aus Aachen",  collapsible = TRUE,
                            p("Hinweis: Zahlen sind sog. aktive Fälle, nicht die 7-Tage Inzidenz."),
                            plotOutput("plot_aachen")
                            )
                    )),
            tabItem(tabName = "tabLastDays", # tab:lastdays ----
                    box(width = 6, title = "Landkreis auswählen", collapsible = TRUE,
                        selectInput("lk_select", label = "Landkreis auswählen", 
                                    choices = c("Gesamt", opt_landkreis), selected = "StadtRegion Aachen")),
                    box(width = 6, title = "Zeitfenster", collapsible = TRUE,
                        sliderInput("sel_days", label = "Wie viele Tage zurück?", min = 3, max = total_days, value = 14)
                    ),
                    box(width = 12, title = "Fälle nach Altersgruppe",
                        plotOutput("fig1"),
                        collapsible = TRUE
                    ),
                    box(width = 12, title = "Summe der Fälle", collapsible = TRUE,
                        plotOutput("fig2")),
                    
                    box(width = 12, title = "Summe der Fälle über alle Tage", collapsible = TRUE,
                        plotOutput("fig3")),
                    box(width = 12, title = "Rangliste der Anzahl Fälle für Kleinkinder (0-4 Jahre)",  collapsible = TRUE,
                        p("Gemessen über die Anzahl Tage, wie oben eingestellt."),
                        DT::DTOutput("top_5")
                        )
                    ),
            
            tabItem(tabName = "tabAllDays", # tab:alldays ----
                    box(width = 6, title = "Landkreis auswählen", collapsible = TRUE,
                        selectInput("lk_select_all", label = "Landkreis auswählen", 
                                    choices = opt_landkreis, selected = c("StadtRegion Aachen", "SK Köln", "LK Wesel", "SK Bremen"), 
                                    multiple = TRUE)),
                    box(width = 12, title = "Inzidenz der letzten 7  (pro 100.000 Einwohner)", collapsible = TRUE,
                        plotOutput("fig6"),
                    ),
                    box(width = 12, title = "Summe der Fälle über alle Tage (Zahlen absolut)", collapsible = TRUE,
                        plotOutput("fig4"),
                        plotOutput("fig5"),
                    ),
            ),
            # tab: map ----
            tabItem(tabName = "map", #tab map ----
                    box(width = 6, title = "Datum auswählen", collapsible = TRUE,
                        sliderInput("date_select", label = "Datum auswählen", opt_start, opt_end, opt_end, animate = TRUE)
                    ),
                    box(width = 6, title = "Inzidenzberechnung", collapsible = TRUE, collapsed = TRUE,
                        sliderInput("windows_select", label = "Tage für Summe auswählen", 2, 14, 7),
                        sliderInput("cutoff", label = "Inzidenzgrenze", 10, 800, 100, step = 5)
                    ),
                    box(width = 12, 
                        plotOutput("plot_map", height = "700px")
                    )
            ),
            tabItem(tabName = "ageGroups", # tab:agegroups ----
                    box(width = 6, title = "Landkreis auswählen", collapsible = TRUE,
                        selectInput("lk_select_age", label = "Landkreis auswählen", 
                                    choices = c("Alle", opt_landkreis), selected = "Alle")),
                    box(width = 12, title = "Verlauf nach Alter",  collapsible = TRUE,
                        plotOutput("age_group_plot")
                        )
                    )
            
            
            
            
            
            
        )
    )
)

server <- function(input, output, session) {
    
    
    observe({
        # Trigger this observer every time an input changes
        reactiveValuesToList(input)
        session$doBookmark()
    })
    onBookmarked(function(url) {
        updateQueryString(url)
    })
    
    rawData <- reactive({
        raw %>% mutate(Altersgruppe = factor(Altersgruppe, levels = c("A00-A04", "A05-A14", "A15-A34",
                                                                      "A35-A59", "A60-A79", "A80+", "unbekannt"),
                                             labels = c("Kleinkinder (0-4)", "Kinder (5-14)", 
                                                        "Junge Erwachsene (15-34)", "Erwachsene (35-59)", 
                                                        "Senioren (60-79)", "Ältere (80+)", "unbekannt")))
    })
    
    selectedData <- reactive({
        if (input$lk_select == "Gesamt") {
            return( rawData() %>% 
                        group_by(Altersgruppe, Meldedatum) %>% 
                        summarise(AnzahlFall = sum(AnzahlFall)) %>% 
                        ungroup() %>% 
                        mutate(Landkreis = "Gesamt") %>% 
                        filter(Meldedatum > start_date()) 
                )
            
        }
        
        rawData() %>% filter(str_detect(Landkreis, input$lk_select)) %>% 
            filter(Meldedatum > start_date()) 
    })
    
    start_date <- reactive({
        today() - input$sel_days
    })
    
    
    selectedDataAll <- reactive({
        rawData() %>% filter(Landkreis %in% input$lk_select_all) 
    })
    
    
    convert_berlin <- function(x){
        if (str_starts(x, "11")) {
            return("11000")
        }
        x
    }
    

    
    mapData <- reactive({
        window <- input$windows_select
        #window <- 7
        
        rawData() %>% 
            #raw %>% 
            #    filter(Landkreis == "StadtRegion Aachen") %>% 
            rowwise() %>% 
            mutate(IdLandkreis = convert_berlin(IdLandkreis)) %>% 
            ungroup() %>% 
            mutate(IdLandkreis = factor(IdLandkreis)) %>% 
            group_by(Meldedatum, IdLandkreis) %>% 
            summarise(AnzahlFall = sum(AnzahlFall)) %>% 
            ungroup() %>% 
            complete(Meldedatum = all_dates, 
                     IdLandkreis, 
                     fill = list(AnzahlFall = 0)) %>% 
                ungroup() %>% 
                arrange(Meldedatum, IdLandkreis) %>% 
            group_by(IdLandkreis) %>% 
            mutate(Inzidenz = slider::slide_dbl(AnzahlFall, sum, .before = window, .after = 0)) %>% 
            left_join(population) %>% 
            mutate(Inzidenz = Inzidenz * 100000 / Gesamt)
    })
    
    
    
    active_case_data <- reactive({
        t_data <- dataAachen() %>% filter(Kommune == input$kommune_select)
        last_date <-  t_data %>% pull(date) %>% max()
        active_c <- t_data %>% filter(date == last_date) %>% pull(value)
        active_c
    })
    
    kommune_pop_data <- reactive({
        pops <- tribble(
            ~kommune, ~pop, 
            "Aachen",	2.48960,
            "Alsdorf",	0.47149,
            "Baesweiler",	0.27093,
            "Eschweiler",	0.56482,
            "Herzogenrath",	0.46375,
            "Monschau", 0.11693,
            "Roetgen",	0.08648,
            "Simmerath",	0.15404,
            "Stolberg", 0.56466,
            "Würselen",	0.38756,
            "Gesamtergebnis", 5.52472
            )
        
        pops %>% filter(kommune == input$kommune_select) %>% pull(pop)
    })
    
    # SECTION value boxes ----
    
    output$relative_risk <- renderValueBox({
        
        bias <- input$as_bias
        guests <- input$contact_count
        prob  <- (active_case_data() * bias) / (kommune_pop_data() * 100000)
        
        #prob <- 0.5
        risk <- 1 - dbinom(x = 0, size = guests, prob = prob)
        
        cvalue <- "black"
        if (risk > 0.1) cvalue <- "yellow"
        if (risk > 0.5) cvalue <- "orange"
        if (risk > 0.75) cvalue <- "red"
        
        valueBox(scales::label_percent()(risk), subtitle = "Relatives Risko für eine Corona-Begegnung", icon = icon("bar_chart"), color = cvalue)
    })
    
    output$active_cases_per_100k <- renderValueBox({
        
        active_c <- round(active_case_data() / kommune_pop_data(), 1)
        
        cvalue <- "black"
        if (active_c > 35) cvalue <- "yellow"
        if (active_c > 50) cvalue <- "orange"
        if (active_c > 100) cvalue <- "red"
        
        valueBox(active_c, subtitle = "Aktive Fälle pro 100.000", icon = icon("bar_chart"), color = cvalue)
    })
    
    output$active_cases <- renderValueBox({
        
        active_c <- active_case_data()
        
        valueBox(active_c, subtitle = "Aktive Fälle", icon = icon("bar_chart"), color = "purple")
    })
    
    output$top_5 <- renderDT({
        dataRKI() %>% dailyCasesAlterLandkreis() %>% 
            filter(Meldedatum > start_date()) %>% 
            group_by(Altersgruppe, Landkreis) %>% 
            summarise(Faelle = sum(AnzahlFall)) %>% 
            ungroup() %>% 
            filter(Altersgruppe == "A00-A04") %>% 
            group_by(Altersgruppe) %>% 
            arrange(desc(Faelle)) %>% 
            #slice_max(Faelle, n = 5) %>% 
            ungroup() %>% 
            select(-Altersgruppe) %>% 
            mutate(Rang = 1:n()) %>% 
            select(Rang, Landkreis, `Fälle Kleinkinder (0-4 Jahre)` = Faelle)
    })
    
    # Plot: Aachen ----
    output$plot_aachen <- renderPlot({
        dataAachen() %>% 
            mutate(value = coalesce(value, 0L)) %>% 
            ggplot() +
            aes(
                x = date,
                y = value,
                color = Kommune,
                group = Kommune
            ) +
            geom_line() +
            facet_wrap( ~ Kommune, scales = "free_y") +
            labs(title = "Aktive Fälle nach Kommune", x = "Datum", y = "Aktive Fälle",
                 caption = "Daten der Pressemitteilungen der Stadt Aachen") +
            dark_theme_minimal() + 
            theme(plot.background = element_rect(color = "black", fill = "black")) 
    })
    
    
    # Plot: Map ----
    output$plot_map <- renderPlot({
        
        top_limit <- input$cutoff
        
        #top_limit <- mapData() %>% pull(Inzidenz) %>% max()
        # print(top_limit)
        mapData() %>% 
            filter(Meldedatum == input$date_select) %>% 
            right_join(map_data, by = c("IdLandkreis" = "id")) %>% 
            ggplot() +
            aes(x = long, y = lat, group = group, fill = Inzidenz, label = Inzidenz) +
            geom_polygon(color = "gray", size = 0.1) +
            #geom_label() +
            scale_fill_viridis_c(option = "C", begin = 1, end = 0.1, limits = c(0,top_limit), na.value = "red") +
            coord_fixed() +
            dark_theme_void() +
            labs(caption = glue::glue("Inzidenz als Wert von 100.000. Werte über {top_limit} werden rot dargestellt.")) -> p1
        
        cowplot::ggdraw(p1) + 
            theme(plot.background = element_rect(fill = "black", color = NA))
    })
    
    # Plot: Agegroup ----
    output$age_group_plot <- renderPlot({
        
        df <- rawData()
        if (input$lk_select_age != "Alle") {
            df <- df %>% filter(Landkreis == input$lk_select_age)
        }
        df %>% 
            #filter(Landkreis == "StadtRegion Aachen") %>% 
            group_by(Meldedatum, Altersgruppe) %>% 
            summarise(AnzahlFall = sum(AnzahlFall)) %>% 
            #mutate(Altersgruppe = factor(Altersgruppe, levels = c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+", "unbekannt"))) %>%   
            complete(Meldedatum = seq.Date(opt_start, opt_end, by = "day"), 
                     Altersgruppe, 
                     fill = list(AnzahlFall = 0)) %>% 
            ggplot() +
            aes(x = Meldedatum) +
            aes(y = Altersgruppe) +
            aes(fill = AnzahlFall) +
            geom_raster() +
            scale_fill_viridis_c(option = "B") +
            scale_x_date(limits = c(opt_start, opt_end)) +
            labs(fill = "Anzahl Fälle") +
            dark_theme_minimal() -> plot_a
        
        df %>% 
        #raw %>% 
            #filter(Landkreis == "StadtRegion Aachen") %>% 
            group_by(Meldedatum) %>% 
            summarise(`Todesfälle` = sum(AnzahlTodesfall),
                      `Fälle` = sum(AnzahlFall)) %>% 
            pivot_longer(cols = c(`Fälle`, `Todesfälle`)) %>% 
            mutate(Art = name, Anzahl = value) %>% 
            ggplot() +
            aes(x = Meldedatum) +
            aes(y = Anzahl) +
            aes(fill = Art) +
            geom_col() +
            scale_fill_viridis_d(begin = 0.3, end = 0.7, option = "B") +
            scale_x_date(limits = c(opt_start, opt_end)) +
            #facet_wrap(~Altersgruppe, ncol = 1) +
            dark_theme_minimal() -> plot_b
        
        ( plot_a / plot_b ) & 
            dark_theme_minimal() +
            theme(plot.background = element_rect(color = "black", fill = "black")) 
    })
    
    # fig:where--- ?
    output$fig4 <- shiny::renderPlot({
        selectedDataAll() %>% 
            group_by(Meldedatum, Landkreis) %>% 
            summarise(AnzahlFall = sum(AnzahlFall)) %>% 
            ungroup() %>% 
            group_by(Landkreis) %>% 
            mutate(sda = slider::slide_dbl(AnzahlFall, mean, .before = 7, .after = 0)) %>% 
            ungroup() %>% 
            ggplot() +
            aes(x = Meldedatum, y = AnzahlFall, fill = Landkreis) +
            geom_col(position = "identity", alpha = 0.5) +
            geom_line(mapping = aes(y = sda, x = Meldedatum, group = Landkreis, color = Landkreis), inherit.aes = FALSE, size = 1) +
            labs(caption = "Linie ist der 7-Tage Durchschnitt", y = "Anzahl Fälle", title = "Gemeldete Fälle im Vergleich") +
            dark_theme_gray()
    })
    
    
    output$fig5 <- shiny::renderPlot({
        selectedDataAll() %>% 
            group_by(Meldedatum, Landkreis) %>% 
            summarise(AnzahlFall = sum(AnzahlFall)) %>% 
            ungroup() %>% 
            group_by(Landkreis) %>% 
            mutate(sda = slider::slide_dbl(AnzahlFall, mean, .before = 7, .after = 0)) %>% 
            ungroup() %>% 
            ggplot() +
            aes(x = Meldedatum, y = AnzahlFall, fill = Landkreis) +
            #geom_col(color = "black", position = "identity", alpha = 0.8) +
            geom_line(mapping = aes(y = sda, x = Meldedatum, group = Landkreis, color = Landkreis), inherit.aes = FALSE, size = 1) +
            labs(caption = "Linie ist der 7-Tage Durchschnitt", y = "Anzahl Fälle", title = "Gemeldete Fälle im 7-Tage Durchschnitt") +
            dark_theme_gray()
    })
    
    # Plot auf Population bezogen ----
    output$fig6 <- shiny::renderPlot({
        selectedDataAll() %>% 
            group_by(Meldedatum, Landkreis, IdLandkreis) %>% 
            summarise(AnzahlFall = sum(AnzahlFall)) %>% 
            ungroup() %>% 
            group_by(Landkreis, IdLandkreis) %>% 
            mutate(sda = slider::slide_dbl(AnzahlFall, sum, .before = 7, .after = 0)) %>% 
            ungroup() %>% 
            left_join(population) %>% 
            mutate(sda = sda * 100000 / Gesamt) -> temp_data
        
        if (nrow(temp_data) == 0) {
            return(ggplot() + labs(title = "Keine Bevölkerungsdaten für diesen Landkreis vorhanden"))
        }
        
        temp_data %>% 
            ggplot() +
            aes(x = Meldedatum, y = AnzahlFall, fill = Landkreis) +
            #geom_col(color = "black", position = "identity", alpha = 0.8) +
            geom_line(mapping = aes(y = sda, x = Meldedatum, group = Landkreis, color = Landkreis), inherit.aes = FALSE, size = 1) +
            geom_hline(yintercept = 35, color = "orange") +
            geom_hline(yintercept = 50, color = "red") +
            labs(caption = "Linie ist der 7-Tage Durchschnitt", y = "Anzahl Fälle", title = "Gemeldete Fälle die letzten 7-Tage pro 100.000 Einwohner") +
            dark_theme_gray()
    })
    
    
    # Plot: All Cases ----
    output$fig3 <- shiny::renderPlot({
        
        selectedData() %>% 
            group_by(Altersgruppe) %>% 
            summarise(Faelle = sum(AnzahlFall)) %>% 
            ungroup() %>% 
            ggplot() +
            aes(x = Altersgruppe, y = Faelle, label = Faelle, fill = Altersgruppe) +
            geom_col() +
            geom_text(position = position_stack(vjust = 0.5)) +
            coord_flip() +
            labs(title = glue("Summe der Fälle in den letzten {input$sel_days} Tagen"),
                              x = "Altersgruppe", y = "Anzahl Fälle") +
            guides(fill = FALSE) +
            dark_theme_gray()
            
    })
    
    
    
    # Plot: ???? ----
    output$fig1 <- shiny::renderPlot({
        date_axis <- scale_x_date(date_minor_breaks = "1 day", date_breaks = "1 day", date_labels = "%d.%b")
        label_print <- geom_text(position = position_stack(vjust = 0.5)) 
        if (input$sel_days > 21) {
            date_axis <- scale_x_date(date_minor_breaks = "1 day", date_labels = "%d.%b")
            label_print <- NULL
        }
        selectedData() %>% 
            #raw %>% 
            group_by(Landkreis, Meldedatum, Altersgruppe) %>% 
            summarise(Faelle = sum(AnzahlFall)) %>% 
            ungroup() %>% 
            ggplot() + 
            aes(x = Meldedatum) +
            aes(y = Faelle) +
            aes(label = Faelle) +
            aes(fill = Altersgruppe) +
            geom_col() +
            label_print +
            #gghighlight() +
            facet_wrap(~Altersgruppe, ncol = 1) +
            #gghighlight(Altersgruppe %in% c("A00-A04", "A05-A14")) +
            date_axis +
            labs(title = glue("Fälle in den letzten {input$sel_days} Tagen"),
                 x = "Datum", y = "Anzahl Fälle") +
            theme(legend.position = "bottom") +
            dark_theme_gray() +
            NULL
    })
    
    
    # Plot: ???? ----
    output$fig2 <- shiny::renderPlot({
        date_axis <- scale_x_date(date_minor_breaks = "1 day", date_breaks = "1 day", date_labels = "%d.%b")
        label_print <- geom_text(position = position_stack(vjust = 0.5)) 
        if (input$sel_days > 21) {
            date_axis <- scale_x_date(date_minor_breaks = "1 day", date_labels = "%d.%b")
            label_print <- NULL
        }
        
        selectedData() %>% 
            #raw %>% 
            group_by(Landkreis, Meldedatum, Altersgruppe) %>% 
            summarise(Faelle = sum(AnzahlFall)) %>% 
            ungroup() %>% 
            ggplot() + 
            aes(x = Meldedatum) +
            aes(y = Faelle) +
            aes(label = Faelle) +
            aes(fill = Altersgruppe) +
            geom_col() +
            label_print +
            #gghighlight() +
            #facet_wrap(~Altersgruppe, ncol = 1) +
            #gghighlight(Altersgruppe %in% c("A00-A04", "A05-A14")) +
            date_axis +
            labs(title = glue("Fälle in den letzten {input$sel_days} Tagen"),
                 x = "Datum", y = "Anzahl Fälle") +
            theme(legend.position = "bottom") +
            dark_theme_gray() + 
            NULL
        
        
        
    })
    
}

shinyApp(ui, server, enableBookmarking = "url")

