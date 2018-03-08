if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               leaflet,
               raster,
               lubridate,
               RMySQL,
               ggplot2,
               scales,
               stringr,
               reshape2,
               magrittr,
               grDevices,
               data.table,
               igraph,
               dplyr)

source("functions.R")
# test
source("declarations.R")

rv <- reactiveValues(click=NULL,
                     click_value=NULL,
                     history_click=NULL,
                     ID_last_processed_time=Sys.time())

server <- function(input, output, session) {
    # autoInvalidates ----
    autoInvalidate_data_fetch_sql <- reactiveTimer(5 * 60 * 1000, session)
    autoInvalidate_IGCC <- reactiveTimer(4 * 60 * 1000, session)
    autoInvalidate_ID <- reactiveTimer(5 * 60 * 1000, session)
    # Dataframes build up ----

    df_ID_data_raw <- reactive({
        df_ID_data_raw <- get_ID_data()
        df_ID_data_raw <- prepare_ID_data_raw(df_ID_data_raw)
        autoInvalidate_ID()
        input$refresh_data
        return(df_ID_data_raw)
    })

    ID_data <- eventReactive({rv$ID_last_processed_time}, {
        df_ID_data_raw <- df_ID_data_raw()
        if (df_ID_data_raw %>% nrow == 0) {return(data.frame())}

        unique_datetimes <- df_ID_data_raw$datetime %>% unique
        countries <- c(df_ID_data_raw$country_from %>% unique, df_ID_data_raw$country_to %>% unique) %>% unique

        list_graphs <- create_graphs_from_raw_ID_data(df_ID_data_raw, unique_datetimes)
        ID_data <- create_initial_empty_ID_data(countries, unique_datetimes)

        ID_data <- withProgress(
            message='Obtaining ID paths from graph',
            detail='Happy New Year! Mathias',
            value=NULL,
            style='old',
            {
                # The actual data fetching
                calculate_all_paths(list_graphs, ID_data, df_ID_data_raw, unique_datetimes, countries)
            })
    })

    up_ID <- reactive({
        print('up_ID')
        ID_data <- ID_data()
        if (ID_data %>% length == 0) {return(data.frame())}
        up_ID <- lapply(ID_data, function(x) (-1. * x[input$ID_choice, ])) %>% melt(id=NULL)
        up_ID$L1 <- up_ID$L1 /4 -.125
        up_ID

    })

    down_ID <- reactive({
        ID_data <- ID_data()
        if (ID_data %>% length == 0) {return(data.frame())}
        down_ID <- lapply(ID_data, function(x) x[, input$ID_choice, drop=FALSE] %>% t) %>% melt
        down_ID$L1 <- down_ID$L1 /4 -.125
        down_ID
    })

    # Complementary stuff ----
    output$compared_time <- renderText({
        compared_time() %>%
            with_tz('Europe/Amsterdam') %>%
            strftime("%d %b %H:%M", tz='Europe/Amsterdam')
    })

    observeEvent({df_ID_data_raw()}, {
        print('here')
        df_ID_data_raw <- df_ID_data_raw()
        if (df_ID_data_raw %>% nrow == 0) {return()}
        if (df_ID_data_raw$processed_time %>% max > rv$ID_last_processed_time) {
            rv$ID_last_processed_time <<- df_ID_data_raw$processed_time %>% max
        }
    })

    output$ID_plot <- renderPlot({
        print('plot')
        up_ID <- up_ID()
        down_ID <- down_ID()
        up_ID %>% head %>% print
        input$ID_choice %>% print
        df_ID_data_raw <- df_ID_data_raw()
        futures <- rep((df_ID_data_raw %>%
                           group_by(datetime) %>%
                           summarise (future = max(future)))$future,
                       up_ID$variable %>%
                           unique %>%
                           length) %>%
            sort
        up_ID$future <- futures
        down_ID$future <- futures
        if (up_ID() %>% nrow == 0) (return())
        p <- ggplot() +
            geom_bar(data=up_ID,
                     aes(x=L1,
                         y=value,
                         fill=variable,
                         alpha=future),
                     stat='identity',
                     color='black',
                     width=.25) +
            geom_bar(data=down_ID,
                     aes(x=L1,
                         y=value,
                         fill=Var2,
                         alpha=future),
                     stat='identity',
                     color='black',
                     width=.25) +
            scale_fill_manual(values=coloring_ID) +
            scale_x_continuous(expand=c(0,0), breaks=seq(0,24,1), minor_breaks = seq(0,25,1)) +
            geom_hline(aes(yintercept=0), size=2) +
            xlab('Hour') + ylab('MW') + scale_alpha(guide = "none")
        p <- p + annotate("text",
                          x= -Inf,
                          y = Inf,
                          hjust=0,
                          vjust=1,
                          label=paste0("Importing into ", input$ID_choice)
        )
        p <- p + annotate("text",
                          x= -Inf,
                          y = -Inf,
                          hjust=0,
                          vjust=-1,
                          label=paste0("Exporting from ", input$ID_choice)
        ) + theme(legend.position = 'bottom') +
            guides(fill = guide_legend(nrow=1))
        p
        return(p)
    })
    
    observeEvent(input$graph_click,{
        x <- input$graph_click$x
        if (is.null(x)) {
            return()
        }
        rv$history_click <<- x
    })
    
    output$history_plot <- renderPlot({
        x <- rv$history_click
        input$refresh_data
        autoInvalidate_ID()
        if (is.null(x)) {
            return()
        }
        hour_graph <- floor(x)
        minute_graph <- floor((x %% 1)*60/15)*15
        hour_minute_graph <- paste0(hour_graph,':',minute_graph)
        df_history_plot <- get_history_ID_hour(hour_minute_graph)
        title_plot <- unique(df_history_plot$datetime)
        df_history_plot$datetime <- df_history_plot$datetime %>% 
            strptime(format="%Y-%m-%d %H:%M:%S", tz='UTC') %>%
            with_tz('Europe/Amsterdam') %>%
            as.POSIXct
        df_history_plot$processed_time <- df_history_plot$processed_time %>% 
            strptime(format="%Y-%m-%d %H:%M:%S", tz='UTC') %>%
            with_tz('Europe/Amsterdam') %>%
            as.POSIXct
        data_df_export <- subset(df_history_plot,country_from == input$ID_choice)
        data_df_export$value<- data_df_export$value * -1
        data_df_import <- subset(df_history_plot,country_to == input$ID_choice)
        p <- ggplot() +
            geom_step(data=data_df_export,
                      aes(x=processed_time,
                          y=value,
                          colour=country_to,
                          group=country_to), size=2) +
            geom_step(data=data_df_import,
                      aes(x=processed_time,
                          y=value,
                          colour=country_from,
                          group=country_from), size=2) +
            geom_point(data=data_df_export,
                      aes(x=processed_time,
                          y=value,
                          colour=country_to,
                          group=country_to), size=2) +
            geom_point(data=data_df_import,
                      aes(x=processed_time,
                          y=value,
                          colour=country_from,
                          group=country_from), size=2) +
            geom_hline(aes(yintercept=0), size=2) +
            xlab('Hour') + ylab('MW') + scale_alpha(guide = "none")
        p <- p + annotate("text",
                          x= paste0(Sys.Date(), '00:00:00') %>% strptime(format="%Y-%m-%d %H:%M:%S", tz='Europe/Amsterdam') %>%
                              as.POSIXct,
                          y = Inf,
                          hjust=0,
                          vjust=1,
                          label=paste0("Importing into ", input$ID_choice)
        )
        p <- p + annotate("text",
                          x= paste0(Sys.Date(), '00:00:00') %>% strptime(format="%Y-%m-%d %H:%M:%S", tz='Europe/Amsterdam') %>%
                                        as.POSIXct,
                          y = -Inf,
                          hjust=0,
                          vjust=-1,
                          label=paste0("Exporting from ", input$ID_choice)
        ) + theme(legend.position = 'bottom') +
            guides(fill = guide_legend(nrow=1, title = 'Border with:'))+
            ggtitle(title_plot)+
            scale_x_datetime(limits = c(paste0(Sys.Date(), '00:00:00') %>% strptime(format="%Y-%m-%d %H:%M:%S", tz='Europe/Amsterdam') %>%
                                            as.POSIXct, Sys.time()), breaks=date_breaks("1 hour"), 
                             labels= date_format("%H:%M", tz="Europe/Amsterdam"))
        return(p)
    })

    # IGCC ----
    IGCC_data <- reactive({
        autoInvalidate_IGCC()
        input$refresh_data
        df_IGCC <- withProgress(
            # This part takes care of showing the notifcation when data is fetched
            message='Fetching IGCC data',
            detail='Always and truly, Mathias',
            value=NULL,
            style='old',
            {
                # The actual data fetching
                get_IGCC_data()
            })
        return(df_IGCC)
    })
    IGCC_data_export <- reactive({
        df_IGCC <- IGCC_data()
        export <- names(df_IGCC)[grepl(names(df_IGCC), pattern='export_vol')]
        IGCC_data_export <- df_IGCC[, c('datetime', export)]
        export <- export %>% str_sub(1, 2) %>% toupper
        names(IGCC_data_export) <- c('datetime', export)

        IGCC_data_export <- IGCC_data_export%>% melt(id.vars='datetime') %>% na.omit

        return(IGCC_data_export)
    })
    IGCC_data_import <- reactive({
        df_IGCC <- IGCC_data()
        import <- names(df_IGCC)[grepl(names(df_IGCC), pattern='import_vol')]
        IGCC_data_import <- df_IGCC[, c('datetime', import)]
        import <- import %>% str_sub(1, 2) %>% toupper
        names(IGCC_data_import) <- c('datetime', import)
        IGCC_data_import[, import] <- -1. * IGCC_data_import[, import]
        IGCC_data_import <- IGCC_data_import%>% melt(id.vars='datetime') %>% na.omit
        return(IGCC_data_import)
    })
    output$igcc_plot <- renderPlot({
        ggplot() +
            geom_bar(data=IGCC_data_import(),
                     aes(x=datetime,
                         y=value,
                         group=variable,
                         fill=variable),
                     width=15*60,
                     color='black',
                     stat='identity') +
            geom_bar(data=IGCC_data_export(),
                     aes(x=datetime,
                         y=value,
                         group=variable,
                         fill=variable),
                     width=15*60,
                     color='black',
                     stat='identity') +
            scale_fill_manual(values=coloring_IGCC) +
            scale_x_datetime(limits=c(Sys.Date() %>% as.POSIXct %>% with_tz('Europe/Amsterdam') %>% trunc('days') %>% as.POSIXct,
                                     (Sys.Date() +1) %>% as.POSIXct) %>% with_tz('Europe/Amsterdam')  %>% trunc('days') %>% as.POSIXct,
                             breaks=date_breaks('2 hours'),
                             expand=c(0, 0),
                             minor_breaks=date_breaks('1 hours'),
                             labels=date_format("%H", tz='Europe/Amsterdam')) +
            geom_hline(aes(yintercept=0), size=2) +
            ylab('MW') +
            xlab('Time') +
            annotate("text",
                     x = Sys.Date() %>% as.POSIXct,
                     y = Inf,
                     vjust = 1,
                     hjust=0,
                     label = "Exported"
            ) +
            annotate("text",
                     x = Sys.Date() %>% as.POSIXct,
                     y = -Inf,
                     vjust=-.1,
                     hjust=0,
                     label="Imported"
            ) + theme(legend.position = 'bottom') +
            guides(fill = guide_legend(nrow=1))
    })
}
