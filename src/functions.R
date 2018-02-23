get_ID_data <- function(date=Sys.Date()) {
    minimal_datetime <- date %>%
        as.POSIXct %>%
        with_tz('Europe/Amsterdam') %>%
        trunc('days') %>%
        with_tz('UTC') %>%
        strftime('%Y-%m-%d %H:%M:%S')
    maximal_datetime <- (date + 1) %>%
        as.POSIXct %>%
        with_tz('Europe/Amsterdam') %>%
        trunc('days') %>%
        with_tz('UTC') %>%
        strftime('%Y-%m-%d %H:%M:%S')

    stmt <- sprintf(stmt_ID_data %>% strwrap(width=10000, simplify=TRUE),
                    minimal_datetime,
                    maximal_datetime)
    a <- run.query(stmt, 'ID_data')
    return(a$result)
}

prepare_ID_data_raw <- function(df_ID_data_raw){
    # add a column to ID_data indicating whether a datetime is in the future or in the past
    df_ID_data_raw$future <-0
    df_ID_data_raw[df_ID_data_raw$datetime >= ceiling_date(Sys.time()+3600,unit = "hours"), 'future'] <- 1

    df_ID_data_raw$myid<-paste(df_ID_data_raw$country_from,df_ID_data_raw$country_to,df_ID_data_raw$datetime,sep='_')
    df_future <- subset(df_ID_data_raw,future == 1)
    df_past <- subset(df_ID_data_raw,future == 0)

    # for datatimes in the future, only keep the last processed time
    df_future_keep <- df_future %>%
        group_by(datetime, country_from, country_to) %>%
        summarise(processed_time = max(processed_time))
    df_future <- left_join(df_future_keep,df_future)
    # for datetimes in the past, check the amount of datetimes available'
    # if only one available, take that one, if more than one available, take the second one

    # create aux variable that we can cumsum later
    df_past$aux <-1
    # order by myid (asc), procssed_time (desc)
    df_past <- df_past %>% group_by(myid) %>% arrange(desc(processed_time))
    #cumsum per delivery date and border
    df_past$cumsumaux <- ave(df_past$aux,df_past$myid,FUN=cumsum)
    # keep only 2 most recent points.
    indices_past<-df_past$cumsumaux <= 2
    df_past_keep <- df_past[indices_past,]
    # keep pen ultimate point. Thus cumsumaux = 2. In case of one obs take last point
    df_past_grp <- df_past_keep[,c("myid","cumsumaux")] %>%
        group_by(myid) %>%
        summarise(cumsumaux = max(cumsumaux))
    df_past <- left_join(df_past_grp,df_past, by = c('myid','cumsumaux'))
    # join past and future together in one dateframe
    df_ID_data_raw <- merge(df_future,df_past,all = TRUE)
    #remove the 1 and 2 of Germany, and add those values
    df_ID_data_raw$country_from <- gsub('1','',df_ID_data_raw$country_from)
    df_ID_data_raw$country_from <- gsub('2','',df_ID_data_raw$country_from)
    df_ID_data_raw$country_to <- gsub('1','',df_ID_data_raw$country_to)
    df_ID_data_raw$country_to <- gsub('2','',df_ID_data_raw$country_to)
    df_ID_data_raw <- df_ID_data_raw %>% group_by(datetime,country_from,country_to) %>%
        summarise(value = sum(value), future=max(future))
    return(df_ID_data_raw)
}

run.query <- function(stmt, short_name = 'Empty. Fill me!') {
    # press start on stopwatch
    ptm <- proc.time()
    # make connection
    conn <- dbConnect(
        drv = RMySQL::MySQL(),
        db = "weatherforecast",
        host = "172.16.1.4",
        port = 3307,
        username = "eetanalytics",
        password = "eet@123")
    on.exit(dbDisconnect(conn), add=TRUE)
    # Do the actual query
    result <- suppressWarnings(dbGetQuery(conn, stmt))
    # time logging
    time <- round(as.numeric((proc.time() - ptm)["elapsed"]), 2)
    print(sprintf("Query took %.2f seconds (%s)", time, short_name))
    return(list(
        result=result,
        time=time
    ))
}

get_IGCC_data <- function() {
    stmt <- sprintf(stmt_igcc %>% strwrap(width=10000, simplify=TRUE),
                    Sys.time() %>%
                        with_tz("Europe/Amsterdam") %>%
                        trunc('days') %>%
                        with_tz('UTC') %>%
                        strftime('%Y-%m-%d %H:%M:%S'))
    df_IGCC <- run.query(stmt, 'IGCC data')$result
    df_IGCC$datetime <- df_IGCC$datetime %>%
        strptime(format="%Y-%m-%d %H:%M:%S", tz='UTC') %>%
        with_tz('Europe/Amsterdam') %>%
        as.POSIXct
    df_IGCC$datetime <- df_IGCC$datetime + 7.5 * 60
    return(df_IGCC)
}

create_graphs_from_raw_ID_data <- function(df_ID_data_raw, unique_datetimes) {
    unique_datetimes <- df_ID_data_raw$datetime %>% unique

    list_graphs <- lapply(seq(1, length(unique_datetimes)), FUN=function(i) {
        d <- df_ID_data_raw[df_ID_data_raw$datetime == unique_datetimes[i], c('country_from', 'country_to', 'value')]
        names(d) <- c('from', 'to', 'w')
        g <- graph_from_data_frame(d)
    })
    return(list_graphs)
}

create_initial_empty_ID_data <- function(countries, unique_datetimes) {
    lapply(seq(1, length(unique_datetimes)), FUN=function(i) {
        df_temp <- data.frame(row.names=countries)
        df_temp[, countries] <- 0
        df_temp
    })
}

calculate_all_paths <- function(list_graphs, ID_data, df_ID_data_raw, unique_datetimes, countries) {
    aggregate_ID <- aggregate(df_ID_data_raw$value, by=list(df_ID_data_raw$datetime), sum)$x
    for (i in seq(1, length(unique_datetimes))) {
        if (aggregate_ID[[i]] == 0) {next}
        g <- list_graphs[[i]]
        for (country_to in countries) {
            for (country_from in countries) {
                if (country_from == country_to) {
                    next
                }

                paths <- all_simple_paths(g, from=country_from, to=country_to)
                volumes <- sapply(paths,
                                  FUN = function(x)
                                      # This function calculates the possible volume between countries.
                                      # In case of price stuff, that should be included here
                                      min(E(g, path=x)$w)
                )
                ID_data[[i]][country_from, country_to] <- max(volumes)

            }
        }
    }
    return(ID_data)
}

