# To make sure the times that are obtain from the system are always in UTC, independent
# on which environment you are working

Sys.setenv(TZ='GMT')

base_path = "../data/"

data_path = file.path(base_path, "data_hot_tub.csv")

#coloring
coloring_ID <- c("Germany" = "white",
                   "Netherlands" = "orange",
                   "Belgium" = "red",
                   "France" = "blue",
                   "Denmark" = "green",
                   "Switzerland" = "black")
coloring_ID_history <- c("Germany" = "white",
                 "Netherlands" = "orange",
                 "Belgium" = "red",
                 "France" = "blue",
                 "Denmark" = "green",
                 "Switzerland" = "black")
coloring_IGCC <- c("DE" = "white",
                   "NL" = "orange",
                   "BE" = "red",
                   "FR" = "blue",
                   "DK" = "green",
                   "AT" = "violet",
                   "CH" = "black",
                   "CZ" = "khaki")

# Query statements ----

stmt_igcc <- "SELECT mk1.*
FROM mkonline_data_source mk1 INNER JOIN
(
    SELECT datetime, MAX(processed_time) as processed_time
    FROM mkonline_data_source
    WHERE datetime >= '%s'
    GROUP BY datetime
) mk2 on mk1.datetime = mk2.datetime AND mk1.processed_time = mk2.processed_time ORDER BY datetime"

stmt_ID_data <- "
SELECT
    datetime,
    country_from,
    country_to,
    SUM(value) as value,
    processed_time
FROM (
    SELECT
        intraday1.datetime,
        REPLACE(REPLACE(intraday1.country_from, '1', ''), '2', '') as country_from,
        REPLACE(REPLACE(intraday1.country_to, '1', ''), '2', '') as country_to,
        intraday1.value,
        intraday1.processed_time
    FROM (select * FROM intraday_data_source) intraday1
    INNER JOIN (
        SELECT datetime,
               country_from,
               country_to,
               max(processed_time) as processed_time
        FROM intraday_data_source
        GROUP BY datetime, country_from, country_to) intraday2
    ON intraday1.datetime = intraday2.datetime
       AND intraday1.country_from = intraday2.country_from
       AND intraday1.country_to = intraday2.country_to
       AND intraday1.processed_time = intraday2.processed_time
    WHERE intraday1.datetime >= '%s' AND intraday1.datetime < '%s') ID_total
GROUP BY datetime, country_from, country_to
ORDER BY datetime, country_from, country_to"

stmt_ID_data_plusHistory <- "SELECT
    datetime,
    country_from,
    country_to,
    value,
    processed_time
FROM intraday_data_source
WHERE (datetime >= '%s' AND datetime < '%s')"
