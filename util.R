#' @title Query keywords from a database table
#' @description
#' Searches through a column of the database table using the
#' given keywords, with options to ignore letter case and matching on all or any
#' of the keywords
#' @param tbl the database table
#' @param kwds the keywords to look for
#' @param column the column to look for the keywords in
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all the keywords
#' (intersection) or any of the keywords (union) (default FALSE; union)
#' @return the subset of rows of the database table that match the search parameters
query_kwds <- function(tbl, kwds, column, match_all = FALSE) {
  kwds <- paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  query <- paste(
    paste0(column, " like ", "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  sqldf(paste0("SELECT * FROM tbl WHERE ", query))
}

#' @title Plots histogram of phases of the studies
#' @description
#' Displays a histogram to show the distribution of phases from the studies in a query
#' @param d the dataframe of the query result
#' @param studies the handle referencing the "studies" database table
#' @return ggplot object that shows histogram of trial phase distribution
create_phase_histogram = function(d, studies) {
  d$phase[is.na(d$phase)] = "NA"

  # save all possible phases in a vector
  sorted_phases <- (studies |> collect())$phase |>
    unique() |>
    append("NA") |>
    sort()

  d$newphase <- factor(d$phase, levels=sorted_phases)

  d <- d |>
    select(newphase) |>
    group_by(newphase) |>
    summarize(n = n()) |>
    complete(newphase)

  ggplot(d, aes(x = newphase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}

#' @title Plots histogram of endpoint status in the studies
#' @description
#' Displays a histogram to show the distribution of whether the endpoint was
#' met or not in the studies in a query
#' @param d the dataframe of the query result
#' @param endpoints the dataframe that contains endpoint information of the studies
#' @return ggplot object that shows histogram of the distribution of whether the
#' endpoint was met or not
create_endpoint_histogram = function(d, endpoints) {
  em = d |>
    select(nct_id) |>
    left_join(endpoints, by = "nct_id") |>
    group_by(endpoint_met) |>
    summarize(n = n())

  # multiply counts by 10 and divide them by 10 when scaling to make counts of 1 visible
  ggplot(em, aes(x = endpoint_met, y = n * 10)) +
    geom_col() +
    scale_y_log10(labels = function(x) x/10) +
    labs(x = "Endpoint Met", y = "Count") +
    theme_bw()
}

#' @title Plots histogram of conditions examined in the studies
#' @description
#' Displays a histogram to show the distribution of conditions examined in the
#' studies in a query
#' @param d the dataframe of the query result
#' @param conditions the handle referencing the "conditions" database table
#' @return ggplot object that shows histogram of the examined conditions distribution
create_conditions_histogram = function(d, conditions) {
  em = d |>
    select(nct_id) |>
    left_join(conditions |> collect(), by = "nct_id")

  # lump together conditions that aren't considered the top 15 most frequent
  em <- em |>
    mutate(name = fct_lump_n(name, 15)) |>
    group_by(name) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    mutate(conditionsordered = factor(name, levels = name))
  # multiply counts by 10 and divide them by 10 when scaling to make counts of 1 visible
  ggplot(em, aes(x = conditionsordered, y = n * 10)) +
    geom_col() +
    scale_y_log10(labels = function(x) x/10) +
    labs(x = "Condition Name", y = "Count") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}

#' @title Plots histogram of study types of the studies
#' @description
#' Displays a histogram to show the distribution of study types of the
#' studies in a query
#' @param d the dataframe of the query result
#' @return ggplot object that shows histogram of the study type distribution
create_study_type_histogram = function(d) {
  d$study_type[is.na(d$study_type)] = "NA"

  d <- d |>
    select(study_type) |>
    # group observational studies together
    mutate(study_type = if_else(study_type == "Observational [Patient Registry]",
                                "Observational", study_type)) |>
    group_by(study_type) |>
    summarize(n = n())
  # multiply counts by 10 and divide them by 10 when scaling to make counts of 1 visible
  ggplot(d, aes(x = study_type, y = n * 10)) +
    geom_col() +
    scale_y_log10(labels = function(x) x/10) +
    theme_bw() +
    xlab("Study Type") +
    ylab("Count")
}

#' @title Outputs the most countries involved in studies in descending order
#' @description
#' Displays a table of countries in decreasing order of involvement (weren't
#' identified as having been removed) from the studies in a query
#' @param d the dataframe of the query result
#' @param countries the handle referencing the "countries" database table
#' @return dataframe object that lists the countries involved in the studies
#' in descending order of involvement
create_topcountries_table = function(d, countries) {
  countries <- countries %>% collect
  countries$name[is.na(countries$name)] = "Unknown"  # to avoid an empty row value

  d |>
    select(nct_id) |>
    left_join(countries |> collect(), by="nct_id") |>
    filter(removed == FALSE) |>
    group_by(name) |>
    summarize(Count = n()) |>
    arrange(desc(Count), name) |>
    rename(Country = name)
}
