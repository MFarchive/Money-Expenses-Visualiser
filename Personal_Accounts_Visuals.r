#Use Code Libraries
library(tibble)
library(dplyr)
library(readr)
library(reactable)
library(plotly)
library(RColorBrewer)

######
# Define Functions
######


# Input expenses within a csv file from a spreadsheet
# See demo document for the format of the csv file
file_path <- "Insert_file_path_here"
excel <- read_csv(file_path)

###Functions

f_date <- function(date) {
    return(as.Date(date, "%d/%m/%Y"))
}

format_dates <- function(file) {
    file %>%
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
    return()
}

total_payments <- function(file) {
    file %>%
    group_by(`In/Out/Transfer`) %>%
    summarise(Total = sum(Payment)) %>%
    return()
}

save_file <- function(data, file_name) {
    write.csv(data, file_name, row.names = FALSE)
}

get_total <- function(file, payment_type) {
    total_payments(file) %>%
    filter(`In/Out/Transfer` == payment_type) %>%
    pull(Total) %>%
    return()
}

as_at_date <- function(file, min_date, max_date) {
    file %>%
    filter(`Date` <= max_date) %>%
    filter(`Date` >= min_date) %>%
    return()
}

account_values <- function(file) {
    file %>%
    group_by(`From`, `To`, `In/Out/Transfer`) %>%
    summarise(Total = sum(Payment)) %>%
    return()
}

balances <- function(values) {
    add <- values %>%
    group_by(`To`) %>%
    summarise(Balance = sum(Total))

    minus <- values %>%
    group_by(`From`) %>%
    summarise(Balance = sum(Total))

    values %>%
    group_by(`To`) %>%
    return(add - minus)
}

net_accounts <- function(file) {
    bal <- balances(account_values(file))
    bal_out <- bal %>%
    ungroup() %>%
    select(From, Total) %>%
    group_by(`Account` = From) %>%
    summarise(`Out` = sum(Total)) %>%
    mutate(`Out` = replace(`Out`, which(is.na(`Out`) == TRUE), 0))

    bal_in <- bal %>%
    ungroup() %>%
    select(To, Total) %>%
    group_by(`Account` = To) %>%
    summarise(`In` = sum(Total)) %>%
    mutate(`In` = replace(`In`, which(is.na(`In`) == TRUE), 0))

    joined <- full_join(bal_in, bal_out) %>%
    mutate(`Out` = replace(`Out`, which(is.na(`Out`) == TRUE), 0)) %>%
    mutate(`In` = replace(`In`, which(is.na(`In`) == TRUE), 0)) %>%
    mutate(`Net` = In - Out) %>%
    select(`Account`, `Net`) %>%
    filter(is.na(`Account`) == FALSE) %>%
    return()
}

d_sort <- function(file) {
    file %>%
    arrange(`Date`) %>%
    return()
}

row <- function(file, d, p, iot, cl, dscrp, f, t = NA) {
    file %>%
    add_row(Date = f_date(d), Payment = p,
    `In/Out/Transfer` = iot, Class = cl, Description = dscrp, From = f,
    To = t) %>%
    d_sort() %>%
    return()
}

outgoings <- function(file) {
    file %>%
    group_by(`From`, `Class`) %>%
    filter(!is.na(`From`)) %>%
    summarise(Total = sum(Payment)) %>%
    return()
}

ingoings <- function(file) {
    file %>%
    group_by(`To`, `Class`) %>%
    filter(`Class` %in% c("Salary", "Interest", "Pension")) %>%
    summarise(Total = sum(`Payment`)) %>%
    return()
}

flow_diagram <- function(file) {
    flow <- file %>%
    group_by(`To`, `From`, `In/Out/Transfer`, `Class`) %>%
    summarise(Total = sum(Payment))

    in_class <- flow %>%
    filter(`In/Out/Transfer` == "In") %>%
    pull(`Class`)
    in_to <- flow %>%
    filter(`In/Out/Transfer` == "In") %>%
    pull(`To`)
    in_total <- flow %>%
    filter(`In/Out/Transfer` == "In") %>%
    pull(`Total`)

    out_from <- flow %>%
    filter(`In/Out/Transfer` == "Out") %>%
    pull(`From`)
    out_class <- flow %>%
    filter(`In/Out/Transfer` == "Out") %>%
    pull(`Class`)
    out_total <- flow %>%
    filter(`In/Out/Transfer` == "Out") %>%
    pull(`Total`)

    transfer_from <- flow %>%
    filter(`In/Out/Transfer` == "Transfer") %>%
    pull(`From`)
    transfer_to <- flow %>%
    filter(`In/Out/Transfer` == "Transfer") %>%
    pull(`To`)
    transfer_total <- flow %>%
    filter(`In/Out/Transfer` == "Transfer") %>%
    pull(`Total`)

    all_nodes <- c(in_class, in_to, out_class, out_from,
     transfer_from, transfer_to)
    unique_nodes <- all_nodes[!duplicated(all_nodes)]
    source <- c(in_class, out_from, transfer_from)
    target <- c(in_to, out_class, transfer_to)
    totals <- c(in_total, out_total, transfer_total)
    source_index <- match(source, unique_nodes) - 1
    target_index <- match(target, unique_nodes) - 1

    colours <- rep("green", length(unique_nodes))
    fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = unique_nodes,
      color = colours,
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.4
      )
    ),

    link = list(
      source = source_index,
      target = target_index,
      value =  totals
    )
  )
  return(fig)
}

worth_tracker_plot <- function(file, date1, date2) {
    date1 <- f_date(date1)
    date2 <- f_date(date2)
    date_vector <- seq(date1, date2, by = 1)
    multiarray <- as_at_date(file, date1, date1) %>%
        net_accounts() %>%
        select(Account, Net) %>%
        setNames(c("Account", date1))
    for (d in 2:length(date_vector)) {
        sub_data <- as_at_date(file, date1, date_vector[d])
        name <- date_vector[d]
        nets <- net_accounts(sub_data) %>%
            select(Account, Net) %>%
            setNames(c("Account", name))
        multiarray <- full_join(multiarray, nets)
    }
    y_groups <- multiarray %>%
        select(-`Account`)
    y_names <- multiarray %>%
        select(`Account`) %>%
        pull()

    palette <- brewer.pal(length(y_names), "Spectral")

    fig <- plot_ly(
        y = y_groups %>%
            slice(1) %>%
            unlist(use.names = FALSE),
        name = y_names[1],
        x = date_vector,
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        fillcolor = palette[1]
    )
    for (n in 2:length(y_names)) {
        fig <- fig %>% add_trace(
            x = date_vector,
            y = y_groups %>%
                slice(n) %>%
                unlist(use.names = FALSE),
            name = y_names[n],
            fillcolor = palette[n]
        )
    }
    fig <- fig %>% layout(title = "Personal Net Worth Tracker",
        yaxis = list(title = "Account Value", showgrid = FALSE),
        xaxis = list(title = "Date", showgrid = FALSE)
    )

    return(fig)
}


# Test some of the functions

reactable(excel)
flow_diagram(excel)
worth_tracker_plot(excel, "01/01/1900", "01/01/2021")