# tabulate conditions by province function

tabulate_condition <- function(
    province = province) {
    agg_dt[prov_ == province] %>%
        uncount(n) %>%
        select(-c(date, prov_)) %>%
        tbl_summary(
            by = year,
            statistic = list(all_categorical() ~ "{n}"),
        )%>%
        as_flex_table()
}


tabulate_province <- function(
    condition = condition) {
    agg_dt[condition == condition] %>%
        uncount(n) %>%
        select(-c(date, condition)) %>%
        tbl_summary(
            by = year,
            statistic = list(all_categorical() ~ "{n}"),
        )
}
