
bench <- microbenchmark::microbenchmark(
  times = 100,
  collapse = {
    dt_c <- df |>
      collapse::fgroup_by(area)|>
      collapse::fmutate(wbpip::gd_clean_data(.data,
                                             welfare = "welfare",
                                             population = "weight",
                                             gd_type = gd_type,
                                             quiet = TRUE
      ))|>
      collapse::fungroup()
  },
  data.table = {
    dt <- df |>
      _[, wbpip::gd_clean_data(
        .SD,
        welfare = "welfare",
        population = "weight",
        gd_type = gd_type,
        quiet = TRUE
      ),
      by = .(area)]

    data.table::setcolorder(dt,"area",after = "gender")
  }
)
if (requireNamespace("highcharter")) {
  hc_dt <- highcharter::data_to_boxplot(bench,
                                        time,
                                        expr,
                                        add_outliers = FALSE,
                                        name = "Time in milliseconds")

  highcharter::highchart() |>
    highcharter::hc_xAxis(type = "category") |>
    highcharter::hc_chart(inverted=TRUE) |>
    highcharter::hc_add_series_list(hc_dt) |>
    highcharter::hc_title(text = "Comparison collapse vs data.table")

} else {
  boxplot(bench, outline = FALSE)
}
