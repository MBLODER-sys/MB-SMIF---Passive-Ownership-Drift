# R/visualization.R
# Minimal plots for the v1 backtest. The full diagnostic-battery visuals
# (regime classification timeline, sector exposure over time, Fama-French
# coefficient charts, etc.) are deferred to v2.

suppressPackageStartupMessages({
  library(ggplot2)
  library(data.table)
  library(scales)
})

plot_equity_curves <- function(equity_dt, out_path,
                               title = "Passive Ownership Drift — equity curves") {
  setDT(equity_dt)
  equity_dt[, date := as.Date(date)]
  p <- ggplot(equity_dt, aes(date, equity, colour = strategy)) +
    geom_line(linewidth = 0.6) +
    scale_y_continuous(trans = "log", labels = label_number(accuracy = 0.1)) +
    labs(title = title,
         x = NULL, y = "Cumulative return (log scale)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(face = "bold"))
  ggsave(out_path, p, width = 10, height = 6, dpi = 300)
  invisible(p)
}

plot_drawdown_curves <- function(equity_dt, out_path,
                                 title = "Drawdowns") {
  setDT(equity_dt)
  equity_dt[, date := as.Date(date)]
  equity_dt[, peak := cummax(equity), by = strategy]
  equity_dt[, dd := equity / peak - 1]
  p <- ggplot(equity_dt, aes(date, dd, colour = strategy)) +
    geom_line(linewidth = 0.6) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(title = title, x = NULL, y = "Drawdown") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(face = "bold"))
  ggsave(out_path, p, width = 10, height = 6, dpi = 300)
  invisible(p)
}

plot_rolling_sharpe <- function(equity_dt, window = 252,
                                out_path,
                                title = "Rolling 12-month Sharpe ratio") {
  setDT(equity_dt)
  equity_dt[, date := as.Date(date)]
  equity_dt[, ret := equity / shift(equity, 1L) - 1, by = strategy]
  equity_dt[, rs := zoo::rollapplyr(
    ret,
    width = window,
    FUN   = function(x) (mean(x, na.rm = TRUE) / stats::sd(x, na.rm = TRUE)) *
                        sqrt(252),
    partial = FALSE,
    fill    = NA_real_
  ), by = strategy]
  p <- ggplot(equity_dt[!is.na(rs)], aes(date, rs, colour = strategy)) +
    geom_line(linewidth = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    labs(title = title, x = NULL, y = "Sharpe (annualised)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(face = "bold"))
  ggsave(out_path, p, width = 10, height = 6, dpi = 300)
  invisible(p)
}
