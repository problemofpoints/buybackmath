
# do buyback calculations and create exhibits


# setup -----------------------------------------------------------

library(knitr)
library(AonInternal)
library(tidyverse)
library(tibbletime)
library(lubridate)
library(tidyquant)
library(flextable)
library(AonECM)
library(officer)
library(patchwork)

AonInternal::SetTheme()
aon_colors <- c("#0083A9", "#808080", "#BFBFBF", "#5EB6E4", "#F0AB00", "#7AB800", 
                "#6E267B", "#BC5FCD","#E9CAEE", "#D3CD8B", "#7E7830", "#3E5B00", "#E11B22")

AonECM::ggSetTheme()


# import data -------------------------------------------------------------

data_to_use <- readRDS("data/buyback-roi-data.rds")
stock_prices <- readRDS("data/stock-prices.rds")

# buyback roi calculations ------------------------------------------------

data_to_use <- data_to_use %>%
  mutate(cum_shares_repurchased = cumsum(sharesrepurchased)) %>%
  mutate(cash_flow = -dollarrepurchase + (commondivpershare + specdivpershare) * cum_shares_repurchased,
         cash_flow_final = cash_flow + if_else(year == 2017 & qtr_ == 4, close * cum_shares_repurchased, 0),
         cash_flow_final2 = cash_flow + if_else(year == 2017 & qtr_ == 4, adjusted * cum_shares_repurchased, 0))

buyback_roi <- data_to_use %>% 
  summarise(n_qtrs = n(),
            market_cap = market_cap[1], init_shares = commonshares[1], 
            shares_repurchased = dplyr::last(cum_shares_repurchased), 
            dollar_repurchased = sum(dollarrepurchase), 
            pct_init_shares = shares_repurchased / init_shares, 
            pct_market_cap = sum(dollarrepurchase) / market_cap[1],
            tsr = ((sum((commondivpershare + specdivpershare) * commonshares) / dplyr::last(commonshares) + 
                      dplyr::last(close)) / dplyr::first(close))^(1/(n_qtrs/4)) - 1,
            irr = (1+FinCal::irr(cash_flow_final))^4 - 1,
            buyback_effectiveness = (1+irr)/(1+tsr) - 1) %>%
  filter(pct_init_shares > 0.01) %>%
  arrange(desc(buyback_effectiveness))

buyback_roi_output <- buyback_roi %>%
  transmute(Ticker = ticker,
            `# of quarters` = n_qtrs,
            `Current Mkt Cap ($m)` = number_format(market_cap / 1e6, 0),
            `Initial Shares (mil)` = number_format(init_shares / 1e6, 1),
            `Shares Repurchased (mil)` = number_format(shares_repurchased / 1e6,1),
            `Dollar Repurchased ($m)` = number_format(dollar_repurchased / 1e6,0),
            `Percent of Initial Shares Repurchased` = pct_format(pct_init_shares, 1),
            `Percent of Mkt Cap Repurchased` = pct_format(pct_market_cap,1),
            `Total Shareholder Return` = pct_format(tsr, 1),
            `Buyback ROI` = pct_format(irr, 1),
            `Buyback Effectiveness` = pct_format(buyback_effectiveness,1))

# create exhibits ---------------------------------------------------------

ft_buyback_summary1 <- buyback_roi_output %>%
  slice(1:25) %>%
  regulartable() %>%
  ft_theme_aon() %>%
  fontsize(part = "all", size = 8) %>%
  autofit(add_w = 0.05, add_h = 0.0) %>%
  height_all(height = 0.2, part = "body") %>%
  width(j = 1:2, width = 0.54) %>%
  width(j = 3:11, width = 0.9) %>%
  add_title_header("Top 25 most effective share repurchasers")

gg_roi_scatter <- buyback_roi %>%
  ggplot(aes(x = irr, y = buyback_effectiveness, size = pct_market_cap)) +
  geom_hline(yintercept = 0, size = 0.5) + geom_vline(xintercept = 0, size = 0.5) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = ticker), size = 2, colour = aon.colors$darkgray, 
                           segment.color = aon.colors$lightgray, segment.size = 0.25, force = 2) +
  scale_x_continuous(labels = scales::percent_format(), breaks = seq(from = -0.25, to = 0.75, by = 0.1), 
                     limits = c(-0.25, 0.75)) +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(from = -0.25, to = 0.75, by = 0.1),
                     limits = c(-0.25, 0.75)) +
  scale_size_continuous(name = "% of Mkt Cap \n Repurchased", 
                    labels = scales::percent_format()) +
  xlab("Buyback ROI") + ylab("Buyback Effectiveness") +
  theme(panel.grid.major.x = element_line(color = aon.colors$lightergray, linetype = "dashed")) +
  ggtitle("Buyback ROI vs. Buyback Effectiveness by Company")


gg_col_roi <- buyback_roi %>%
  mutate(ticker = fct_reorder(ticker, irr)) %>%
  mutate(y_irr = if_else(irr > 0, irr + 0.03, irr - 0.03)) %>%
  ggplot(aes(x = ticker, y = irr)) +
  geom_col() + 
  geom_text(aes(y = y_irr, x = ticker, label = pct_format(irr, digits = 0)), size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(from = -0.25, to = 0.75, by = 0.1), 
                     limits = c(-0.25, 0.75)) +
  xlab(NULL) + ylab("Buyback ROI") +
  theme(panel.grid.major.x = element_line(color = aon.colors$lightergray, linetype = "dashed"),
        panel.grid.major.y = element_blank()) +
  ggtitle("Buyback ROI by Company (2013 - 2017)")

gg_col_eff <- buyback_roi %>%
  filter(!is.na(buyback_effectiveness) & buyback_effectiveness > -1) %>%
  mutate(ticker = fct_reorder(ticker, buyback_effectiveness)) %>%
  mutate(y_irr = if_else(buyback_effectiveness > 0, buyback_effectiveness + 0.02, buyback_effectiveness - 0.02)) %>%
  ggplot(aes(x = ticker, y = buyback_effectiveness)) +
  geom_col() + 
  geom_text(aes(y = y_irr, x = ticker, label = pct_format(buyback_effectiveness, digits = 0)), size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(from = -0.25, to = 0.75, by = 0.1), 
                     limits = c(-0.25, 0.75)) +
  xlab(NULL) + ylab("Buyback ROI") +
  theme(panel.grid.major.x = element_line(color = aon.colors$lightergray, linetype = "dashed"),
        panel.grid.major.y = element_blank()) +
  ggtitle("Buyback Effectiveness by Company (2013 - 2017)")


create_by_company_slide <- function(co_ticker, company_name, data, data_roi, stock_prices, pptx){
  
  co_ticker_enquo <- enquo(co_ticker)

  data_filtered <- data %>%
    ungroup(ticker) %>%
    filter(ticker == !!co_ticker_enquo) %>%
    mutate(dec_date = year + qtr_/4)
  
  roi_filtered <- data_roi %>%
    filter(Ticker == !!co_ticker_enquo)

  stock_prices_filtered <- stock_prices %>%
    filter(symbol == !!co_ticker_enquo) %>%
    mutate(dec_date = decimal_date(date))
  
  gg_cum_shares <- data_filtered %>%
    ggplot(aes(x = dec_date, y = cum_shares_repurchased/1e6)) +
    geom_line() + geom_point() +
    scale_x_continuous(breaks = data_filtered$dec_date[seq(from = 1, to = nrow(data_filtered), by = 2)], 
                       labels = data_filtered$yr_qtr[seq(from = 1, to = nrow(data_filtered), by = 2)]) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    theme(axis.text.x = element_text(angle=315, vjust = 0.5), text = element_text(size = 7), 
          plot.title = element_text(size = 8)) +
    ylab("Cumulative shares repurchased (millions)") + xlab("Quarter") +
    ggtitle(paste0("Cumulative number of common shares repurchased since ", min(data_filtered$year)))
  
  gg_cash_flows <- data_filtered %>%
    mutate(div_saved = -((commondivpershare + specdivpershare) * cum_shares_repurchased),
           net_repurchase = -(dollarrepurchase - div_saved),
           final_cashflow = if_else(year == max(data_filtered$year) & qtr_ == 4, close * cum_shares_repurchased, 0)) %>%
    select(dec_date, div_saved, net_repurchase, final_cashflow) %>%
    gather(key, value, -dec_date) %>%
    mutate(key = factor(key, levels = c("div_saved","net_repurchase","final_cashflow"))) %>%
    ggplot(aes(x = dec_date)) +
    geom_col(aes(y = value/1e6, fill = key)) + 
    scale_fill_manual(name = NULL, 
                      labels = c("Dividends avoided", "Net repurchases", "Value of shares \n repurchased"),
                      values = c(aon.colors$lightgray, aon.colors$teal, aon.colors$darkgray)) +
    scale_x_continuous(breaks = data_filtered$dec_date[seq(from = 1, to = nrow(data_filtered), by = 2)], 
                       labels = data_filtered$yr_qtr[seq(from = 1, to = nrow(data_filtered), by = 2)]) +
    scale_y_continuous(labels = scales::comma_format(), breaks = scales::pretty_breaks(n = 5)) +
    theme(axis.text.x = element_text(angle=315, vjust = 0.5), text = element_text(size = 7), 
          plot.title = element_text(size = 8)) +
    ylab("Dollar Amount of Net Share Repurchases (millions)") + xlab("Quarter") +
    ggtitle(paste0("Dollar amount of net repurchases by quarter since ", min(data_filtered$year)))
  
  ft_buyback_summary <- roi_filtered %>%
    regulartable() %>%
    ft_theme_aon() %>%
    fontsize(part = "all", size = 8) %>%
    autofit(add_w = 0.05, add_h = 0.0) %>%
    height_all(height = 0.2, part = "body") %>%
    width(j = 1:2, width = 0.54) %>%
    width(j = 3:11, width = 0.9)
  
  gg_p_to_b <- stock_prices_filtered %>%
    mutate(yr_qtr = paste0(year(date), "Q", quarter(date))) %>%
    left_join(data_filtered %>% 
                select(-market_cap, -close, - adjusted, - company, - ticker, -date, -dec_date), 
              by = c("yr_qtr"))  %>%
    mutate(`Price to book` = adjusted / bvps) %>%
    mutate(`Dollar Repurchase` = dollarrepurchase / 1e6) %>%
    select(dec_date, `Price to book`, adjusted, sharesrepurchased, `Dollar Repurchase`) %>%
    gather(key = metric, value = value, -dec_date) %>%
    filter(metric %in% c("Price to book", "Dollar Repurchase")) %>%
    ggplot(aes(x = dec_date)) +
    geom_line(aes(y = value)) +
    facet_grid(metric ~ ., scales = "free_y") +
    xlab("Date") + ylab(NULL) +
    scale_x_continuous(breaks = data_filtered$dec_date[seq(from = 1, to = nrow(data_filtered), by = 2)],
                       labels = data_filtered$yr_qtr[seq(from = 1, to = nrow(data_filtered), by = 2)]) +
    theme(text = element_text(size = 7), plot.title = element_text(size = 8)) +
    ggtitle("Dollar Amount of Repurchases ($m) vs. Price-to-book Ratio")
  
  
  pptx <- pptx %>%
    add_slide(layout = "Title and Content", master = "Template with examples_US letter") %>%
    ph_with_text(company_name, type = "title") %>%
    ph_with_flextable(ft_buyback_summary, type = "body", index = 2) %>%
    ph_with_gg_at(gg_cum_shares, width = 4.88, height = 2.99, left = 0.54, top = 2.2) %>%
    ph_with_gg_at(gg_cash_flows, width = 4.88, height = 2.99, left = 0.54, top = 5.12) %>%
    ph_with_gg_at(gg_p_to_b, width = 5.1, height = 4.87, left = 5.53, top = 2.2)
  
  list(ft_buyback_summary, gg_cum_shares, gg_cash_flows, gg_p_to_b)
  
}


# create deck -------------------------------------------------------------

# start with blank pptx template
pptx <- read_pptx("share_buyback_roi.pptx")

pptx <- pptx %>% 
  add_slide(layout = "Title and Content", master = "Template with examples_US letter") %>%
  ph_with_text("Buyback ROI by Company", type = "title") %>%
  ph_with_gg(gg_col_roi, type = "body", index = 2) %>%
  add_slide(layout = "Title and Content", master = "Template with examples_US letter") %>%
  ph_with_text("Buyback Effectiveness by Company", type = "title") %>%
  ph_with_gg(gg_col_eff, type = "body", index = 2) %>%
  add_slide(layout = "Title and Content", master = "Template with examples_US letter") %>%
  ph_with_text("Buyback ROI vs. Buyback Effectiveness by Company", type = "title") %>%
  ph_with_gg(gg_roi_scatter, type = "body", index = 2) %>%
  add_slide(layout = "Title and Content", master = "Template with examples_US letter") %>%
  ph_with_text("Top 25 most effective share repurchasers", type = "title") %>%
  ph_with_flextable(ft_buyback_summary1, type = "body", index = 2) %>%
  add_slide(layout = "Sub-Section Divider", master = "Template with examples_US letter") %>%
  ph_with_text("By Company Detail", type = "ctrTitle")

walk2(unique(data_to_use$ticker), unique(data_to_use$company), 
      ~ possibly(create_by_company_slide(.x, .y, data_to_use, buyback_roi_output, stock_prices, pptx), NA_complex_))


# write out slide deck
print(pptx, target = "P&C (Re)Insurers Share Buyback ROI_2013-2017.pptx") %>% invisible()






