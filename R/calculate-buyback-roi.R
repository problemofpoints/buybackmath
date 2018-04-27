
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
  filter(pct_market_cap > 0.01) %>%
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

# - overall table, bar graph
# - scatter plot with buyback ROI vs. buyback eff; sized by % repurchased
# - summary table on top
# - p:b vs repurchase on right; cash flows or shares repurchased on left

ft_buyback_summary1 <- buyback_roi_output %>%
  slice(1:25) %>%
  regulartable() %>%
  ft_theme_aon() %>%
  add_title_header("Top 25 most effective share repurchasers")

gg_roi_scatter <- buyback_roi %>%
  ggplot(aes(x = irr, y = buyback_effectiveness, size = pct_market_cap)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = ticker), size = 3) +
  scale_x_continuous(labels = scales::percent_format(), breaks = seq(from = -0.25, to = 0.75, by = 0.1), 
                     limits = c(-0.25, 0.75)) +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(from = -0.25, to = 0.75, by = 0.1),
                     limits = c(-0.25, 0.75)) +
  scale_size_continuous(name = "% of Mkt Cap", 
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


# create deck -------------------------------------------------------------

# start with blank pptx template
pptx <- read_pptx("share_buyback_roi.pptx")

pptx <- pptx %>% 
  add_slide(layout = "Title and Content", master = "Template with examples_US letter") %>%
  ph_with_text("Summary table", type = "title") %>%
  ph_with_flextable(ft_buyback_summary1, type = "body") %>%
  add_slide(layout = "Title and Content", master = "Template with examples_US letter") %>%
  ph_with_text("Scatterplot", type = "title") %>%
  ph_with_gg(gg_roi_scatter) %>%
  add_slide(layout = "Title and Content", master = "Template with examples_US letter") %>%
  ph_with_text("gg_col_roi", type = "title") %>%
  ph_with_gg(gg_col_roi) %>%
  add_slide(layout = "Title and Content", master = "Template with examples_US letter") %>%
  ph_with_text("gg_col_eff", type = "title") %>%
  ph_with_gg(gg_col_eff)

# write out slide deck
print(pptx, target = "share_buyback_roi2.pptx") %>% invisible()






