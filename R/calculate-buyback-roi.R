
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

