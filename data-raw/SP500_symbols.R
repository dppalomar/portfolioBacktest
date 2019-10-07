library(rvest)  # for web scrapping
library(dplyr)

# download the database from the website
df <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
  html_nodes("table") %>%
  .[[1]] %>% 
  html_table() %>% 
  select(c("Symbol", "Security", "GICS Sector", "GICS Sub Industry")) %>% 
  mutate_each(function(x) {gsub("'", "\\\\'", x)}) %>% 
  mutate(Symbol = gsub("\\.", "-", Symbol)) %>%
  rename(symbol = Symbol, security = Security, GICS_sector = "GICS Sector", GICS_industry = "GICS Sub Industry") %>%
  mutate(created_time = Sys.time())


SP500_symbols <- df$symbol
attr(SP500_symbols, "index_symbol") = "^GSPC"

save(SP500_symbols, file = "data-raw/SP500_symbols.RData", version = 2)

