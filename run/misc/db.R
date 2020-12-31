# Libraries
# Need following sys packages : sudo apt install r-cran-curl r-cran-tidyverse r-cran-plotly r-cran-ggplot2 r-cran-lubridate r-cran-viridis

list.of.packages <- c("hrbrthemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(viridis)
library(htmlwidgets)
library(jsonlite)

hrbrthemes::import_roboto_condensed()

# Set Working Dir
#setwd("@WD@")

plot.save <- function(plot, 
                       width = 800, 
                       height = 500, 
                       text.factor = 1, 
                       filename = paste0(
                                    format(
                                      Sys.time(), 
                                      format = '%Y%m%d-%H%M%S'), '-Rplot.png'
                                    )
                                  ) {

    dpi <- text.factor * 100
    width.calc <- width / dpi
    height.calc <- height / dpi

    ggsave(filename = filename,
                 dpi = dpi,
                 width = width.calc,
                 height = height.calc,
                 units = 'in',
                 plot = plot)
}

# Load datas
result        <- read.csv("./data/result.csv", stringsAsFactors = FALSE)
db_info       <- read.csv("./data/db_info.csv", stringsAsFactors = FALSE)

# Prepare Datas
TPCC <- result %>% mutate(duree = round(elapsed / 60000)) %>% filter(ttype == 'NEW_ORDER')   %>% count(duree) %>% rename(nopm=n)
TPM  <- result %>% mutate(duree = round(elapsed / 60000)) %>% filter(ttype != 'DELIVERY_BG') %>% count(duree) %>% rename(tpm=n)
TPCC_TPM <- left_join(TPCC,TPM)


# Graphs
coeff  <- max(TPM$tpm)/100
colors <- c( "nopm" = "orange", "tpm" = "red" ,"%IO" = "blue", "%CPU" = "yellow", "%MEM" = "green")

# Wait event type (bar chart)
p_db <- db_info %>% filter(wait_event_type != '') %>% mutate(duree = round(time / 60000)) %>%
ggplot(aes(fill=wait_event_type , y=count, x=duree)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T) +
    ggtitle("Database wait events / min") +
    theme_ipsum() +
    xlab("Elapsed minutes")

# Save ggplot
#ggsave("p_db.png", plot =  p_db,  width=@WIDTH@/200, height=@HEIGHT@/200,units="in")
plot.save(p_db, width = @WIDTH@, height = @HEIGHT@, text.factor = 1,"p_db.png")