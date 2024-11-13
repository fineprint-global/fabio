##############################################################################################
##  FABIO Footprints
##############################################################################################

library(Matrix)
library(tidyverse)
library(data.table)

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

# Read labels
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/current/"
regions <- fread(file=paste0(input_path,"regions.csv"))
items <- fread(file=paste0(input_path,"items.csv"))
nrreg <- nrow(regions)
nrcom <- nrow(items)
io <- fread(paste0(input_path,"io_labels.csv"))
su <- fread(paste0(input_path,"su_labels.csv"))

use <- readRDS("data/use_final.rds")
mr_use <-  readRDS(paste0(input_path,"mr_use.rds"))

areas <- regions$iso3c
processes <- unique(su[, .(proc_code,proc)])
commodities <- items$comm_code

yr <- 2021

#1. filtering mr_use to only include feed use -> filter livestock processes and feed commodities, maybe need shares of feed 
#use if some commodities have several use types

#naming rows
mr_use_yr <- mr_use[[as.character(yr)]]
rownames(mr_use_yr) <- paste0(rep(areas, each=length(commodities)), "_", commodities)
colnames(mr_use_yr) <- su$proc_code
livestock_proc <- processes[proc %like% "farming" | proc %like% "husbandry", proc_code]
feed_labels <- su[proc_code %in% livestock_proc, ]

mr_feed_use_yr <- mr_use_yr[, colnames(mr_use_yr) %in% livestock_proc]
colnames(mr_feed_use_yr) <- paste0(feed_labels$iso3c, "_", feed_labels$proc_code)


#2. trade embodiment

# Read data 
allocation <- "value"
X <- readRDS(file=paste0(input_path,"losses/X.rds"))
L <- readRDS(file=paste0(input_path,"losses/",yr,"_L_",allocation,".rds"))
Y <- mr_feed_use_yr


# Calculate footprints
FP <- L %*% Y
colnames(FP) <- colnames(Y)
rownames(FP) <- paste0(io$iso3c, "_", io$item)
FP <- as(FP, "TsparseMatrix")
results <- data.table(origin=rownames(FP)[FP@i + 1], target=colnames(FP)[FP@j + 1], value =FP@x)
results[,`:=`(year = yr,
              unit = "tonnes",
              iso_origin = substr(origin,1,3),
              item_origin = substr(origin,5,100),
              iso_target = substr(target,1,3),
              proc_target = substr(target,5,100))]

results[,`:=`(comm_code = items$comm_code[match(results$item_origin,items$item)],
              group_origin = items$comm_group[match(results$item_origin,items$item)],
              continent_origin = regions$continent[match(results$iso_origin, regions$iso3c)],
              continent_target = regions$continent[match(results$iso_target, regions$iso3c)],
              origin = NULL, target = NULL)]
results <- results %>% relocate(value, .after = last_col())
results[item_origin %in% items[group!="Primary crops", item] | 
          item_origin %in% c("Grazing", "Fodder crops"), value := 0]

data <- results %>% 
  filter(value != 0) %>% 
  mutate(continent_origin = if_else(continent_origin=="EU", "EUR", continent_origin),
         continent_target = if_else(continent_target=="EU", "EUR", continent_target)) %>% 
  filter(continent_target == "EUR") %>% 
  mutate(import = if_else(continent_origin==continent_target, "domestic", "imported")) %>% 
  group_by(import, continent_target, item_origin) %>% 
  summarise(value = round(sum(value)/1000000)) %>% 
  spread(import, value, fill = 0) %>% 
  mutate(total = domestic + imported) %>% 
  mutate(import_share = round(imported / total * 100, 1)) %>% 
  arrange(desc(total), ) %>% 
  filter(total > 5) %>% 
  mutate(item_origin = str_remove(item_origin, " and products")) %>%
  mutate(item_origin = factor(item_origin, levels = item_origin))

data.table::fwrite(data, file=paste0("./output/feed_EUR_",yr,".csv"))


library(ggplot2)

# Plot stacked bar chart
plot <- ggplot(data, aes(x = item_origin)) +
  geom_bar(aes(y = total, fill = "Domestic"), stat = "identity") +
  geom_bar(aes(y = imported, fill = "Imported"), stat = "identity", position = "stack") +
  scale_fill_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
  labs(x = "", y = "Quantity in Mio tonnes", fill = "Source") +
  theme_classic(base_size = 12) +
  # theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, max(data$total) * 1.1))

ggsave(filename = "output/feed_EUR_2021.png", plot, width = 12, height = 6.3)
  

