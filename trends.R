library(tidyverse)

find_trends_file <- function() {
  item_pattern <- "(^ItemSelectionDetails).*(\\.csv$)"
  csv_files <- file.info(list.files(pattern = item_pattern))
  most_recent_csv = rownames(csv_files)[which.max(csv_files$mtime)]
  
  
  ## WE'RE MAKING A LOT OF ASSUMPTIONS HERE
  ## assumed format: 'pmix_[startDate]-[endDate].csv'
  DATE_RANGE = substr(most_recent_csv, 22, nchar(most_recent_csv) - 4)
  
  list(most_recent_csv = most_recent_csv, date_range = DATE_RANGE)
}

trends_file_info = find_trends_file()

const_setup = function(dateRange) {
  FILE_TYPE = ".pdf"
  FOLDER_NAME = paste("BBP_Trend_Charts_",dateRange, sep = "")
  
  if(!file.exists(FOLDER_NAME)) {
    print(paste("Folder", FOLDER_NAME, "not found, creating..."))
    dir.create(FOLDER_NAME)
  }
  
  list(file_type = FILE_TYPE, output_folder = FOLDER_NAME)
}

consts = const_setup(trends_file_info$date_range)

data_setup = function(fileName) {
  
  RAW_DATA = read.csv(fileName) %>%
    select(Order.Id, Order.Date, Item.Id, Menu.Item, Menu.Group, Menu, Gross.Price, Qty) %>%
    mutate(Order.Day = format(strptime(Order.Date, format="%m/%d/%y %I:%M %p"),format='%m/%d/%Y'))
  
  list(raw = RAW_DATA)
    
}

data = data_setup(trends_file_info$most_recent_csv)

## find egg sandwich sales by day of week 
egg_sales = data$raw %>%
  filter(Menu.Item == 'Egg Sandwich')

qty_sold_by_day = function(to_analyze) {
  sold_by_day = to_analyze %>%
    group_by(Order.Day) %>%
    summarise(total_sold = sum(Qty))
  
  graph = ggplot(data=sold_by_day, aes(x=Order.Day, y=total_sold)) + geom_bar(stat="identity") + theme_minimal()
  graph
}

egg_sales_graph = qty_sold_by_day(egg_sales)

lawsons_sales = data$raw %>%
  filter(Menu.Group == "Lawson's Finest Liquids")

lawsons_sales_graph = qty_sold_by_day(lawsons_sales)
lawsons_sales_graph

