maddata <- readxl::read_xlsx("c:/Users/morte/Downloads/Hverdagsmad.xlsx")

arrow::write_parquet(x = maddata, here::here("Data/maddata.parquet"))
