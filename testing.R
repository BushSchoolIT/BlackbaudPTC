library("readxl")
test = read_excel("/home/mlindner/BlackbaudPTC/data/Fall 2023_USPTC.xlsx")
saveRDS(test, file = paste0("./data/test1.rds"))
