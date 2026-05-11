library("pdftools")

handle1 <- "C:\\Users\\dustin.smith\\OneDrive - Lubbock Independent School District\\Pictures\\Documents\\Codes\\Python\\Climate data project\\PDFs\\BSTS.pdf"
handle2 <- "C:\\Users\\dustin.smith\\OneDrive - Lubbock Independent School District\\Pictures\\Documents\\Codes\\Python\\Climate data project\\PDFs\\Model_comparison.pdf"
handle3 <- "C:\\Users\\dustin.smith\\OneDrive - Lubbock Independent School District\\Pictures\\Documents\\Codes\\Python\\Climate data project\\PDFs\\NN modeling_Final.pdf"
handle4 <- "C:\\Users\\dustin.smith\\OneDrive - Lubbock Independent School District\\Pictures\\Documents\\Codes\\Python\\Climate data project\\PDFs\\Random_Forest.pdf"
handle5 <- "C:\\Users\\dustin.smith\\OneDrive - Lubbock Independent School District\\Pictures\\Documents\\Codes\\Python\\Climate data project\\PDFs\\Regularized_models_elasticnet.pdf"
handle6 <- "C:\\Users\\dustin.smith\\OneDrive - Lubbock Independent School District\\Pictures\\Documents\\Codes\\Python\\Climate data project\\PDFs\\SARIMA model.pdf"
  
outfile <- "C:\\Users\\dustin.smith\\OneDrive - Lubbock Independent School District\\Pictures\\Documents\\Codes\\Python\\Climate data project\\PDFs\\merged.pdf"

files_to_merge <- c(handle6, handle3, handle5, handle1, handle4, handle2)
pdf_combine(files_to_merge, output = outfile)
