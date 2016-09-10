library(devtools)
library(roxygen2)

roxygenize('qlib')
dev_mode()

document('qlib')
load_all('qlib')
