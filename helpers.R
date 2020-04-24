library('reticulate')

# randn_py <- runif

# py_install(c('numpy', 'matplotlib'))
# Setup Python Environment
# virtualenv_create(envname = 'python_env', python = 'python3')
# # virtualenv_install('python_env', packages = c('numpy', 'matplotlib'), ignore_installed = TRUE)
# virtualenv_install('python_env', packages = c('numpy', 'matplotlib', 'pip==19.0'), ignore_installed = TRUE)
# use_virtualenv('python_env', required = TRUE)

source_python('source.py')

# https://community.rstudio.com/t/python-virtual-environment-in-r-shinyapp-io-stopped-working-suddenly/62561/2
# https://community.rstudio.com/t/using-cron-to-import-data-daily-and-update-shiny-app/24217/4
# https://community.rstudio.com/t/running-code-in-shiny-periodically/27624