


shinyApp(ui, server)
library(rsconnect)
deployApp("C:/Users/Ottavia/Documents/GitHub/beyond-summer-school/algorithms/app-tired")
rsconnect::setAccountInfo(name='ottaviae',
                          token='ACCE4C164C49E808DE6921D1DFF87A0B',
                          secret='K9lJMrMbZMWWu35FjYTnlMQ8ddpeYXowcnAI9Y/f')
