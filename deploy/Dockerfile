FROM suscreener_base
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'
COPY SUscreener_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_github("EwokOzwok/SUScreener")'
RUN rm /app.tar.gz
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(SUscreener);SUscreener::run_app()"
