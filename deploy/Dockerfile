ARG BASE_IMAGE=mihem/shinyapps_3838:v9.0
FROM ${BASE_IMAGE}
USER root
COPY DESCRIPTION deploy/install-viewer-dependencies.R /tmp/cerebro-build/
RUN Rscript /tmp/cerebro-build/install-viewer-dependencies.R
COPY . /opt/CerebroNexus
RUN R CMD INSTALL --no-docs /opt/CerebroNexus \
    && Rscript -e 'stopifnot(requireNamespace("CerebroNexus"), requireNamespace("qs2"), requireNamespace("BPCells"), requireNamespace("shinymanager"))'
USER shiny
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/srv/shiny-server/my_app', host='0.0.0.0', port=3838)"]
