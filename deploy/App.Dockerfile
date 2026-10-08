ARG VIEWER_IMAGE
FROM ${VIEWER_IMAGE}
USER root
COPY --chown=shiny:shiny my_app /srv/shiny-server/my_app
RUN if [ -d /srv/shiny-server/my_app/private-data/auth ]; then \
      chmod 700 /srv/shiny-server/my_app/private-data/auth \
      && chmod 600 /srv/shiny-server/my_app/private-data/auth/credentials.sqlite; \
    fi
USER shiny
