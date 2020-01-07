FROM openanalytics/r-base

# Initial apt update to make everything else work
#RUN apt-get update
#RUN apt-get install -y lsb-release

# Add the PGSql repo and signing key
#RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
# RUN export RELEASE=$(lsb_release -cs)
# RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ ${RELEASE}"-pgdg main | tee  /etc/apt/sources.list.d/pgdg.list

# system libraries of general use
RUN apt-get update && apt-get install -y \
    gfortran-7 \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libmpfr-dev \
    libpq-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    libxml2-dev \
    pandoc \
    pandoc-citeproc \
    postgresql-client \
    sudo 

# copy the app to the image
RUN mkdir /root/iatlas
COPY . /root/iatlas
WORKDIR /root/iatlas

# Resolve dependencies
ENV DOCKERBUILD 1
RUN R -e "Sys.getenv(\"DOCKERBUILD\")"

# Run tests
# RUN R -e "devtools::test()"

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(appDir = '/root/iatlas')"]