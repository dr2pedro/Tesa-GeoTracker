# argument for download a required osm.pbf image, sudeste will be default
ARG BUILD_VERSION=sudeste

FROM ubuntu:focal AS base

# setting the enviroment variables
ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    NOMINATIM_VERSION=v3.5.1
# install dependencies
RUN apt-get -y update -qq && apt-get -y install locales && \
    locale-gen en_US.UTF-8 && \
    update-locale LANG=en_US.UTF-8 && \
    apt-get install -o APT::Install-Recommends="false" -o APT::Install-Suggests="false" -y \
    build-essential \
    cmake \
    g++ \
    libboost-dev \
    libboost-system-dev \
    libboost-filesystem-dev \
    libexpat1-dev \
    zlib1g-dev \
    libbz2-dev \
    libpq-dev \
    libproj-dev \
    postgresql-server-dev-12 \
    postgresql-12-postgis-3 \
    postgresql-contrib \
    postgresql-12-postgis-3-scripts \
    apache2 \
    php \
    php-pgsql \
    libapache2-mod-php \
    php-intl \
    python3-setuptools \
    python3-dev \
    python3-pip \
    python3-psycopg2 \
    python3-tidylib \
    git \
    curl \
    sudo && \
    apt-get clean && \
    # cleaning the cached files
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/* /var/tmp/* && \
    # Configure postgres
    echo "host all  all    0.0.0.0/0  trust" >> /etc/postgresql/12/main/pg_hba.conf && \
    echo "listen_addresses='*'" >> /etc/postgresql/12/main/postgresql.conf && \
    # Osmium install to run continuous updates
    pip3 install osmium && \
    # Nominatim install
    git clone --recursive https://github.com/openstreetmap/Nominatim ./src && \
    cd ./src && \
    git checkout tags/$NOMINATIM_VERSION && \
    git submodule update --recursive --init && \
    mkdir build && \
    cd build && \
    # compilation of nominatim (this helps with multi-arch)
    cmake .. && \
    make -j`nproc` && \
    cd .. && \
    # Load initial data
    curl http://www.nominatim.org/data/country_grid.sql.gz > ./data/country_osm_grid.sql.gz && \
    chmod o=rwx ./build 

# just for point the entry there.   
WORKDIR /app

# Apache configure
COPY local.php /src/build/settings/local.php
COPY nominatim.conf /etc/apache2/sites-enabled/000-default.conf
COPY start.sh /app/scripts/

# download conditionally the osm.pdf file desired for build the maps.
FROM base AS branch-version-sudeste
RUN mkdir data && \
    curl https://download.geofabrik.de/south-america/brazil/sudeste-latest.osm.pbf > ./data/place.osm.pbf

FROM base AS branch-version-sul
RUN mkdir data && \
    curl https://download.geofabrik.de/south-america/brazil/sul-latest.osm.pbf > ./data/place.osm.pbf

FROM base AS branch-version-nordeste
RUN mkdir data && \
    curl https://download.geofabrik.de/south-america/brazil/nordeste-latest.osm.pbf > ./data/place.osm.pbf

FROM base AS branch-version-norte
RUN mkdir data && \
    curl https://download.geofabrik.de/south-america/brazil/norte-latest.osm.pbf > ./data/place.osm.pbf

FROM base AS branch-version-centro-oeste
RUN mkdir data && \
    curl https://download.geofabrik.de/south-america/brazil/centro-oeste-latest.osm.pbf > ./data/place.osm.pbf

# take the build that has the osm.pbf desired and continue the process
FROM branch-version-${BUILD_VERSION} AS after-condition
# sudo is complaining nowdays and that disable it.
RUN echo "Set disable_coredump false" >> /etc/sudo.conf && \
    # creating a folder to stores the data unziped latter.
    rm -rf /data/postgresdata && \
    mkdir -p /data/postgresdata && \
    chown postgres:postgres /data/postgresdata && \
    export  PGDATA=/data/postgresdata && \
    # Starting a database in this path.
    sudo -u postgres /usr/lib/postgresql/12/bin/initdb -D /data/postgresdata && \
    sudo -u postgres /usr/lib/postgresql/12/bin/pg_ctl -D /data/postgresdata start && \
    # Setting password and users.
    sudo -u postgres psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='nominatim'" | grep -q 1 || sudo -u postgres createuser -s nominatim && \
    sudo -u postgres psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='www-data'" | grep -q 1 || sudo -u postgres createuser -SDR www-data && \
    sudo -u postgres psql postgres -c "DROP DATABASE IF EXISTS nominatim" && \
    useradd -m -p password1234 nominatim && \
    # Changing the permissions.
    chown -R nominatim:nominatim /src && \
    # Compiling the tables for database.
    sudo -u nominatim /src/build/utils/setup.php --osm-file /app/data/place.osm.pbf  --all --threads 4 && \
    # Cheking the integrity
    sudo -u nominatim /src/build/utils/check_import_finished.php && \
    # stop the data to transfer to the main folder, the default of postgres
    sudo -u postgres /usr/lib/postgresql/12/bin/pg_ctl -D /data/postgresdata stop && \
    # Permissions.
    sudo chown -R postgres:postgres /data/postgresdata && \
    chmod 700 /data/postgresdata && \
    # Transfering the data
    rsync -av /data/postgresdata /var/lib/postgresql/12/main && \
    rm -rf /data/postgresdata/* 

# setting this in the root as a volume instance.
VOLUME /var/lib/postgresql/12/main

# exposing the ports to conect
EXPOSE 5432
EXPOSE 8080

# start the server and restarting the database. 
ENTRYPOINT ["sh", "/app/scripts/start.sh"]