FROM ubuntu:focal

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    NOMINATIM_VERSION=v3.5.1

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
    cmake .. && \
    make -j`nproc` && \
    cd .. && \
    # Load initial data
    # We need a VOLUME here
    curl http://www.nominatim.org/data/country_grid.sql.gz > ./data/country_osm_grid.sql.gz && \
    chmod o=rwx ./build 
    
WORKDIR /app

# Apache configure
COPY local.php /src/build/settings/local.php 
# Tem que confirmar se esse local.php é para ficar aqui ou precisar estar no /app
COPY nominatim.conf /etc/apache2/sites-enabled/000-default.conf
COPY start.sh /app/scripts/

RUN curl https://download.geofabrik.de/south-america/brazil/sudeste-latest.osm.pbf > ./data/sudeste-latest.osm.pbf && \
echo "Set disable_coredump false" >> /etc/sudo.conf && \
rm -rf /data/postgresdata && \
mkdir -p /data/postgresdata && \
chown postgres:postgres /data/postgresdata && \
export  PGDATA=/data/postgresdata && \
sudo -u postgres /usr/lib/postgresql/12/bin/initdb -D /data/postgresdata && \
sudo -u postgres /usr/lib/postgresql/12/bin/pg_ctl -D /data/postgresdata start && \
sudo -u postgres psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='nominatim'" | grep -q 1 || sudo -u postgres createuser -s nominatim && \
sudo -u postgres psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='www-data'" | grep -q 1 || sudo -u postgres createuser -SDR www-data && \
sudo -u postgres psql postgres -c "DROP DATABASE IF EXISTS nominatim" && \
useradd -m -p password1234 nominatim && \
chown -R nominatim:nominatim /src && \
sudo -u nominatim /src/build/utils/setup.php --osm-file /src/data/sudeste-latest.osm.pbf  --all --threads 4 && \
sudo -u nominatim /src/build/utils/check_import_finished.php && \
sudo -u postgres /usr/lib/postgresql/12/bin/pg_ctl -D /data/postgresdata stop && \
sudo chown -R postgres:postgres /data/postgresdata && \
chmod 700 /data/postgresdata && \
rsync -av /data/postgresdata /var/lib/postgresql/12/main 

EXPOSE 5432
EXPOSE 8080

ENTRYPOINT ["sh", "/app/scripts/start.sh"]