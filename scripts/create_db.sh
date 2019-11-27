#!/bin/sh

# env can be dev or test. If no argument is passed, it will default to dev.
env=${1:-dev}

YELLOW="\033[1;33m"
GREEN="\033[0;32m"
# No Color
NC='\033[0m'

# The local project directory (assumes this file is stll in a child folder of the project).
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && cd ../ && pwd )"
>&2 echo -e "${GREEN}Current dir - ${DIR}${NC}"

docker_image="pg-docker"

create_db_sql="create_${env}_db.sql"
db_data_dir="$DIR/docker/volumes/postgres"
db_port=5432
db_user="postgres"
db_pw="docker"

# If $db_data_dir doesn't exist create it.
if [ ! -d "$db_data_dir" ]; then
    >&2 echo -e "${GREEN}Creating '$db_data_dir' for data.${NC}"
    mkdir -p $db_data_dir
fi

# Ensure the docker image has been downloaded.
docker pull postgres:11.5

# Ensure the docker container isn't already in the docker processes.
if [ ! "$(docker ps -q -f name=$docker_image)" ]; then
    if [ "$(docker ps -aq -f status=exited -f name=$docker_image)" ]; then
        # Cleanup
        docker rm $docker_image
    fi
    # Run the container
    docker run --rm --name $docker_image -e POSTGRES_PASSWORD=$db_pw -d -p $db_port:$db_port -v /$db_data_dir:/var/lib/postgresql postgres:11.5
fi

>&2 echo -e "${YELLOW}Postgres is starting - please be patient${NC}"
until docker exec $docker_image psql -U $db_user  2> /dev/null; do
    sleep 1
done

>&2 echo -e "${GREEN}Postgres is up - building database and tables${NC}"

# Copy the SQL file into the docker container.
docker cp $DIR/sql/$create_db_sql $docker_image:/$create_db_sql

# Run the SQL script within the docker container using the docker container's psql.
docker exec -u $db_user $docker_image psql -f //$create_db_sql
