#!/bin/sh

# env can be dev or test. If no argument is passed, it will default to dev.
env=${1:-dev}

docker_exec=`which docker`
docker_image="pg-docker"

create_db_sql="create_${env}_db.sql"
db_data_dir="$HOME/docker/volumes/postgres"
db_port=5432
db_user="postgres"
db_pw="docker"

# If $db_data_dir doesn't exist create it.
if [ ! -d "$db_data_dir" ]; then
    mkdir -p $db_data_dir
fi
$docker_exec pull postgres:11.5
if [ ! "$($docker_exec ps -q -f name=$docker_image)" ]; then
    if [ "$($docker_exec ps -aq -f status=exited -f name=$docker_image)" ]; then
        # Cleanup
        $docker_exec rm $docker_image
    fi
    # Run the container
    $docker_exec run --rm  --name $docker_image -e POSTGRES_PASSWORD=$db_pw -d -p $db_port:$db_port -v $db_data_dir:/var/lib/postgresql/data postgres:11.5
fi

$docker_exec cp ./sql/$create_db_sql $docker_image:/$create_db_sql
$docker_exec exec -u $db_user $docker_image psql -f /$create_db_sql