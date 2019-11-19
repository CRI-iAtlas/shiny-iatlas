# PostgreSQL

- [Back to main README.md](./README.md)

The easiest way to get PostgreSQL up and running is to use Docker. Since the application will handle seeding data into the database, this should be as simple as:

```bash
# On MacOS/Linux
mkdir -p $HOME/docker/volumes/postgres
docker pull postgres:11.4
docker run --rm  --name pg-docker -e POSTGRES_PASSWORD=docker -d -p 5432:5432 -v $HOME/docker/volumes/postgres:/var/lib/postgresql/data postgres:11.4
```

```powershell
# On Windows
mkdir $HOME\docker\volumes\postgres
docker pull postgres:11.4
docker run --rm --name pg-docker -e POSTGRES_PASSWORD=docker -d -p 5432:5432 postgres:11.4
```

The password the application uses to access the database has to match what's been configured, which happens in the `.Rprofile.d/ENV=dev.R` file. The database values may alos be set with environment variables. PostgreSQL 11.4 is specifically used because it tracks the newest version that's supported on GCP Cloud SQL.

The `docker` command for MacOS/Linux maps a local directory into the container, which will allow the database to be persisted across container restarts. This is not possible on Windows without significant additional configuration, but should be noted. **Data will not persist across container restarts on Windows**
