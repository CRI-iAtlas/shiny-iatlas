-- Terminate activity on DB before dropping it. See https://stackoverflow.com/questions/7073773/drop-postgresql-database-through-command-line
SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='iatlas_dev';
-- Drop the dev database if it already exists.
DROP DATABASE IF EXISTS iatlas_dev;

-- Create the dev database (used for development).
CREATE DATABASE iatlas_dev;

-- Connect to the database.
\connect iatlas_dev

-- Include the common table building SQL.
\i build_tables.sql
