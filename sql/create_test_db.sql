-- Terminate activity on DB before dropping it. See https://stackoverflow.com/questions/7073773/drop-postgresql-database-through-command-line
SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='iatlas_shiny_test';
-- Drop the test database if it already exists.
DROP DATABASE IF EXISTS iatlas_shiny_test;

-- Create the test database (used for running tests).
CREATE DATABASE iatlas_shiny_test;

-- Connect to the database.
\connect iatlas_shiny_test

-- Include the common table building SQL.
\i build_tables.sql
