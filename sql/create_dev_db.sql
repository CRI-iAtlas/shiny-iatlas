-- Drop the dev database if it already exists.
SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='iatlas_shiny_dev';
DROP DATABASE IF EXISTS iatlas_shiny_dev;

-- Create the dev database (used for development).
CREATE DATABASE iatlas_shiny_dev;

\connect iatlas_shiny_dev

SELECT pg_sleep(5);

CREATE TABLE samples (id serial, sample_id varchar NOT NULL, tissue_id varchar, PRIMARY KEY (id));

CREATE TABLE entrez (id serial, value integer NOT NULL, PRIMARY KEY (id));

CREATE TABLE immune_checkpoints (id serial, name varchar NOT NULL, PRIMARY KEY (id));

CREATE TABLE gene_functions (id serial, name varchar NOT NULL, PRIMARY KEY (id));

CREATE TABLE pathways (id serial, name varchar NOT NULL, PRIMARY KEY (id));

CREATE TABLE therapy_types (id serial, name varchar NOT NULL, PRIMARY KEY (id));

CREATE TABLE gene_families (id serial, name varchar NOT NULL, PRIMARY KEY (id));

CREATE TABLE super_categories (id serial, name varchar NOT NULL, PRIMARY KEY (id));

CREATE TABLE gene_types (id serial, name varchar NOT NULL, display varchar, PRIMARY KEY (id));

CREATE TABLE classes (id serial, name varchar NOT NULL, PRIMARY KEY (id));

CREATE TABLE result_labels (id serial, name varchar NOT NULL, PRIMARY KEY (id));

CREATE TABLE groups (id serial, name varchar NOT NULL, characteristics varchar, display varchar, color varchar, PRIMARY KEY (id));
ALTER TABLE groups ADD COLUMN subgroup integer REFERENCES groups;
ALTER TABLE groups ADD COLUMN parent integer REFERENCES groups;
