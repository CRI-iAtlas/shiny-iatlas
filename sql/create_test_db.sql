-- Terminate activity on DB before dropping it. See https://stackoverflow.com/questions/7073773/drop-postgresql-database-through-command-line
SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='iatlas_shiny_test';
-- Drop the test database if it already exists.
DROP DATABASE IF EXISTS iatlas_shiny_test;

-- Create the test database (used for running tests).
CREATE DATABASE iatlas_shiny_test;

-- Connect to the database.
\connect iatlas_shiny_test

-- Create STATUS_ENUM ENUM
DO $$ BEGIN
    CREATE TYPE STATUS_ENUM AS ENUM ('Wt', 'Mut');
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

-- Create UNIT_ENUM ENUM
DO $$ BEGIN
    CREATE TYPE UNIT_ENUM AS ENUM ('Count', 'Fraction', 'Per Megabase', 'Score', 'Year' );
EXCEPTION
    WHEN duplicate_object THEN NULL;
END $$;

-- samples table
CREATE TABLE samples (id SERIAL, sample_id VARCHAR NOT NULL, tissue_id VARCHAR, PRIMARY KEY (id));
CREATE UNIQUE INDEX sample_id_index ON samples (sample_id);

-- entrez table
CREATE TABLE entrez (id SERIAL, "value" INTEGER NOT NULL, PRIMARY KEY (id));

-- immune_checkpoints table
CREATE TABLE immune_checkpoints (id SERIAL, "name" VARCHAR NOT NULL, PRIMARY KEY (id));

-- gene_functions table
CREATE TABLE gene_functions (id SERIAL, "name" VARCHAR NOT NULL, PRIMARY KEY (id));

-- pathways table
CREATE TABLE pathways (id SERIAL, "name" VARCHAR NOT NULL, PRIMARY KEY (id));

-- therapy_types table
CREATE TABLE therapy_types (id SERIAL, "name" VARCHAR NOT NULL, PRIMARY KEY (id));

-- gene_families table
CREATE TABLE gene_families (id SERIAL, "name" VARCHAR NOT NULL, PRIMARY KEY (id));

-- super_categories table
CREATE TABLE super_categories (id SERIAL, "name" VARCHAR NOT NULL, PRIMARY KEY (id));

-- gene_types table
CREATE TABLE gene_types (id SERIAL, "name" VARCHAR NOT NULL, display VARCHAR, PRIMARY KEY (id));

-- classes table
CREATE TABLE classes (id SERIAL, "name" VARCHAR NOT NULL, PRIMARY KEY (id));

-- method_tags table
CREATE TABLE method_tags (id SERIAL, "name" VARCHAR NOT NULL, PRIMARY KEY (id));

-- result_labels table
CREATE TABLE result_labels (id SERIAL, "name" VARCHAR NOT NULL, PRIMARY KEY (id));
CREATE UNIQUE INDEX name_index ON result_labels ("name");

-- groups table
CREATE TABLE groups (
    id SERIAL,
    "name" VARCHAR NOT NULL,
    characteristics VARCHAR,
    display VARCHAR,
    color VARCHAR,
    PRIMARY KEY (id)
);
ALTER TABLE groups ADD COLUMN subgroup INTEGER REFERENCES groups;
ALTER TABLE groups ADD COLUMN parent INTEGER REFERENCES groups;
CREATE UNIQUE INDEX group_name_index ON groups ("name");

-- features table
CREATE TABLE features (
    id SERIAL,
    "name" VARCHAR NOT NULL,
    display VARCHAR,
    "order" INTEGER,
    unit UNIT_ENUM,
    "value" NUMERIC,
    PRIMARY KEY (id)
);
CREATE UNIQUE INDEX feature_name_index ON features ("name");
ALTER TABLE features ADD COLUMN class INTEGER REFERENCES classes;
ALTER TABLE features ADD COLUMN method_tag_id INTEGER REFERENCES method_tags;

-- genes table
CREATE TABLE genes (
    id SERIAL,
    gene_id VARCHAR NOT NULL,
    "description" VARCHAR,
    "references" TEXT[],
    PRIMARY KEY (id)
);
ALTER TABLE genes ADD COLUMN family_id INTEGER REFERENCES result_labels;
ALTER TABLE genes ADD COLUMN super_cat_id INTEGER REFERENCES groups;
ALTER TABLE genes ADD COLUMN entrez_id INTEGER REFERENCES entrez;
ALTER TABLE genes ADD COLUMN immune_checkpoint_id INTEGER REFERENCES immune_checkpoints;
ALTER TABLE genes ADD COLUMN function_id INTEGER REFERENCES gene_functions;
ALTER TABLE genes ADD COLUMN pathway_id INTEGER REFERENCES pathways;
ALTER TABLE genes ADD COLUMN therapy_type_id INTEGER REFERENCES therapy_types;

-- results table
CREATE TABLE results (
    id SERIAL,
    p_value NUMERIC,
    fold_change NUMERIC,
    log10_p_value NUMERIC,
    log10_fold_change NUMERIC,
    correlation SMALLINT,
    n integer,
    PRIMARY KEY (id)
);
ALTER TABLE results ADD COLUMN label_id INTEGER REFERENCES result_labels;
ALTER TABLE results ADD COLUMN group_id INTEGER REFERENCES groups;
ALTER TABLE results ADD COLUMN gene_id INTEGER REFERENCES genes;

-- genes_to_types table
CREATE TABLE genes_to_types (gene_id INTEGER REFERENCES genes, "type_id" INTEGER REFERENCES gene_types);

-- genes_to_samples table
CREATE TABLE genes_to_samples (
    gene_id INTEGER REFERENCES genes NOT NULL,
    sample_id INTEGER REFERENCES samples NOT NULL,
    "status" STATUS_ENUM,
    "value" NUMERIC
);

-- samples_to_groups table
CREATE TABLE samples_to_groups (sample_id INTEGER REFERENCES samples, group_id INTEGER REFERENCES groups);

-- features_to_samples table
CREATE TABLE features_to_samples (feature_id INTEGER REFERENCES features, sample_id INTEGER REFERENCES samples);

-- features_to_results table
CREATE TABLE features_to_results (feature_id INTEGER REFERENCES features, result_id INTEGER REFERENCES results);
