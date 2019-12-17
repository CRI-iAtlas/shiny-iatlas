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
CREATE TABLE samples (
    id SERIAL,
    sample_id VARCHAR NOT NULL,
    tissue_id VARCHAR,
    gender VARCHAR,
    race VARCHAR,
    ethnicity VARCHAR,
    PRIMARY KEY (id)
);
CREATE UNIQUE INDEX sample_id_index ON samples (sample_id);

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

-- tags table
CREATE TABLE tags (
    id SERIAL,
    "name" VARCHAR NOT NULL,
    characteristics VARCHAR,
    display VARCHAR,
    color VARCHAR,
    PRIMARY KEY (id)
);
CREATE UNIQUE INDEX tag_name_index ON tags ("name");

-- tags_to_tags table
CREATE TABLE tags_to_tags (tag_id INTEGER REFERENCES tags NOT NULL, related_tag_id INTEGER REFERENCES tags NOT NULL);

-- features table
CREATE TABLE features (
    id SERIAL,
    "name" VARCHAR NOT NULL,
    display VARCHAR,
    "order" INTEGER,
    unit UNIT_ENUM,
    PRIMARY KEY (id)
);
ALTER TABLE features ADD COLUMN class_id INTEGER REFERENCES classes;
ALTER TABLE features ADD COLUMN method_tag_id INTEGER REFERENCES method_tags;
CREATE UNIQUE INDEX feature_name_index ON features ("name");

-- genes table
CREATE TABLE genes (
    id SERIAL,
    entrez INTEGER,
    hgnc VARCHAR NOT NULL,
    "canonical" VARCHAR,
    "display" VARCHAR,
    "description" VARCHAR,
    "references" TEXT[],
    PRIMARY KEY (id)
);
ALTER TABLE genes ADD COLUMN family_id INTEGER REFERENCES gene_families;
ALTER TABLE genes ADD COLUMN super_cat_id INTEGER REFERENCES super_categories;
ALTER TABLE genes ADD COLUMN immune_checkpoint_id INTEGER REFERENCES immune_checkpoints;
ALTER TABLE genes ADD COLUMN function_id INTEGER REFERENCES gene_functions;
ALTER TABLE genes ADD COLUMN pathway_id INTEGER REFERENCES pathways;
ALTER TABLE genes ADD COLUMN therapy_type_id INTEGER REFERENCES therapy_types;
CREATE UNIQUE INDEX hgnc_index ON genes ("hgnc");

-- results table
CREATE TABLE results (
    id SERIAL,
    p_value NUMERIC,
    fold_change NUMERIC,
    log10_p_value NUMERIC,
    log10_fold_change NUMERIC,
    correlation SMALLINT,
    n_wt INTEGER,
    n_mut INTEGER,
    PRIMARY KEY (id)
);
ALTER TABLE results ADD COLUMN label_id INTEGER REFERENCES result_labels;
ALTER TABLE results ADD COLUMN tag_id INTEGER REFERENCES tags;

-- genes_to_types table
CREATE TABLE genes_to_types (gene_id INTEGER REFERENCES genes, "type_id" INTEGER REFERENCES gene_types);

-- genes_to_samples table
CREATE TABLE genes_to_samples (
    gene_id INTEGER REFERENCES genes NOT NULL,
    sample_id INTEGER REFERENCES samples NOT NULL,
    "status" STATUS_ENUM,
    "rna_seq_expr" NUMERIC
);

-- samples_to_tags table
CREATE TABLE samples_to_tags (sample_id INTEGER REFERENCES samples, tag_id INTEGER REFERENCES tags);

-- features_to_samples table
CREATE TABLE features_to_samples (
    feature_id INTEGER REFERENCES features,
    sample_id INTEGER REFERENCES samples,
    "value" NUMERIC,
    "inf_value" REAL
);

-- features_to_results table
CREATE TABLE features_to_results (feature_id INTEGER REFERENCES features, result_id INTEGER REFERENCES results);
