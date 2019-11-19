
DROP TABLE IF EXISTS geo_levels;

CREATE TABLE geo_levels
(
   country character(2) NOT NULL, 
   code_nuts1 character(3) NOT NULL, 
   code_nuts2 character(4) NOT NULL,
   code_nuts3 character(5) NOT NULL
);

DROP TABLE IF EXISTS geo_zip;

CREATE TABLE geo_zip
(
  code_zip text NOT NULL, -- zip code
  country character(2) NOT NULL,
  code_nuts1 character(3), -- Code level NUTS1
  code_nuts2 character(4), -- code level NUTS2
  code_nuts3 character(5), -- code level NUTS3
  CONSTRAINT pk_geo_zip PRIMARY KEY (code_zip, country)
);

COMMENT ON COLUMN geo_zip.code_zip IS 'zip code';
COMMENT ON COLUMN geo_zip.code_nuts1 IS 'Code level NUTS1';
COMMENT ON COLUMN geo_zip.code_nuts2 IS 'code level NUTS2';
COMMENT ON COLUMN geo_zip.code_nuts3 IS 'code level NUTS3';

DROP TABLE IF EXISTS geo_country;
CREATE TABLE geo_country (
   country character(2) NOT NULL, 
   title text NOT NULL,
   CONSTRAINT pk_geo_country PRIMARY KEY (country)
);

DROP TABLE IF EXISTS geo_nuts1;

CREATE TABLE geo_nuts1
(
   country character(2) NOT NULL, 
   code_nuts1 character(3) NOT NULL, 
   title text NOT NULL,
   CONSTRAINT pk_geo_nuts1 PRIMARY KEY (code_nuts1)
);

DROP TABLE IF EXISTS geo_nuts2;
CREATE TABLE geo_nuts2
(
   country character(2) NOT NULL, 
   code_nuts1 character(3) NOT NULL, 
   code_nuts2 character(4) NOT NULL,
   title text NOT NULL,
   CONSTRAINT pk_geo_nuts2 PRIMARY KEY (code_nuts2)
);

DROP TABLE IF EXISTS geo_nuts3;
CREATE TABLE geo_nuts3
(
   country character(2) NOT NULL, 
   code_nuts1 character(3) NOT NULL, 
   code_nuts2 character(4) NOT NULL,
   code_nuts3 character(5) NOT NULL, 
   title text NOT NULL,
   CONSTRAINT pk_geo_nuts3 PRIMARY KEY (code_nuts3)
);

-- Population tables

DROP TABLE IF EXISTS pop_nuts1;

CREATE TABLE pop_nuts1 (
   code_nuts1 character(3) NOT NULL, 
   "country" character(2) NOT NULL,
   "year"	integer  NOT NULL,
   "all" integer NOT NULL,
   "male" integer NOT NULL,
   "female" integer NOT NULL,
   "year_ref" integer NOT NULL,
   CONSTRAINT pk_pop_nuts1 PRIMARY KEY ("year", code_nuts1)
);

DROP TABLE IF EXISTS pop_age5_nuts1;
CREATE TABLE pop_age5_nuts1 (
   code_nuts1 character(3) NOT NULL, 
   age_min integer NOT NULL,
   age_max integer,
   "country" character(2) NOT NULL,
   "year"	integer  NOT NULL,
   "all" integer NOT NULL,
   "male" integer NOT NULL,
   "female" integer NOT NULL,
   "year_ref" integer NOT NULL,
   CONSTRAINT pk_pop_age5_nuts1 PRIMARY KEY ("year", code_nuts1,"age_min")
);

DROP TABLE IF EXISTS pop_nuts2;
CREATE TABLE pop_nuts2 (
   code_nuts2 character(4) NOT NULL, 
   "country" character(2) NOT NULL,
   "year"	integer  NOT NULL,
   "all" integer NOT NULL,
   "male" integer NOT NULL,
   "female" integer NOT NULL,
   "year_ref" integer NOT NULL,
   CONSTRAINT pk_pop_nuts2 PRIMARY KEY ("year", code_nuts2)
);

DROP TABLE IF EXISTS pop_age5_nuts2;
CREATE TABLE pop_age5_nuts2 (
   code_nuts2 character(4) NOT NULL, 
   age_min integer NOT NULL,
   age_max integer,
   "country" character(2) NOT NULL,
   "year"	integer  NOT NULL,
   "all" integer NOT NULL,
   "male" integer NOT NULL,
   "female" integer NOT NULL,
   "year_ref" integer NOT NULL,
   CONSTRAINT pk_pop_age5_nuts2 PRIMARY KEY ("year", code_nuts2,"age_min")
);

DROP TABLE IF EXISTS pop_country;
CREATE TABLE pop_country (
   "country" character(2) NOT NULL,
   "year"	integer  NOT NULL,
   "all" integer NOT NULL,
   "male" integer NOT NULL,
   "female" integer NOT NULL,
   "year_ref" integer NOT NULL,
   CONSTRAINT pk_pop_country PRIMARY KEY ("year", "country")
);

DROP TABLE IF EXISTS pop_age5_country;
CREATE TABLE pop_age5_country (
   "country" character(2) NOT NULL,
   age_min integer NOT NULL,
   age_max integer,
   "year"	integer  NOT NULL,
   "all" integer NOT NULL,
   "male" integer NOT NULL,
   "female" integer NOT NULL,
   "year_ref" integer NOT NULL,
   CONSTRAINT pk_pop_age5_country PRIMARY KEY ("year", "country","age_min")
);

