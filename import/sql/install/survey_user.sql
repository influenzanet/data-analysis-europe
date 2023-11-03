-- mimic survey_surveyuser table mapping global id to sequence id

DROP TABLE IF EXISTS survey_surveyuser CASCADE;
DROP SEQUENCE IF EXISTS survey_surveyuser_id_seq;

CREATE SEQUENCE survey_surveyuser_id_seq START WITH 1 INCREMENT BY 1 NO MAXVALUE NO MINVALUE CACHE 1;

CREATE TABLE survey_surveyuser (
  global_id character varying(36),
  country character(2),
  id integer not null DEFAULT nextval('survey_surveyuser_id_seq'),
  CONSTRAINT survey_surveyuser_pkey PRIMARY KEY(id),
  CONSTRAINT survey_surveyuser_uq UNIQUE(global_id, country)
);

ALTER TABLE survey_surveyuser ALTER COLUMN global_id TYPE character varying(45);
