DROP TABLE IF EXISTS survey_surveyuser CASCADE;

CREATE TABLE survey_surveyuser as SELECT DISTINCT global_id FROM (select global_id from epidb_results_intake union select global_id from epidb_results_weekly) t;

DROP SEQUENCE IF EXISTS survey_surveyuser_id_seq;
CREATE SEQUENCE survey_surveyuser_id_seq START WITH 1 INCREMENT BY 1 NO MAXVALUE NO MINVALUE CACHE 1;
ALTER TABLE survey_surveyuser ADD COLUMN id integer;
UPDATE survey_surveyuser SET id = nextval('survey_surveyuser_id_seq');
ALTER TABLE survey_surveyuser ALTER COLUMN id SET DEFAULT nextval('survey_surveyuser_id_seq');
ALTER TABLE survey_surveyuser ALTER COLUMN id SET NOT NULL;
ALTER TABLE survey_surveyuser ADD UNIQUE(id);
ALTER TABLE survey_surveyuser ADD PRIMARY KEY (id);

ALTER TABLE epidb_results_weekly ADD COLUMN user_id int;
ALTER TABLE epidb_results_intake ADD COLUMN user_id int;
