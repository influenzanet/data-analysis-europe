

-- INSERT INTO survey_surveyuser (global_id, country) select distinct global_id, country from epidb_results_intake ON CONFLICT DO NOTHING;
-- INSERT INTO survey_surveyuser (global_id, country) select distinct global_id, country from epidb_results_weekly ON CONFLICT DO NOTHING;

INSERT INTO survey_surveyuser (global_id, country) 
  SELECT DISTINCT global_id, country FROM (
      select global_id, country from epidb_results_intake 
      union select global_id,country from epidb_results_weekly
  ) t 
 ON CONFLICT DO NOTHING;

-- Not in use ?
ALTER TABLE epidb_results_weekly ADD COLUMN user_id int;
ALTER TABLE epidb_results_intake ADD COLUMN user_id int;
ALTER TABLE epidb_results_vaccination ADD COLUMN user_id int;