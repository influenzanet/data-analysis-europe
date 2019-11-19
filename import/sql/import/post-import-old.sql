
ALTER TABLE epidb_results_intake ADD COLUMN dump integer;

UPDATE epidb_results_intake SET dump=2013;

ALTER TABLE epidb_results_weekly ADD COLUMN dump integer;

UPDATE epidb_results_weekly SET dump=2013;

