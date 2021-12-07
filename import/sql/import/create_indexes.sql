CREATE INDEX IF NOT EXISTS idx_intake_q3_country ON epidb_results_intake USING btree (country, "Q3");

CREATE INDEX IF NOT EXISTS epidb_results_intake_timestamp ON public.epidb_results_intake USING brin("timestamp");
CREATE INDEX IF NOT EXISTS idx_intake_country ON epidb_results_intake USING btree (country);
CREATE INDEX IF NOT EXISTS idx_intake_global_id ON epidb_results_intake USING btree (global_id);

CREATE INDEX IF NOT EXISTS  epidb_results_weekly_timestamp ON public.epidb_results_weekly USING brin("timestamp");
CREATE INDEX IF NOT EXISTS idx_weekly_country ON epidb_results_weekly USING btree (country);
CREATE INDEX IF NOT EXISTS idx_weekly_global_id ON epidb_results_weekly USING btree (global_id);
  
CREATE INDEX IF NOT EXISTS epidb_results_vaccination_timestamp ON public.epidb_results_vaccination USING brin("timestamp");
CREATE INDEX IF NOT EXISTS idx_vaccination_country ON epidb_results_vaccination USING btree (country);
CREATE INDEX IF NOT EXISTS idx_vaccination_global_id ON epidb_results_vaccination USING btree (global_id);
