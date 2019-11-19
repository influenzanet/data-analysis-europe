UPDATE epidb_results_intake set "country"=upper("country");
UPDATE epidb_results_intake set "Q3"=upper("Q3");

UPDATE epidb_results_intake set "country"='BE' where "country"='BN';
UPDATE epidb_results_weekly set "country"='BE' where "country"='BN';

CREATE INDEX idx_intake_q3_country ON epidb_results_intake USING btree (country, "Q3");

-- Cleanup geographical code
UPDATE epidb_results_intake SET "Q3"=btrim("Q3");

-- Cleanup Portugal codes

-- Extract prefix zip code from full zip code format
UPDATE epidb_results_intake SET "Q3"=LEFT("Q3",4) where "country"='PT' and "Q3" ~ '^\d{4}\-\d{3}'; 

-- Extract prefix zip from zip + town name
UPDATE epidb_results_intake SET "Q3"=LEFT("Q3",4) where "country"='PT' and "Q3" ~ '^\d{4} \w'; 

-- Cleanup Ireland region codes
UPDATE epidb_results_intake SET "Q3"='IE024' WHERE "Q3"='CARLOW-KILKENNY' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE011' WHERE "Q3"='CAVAN-MONAGHAN' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE023' WHERE "Q3"='CLARE' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE025' WHERE "Q3"='CORK EAST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE025' WHERE "Q3"='CORK NORTH-CENTRAL' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE025' WHERE "Q3"='CORK NORTH-WEST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE025' WHERE "Q3"='CORK SOUTH-CENTRAL' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE025' WHERE "Q3"='CORK SOUTH-WEST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE011' WHERE "Q3"='DONEGAL NORTH-EAST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE011' WHERE "Q3"='DONEGAL SOUTH-WEST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN CENTRAL' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN MID-WEST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN NORTH' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN NORTH-CENTRAL' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN NORTH-EAST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN NORTH-WEST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN SOUTH' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN SOUTH-CENTRAL' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN SOUTH-EAST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN SOUTH-WEST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUBLIN WEST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE021' WHERE "Q3"='DUN LAOGHAIRE' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE013' WHERE "Q3"='GALWAY EAST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE013' WHERE "Q3"='GALWAY WEST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE025' WHERE "Q3"='KERRY NORTH-WEST LIMERICK' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE025' WHERE "Q3"='KERRY SOUTH' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE022' WHERE "Q3"='KILDARE NORTH' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE022' WHERE "Q3"='KILDARE SOUTH' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE012' WHERE "Q3"='LAOIS OFFALY' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE012' WHERE "Q3"='LAOIS-OFFALY' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE023' WHERE "Q3"='LIMERICK' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE023' WHERE "Q3"='LIMERICK CITY' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE012' WHERE "Q3"='LONGFORD-WESTMEATH' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE011' WHERE "Q3"='LOUTH' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE013' WHERE "Q3"='MAYO' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE022' WHERE "Q3"='MEATH EAST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE022' WHERE "Q3"='MEATH WEST' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE013' WHERE "Q3"='ROSCOMMON-SOUTH LEITRIM' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE011' WHERE "Q3"='SLIGO-NORTH LEITRIM' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE023' WHERE "Q3"='TIPPERARY NORTH' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE024' WHERE "Q3"='TIPPERARY SOUTH' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE024' WHERE "Q3"='WATERFORD' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE024' WHERE "Q3"='WEXFORD' AND "country"='IE';
UPDATE epidb_results_intake SET "Q3"='IE022' WHERE "Q3"='WICKLOW' AND "country"='IE';
