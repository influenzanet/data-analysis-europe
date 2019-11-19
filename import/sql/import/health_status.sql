DROP VIEW IF EXISTS epidb_health_status_2011;
CREATE  VIEW epidb_health_status_2011 AS
    SELECT epidb_results_weekly.id AS weekly_id, CASE true
    WHEN epidb_results_weekly."Q1_0" THEN 'NO-SYMPTOMS'::text WHEN
    (((epidb_results_weekly."Q5" = 0) AND (((epidb_results_weekly."Q1_1"
    OR epidb_results_weekly."Q1_11") OR epidb_results_weekly."Q1_8") OR
    epidb_results_weekly."Q1_9")) AND ((epidb_results_weekly."Q1_5" OR
    epidb_results_weekly."Q1_6") OR epidb_results_weekly."Q1_7")) THEN
    'ILI'::text WHEN ((epidb_results_weekly."Q5" = 1) AND
    (((epidb_results_weekly."Q1_4" OR epidb_results_weekly."Q1_5") OR
    epidb_results_weekly."Q1_6") OR epidb_results_weekly."Q1_7")) THEN
    'COMMON-COLD'::text WHEN ((epidb_results_weekly."Q1_15" OR
    epidb_results_weekly."Q1_16") OR (epidb_results_weekly."Q1_17" AND
    epidb_results_weekly."Q1_18")) THEN 'GASTROINTESTINAL'::text ELSE
    'NON-INFLUENZA'::text END AS status FROM epidb_results_weekly;

DROP VIEW IF EXISTS epidb_health_status_2012;
CREATE VIEW epidb_health_status_2012 AS
               SELECT id as weekly_id,
                      case true
                          when "Q1_0"
                              then 'NO-SYMPTOMS'

                          when ("Q5" = 0 or "Q6b" = 0)
                           and ("Q1_1" or "Q1_2"  or "Q6d" = 3 or "Q6d" = 4 or "Q6d" = 5 or "Q1_11" or "Q1_8" or "Q1_9")
                           and ("Q1_5" or "Q1_6" or "Q1_7")
                              then 'ILI'

                          when 
                            (
                                (not "Q1_1") and (not "Q1_2") 
                                and (("Q6d" = 0) or ("Q6d" is null)) 
                                and ("Q1_3" or "Q1_4" or "Q1_14")
                                and ("Q11" = 2)
                            ) and (
                                case true when "Q1_17" then 1 else 0 end + 
                                case true when "Q1_15" then 1 else 0 end + 
                                case true when "Q1_16" then 1 else 0 end + 
                                case true when "Q1_18" then 1 else 0 end >= 2
                            ) then 'ALLERGY-or-HAY-FEVER-and-GASTROINTESTINAL'

                          when (not "Q1_1") and (not "Q1_2") 
                           and (("Q6d" = 0) or ("Q6d" is null)) 
                           and ("Q1_3" or "Q1_4" or "Q1_14")
                           and ("Q11" = 2)
                              then 'ALLERGY-or-HAY-FEVER' 

                          when
                            (
                                case true when "Q1_3" then 1 else 0 end + 
                                case true when "Q1_4" then 1 else 0 end + 
                                case true when "Q1_6" then 1 else 0 end + 
                                case true when "Q1_5" then 1 else 0 end >= 2
                                  -- note: common cold after all allergy-related branches
                            ) and (
                                case true when "Q1_17" then 1 else 0 end + 
                                case true when "Q1_15" then 1 else 0 end + 
                                case true when "Q1_16" then 1 else 0 end + 
                                case true when "Q1_18" then 1 else 0 end >= 2
                            ) then 'COMMON-COLD-and-GASTROINTESTINAL'

                          when 
                            case true when "Q1_3" then 1 else 0 end + 
                            case true when "Q1_4" then 1 else 0 end + 
                            case true when "Q1_6" then 1 else 0 end + 
                            case true when "Q1_5" then 1 else 0 end >= 2
                              -- note: common cold after all allergy-related branches
                              then 'COMMON-COLD'

                          when 
                            case true when "Q1_17" then 1 else 0 end + 
                            case true when "Q1_15" then 1 else 0 end + 
                            case true when "Q1_16" then 1 else 0 end + 
                            case true when "Q1_18" then 1 else 0 end >= 2
                              then 'GASTROINTESTINAL'

                          else 'NON-SPECIFIC-SYMPTOMS'
                      end as status
                 FROM epidb_results_weekly;

    
