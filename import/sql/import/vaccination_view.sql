-- Caution this file is formated to preserve vertical alignement between columns of both queries, dont reformat it
DROP VIEW IF EXISTS epidb_vaccination; 
CREATE VIEW epidb_vaccination as
  SELECT  
     True "intake", 
     "country", "global_id", "timestamp", "Q9", 
     "Q10", "Q10b", "Q10b_1_open", "Q10c_0", "Q10c_1", "Q10c_2", "Q10c_3", "Q10c_4", "Q10c_5", "Q10c_6", "Q10c_7", "Q10c_8", "Q10c_9", NULL as "Q10c_12", "Q10d_0", "Q10d_1", "Q10d_2", "Q10d_3", "Q10d_4", "Q10d_5", "Q10d_6", "Q10d_7", "Q10d_8", "Q10d_9", "Q10d_10", "Q10d_11", "Q10d_12", "Q10d_13", "Q10d_14", 
     "Q35" , "Q35b" , "Q35c" , "Q35d" , "Q35d_1_open", "Q35e" , "Q35e_1_open", "Q35f_0", "Q35f_1", "Q35f_2", "Q35f_20", "Q35f_21", "Q35f_3", "Q35f_4", "Q35f_5", "Q35f_6", "Q35f_7", "Q35f_8", "Q35f_9", "Q35f_9_open" "Q35g", 
     -- New 2021 questions, only available in  
     NULL "Q35i_1", NULL "Q35i_2", NULL  "Q35i_3", NULL  "Q35i_4", NULL  "Q35i_99",  NULL "Q35j",  NULL "Q35j_1_open",  NULL "Q35k",  NULL "Q35l",  NULL "Q35l_6_open",  NULL "Q35m_0",  NULL "Q35m_1",  NULL "Q35m_2",  NULL "Q35m_3",  NULL "Q35m_4",  NULL "Q35m_5",  NULL "Q35m_6", NULL "Q35m_7",  NULL "Q35m_8",  NULL "Q35m_9",  NULL "Q35m_10",  NULL "Q35m_14",  NULL "Q35m_14_open",  NULL "Q35m_12",  NULL "Q35m_15",  NULL "Q35m_16",  NULL "Q35m_17",  NULL "Q35m_20"
  FROM epidb_results_intake where not(
      "country" = 'FR' and timestamp > '2021-11-01'
  )
  UNION
  SELECT 
    False "intake",
    "country", "global_id", "timestamp", "Q9", 
    "Q10", "Q10b", "Q10b_1_open", "Q10c_0", "Q10c_1", "Q10c_2", "Q10c_3", "Q10c_4", "Q10c_5", "Q10c_6", "Q10c_7", "Q10c_8", "Q10c_9", NULL as "Q10c_12", "Q10d_0", "Q10d_1", "Q10d_2", "Q10d_3", "Q10d_4", "Q10d_5", "Q10d_6", "Q10d_7", "Q10d_8", "Q10d_9", "Q10d_10", "Q10d_11", "Q10d_12", "Q10d_13", "Q10d_14", 
    "Q35" , "Q35b" , "Q35c" , "Q35d" , "Q35d_1_open", "Q35e" , "Q35e_1_open", "Q35f_0", "Q35f_1", "Q35f_2", "Q35f_20", "Q35f_21", "Q35f_3", "Q35f_4", "Q35f_5", "Q35f_6", "Q35f_7", "Q35f_8", "Q35f_9", "Q35f_9_open"  "Q35g",  
          "Q35i_1",     "Q35i_2",        "Q35i_3",        "Q35i_4",      "Q35i_99",       "Q35j",       "Q35j_1_open",        "Q35k",       "Q35l",       "Q35l_6_open",      "Q35m_0",       "Q35m_1",       "Q35m_2",       "Q35m_3",       "Q35m_4",       "Q35m_5",       "Q35m_6",      "Q35m_7",       "Q35m_8",       "Q35m_9",       "Q35m_10",       "Q35m_14",        "Q35m_14_open",        "Q35m_12",        "Q35m_15",     "Q35m_16",       "Q35m_17",       "Q35m_20"
    FROM epidb_results_vaccination
;

