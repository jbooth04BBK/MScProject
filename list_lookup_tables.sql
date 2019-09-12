SELECT 
	ha_concepts.concept_type, 
    ha_concepts.code, 
    ha_concepts.label, 
    ha_concepts.term 
FROM   
	ha_concepts 
WHERE  
	(( ha_concepts.code LIKE "*nutrition*" ) OR ( ha_concepts.concept_type LIKE "*nutrition*" ) ) 
	OR (( ha_concepts.code LIKE "*dysmorphic*" ) OR ( ha_concepts.concept_type LIKE "*dysmorphic*" ) ) 
	OR (( ha_concepts.code LIKE "*neglect*" ) OR ( ha_concepts.concept_type LIKE "*neglect*" ) ) 
	OR (( ha_concepts.code LIKE "*jaundice*" ) OR ( ha_concepts.concept_type LIKE "*jaundice*" ) ) 
	OR (( ha_concepts.code LIKE "*oedema*" ) OR ( ha_concepts.concept_type LIKE "*oedema*" ) ) 
	OR (( ha_concepts.code LIKE "*pallor*" ) OR ( ha_concepts.concept_type LIKE "*pallor*" ) ) 
	OR (( ha_concepts.code LIKE "*blood_at_mouth*" ) OR ( ha_concepts.concept_type LIKE "*blood_at_mouth*" ) ) 
	OR (( ha_concepts.code LIKE "*signs_of_trauma*" ) OR ( ha_concepts.concept_type LIKE "*signs_of_trauma*" ) ) 
	OR (( ha_concepts.code LIKE "*signs_of_treatment*" ) OR ( ha_concepts.concept_type LIKE "*signs_of_treatment*" ) ) 
	OR (( ha_concepts.code LIKE "*cod2_summ*" ) OR ( ha_concepts.concept_type LIKE "*cod2_summ*" ) ) 
	OR (( ha_concepts.code LIKE "*include_in_study*" ) OR ( ha_concepts.concept_type LIKE "*include_in_study*" ) ) 
	OR (( ha_concepts.code LIKE "*macro*" ) OR ( ha_concepts.concept_type LIKE "*macro*" ) ) 
ORDER  BY 
    ha_concepts.concept_type
	, ha_concepts.code; 