SELECT
	ha_concepts.category,
    ha_concepts.code,
    ha_concepts.label
FROM
	ha_concepts
WHERE
	(( ha_concepts.code LIKE "*nutrition*" ) OR ( ha_concepts.category LIKE "*nutrition*" ) )
	OR (( ha_concepts.code LIKE "*dysmorphic*" ) OR ( ha_concepts.category LIKE "*dysmorphic*" ) )
	OR (( ha_concepts.code LIKE "*neglect*" ) OR ( ha_concepts.category LIKE "*neglect*" ) )
	OR (( ha_concepts.code LIKE "*jaundice*" ) OR ( ha_concepts.category LIKE "*jaundice*" ) )
	OR (( ha_concepts.code LIKE "*oedema*" ) OR ( ha_concepts.category LIKE "*oedema*" ) )
	OR (( ha_concepts.code LIKE "*pallor*" ) OR ( ha_concepts.category LIKE "*pallor*" ) )
	OR (( ha_concepts.code LIKE "*blood_at_mouth*" ) OR ( ha_concepts.category LIKE "*blood_at_mouth*" ) )
	OR (( ha_concepts.code LIKE "*signs_of_trauma*" ) OR ( ha_concepts.category LIKE "*signs_of_trauma*" ) )
	OR (( ha_concepts.code LIKE "*signs_of_treatment*" ) OR ( ha_concepts.category LIKE "*signs_of_treatment*" ) )
	OR (( ha_concepts.code LIKE "*cod2_summ*" ) OR ( ha_concepts.category LIKE "*cod2_summ*" ) )
	OR (( ha_concepts.code LIKE "*include_in_study*" ) OR ( ha_concepts.category LIKE "*include_in_study*" ) )
	OR (( ha_concepts.code LIKE "*SyFi*" ) OR ( ha_concepts.category LIKE "*SyFi*" ) )
	OR (( ha_concepts.code LIKE "*SyHi*" ) OR ( ha_concepts.category LIKE "*SyHi*" ) )
ORDER  BY
    ha_concepts.category
	, ha_concepts.code;