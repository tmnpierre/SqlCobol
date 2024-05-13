-- Supprimer la table existante si elle existe
DROP TABLE IF EXISTS gender_proportions;

-- Créer une nouvelle table pour stocker les proportions par genre
CREATE TABLE gender_proportions (
    country VARCHAR(50) PRIMARY KEY,
    male_proportion NUMERIC(5, 2),
    female_proportion NUMERIC(5, 2),
    other_proportion NUMERIC(5, 2)
);

-- Calcul des proportions par genre
WITH gender_counts AS (
    SELECT
        country,
        COUNT(*) FILTER (WHERE gender = 'Male') AS male_count,
        COUNT(*) FILTER (WHERE gender = 'Female') AS female_count,
        COUNT(*) FILTER (WHERE gender NOT IN ('Male', 'Female')) AS other_count,
        COUNT(*) AS total_count
    FROM
        databank
    GROUP BY
        country
)
-- Insertion des proportions dans la nouvelle table
INSERT INTO gender_proportions (country, male_proportion, female_proportion, other_proportion)
SELECT
    country,
    ROUND(male_count * 100.0 / NULLIF(total_count, 0), 2),
    ROUND(female_count * 100.0 / NULLIF(total_count, 0), 2),
    ROUND(other_count * 100.0 / NULLIF(total_count, 0), 2)
FROM gender_counts;
