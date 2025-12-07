package com.legacymap.backend.service;

import com.legacymap.backend.dto.response.TreeStatsDTO;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class TreeStatsService {

    private final JdbcTemplate jdbcTemplate;

    public TreeStatsDTO calculateTreeStats(UUID treeId) {
        log.info("Calculating tree stats for treeId: {}", treeId);
        
        String sql = """
            WITH RECURSIVE generation_tree AS (
                -- Base case: Tìm tất cả roots (người không có cha mẹ)
                SELECT 
                    p.id,
                    1 as generation_level
                FROM persons p
                WHERE p.family_tree_id = ?
                AND NOT EXISTS (
                    SELECT 1 FROM relationships r
                    WHERE r.family_tree_id = ?
                    AND r.person2_id = p.id
                    AND LOWER(r.relationship_type) = 'parent'
                )
                
                UNION ALL
                
                -- Recursive case: Tìm con của mỗi người
                SELECT 
                    p.id,
                    gt.generation_level + 1
                FROM generation_tree gt
                INNER JOIN relationships r ON r.person1_id = gt.id 
                    AND LOWER(r.relationship_type) = 'parent'
                    AND r.family_tree_id = ?
                INNER JOIN persons p ON r.person2_id = p.id
            )
            SELECT 
                COALESCE(MAX(generation_level), 0) as max_generation,
                COUNT(DISTINCT id) as total_persons_in_tree
            FROM generation_tree
            """;

        try {
            TreeStatsDTO result = jdbcTemplate.queryForObject(sql,
                new Object[]{treeId, treeId, treeId},
                (rs, rowNum) -> {
                    int totalPersonsInTree = rs.getInt("total_persons_in_tree");
                    int maxGeneration = rs.getInt("max_generation");
                    
                    // Nếu không có ai trong tree có relationships, đếm tất cả persons
                    if (totalPersonsInTree == 0) {
                        String countSql = "SELECT COUNT(*) FROM persons WHERE family_tree_id = ?";
                        Integer totalPersons = jdbcTemplate.queryForObject(countSql, 
                            new Object[]{treeId}, Integer.class);
                        return new TreeStatsDTO(totalPersons != null ? totalPersons : 0, 0);
                    }
                    
                    return new TreeStatsDTO(totalPersonsInTree, maxGeneration);
                });

            log.info("Tree stats calculated - Members: {}, Generations: {}", 
                result.getMemberCount(), result.getGenerationCount());
            
            return result;
            
        } catch (Exception e) {
            log.error("Error calculating tree stats for treeId: {}", treeId, e);
            
            // Fallback: đếm tổng số persons
            String fallbackSql = "SELECT COUNT(*) FROM persons WHERE family_tree_id = ?";
            Integer totalPersons = jdbcTemplate.queryForObject(fallbackSql, 
                new Object[]{treeId}, Integer.class);
            
            return new TreeStatsDTO(totalPersons != null ? totalPersons : 0, 0);
        }
    }
}
