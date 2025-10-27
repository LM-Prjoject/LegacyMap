package com.legacymap.backend.ml.relationship;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MlRelationshipConfig {

    @Bean
    public RelationshipModel relationshipModel() {
        // For now, return Smile-based heuristic stub. Replace with trained model loader later.
        return new SmileRelationshipModel();
    }
}
