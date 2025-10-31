package com.legacymap.backend.ml.relationship;

import com.legacymap.backend.entity.Person;
import com.legacymap.backend.enums.RelationshipType;

import java.util.EnumMap;

import static com.legacymap.backend.ml.relationship.FeatureExtractor.ageDelta;
import static com.legacymap.backend.ml.relationship.FeatureExtractor.sameSurname;

public class SmileRelationshipModel implements RelationshipModel {

    @Override
    public EnumMap<RelationshipType, Double> predict(Person source, Person candidate) {
        EnumMap<RelationshipType, Double> probs = new EnumMap<>(RelationshipType.class);

        int delta = ageDelta(candidate, source); // candidate - source
        int absDelta = delta == Integer.MIN_VALUE ? Integer.MAX_VALUE : Math.abs(delta);
        boolean surnameMatch = sameSurname(source, candidate);

        double pParent = 0.0;
        if (delta != Integer.MIN_VALUE && delta >= 15 && delta <= 60) {
            pParent = 0.6;
            if (surnameMatch) pParent += 0.1;
        }

        double pChild = 0.0;
        if (delta != Integer.MIN_VALUE && delta <= -15 && delta >= -60) {
            pChild = 0.6;
            if (surnameMatch) pChild += 0.1;
        }

        double pSpouse = 0.0;
        if (absDelta <= 25) {
            pSpouse = 0.55;
            if (surnameMatch) pSpouse += 0.05; // sometimes shared surname
        }

        double pSibling = 0.0;
        if (absDelta <= 20 && surnameMatch) {
            pSibling = 0.6;
        }

        probs.put(RelationshipType.PARENT, clamp01(pParent));
        probs.put(RelationshipType.CHILD, clamp01(pChild));
        probs.put(RelationshipType.SPOUSE, clamp01(pSpouse));
        probs.put(RelationshipType.SIBLING, clamp01(pSibling));
        return probs;
    }

    private static double clamp01(double v) {
        if (v < 0) return 0;
        if (v > 1) return 1;
        return v;
    }
}
