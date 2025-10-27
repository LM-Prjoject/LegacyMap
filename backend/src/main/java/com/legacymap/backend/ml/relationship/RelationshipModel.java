package com.legacymap.backend.ml.relationship;

import com.legacymap.backend.entity.Person;
import com.legacymap.backend.enums.RelationshipType;

import java.util.EnumMap;

public interface RelationshipModel {
    EnumMap<RelationshipType, Double> predict(Person source, Person candidate);
}
