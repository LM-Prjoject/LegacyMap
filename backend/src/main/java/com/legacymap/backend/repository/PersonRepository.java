package com.legacymap.backend.repository;

import com.legacymap.backend.entity.Person;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface PersonRepository extends JpaRepository<Person, UUID> {
    List<Person> findAllByFamilyTree_Id(UUID familyTreeId);
}
