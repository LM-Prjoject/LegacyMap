package com.legacymap.backend.repository;

import com.legacymap.backend.entity.Person;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.UUID;

public interface PersonRepository extends JpaRepository<Person, UUID> {
    List<Person> findAllByFamilyTree_Id(UUID familyTreeId);

    // NEW: Count persons by family tree
    long countByFamilyTree_Id(UUID familyTreeId);

    // NEW: Count all persons across all family trees
    @Query("SELECT COUNT(p) FROM Person p")
    long countAllPersons();
}