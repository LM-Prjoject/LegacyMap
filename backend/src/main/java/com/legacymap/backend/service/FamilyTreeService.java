package com.legacymap.backend.service;

import com.legacymap.backend.dto.request.FamilyTreeCreateRequest;
import com.legacymap.backend.dto.request.FamilyTreeUpdateRequest;
import com.legacymap.backend.dto.request.PersonCreateRequest;
import com.legacymap.backend.entity.FamilyTree;
import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.FamilyTreeRepository;
import com.legacymap.backend.repository.PersonRepository;
import com.legacymap.backend.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Service
public class FamilyTreeService {

    @Autowired
    private FamilyTreeRepository familyTreeRepository;

    @Autowired
    private PersonRepository personRepository;

    @Autowired
    private UserRepository userRepository;

    private User loadUserOrThrow(UUID userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
    }

    @Transactional
    public FamilyTree create(UUID userId, FamilyTreeCreateRequest req) {
        User creator = loadUserOrThrow(userId);
        FamilyTree tree = FamilyTree.builder()
                .name(req.getName())
                .description(req.getDescription())
                .isPublic(req.getIsPublic() != null ? req.getIsPublic() : false)
                .coverImageUrl(req.getCoverImageUrl())
                .createdBy(creator)
                .build();
        return familyTreeRepository.save(tree);
    }

    private FamilyTree findOwnedTreeOrThrow(UUID treeId, UUID userId) {
        User user = loadUserOrThrow(userId);
        return familyTreeRepository.findByIdAndCreatedBy(treeId, user)
                .orElseThrow(() -> new AppException(ErrorCode.FAMILY_TREE_NOT_FOUND));
    }

    @Transactional
    public FamilyTree update(UUID treeId, UUID userId, FamilyTreeUpdateRequest req) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        if (req.getName() != null) tree.setName(req.getName());
        if (req.getDescription() != null) tree.setDescription(req.getDescription());
        if (req.getIsPublic() != null) tree.setIsPublic(req.getIsPublic());
        if (req.getCoverImageUrl() != null) tree.setCoverImageUrl(req.getCoverImageUrl());
        return familyTreeRepository.save(tree);
    }

    @Transactional
    public void delete(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        familyTreeRepository.delete(tree);
    }

    @Transactional
    public Person addMember(UUID treeId, UUID userId, PersonCreateRequest req) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        User creator = loadUserOrThrow(userId);
        Person p = Person.builder()
                .familyTree(tree)
                .fullName(req.getFullName())
                .gender(req.getGender())
                .birthDate(req.getBirthDate())
                .deathDate(req.getDeathDate())
                .birthPlace(req.getBirthPlace())
                .deathPlace(req.getDeathPlace())
                .biography(req.getBiography())
                .avatarUrl(req.getAvatarUrl())
                .createdBy(creator)
                .build();
        return personRepository.save(p);
    }

    @Transactional(readOnly = true)
    public List<FamilyTree> listByUser(UUID userId) {
        User user = loadUserOrThrow(userId);
        return familyTreeRepository.findAllByCreatedBy(user);
    }

    @Transactional(readOnly = true)
    public List<Person> listMembers(UUID treeId, UUID userId) {
        FamilyTree tree = findOwnedTreeOrThrow(treeId, userId);
        return personRepository.findAllByFamilyTree_Id(tree.getId());
    }
}
