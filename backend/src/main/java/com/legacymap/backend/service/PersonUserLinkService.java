package com.legacymap.backend.service;

import com.legacymap.backend.entity.Person;
import com.legacymap.backend.entity.PersonUserLink;
import com.legacymap.backend.entity.User;
import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;
import com.legacymap.backend.repository.PersonRepository;
import com.legacymap.backend.repository.PersonUserLinkRepository;
import com.legacymap.backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PersonUserLinkService {
    
    private final PersonUserLinkRepository personUserLinkRepository;
    private final UserRepository userRepository;
    private final PersonRepository personRepository;
    private final ChatSyncService chatSyncService;
    
    @Transactional
    public PersonUserLink createLink(UUID personId, UUID userId, String linkType) {
        // Validate person exists
        Person person = personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
        
        // Validate user exists
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new AppException(ErrorCode.USER_NOT_FOUND));
        
        // Check if link already exists
        if (personUserLinkRepository.existsByPersonIdAndUserId(personId, userId)) {
            throw new AppException(ErrorCode.RESOURCE_ALREADY_EXISTS, "User is already linked to this person");
        }
        
        // Convert String to LinkType enum
        PersonUserLink.LinkType linkTypeEnum;
        try {
            linkTypeEnum = PersonUserLink.LinkType.valueOf(linkType.toLowerCase());
        } catch (IllegalArgumentException e) {
            linkTypeEnum = PersonUserLink.LinkType.self;
        }
        
        // Create and save the link
        PersonUserLink link = PersonUserLink.builder()
                .person(person)
                .user(user)
                .linkType(linkTypeEnum)
                .status(PersonUserLink.Status.approved)
                .build();
        
        PersonUserLink savedLink = personUserLinkRepository.save(link);
        
        // Sync user to relevant rooms (family room + branch rooms)
        // Only sync if status = approved
        if (savedLink.getStatus() == PersonUserLink.Status.approved) {
            chatSyncService.syncUserToRooms(userId, personId);
            chatSyncService.syncAllMembersToFamilyRoom(savedLink.getPerson().getFamilyTree().getId());
        }
        
        return savedLink;
    }
    
    @Transactional(readOnly = true)
    public boolean isLinked(UUID personId, UUID userId) {
        return personUserLinkRepository.existsByPersonIdAndUserId(personId, userId);
    }
    
    @Transactional
    public void removeLink(UUID personId, UUID userId) {
        personUserLinkRepository.deleteByPersonIdAndUserId(personId, userId);
    }
    
    @Transactional(readOnly = true)
    public PersonUserLink getLink(UUID personId, UUID userId) {
        return personUserLinkRepository.findByPersonIdAndUserId(personId, userId)
                .orElseThrow(() -> new AppException(ErrorCode.RESOURCE_NOT_FOUND, "Link not found"));
    }
}