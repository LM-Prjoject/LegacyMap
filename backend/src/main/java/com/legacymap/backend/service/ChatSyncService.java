package com.legacymap.backend.service;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;

import com.legacymap.backend.entity.*;
import com.legacymap.backend.repository.*;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.legacymap.backend.exception.AppException;
import com.legacymap.backend.exception.ErrorCode;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ChatSyncService {
    private final ChatRoomMemberRepository chatRoomMemberRepository;
    private final ChatRoomBranchRepository chatRoomBranchRepository;
    private final RelationshipRepository relationshipRepository;
    private final UserRepository userRepository;
    private final PersonRepository personRepository;
    private final ChatRoomRepository chatRoomRepository;
    private final PersonUserLinkRepository personUserLinkRepository;
    
    @Async
    @Transactional
    public void syncUserToRooms(UUID userId, UUID personId) {
        Person person = personRepository.findById(personId)
                .orElseThrow(() -> new AppException(ErrorCode.PERSON_NOT_FOUND));
        
        FamilyTree familyTree = person.getFamilyTree();
        
        // 1. Auto-join Family Room
        syncUserToFamilyRoom(userId, familyTree.getId());
        
        // 2. Auto-join Branch Rooms (where ancestors are root)
        syncUserToBranchRooms(userId, personId);
    }
    
    private void syncUserToFamilyRoom(UUID userId, UUID familyTreeId) {
        chatRoomRepository.findByFamilyTreeIdAndRoomType(familyTreeId, ChatRoom.ChatRoomType.family)
                .ifPresent(familyRoom -> {
                    if (!chatRoomMemberRepository.existsByRoomIdAndUserId(familyRoom.getId(), userId)) {
                        ChatRoomMember member = ChatRoomMember.builder()
                                .id(new ChatRoomMemberId(familyRoom.getId(), userId))
                                .room(familyRoom)
                                .user(userRepository.getReferenceById(userId))
                                .person(null)
                                .role(ChatRoomMember.ChatMemberRole.member)
                                .build();
                        chatRoomMemberRepository.save(member);
                    }
                });
    }
    
    private void syncUserToBranchRooms(UUID userId, UUID personId) {
        // 1. Find all ancestor IDs of the person (trace up to find all ancestors)
        Set<UUID> ancestorIds = findAncestors(personId);
        
        // 2. Find all branch rooms where any ancestor is the root person
        List<ChatRoomBranch> branchRooms = chatRoomBranchRepository.findByBranchPersonIdIn(ancestorIds);
        
        // 3. Add user to all relevant branch rooms
        for (ChatRoomBranch branch : branchRooms) {
            if (!chatRoomMemberRepository.existsByRoomIdAndUserId(branch.getRoom().getId(), userId)) {
                ChatRoomMember member = ChatRoomMember.builder()
                        .id(new ChatRoomMemberId(branch.getRoom().getId(), userId))
                        .room(branch.getRoom())
                        .user(userRepository.getReferenceById(userId))
                        .person(personRepository.getReferenceById(personId))
                        .role(ChatRoomMember.ChatMemberRole.member)
                        .build();
                chatRoomMemberRepository.save(member);
            }
        }
    }

    @Async
    @Transactional
    public void syncAllMembersToFamilyRoom(UUID familyTreeId) {
        // Lấy phòng chat gia đình
        chatRoomRepository.findByFamilyTreeIdAndRoomType(familyTreeId, ChatRoom.ChatRoomType.family)
            .ifPresent(familyRoom -> {
                // Lấy tất cả người dùng đã xác minh trong cây gia phả
                List<PersonUserLink> verifiedLinks = personUserLinkRepository
                    .findApprovedLinksByFamilyTreeId(familyTreeId);
                
                for (PersonUserLink link : verifiedLinks) {
                    if (!chatRoomMemberRepository.existsByRoomIdAndUserId(familyRoom.getId(), link.getUser().getId())) {
                        ChatRoomMember member = ChatRoomMember.builder()
                            .id(new ChatRoomMemberId(familyRoom.getId(), link.getUser().getId()))
                            .room(familyRoom)
                            .user(link.getUser())
                            .person(link.getPerson())
                            .role(ChatRoomMember.ChatMemberRole.member)
                            .build();
                        chatRoomMemberRepository.save(member);
                    }
                }
            });
    }
    
    //Find all ancestors by traversing up the tree (from person to parents, grandparents, etc.)
    private Set<UUID> findAncestors(UUID personId) {
        Set<UUID> ancestorIds = new HashSet<>();
        Queue<UUID> queue = new LinkedList<>();
        queue.add(personId);
        
        while (!queue.isEmpty()) {
            UUID currentId = queue.poll();
            List<UUID> parentIds = relationshipRepository.findParentIdsByPersonId(currentId);
            
            for (UUID parentId : parentIds) {
                if (!ancestorIds.contains(parentId)) {
                    ancestorIds.add(parentId);
                    queue.add(parentId);
                }
            }
        }
        
        return ancestorIds;
    }
}