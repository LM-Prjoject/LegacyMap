package com.legacymap.backend.family.service;

import com.legacymap.backend.family.dto.request.ProfileCreateRequest;
import com.legacymap.backend.family.entity.Profile;
import com.legacymap.backend.family.repository.ProfileRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ProfileService {
    @Autowired
    private ProfileRepository profileRepository;
    public Profile createAccount ( ProfileCreateRequest request) {
        Profile profile = new Profile();
        profile.setUsername(request.getUsername());
        profile.setPassword(request.getPassword());
        profile.setFullName(request.getFullName());
        profile.setGender(request.getGender());
        profile.setEmail(request.getEmail());
        profile.setPhone(request.getPhone());
        profile.setDob(request.getDob());
        profile.setClanName(request.getClanName());
        profile.setCity(request.getCity());
        profile.setWard(request.getWard());
        profile.setHouseNumber(String.valueOf(request.getHouseNumber()));
        return profileRepository.save(profile);
    }

}
