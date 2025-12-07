package com.legacymap.backend.dto.response;

public class TreeStatsDTO {
    private int memberCount;
    private int generationCount;

    public TreeStatsDTO() {}

    public TreeStatsDTO(int memberCount, int generationCount) {
        this.memberCount = memberCount;
        this.generationCount = generationCount;
    }

    public int getMemberCount() {
        return memberCount;
    }

    public void setMemberCount(int memberCount) {
        this.memberCount = memberCount;
    }

    public int getGenerationCount() {
        return generationCount;
    }

    public void setGenerationCount(int generationCount) {
        this.generationCount = generationCount;
    }
}
