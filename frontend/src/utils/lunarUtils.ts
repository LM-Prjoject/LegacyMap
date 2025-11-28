import lunisolar from 'lunisolar';

export const getVietnameseLunarDay = (day: number): string => {
    if (day <= 10) {
        return ['Mùng 1', 'Mùng 2', 'Mùng 3', 'Mùng 4', 'Mùng 5', 'Mùng 6', 'Mùng 7', 'Mùng 8', 'Mùng 9', 'Mùng 10'][day - 1];
    }
    return day.toString();
};

export const getVietnameseLunarMonth = (month: number, isLeap: boolean = false): string => {
    const monthNumber = Math.abs(month);
    return `${isLeap ? 'Nhuận ' : ''}${monthNumber}`;
};

export const getVietnameseStemBranch = (stemBranch: string): string => {
    const stemMap: Record<string, string> = {
        '甲': 'Giáp', '乙': 'Ất', '丙': 'Bính', '丁': 'Đinh', '戊': 'Mậu',
        '己': 'Kỷ', '庚': 'Canh', '辛': 'Tân', '壬': 'Nhâm', '癸': 'Quý'
    };
    const branchMap: Record<string, string> = {
        '子': 'Tý', '丑': 'Sửu', '寅': 'Dần', '卯': 'Mão', '辰': 'Thìn', '巳': 'Tỵ',
        '午': 'Ngọ', '未': 'Mùi', '申': 'Thân', '酉': 'Dậu', '戌': 'Tuất', '亥': 'Hợi'
    };
    let stem = '';
    let branch = '';
    for (const char of stemBranch) {
        if (stemMap[char]) stem = stemMap[char];
        if (branchMap[char]) branch = branchMap[char];
    }
    return `${stem} ${branch}`.trim();
};

export const getVietnameseZodiac = (zodiac: string): string => {
    const map: Record<string, string> = {
        '鼠': 'Tý', '牛': 'Sửu', '虎': 'Dần', '兔': 'Mão', '龙': 'Thìn', '蛇': 'Tỵ',
        '马': 'Ngọ', '羊': 'Mùi', '猴': 'Thân', '鸡': 'Dậu', '狗': 'Tuất', '猪': 'Hợi'
    };
    return map[zodiac] || zodiac;
};

export const getLunarInfo = (date: Date) => {
    const lsr = lunisolar(date);
    const isLeap = lsr.lunar.isLeapMonth;
    const hour = date.getHours();

    const hourBranchIndex = Math.floor((hour + 1) / 2) % 12;
    const hourBranch = ['Tý', 'Sửu', 'Dần', 'Mão', 'Thìn', 'Tỵ', 'Ngọ', 'Mùi', 'Thân', 'Dậu', 'Tuất', 'Hợi'][hourBranchIndex];

    const lunarMonthNumber = Math.abs(lsr.lunar.month);

    // Can giờ (dựa trên can ngày + chi giờ)
    const dayStem = lsr.format('cD')[0];
    const dayStemIndex = ['Giáp', 'Ất', 'Bính', 'Đinh', 'Mậu', 'Kỷ', 'Canh', 'Tân', 'Nhâm', 'Quý'].indexOf(getVietnameseStemBranch(dayStem));
    const hourStemIndex = (dayStemIndex * 2 + hourBranchIndex) % 10;
    const hourStem = ['Giáp', 'Ất', 'Bính', 'Đinh', 'Mậu', 'Kỷ', 'Canh', 'Tân', 'Nhâm', 'Quý'][hourStemIndex];

    return {
        day: lsr.lunar.day,
        month: lsr.lunar.month,
        year: lsr.lunar.year,
        isLeap,
        dayStr: getVietnameseLunarDay(lsr.lunar.day),
        monthStr: getVietnameseLunarMonth(lunarMonthNumber, isLeap),
        yearStemBranch: getVietnameseStemBranch(lsr.format('cY')),
        dayStemBranch: getVietnameseStemBranch(lsr.format('cD')),
        hourCanChi: `${hourStem} ${hourBranch}`,
        zodiac: getVietnameseZodiac(lsr.format('cz'))
    };
};