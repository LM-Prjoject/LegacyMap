import {X} from "lucide-react";
import {useEffect, useState} from "react";
import {Relationship} from "@/api/trees";

interface PersonDetailsModalProps {
    isOpen: boolean;
    person: any;
    persons: any[];
    relationships: Relationship[];
    onClose: () => void;
    onEditClick: () => void;
}

const formatDate = (dateString?: string | null) => {
    if (!dateString) return 'Chưa cập nhật';
    const date = new Date(dateString);
    if (isNaN(date.getTime())) return 'Chưa cập nhật';
    return date.toLocaleDateString('vi-VN');
};


const getRelationshipText = (rel: Relationship, currentPersonId: string, persons: any[]) => {
    try {
        // Debug log
        console.log('Processing relationship:', rel);

        const otherPersonId = rel.fromPersonId === currentPersonId ? rel.toPersonId : rel.fromPersonId;
        const otherPerson = persons.find(p => p.id === otherPersonId);

        if (!otherPerson) {
            console.warn(`Could not find person with ID: ${otherPersonId}`);
            return null;
        }

        const otherPersonName = otherPerson.fullName || 'Không rõ';
        const isFromPerson = rel.fromPersonId === currentPersonId;

        // Determine relationship type in Vietnamese
        let relationshipText = '';
        // Get current person's gender
        const currentPerson = persons.find(p => p.id === currentPersonId);
        console.log('Current person data:', {
            id: currentPersonId,
            gender: currentPerson?.gender,
            name: currentPerson?.fullName,
            allPersons: persons.map(p => ({ id: p.id, name: p.fullName, gender: p.gender }))
        });
        
        const isCurrentPersonFemale = currentPerson?.gender?.toUpperCase() === 'FEMALE' || currentPerson?.gender === 'Nữ';
        console.log('isCurrentPersonFemale:', isCurrentPersonFemale);
        
        switch (rel.type) {
            case 'SPOUSE':
                // If current person is female, show "Chồng:" (husband), otherwise show "Vợ:" (wife)
                relationshipText = isCurrentPersonFemale 
                    ? `Chồng: ${otherPersonName}`
                    : `Vợ: ${otherPersonName}`;
                console.log('Setting spouse text:', { relationshipText, isCurrentPersonFemale });
                break;
            case 'PARENT':
                if (isFromPerson) {
                    relationshipText = `Con: ${otherPersonName}`;
                } else {
                    const otherPerson = persons.find(p => p.id === otherPersonId);
                    const isOtherPersonFemale = otherPerson?.gender?.toUpperCase() === 'FEMALE' || otherPerson?.gender === 'Nữ';
                    relationshipText = isOtherPersonFemale ? `Mẹ: ${otherPersonName}` : `Bố: ${otherPersonName}`;
                }
                break;
            case 'CHILD':
                if (isFromPerson) {
                    const otherPerson = persons.find(p => p.id === otherPersonId);
                    const isOtherPersonFemale = otherPerson?.gender?.toUpperCase() === 'FEMALE' || otherPerson?.gender === 'Nữ';
                    relationshipText = isOtherPersonFemale ? `Mẹ: ${otherPersonName}` : `Bố: ${otherPersonName}`;
                } else {
                    relationshipText = `Con: ${otherPersonName}`;
                }
                break;
            case 'SIBLING':
                relationshipText = `Anh/Chị/Em: ${otherPersonName}`;
                break;
            default:
                relationshipText = `Quan hệ: ${otherPersonName} (${rel.type})`;
        }

        return {
            text: relationshipText,
            person: otherPerson,
            type: rel.type,
            relationshipId: rel.id
        };
    } catch (error) {
        console.error('Error in getRelationshipText:', error);
        return null;
    }
};

export default function PersonDetailsModal({
                                               isOpen,
                                               person,
                                               persons,
                                               relationships,
                                               onClose,
                                               onEditClick
                                           }: PersonDetailsModalProps) {
    const [filteredRelationships, setFilteredRelationships] = useState<Array<{
        text: string;
        person: any;
        type: string;
        relationshipId?: string;
    }>>([]);

    useEffect(() => {
        if (!person?.id || !relationships?.length || !persons?.length) {
            console.log('Missing required data, clearing relationships');
            setFilteredRelationships([]);
            return;
        }

        try {
            console.log('Filtering relationships for person:', person.id);
            
            // Track processed person IDs to avoid duplicates
            const processedPersonIds = new Set<string>();
            
            // First, filter and process all relationships
            const allRelationships = relationships
                .filter(rel => {
                    // Only include relationships where the current person is involved
                    if (rel.fromPersonId !== person.id && rel.toPersonId !== person.id) {
                        return false;
                    }
                    
                    // Get the other person's ID
                    const otherPersonId = rel.fromPersonId === person.id ? rel.toPersonId : rel.fromPersonId;
                    
                    // Skip if we've already processed a relationship with this person
                    if (processedPersonIds.has(otherPersonId)) {
                        return false;
                    }
                    
                    // Mark this person as processed
                    processedPersonIds.add(otherPersonId);
                    return true;
                })
                .map(rel => getRelationshipText(rel, person.id, persons))
                .filter((rel): rel is NonNullable<typeof rel> => rel !== null);

            // Sort relationships: PARENT -> SPOUSE -> CHILD
            const personRelationships = allRelationships.sort((a, b) => {
                const order = { 'PARENT': 1, 'SPOUSE': 2, 'CHILD': 3 };
                // Determine the effective type for sorting
                const getEffectiveType = (item: typeof a) => {
                    if (item.text.startsWith('Con:')) return 'CHILD';
                    if (item.text.startsWith('Bố/Mẹ:')) return 'PARENT';
                    if (item.text.startsWith('Vợ:') || item.text.startsWith('Chồng:')) return 'SPOUSE';
                    return item.type;
                };
                
                const typeA = getEffectiveType(a);
                const typeB = getEffectiveType(b);
                
                return (order[typeA as keyof typeof order] || 99) - (order[typeB as keyof typeof order] || 99);
            });

            console.log('Filtered relationships:', personRelationships);
            setFilteredRelationships(personRelationships);
        } catch (error) {
            console.error('Error processing relationships:', error);
            setFilteredRelationships([]);
        }
    }, [person?.id, relationships, persons]);

    if (!isOpen || !person) return null;

    return (
        <div className="fixed inset-0 z-50 overflow-y-auto">
            <div className="flex min-h-screen items-center justify-center p-4">
                <div className="fixed inset-0 bg-black/50 transition-opacity" onClick={onClose}></div>

                <div
                    className="relative z-10 w-full max-w-2xl max-h-[90vh] overflow-y-auto rounded-xl bg-white shadow-xl">
                    <div className="sticky top-0 bg-white z-20 flex justify-between items-center p-4 border-b">
                        <h2 className="text-xl font-semibold">Thông tin chi tiết</h2>
                        <button
                            onClick={onClose}
                            className="p-1 rounded-full hover:bg-gray-100"
                        >
                            <X className="h-5 w-5"/>
                        </button>
                    </div>

                    <div className="p-6">
                        <div className="flex flex-col md:flex-row gap-6">
                            <div className="flex-shrink-0">
                                <div className="w-32 h-32 rounded-lg overflow-hidden bg-gray-100">
                                    {person.avatarUrl ? (
                                        <img
                                            src={person.avatarUrl}
                                            alt={person.fullName}
                                            className="w-full h-full object-cover"
                                        />
                                    ) : (
                                        <div
                                            className="w-full h-full flex items-center justify-center bg-gray-200 text-gray-400">
                                            <span className="text-sm">No photo</span>
                                        </div>
                                    )}
                                </div>
                            </div>

                            <div className="flex-1">
                                <div className="flex justify-between items-start">
                                    <div>
                                        <h1 className="text-2xl font-bold">{person.fullName}</h1>
                                        <p className="text-gray-600">
                                            {person.gender === 'male' ? 'Nam' : person.gender === 'female' ? 'Nữ' : 'Khác'}
                                        </p>
                                    </div>
                                    <button
                                        onClick={onEditClick}
                                        className="px-3 py-1.5 text-sm font-medium rounded-md border border-gray-300 bg-white text-gray-700 hover:bg-gray-50"
                                    >
                                        Chỉnh sửa
                                    </button>
                                </div>

                                <div className="mt-6 grid grid-cols-1 md:grid-cols-2 gap-4">
                                    <div>
                                        <p className="text-sm font-medium text-gray-500">Ngày sinh</p>
                                        <p>{formatDate(person.birthDate)}</p>
                                    </div>

                                    {person.deathDate && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Ngày mất</p>
                                            <p>{formatDate(person.deathDate)}</p>
                                        </div>
                                    )}

                                    {person.birthPlace && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Nơi sinh</p>
                                            <p>{person.birthPlace}</p>
                                        </div>
                                    )}

                                    {person.deathPlace && (
                                        <div>
                                            <p className="text-sm font-medium text-gray-500">Nơi mất</p>
                                            <p>{person.deathPlace}</p>
                                        </div>
                                    )}
                                </div>

                                {person.biography && (
                                    <div className="mt-6">
                                        <p className="text-sm font-medium text-gray-500 mb-2">Tiểu sử</p>
                                        <p className="whitespace-pre-line text-gray-800">{person.biography}</p>
                                    </div>
                                )}

                                <div className="mt-6">
                                    <p className="text-sm font-medium text-gray-500 mb-3">
                                        Mối quan hệ
                                    </p>
                                    {filteredRelationships.length > 0 ? (
                                        <div className="space-y-3">
                                            {filteredRelationships.map((rel, index) => {
                                                // Log relationship details for debugging
                                                console.log(`[Relationship ${index + 1}]`, {
                                                    relationshipId: rel.relationshipId,
                                                    type: rel.type,
                                                    person: {
                                                        id: rel.person?.id,
                                                        name: rel.person?.fullName,
                                                        gender: rel.person?.gender
                                                    },
                                                    relationshipText: rel.text,
                                                    timestamp: new Date().toISOString()
                                                });

                                                return (
                                                    <div
                                                        key={`${rel.relationshipId || index}-${rel.type}`}
                                                        className="p-3 bg-gray-50 rounded-lg hover:bg-gray-100 transition-colors cursor-pointer"
                                                    >
                                                        <div className="flex items-center gap-3">
                                                            {rel.person?.avatarUrl ? (
                                                                <img
                                                                    src={rel.person.avatarUrl}
                                                                    alt={rel.person.fullName}
                                                                    className="w-10 h-10 rounded-full object-cover"
                                                                    onError={(e) => {
                                                                        const target = e.target as HTMLImageElement;
                                                                        target.onerror = null;
                                                                        target.src = "";
                                                                        console.warn(`Failed to load avatar for ${rel.person?.fullName}`);
                                                                    }}
                                                                />
                                                            ) : (
                                                                <div className="w-10 h-10 rounded-full bg-gray-200 flex items-center justify-center text-gray-500">
                                    <span className="text-xs">
                                        {rel.person?.fullName?.charAt(0) || "?"}
                                    </span>
                                                                </div>
                                                            )}
                                                            <div>
                                                                <p className="font-medium text-gray-800">
                                                                    {rel.person?.fullName || "Không rõ"}
                                                                </p>
                                                                <p className="text-sm text-gray-600">
                                                                    {rel.text}
                                                                </p>
                                                            </div>
                                                        </div>
                                                    </div>
                                                );
                                            })}
                                        </div>
                                    ) : (
                                        <p className="text-gray-500">
                                            Người này chưa có thông tin mối quan hệ
                                        </p>
                                    )}
                                </div>
                            </div>
                        </div>
                    </div>

                    <div className="sticky bottom-0 bg-white px-6 py-4 border-t flex justify-end space-x-3">
                        <button
                            onClick={onClose}
                            className="px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md hover:bg-gray-50"
                        >
                            Đóng
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
}