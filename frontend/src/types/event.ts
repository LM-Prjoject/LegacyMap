export interface Event {
    id: string;
    title: string;
    description?: string;
    eventType: EventType;
    startDate: string;
    endDate?: string;
    isFullDay?: boolean;
    calendarType: CalendarType;
    isRecurring?: boolean;
    recurrenceRule?: RecurrenceRule;
    relatedPersons?: RelatedPerson[];
    location?: string;
    locationCoordinates?: Record<string, any>;
    reminder?: ReminderConfig;
    isPublic?: boolean;
    status?: EventStatus;
    familyTreeId?: string;
    createdBy?: string;
    createdAt?: string;
    updatedAt?: string;
    personalOwnerId?: string;
}

export interface EventCreateRequest {
    title: string;
    description?: string;
    eventType: EventType;
    startDate: string;
    endDate?: string;
    isFullDay?: boolean;
    calendarType?: CalendarType;
    isRecurring?: boolean;
    recurrenceRule?: RecurrenceRule;
    relatedPersons?: RelatedPerson[];
    location?: string;
    locationCoordinates?: Record<string, any>;
    reminder?: ReminderConfig;
    isPublic?: boolean;
    personalOwnerId?: string;
}

export interface EventUpdateRequest {
    title?: string;
    description?: string;
    eventType?: EventType;
    startDate?: string;
    endDate?: string;
    isFullDay?: boolean;
    calendarType?: CalendarType;
    isRecurring?: boolean;
    recurrenceRule?: RecurrenceRule;
    relatedPersons?: RelatedPerson[];
    location?: string;
    locationCoordinates?: Record<string, any>;
    reminder?: ReminderConfig;
    isPublic?: boolean;
    status?: EventStatus;
    personalOwnerId?: string;
}

export interface RelatedPerson {
    id: string;
    name: string;
}

export interface ReminderConfig {
    daysBefore: number;
    methods: string[];
}

export enum EventType {
    DEATH_ANNIVERSARY = 'DEATH_ANNIVERSARY',
    WEDDING_ANNIVERSARY = 'WEDDING_ANNIVERSARY',
    BIRTHDAY = 'BIRTHDAY',
    FUNERAL = 'FUNERAL',
    WEDDING = 'WEDDING',
    FAMILY_REUNION = 'FAMILY_REUNION',
    CEREMONY = 'CEREMONY',
    OTHER = 'OTHER'
}

export enum CalendarType {
    SOLAR = 'SOLAR',
    LUNAR = 'LUNAR'
}

export enum RecurrenceRule {
    YEARLY = 'YEARLY',
    MONTHLY = 'MONTHLY',
    NONE = 'NONE'
}

export enum EventStatus {
    ACTIVE = 'ACTIVE',
    CANCELLED = 'CANCELLED',
    COMPLETED = 'COMPLETED'
}