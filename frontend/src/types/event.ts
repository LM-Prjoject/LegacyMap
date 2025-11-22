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
    DEATH_ANNIVERSARY = 'death_anniversary',
    WEDDING_ANNIVERSARY = 'wedding_anniversary',
    BIRTHDAY = 'birthday',
    FUNERAL = 'funeral',
    WEDDING = 'wedding',
    FAMILY_REUNION = 'family_reunion',
    CEREMONY = 'ceremony',
    OTHER = 'other'
}

export enum CalendarType {
    SOLAR = 'solar',
    LUNAR = 'lunar'
}

export enum RecurrenceRule {
    YEARLY = 'YEARLY',
    MONTHLY = 'MONTHLY',
    NONE = 'NONE'
}

export enum EventStatus {
    ACTIVE = 'active',
    CANCELLED = 'cancelled',
    COMPLETED = 'completed'
}