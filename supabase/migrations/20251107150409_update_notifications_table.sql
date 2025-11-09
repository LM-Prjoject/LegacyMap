ALTER TABLE notifications 
ALTER COLUMN type TYPE VARCHAR(50);

ALTER TABLE notifications 
DROP CONSTRAINT notifications_type_check;

ALTER TABLE notifications 
ADD CONSTRAINT notifications_type_check 
CHECK (type IN ('system', 'update', 'invite', 'alert', 'event_reminder'));