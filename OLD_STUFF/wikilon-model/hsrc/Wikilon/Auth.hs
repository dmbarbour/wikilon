
-- | Wikilon is intended to fit nicely with web services and FUSE 
-- virtual filesystem adapters. User and password authentication
-- with basic permissions seems appropriate. 
--
-- I'll probably want some e-mail integration, and maybe also a
-- simple quotas model tracking user activity and overhead. But a
-- lot of this may happen later.
--
-- Dictionaries will be the finest granularity for access, I think.
module Wikilon.Auth
    (
    ) where
