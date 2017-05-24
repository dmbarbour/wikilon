
-- | This module is concerned with the concrete representation of a
-- dictionary binary. Given a dictionary binary, it can quickly parse
-- the structure with varying levels of detail.
--
-- Note: For now, this focuses on extracting data from the binary,
-- or producing a binary from suitable data. I assume we won't want
-- to leverage offsets within a binary too much, if only because 
-- naming a binary and its offset for data hurts structure sharing
-- when interacting with dictionaries that have the same data but
-- a slightly different format.
module Awelon.Dict.Format
    (
    ) where


