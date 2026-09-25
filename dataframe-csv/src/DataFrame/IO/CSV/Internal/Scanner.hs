{-# LANGUAGE BangPatterns #-}

{- | RFC 4180 field scanner for the default CSV reader (Round-2 WS-D).
One pass over a strict 'BS.ByteString'; cassava-parity semantics pinned
by the @IO.CsvGolden@ suite: quotes open fields only at the first byte,
a double quote inside an unquoted field is an error, garbage after a
closing quote is an error, an unclosed quote consumes to EOF, and rows
end at @\\n@, @\\r\\n@ or a lone @\\r@.
-}
module DataFrame.IO.CSV.Internal.Scanner (
    Term,
    termSep,
    termEol,
    termEof,
    withField,
    withStripUtf8,
    withStripAscii,
    unescapeQuotes,
    isBlankRecordAt,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)

-- | What ended a field: another field follows, the row ended, or EOF.
type Term = Int

termSep, termEol, termEof :: Term
termSep = 0
termEol = 1
termEof = 2

quote, nl, cr :: Word8
quote = 34
nl = 10
cr = 13

parseError :: Int -> String -> a
parseError off what =
    error ("CSV Parse Error: " ++ what ++ " at byte " ++ show off)

{- | Scan one field starting at @pos@ and continue with
@k contentStart contentEnd needsUnescape term nextPos@. Quoted fields
yield the bytes between the outer quotes; @needsUnescape@ is set when
the content contains doubled quotes ('unescapeQuotes' collapses them).
-}
{-# INLINE withField #-}
withField ::
    BS.ByteString ->
    Int ->
    Word8 ->
    Int ->
    (Int -> Int -> Bool -> Term -> Int -> r) ->
    r
withField bs !len !sep !pos k
    | pos < len && BSU.unsafeIndex bs pos == quote = quoted (pos + 1) False
    | otherwise = unquoted pos
  where
    -- Field terminator dispatch shared by both shapes.
    {-# INLINE finish #-}
    finish !cs !ce !unesc !i
        | i >= len = k cs ce unesc termEof i
        | otherwise = case BSU.unsafeIndex bs i of
            b
                | b == sep -> k cs ce unesc termSep (i + 1)
                | b == nl -> k cs ce unesc termEol (i + 1)
                | b == cr ->
                    if i + 1 < len && BSU.unsafeIndex bs (i + 1) == nl
                        then k cs ce unesc termEol (i + 2)
                        else k cs ce unesc termEol (i + 1)
                | otherwise ->
                    parseError i "malformed quoted field (garbage after closing quote)"
    unquoted !i
        | i >= len = k pos i False termEof i
        | otherwise = case BSU.unsafeIndex bs i of
            b
                | b == sep -> k pos i False termSep (i + 1)
                | b == nl -> k pos i False termEol (i + 1)
                | b == cr ->
                    if i + 1 < len && BSU.unsafeIndex bs (i + 1) == nl
                        then k pos i False termEol (i + 2)
                        else k pos i False termEol (i + 1)
                | b == quote -> parseError i "stray double quote in unquoted field"
                | otherwise -> unquoted (i + 1)
    quoted !i !unesc
        | i >= len = k (pos + 1) len unesc termEof len -- unclosed: rest of input
        | BSU.unsafeIndex bs i == quote =
            if i + 1 < len && BSU.unsafeIndex bs (i + 1) == quote
                then quoted (i + 2) True
                else finish (pos + 1) i unesc (i + 1)
        | otherwise = quoted (i + 1) unesc

{- | Whether the record starting at @pos@ is a blank record (a single
empty field, quoted or not, then end of row) — cassava drops these.
Assumes @pos < len@. Errors propagate from 'withField'.
-}
{-# INLINE isBlankRecordAt #-}
isBlankRecordAt :: BS.ByteString -> Int -> Word8 -> Int -> Bool
isBlankRecordAt bs !len !sep !pos =
    withField bs len sep pos (\cs ce _ term _ -> cs == ce && term /= termSep)

{- | Collapse doubled quotes (@\"\"@ → @\"@) in @[s, e)@ into a fresh strict
'BS.ByteString'. Every quote in the range is the first of a pair.
-}
unescapeQuotes :: BS.ByteString -> Int -> Int -> BS.ByteString
unescapeQuotes bs !s !e = BSI.unsafeCreateUptoN (e - s) (go s 0)
  where
    go !i !j !dst
        | i >= e = pure j
        | otherwise = do
            let w = BSU.unsafeIndex bs i
            pokeByteOff (dst :: Ptr Word8) j w
            if w == quote then go (i + 2) (j + 1) dst else go (i + 1) (j + 1) dst

{- | Strip @[s0, e0)@ as @T.strip . decodeUtf8Lenient@ would, on raw
bytes: ASCII whitespace and whole UTF-8 encodings of U+00A0 and the
@Zs@ spaces. Multi-byte sequences are never split.
-}
{-# INLINE withStripUtf8 #-}
withStripUtf8 :: BS.ByteString -> Int -> Int -> (Int -> Int -> r) -> r
withStripUtf8 bs !s0 !e0 k = k s e
  where
    at = BSU.unsafeIndex bs
    s = skipF s0
    e = skipB e0
    skipF !i
        | i < e0 && isAsciiSpace (at i) = skipF (i + 1)
        | i + 1 < e0 && isNbsp (at i) (at (i + 1)) = skipF (i + 2)
        | i + 2 < e0 && isSpace3 (at i) (at (i + 1)) (at (i + 2)) =
            skipF (i + 3)
        | otherwise = i
    skipB !i
        | i > s && isAsciiSpace (at (i - 1)) = skipB (i - 1)
        | i - 1 > s && isNbsp (at (i - 2)) (at (i - 1)) = skipB (i - 2)
        | i - 2 > s && isSpace3 (at (i - 3)) (at (i - 2)) (at (i - 1)) =
            skipB (i - 3)
        | otherwise = i

{- | Strip ASCII whitespace (the ASCII subset of @Data.Text.strip@) and
continue with the stripped range. Callers must fall back to a full
'Data.Text.strip' when the stripped range still has non-ASCII edges.
-}
{-# INLINE withStripAscii #-}
withStripAscii :: BS.ByteString -> Int -> Int -> (Int -> Int -> r) -> r
withStripAscii bs !s0 !e0 k = k s e
  where
    s = skipF s0
    e = skipB e0
    skipF !i
        | i < e0 && isAsciiSpace (BSU.unsafeIndex bs i) = skipF (i + 1)
        | otherwise = i
    skipB !i
        | i > s && isAsciiSpace (BSU.unsafeIndex bs (i - 1)) = skipB (i - 1)
        | otherwise = i

isAsciiSpace :: Word8 -> Bool
isAsciiSpace w = w == 32 || (w - 9) <= 4
{-# INLINE isAsciiSpace #-}

isNbsp :: Word8 -> Word8 -> Bool
isNbsp b0 b1 = b0 == 0xC2 && b1 == 0xA0
{-# INLINE isNbsp #-}

-- | U+1680, U+2000..U+200A, U+202F, U+205F or U+3000.
isSpace3 :: Word8 -> Word8 -> Word8 -> Bool
isSpace3 b0 b1 b2 = case b0 of
    0xE2 ->
        (b1 == 0x80 && ((b2 >= 0x80 && b2 <= 0x8A) || b2 == 0xAF))
            || (b1 == 0x81 && b2 == 0x9F)
    0xE3 -> b1 == 0x80 && b2 == 0x80
    0xE1 -> b1 == 0x9A && b2 == 0x80
    _ -> False
{-# INLINE isSpace3 #-}
