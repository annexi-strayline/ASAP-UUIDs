------------------------------------------------------------------------------
--                                                                          --
--                       Common UUID Handling Package                       --
--                       - RFC 4122  Implementation -                       --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018-2021 ANNEXI-STRAYLINE Trans-Human Ltd.               --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai, Ensi Martini, Aninda Poddar, Noshen Atashe               --
--    (ANNEXI-STRAYLINE)                                                    --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--                                                                          --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT      --
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        --
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   --
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   --
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     --
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   --
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces;
with Ada.Streams;
with Ada.Containers;

package UUIDs is
   
   ----------------
   -- Exceptions --
   ----------------
   
   Generation_Failed: exception;
   -- Raised by ID Generation subprograms that cannot generate a valid UUID.
   
   
   -----------------
   -- UUID_String --
   -----------------
   -- The UUID_String format is sized to contain the standard UUID encoding
   -- format as specified in RFC 4122, Section 3, under "Declaration of 
   -- syntactic structure"
   
   subtype UUID_String_Index is Positive range 1 .. 36; -- 16 "hexOctets", 4 '-'
   subtype UUID_String       is String(UUID_String_Index);
   
   
   UUID_Nil_String: constant UUID_String
     := "00000000-0000-0000-0000-000000000000";
   
   -- An string-encoded representation of a Nil UUID, as defined in RFC 4122,
   -- Section 4.1.7
   
   UUID_Format_Error: exception;
   
   -- Raised when attempting to "Decode" an invalid UUID_String, which either 
   -- has non hex-digits, or improperly placed/missing hyphens.
   
   -- RFC 4122 Field Ranges --
   
   subtype UUID_String_time_low
      is UUID_String_Index 
     -- "4hexOctet" = 8 Characters
     range UUID_String_Index'First .. UUID_String_Index'First + 8 - 1;
   
   subtype UUID_String_Hyphen_1
      is UUID_String_Index
     range UUID_String_time_low'Last + 1 .. UUID_String_time_low'Last + 1; 
   
   subtype UUID_String_time_mid
      is UUID_String_Index 
     -- "2hexOctet" = 4 Characters
     range UUID_String_Hyphen_1'Last + 1 .. UUID_String_Hyphen_1'Last + 4;
   
   subtype UUID_String_Hyphen_2
      is UUID_String_Index
     range UUID_String_time_mid'Last + 1 .. UUID_String_time_mid'Last + 1; 
   
   subtype UUID_String_time_high_and_version
      is UUID_String_Index 
     -- "2hexOctet" = 4 Characters
     range UUID_String_Hyphen_2'Last + 1 .. UUID_String_Hyphen_2'Last + 4;
   
   subtype UUID_String_Hyphen_3               
      is UUID_String_Index
     range UUID_String_time_high_and_version'Last + 1 ..
           UUID_String_time_high_and_version'Last + 1;
   
   subtype UUID_String_clock_seq_and_reserved
      is UUID_String_Index 
     -- "hexOctet" = 2 Characters
     range UUID_String_Hyphen_3'Last + 1 .. UUID_String_Hyphen_3'Last + 2;
   
   subtype UUID_String_clock_seq_low
      is UUID_String_Index
     -- "hexOctet" = 2 Characters
     range UUID_String_clock_seq_and_reserved'Last + 1 ..
           UUID_String_clock_seq_and_reserved'Last + 2;
   
   subtype UUID_String_Hyphen_4
      is UUID_String_Index
     range UUID_String_clock_seq_low'Last + 1 ..
           UUID_String_clock_seq_low'Last + 1;
   
   subtype UUID_String_node
     is UUID_String_Index
     -- "6hexOctet" = 12 Characters
     range UUID_String_Hyphen_4'Last + 1 .. UUID_String_Hyphen_4'Last + 12;
   
   
   ----------
   -- UUID --
   ----------
   
   type UUID is private with Preelaborable_Initialization;
   
   -- Unassigned UUIDs are initialized to the equivalent of Nil_UUID
   --
   -- Note that the underlying representation of UUID is not guarunteed to
   -- be "correct" (contiguous 128-bit big-endian), since this is rarely
   -- important within a common system. However, the streaming attributes
   -- of UUID will send the correct 128-bit big-endian value "over the wire"
   
   function ">" (Left, Right: UUID) return Boolean;
   function "<" (Left, Right: UUID) return Boolean;
   
   -- Hex Encode and Decode
   
   function Encode (ID: UUID) return UUID_String;
   
   -- Encodes any UUID into it's representative UUID_String value, according to
   -- the rules specified by RFC 4122, Section 3.
   
   function Decode (ID_String: UUID_String) return UUID;
   
   -- Decodes a UUID_String into a UUID object.
   -- -- Explicit Exceptions --
   -- *  UUID_Format_Error: ID_String was non-complaint with RFC 4122,
   --                       Section 3
   
   -- Hashing
   
   function Hash (ID: UUID) return Ada.Containers.Hash_Type;
   
   -- Returns a hash value generally suitable for use in Containers.
   -- This hash is produced via binary xors of various Hash_Type'Mod results
   -- from each discrete binary "field" of the UUID
   
   -- Binary IO
   
   UUID_Binary_LSB: constant := 0;
   UUID_Binary_MSB: constant := 15;
   
   type Binary_UUID is array (UUID_Binary_LSB .. UUID_Binary_MSB)
       of Interfaces.Unsigned_8;
   
   -- UUID_Binary is a Little-endian representation of the 128-bit UUID value
   -- as a 
   
   function  To_Binary   (ID: in UUID) return Binary_UUID;
   function  From_Binary (ID: in Binary_UUID) return UUID;
   
   -- Converion to/from a binary UUID representation
   
   procedure Write (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
                    ID    : in UUID);
   
   procedure Read (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
                   ID    : out UUID);
   
   -- The binary value Write or Read from stram is strictly occording to
   -- RFC 4122, Section 4.1.2 - "Layout and Byte Order". Specifically, the
   -- binary value is a single 128-bit big-endian value.
   --
   -- The effect of Write is equivalent to:
   --
   -- declare
   --   Bin: constant Binary_UUID := To_Binary (ID);
   -- begin
   --   for Octet of reverse Bin loop
   --      Interfaces.Unsigned_8'Write (Stream, Octet);
   --   end loop;
   -- end;
   --
   -- Ergo, UUID_Binary_MSB is written to the stream first.
   
   for UUID'Write use Write;
   for UUID'Read  use Read;
   
   -- Nil UUID --
   --------------
   
   Nil_UUID: constant UUID;
   
   -- A Nil UUID as defined in RFC 4122, Section 4.1.7
   
   ------------------
   -- UUID_Version --
   ------------------
   
   subtype UUID_Version is Integer range 0 .. (2**4) - 1;
   
   -- RFC 4122 specifies that they Version of a UUID is represented by an 
   -- unsigned 4-bit value. However, we do not want to export wrap-around 
   -- semantics here
   
   function Version(ID: UUID) return UUID_Version;
   
   -- Returns the reported Version of any UUID. If the UUID is a Nil UUID,
   -- Version returns zero. On any kind of internal error, Version returns
   -- zero.
   --
   -- -- Suppresses All Exceptions --
   
private
   
   type Bitfield_8  is mod 2**8;
   type Bitfield_16 is mod 2**16;
   type Bitfield_32 is mod 2**32;
   type Bitfield_48 is mod 2**48;
   
   -- As demanded by the RFC 4122 specification implemented by the UUID type.
   
   type UUID is
      record
         time_low                 : Bitfield_32  := 0;  -- Octet #0-3
         time_mid                 : Bitfield_16  := 0;  -- Octet #4-5
         time_hi_and_version      : Bitfield_16  := 0;  -- Octet #6-7
         clock_seq_hi_and_reserved: Bitfield_8   := 0;  -- Octet #8
         clock_seq_low            : Bitfield_8   := 0;  -- Octet #9
         node                     : Bitfield_48  := 0;  -- Octet #10-15
      end record;
   
   -- Basic specification of UUID as specified by RFC 4122, Section 4.1.2 -
   -- "Layout and Byte Order".
   --
   -- However, to improve efficiency of the system, the UUID is stored in
   -- a conventional record with native byte order.
   --
   -- When output/input via the stream attributes, the stream representation
   -- will be as specified by RFC 4122 - exactly 128 contiguous bits, big-
   -- endian
   
   Nil_UUID: constant UUID := UUID'(others => <>);
   
end UUIDs;
