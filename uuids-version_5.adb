------------------------------------------------------------------------------
--                                                                          --
--                       Common UUID Handling Package                       --
--                       - RFC 4122  Implementation -                       --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--            Version 5 (SHA-1 Name-Based) UUID Generation Package          --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2021 ANNEXI-STRAYLINE Trans-Human Ltd.                    --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai (ANNEXI-STRAYLINE)                                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of ANNEXI-STRAYLINE nor the names of its         --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL ANNEXI-STRAYLINE   --
--  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR  --
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF    --
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR         --
--  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,   --
--  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR --
--  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF  --
--  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                              --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces;           use Interfaces;
with Modular_Hashing.SHA1; use Modular_Hashing, Modular_Hashing.SHA1;

function UUIDs.Version_5 (Namespace: UUIDs.UUID; Name: String) 
                         return UUIDs.UUID
is
   Engine             : aliased SHA1_Engine;
   Name_Hash          : SHA1_Hash;
   Name_Hash_Binary_LE: Hash_Binary_Value (1 .. SHA1_Hash_Bytes);
   
   Accumulator: Unsigned_64;
   
   Result: UUID;
begin
   
   -- From RFC 4122:
   
   -- The algorithm for generating a UUID from a name and a name space are
   -- as follows:
   
   -- o  Allocate a UUID to use as a "name space ID" for all UUIDs
   --    generated from names in that name space; see Appendix C for some
   --    pre-defined values.
   
   -- => User-supplied by the Namespace parameter
   
   -- o  Choose either MD5 [4] or SHA-1 [8] as the hash algorithm; If
   --    backward compatibility is not an issue, SHA-1 is preferred.
   
   -- => Version 5 mandates SHA-1
   
   -- o  Convert the name to a canonical sequence of octets (as defined by
   --    the standards or conventions of its name space); put the name
   --    space ID in network byte order.
   
   -- => This is done implicitly. Name is a String, which is should always 
   --    be an array of octets. When we stream Namespace to the Hash engine,
   --    it will be in "network byte order" (big endian)
   
   -- o  Compute the hash of the name space ID concatenated with the name.
   
   UUID'Write   (Engine'Access, Namespace);
   String'Write (Engine'Access, Name);
   
   Name_Hash           := SHA1_Hash (Engine.Digest);
   Name_Hash_Binary_LE := Name_Hash.Binary;  -- This is little endian
   
   -- Note 
   -- ----
   --
   -- The way the RFC describes this is really awkward, with a switch between
   -- network byte order and local byte order all over the place. Only by
   -- looking at the reference implementation can we see that the (big endian)
   -- hash first (most significant) 16 octets of the SHA1 hash is direcly
   -- overlayed ontop of the binary UUID, for which each feild is then
   -- converted to local host order.
   --
   -- What this really means is that the conversions cancel eachother out,
   -- and if we consider a binary UUID in big endian, the BE SHA1 hash is
   -- just dropped ontop of it, with a few bits twiddled.

   
   -- o  Set octets zero through 3 of the time_low field to octets zero
   --    through 3 of the hash.
   
   Accumulator := 0;
   
   for Octet of reverse 
     Name_Hash_Binary_LE (SHA1_Hash_Bytes - 3 .. SHA1_Hash_Bytes - 0) 
   loop
      Accumulator := Shift_Left (Value => Accumulator, Amount => 8);
      Accumulator := Accumulator + Unsigned_64 (Octet);
   end loop;
   
   Result.time_low := Bitfield_32 (Accumulator);
   
   -- o  Set octets zero and one of the time_mid field to octets 4 and 5 of
   --    the hash.
   
   Accumulator := 0;
   
   for Octet of reverse
     Name_Hash_Binary_LE (SHA1_Hash_Bytes - 5 .. SHA1_Hash_Bytes - 4)
   loop
      Accumulator := Shift_Left (Value => Accumulator, Amount => 8);
      Accumulator := Accumulator + Unsigned_64 (Octet);
   end loop;
   
   Result.time_mid := Bitfield_16 (Accumulator);
   
   -- o  Set octets zero and one of the time_hi_and_version field to octets
   --    6 and 7 of the hash.
   
   Accumulator := 0;
   
   for Octet of reverse
     Name_Hash_Binary_LE (SHA1_Hash_Bytes - 7 .. SHA1_Hash_Bytes - 6)
   loop
      Accumulator := Shift_Left (Value => Accumulator, Amount => 8);
      Accumulator := Accumulator + Unsigned_64 (Octet);
   end loop;
   
   Result.time_hi_and_version := Bitfield_16 (Accumulator);
   
   -- o  Set the four most significant bits (bits 12 through 15) of the
   --    time_hi_and_version field to the appropriate 4-bit version number
   --    from Section 4.1.3.
   
   Result.time_hi_and_version
     := (Result.time_hi_and_version and 16#0FFF#) + 16#5000#;

   -- o  Set the clock_seq_hi_and_reserved field to octet 8 of the hash.
   --
   -- o  Set the two most significant bits (bits 6 and 7) of the
   --    clock_seq_hi_and_reserved to zero and one, respectively.
   
   Result.clock_seq_hi_and_reserved 
     := (Bitfield_8 (Name_Hash_Binary_LE(SHA1_Hash_Bytes - 8)) 
           and 2#0011_1111#) + 2#1000_0000#;
   
   -- o  Set the clock_seq_low field to octet 9 of the hash.
   
   Result.clock_seq_low
     := Bitfield_8 (Name_Hash_Binary_LE(SHA1_Hash_Bytes - 9));
   
   -- o  Set octets zero through five of the node field to octets 10
   --    through 15 of the hash.
   
   Accumulator := 0;
   
   for Octet of reverse
     Name_Hash_Binary_LE (SHA1_Hash_Bytes - 15 .. SHA1_Hash_Bytes - 10)
   loop
      Accumulator := Shift_Left (Value => Accumulator, Amount => 8);
      Accumulator := Accumulator + Unsigned_64 (Octet);
   end loop;
   
   Result.node := Bitfield_48 (Accumulator);
   
   -- o  Convert the resulting UUID to local byte order.
   --
   -- => This is done implicitly by UUID'Write, but otherwise the fields are
   --    stored distinctly
   
   return Result;
   
end UUIDs.Version_5;
