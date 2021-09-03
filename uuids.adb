------------------------------------------------------------------------------
--                                                                          --
--                       Common UUID Handling Package                       --
--                       - RFC 4122  Implementation -                       --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018-2020 ANNEXI-STRAYLINE Trans-Human Ltd.               --
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

with System;
with Ada.Exceptions;    use Ada;

with Hex.Modular_Codec;

package body UUIDs is
   
   -- Portability Constraints --
   -----------------------------
   -- The bitwise manipulation conventions of this package assumes that
   -- Storage_Unit is 8 bits. For other machines, the relevant Record
   -- Representation Clause position values should be modified as appropriate
   -- for this package, and all relevant child packages
   
   pragma Assert (Check   => System.Storage_Unit = 8,
                  Message => "This package requires a target with an 8-bit " &
                    "Storage_Element size.");
   
   package HMC_48 is new Hex.Modular_Codec(Bitfield_48, 48);
   package HMC_32 is new Hex.Modular_Codec(Bitfield_32, 32);
   package HMC_16 is new Hex.Modular_Codec(Bitfield_16, 16);
   package HMC_8  is new Hex.Modular_Codec(Bitfield_8,  8);
   
   --
   -- UUID Comparison and Equality Operators
   --
   
   ---------
   -- ">" --
   ---------
   
   function ">"(Left, Right: UUID) return Boolean is
   begin
      
      if Left.time_low > Right.time_low then
         return True;
         
      elsif Left.time_low = Right.time_low then
         
         if Left.time_mid > Right.time_mid then
            return True;
            
         elsif Left.time_mid = Right.time_mid then
            
            if Left.time_hi_and_version > Right.time_hi_and_version then
               return True;
               
            elsif Left.time_hi_and_version = Right.time_hi_and_version then
               
               if Left.clock_seq_hi_and_reserved >
                 Right.clock_seq_hi_and_reserved then
                  return True;
                  
               elsif Left.clock_seq_hi_and_reserved =
                 Right.clock_seq_hi_and_reserved then
                  
                  if Left.clock_seq_low > Right.clock_seq_low then
                     return True;
                     
                  elsif Left.clock_seq_low = Right.clock_seq_low then
                     
                     if Left.node > Right.node then
                        return True;
                        
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
      
      return False;
      
   end ">";
   
   
   ---------
   -- "<" --
   ---------
   
   function "<"(Left, Right: UUID) return Boolean is
   begin 
      
      if Left.time_low < Right.time_low then
         return True;
         
      elsif Left.time_low = Right.time_low then
         
         if Left.time_mid < Right.time_mid then
            return True;
            
         elsif Left.time_mid = Right.time_mid then
            
            if Left.time_hi_and_version < Right.time_hi_and_version then
               return True;
               
            elsif Left.time_hi_and_version = Right.time_hi_and_version then
               
               if Left.clock_seq_hi_and_reserved <
                 Right.clock_seq_hi_and_reserved then
                  return True;
                  
               elsif Left.clock_seq_hi_and_reserved =
                 Right.clock_seq_hi_and_reserved then
                  
                  if Left.clock_seq_low < Right.clock_seq_low then
                     return True;
                     
                  elsif Left.clock_seq_low = Right.clock_seq_low then
                     
                     if Left.node < Right.node then
                        return True;
                        
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
      
      return False;
      
   end "<";
   
   
   --
   -- UUID Encode/Decode Operations
   --
   
   ------------
   -- Encode --
   ------------
   
   function Encode (ID : UUID) return UUID_String is
      use Hex;
      
      Output : UUID_String;
      
      -- We need to have zero-filled values of the exact length in
      -- all cases
      Hex_8 : String (1 .. HMC_8.Max_Nibbles);
      Hex_16: String (1 .. HMC_16.Max_Nibbles);
      Hex_32: String (1 .. HMC_32.Max_Nibbles);
      Hex_48: String (1 .. HMC_48.Max_Nibbles);
   begin
      -- time_low
      HMC_32.Encode (Value  => ID.time_low, Buffer => Hex_32);
      Output(UUID_String_time_low) := Hex_32;
      
      Output(UUID_String_Hyphen_1) := "-";
      
      -- time_mid
      HMC_16.Encode (Value => ID.time_mid, Buffer => Hex_16);
      Output(UUID_String_time_mid) := Hex_16;
      
      Output(UUID_String_Hyphen_2) := "-";
      
      -- time_high_and_version
      HMC_16.Encode (Value => ID.time_hi_and_version, Buffer => Hex_16);
      Output(UUID_String_time_high_and_version) := Hex_16;
      
      Output(UUID_String_Hyphen_3) := "-";
      
      -- clock_seq_and_reserved
      HMC_8.Encode (Value => ID.clock_seq_hi_and_reserved, Buffer => Hex_8);
      Output(UUID_String_clock_seq_and_reserved) := Hex_8;
      
      -- clock_seq_low
      HMC_8.Encode (Value => ID.clock_seq_low, Buffer => Hex_8);
      Output(UUID_String_clock_seq_low) := Hex_8;
      
      Output(UUID_String_Hyphen_4) := "-";
      
      -- node
      HMC_48.Encode (Value => ID.node, Buffer => Hex_48);
      Output(UUID_String_node) := Hex_48;
     
      return Output;
      
   end Encode;
   
   
   ------------
   -- Decode --
   ------------
   
   function Decode (ID_String: UUID_String) return UUID is
      Output_UUID : UUID;
      
      Basic_Error: constant String
        := "String is not a valid UUID value. ";
      
      procedure Assert_Hex (Candidate: String) with Inline is
      begin
         if not Hex.Valid_Hex_String (Candidate) then
            raise UUID_Format_Error with
              Basic_Error & "Expected hexadecimal.";
         end if;
      end;
      
      procedure Assert_Length (Candidate: String; Expected: Positive) 
      with Inline is
      begin
         -- This assertion should technically not possibly fail.. But
         -- above all else it will be stable, since the lengths are set
         -- by the spec of this package. This is a good candidate to
         -- remove as an optimization
         
         if Candidate'Length /= Expected then
            raise UUID_Format_Error with
                 Basic_Error & "Field is not the correct length";
         end if;
      end;
      
      
   begin
      
      if ID_String(UUID_String_Hyphen_1) /= "-"
        or else ID_String(UUID_String_Hyphen_2) /= "-"
        or else ID_String(UUID_String_Hyphen_3) /= "-"
        or else ID_String(UUID_String_Hyphen_4) /= "-"
      then
         
         raise UUID_Format_Error
           with "String is not a valid UUID value. Hyphens are missing, or " &
           "incorrectly placed.";
         
      end if;
      
      -- We need to ensure we can satisfy the preconditions of the Decode
      -- subprogram of the Hex codecs
      
      -- time_low field --
      declare
         TLS: String renames ID_String(UUID_String_time_low);
      begin
         Assert_Hex (TLS);
         Assert_Length (TLS, HMC_32.Max_Nibbles);
         
         Output_UUID.time_low := HMC_32.Decode (TLS);
      end;
      
      -- time_mid field --
      
      declare
         TMS: String renames ID_String(UUID_String_time_mid);
      begin
         Assert_Hex (TMS);
         Assert_Length (TMS, HMC_16.Max_Nibbles);
         Output_UUID.time_mid := HMC_16.Decode (TMS);
      end;
            
      -- time_hi_and_Version field --
      
      declare
         THVS: String renames ID_String(UUID_String_time_high_and_version);
      begin
         Assert_Hex (THVS);
         Assert_Length (THVS, HMC_16.Max_Nibbles);
         Output_UUID.time_hi_and_version := HMC_16.Decode (THVS);
      end;
      
      -- clock_seq_hi_and_reserved field --
      
      declare
         CSHRS: String renames ID_String(UUID_String_clock_seq_and_reserved);
      begin
         Assert_Hex (CSHRS);
         Assert_Length (CSHRS, HMC_8.Max_Nibbles);
         Output_UUID.clock_seq_hi_and_reserved := HMC_8.Decode (CSHRS);
      end;
      
      -- clock_seq_low field --
      
      declare
         CSLS: String renames ID_String(UUID_String_clock_seq_low);
      begin
         Assert_Hex (CSLS);
         Assert_Length (CSLS, HMC_8.Max_Nibbles);
         Output_UUID.clock_seq_low := HMC_8.Decode (CSLS);
      end;
      
      -- node field --
      
      declare
         NS: String renames ID_String(UUID_String_node);
      begin
         Assert_Hex (NS);
         Assert_Length (NS, HMC_48.Max_Nibbles);
         Output_UUID.node := HMC_48.Decode (NS);
      end;
      
      return Output_UUID;
      
   end Decode;
   
   
   --
   -- Hashing
   --
   
   ----------
   -- Hash --
   ----------
   
   function Hash (ID: UUID) return Ada.Containers.Hash_Type is
      use Ada.Containers;
   begin
      return Result: Hash_Type do
         Result :=            Hash_Type'Mod (ID.time_low);
         Result := Result xor Hash_Type'Mod (ID.time_mid);
         Result := Result xor Hash_Type'Mod (ID.time_hi_and_version);
         Result := Result xor Hash_Type'Mod (ID.clock_seq_hi_and_reserved);
         Result := Result xor Hash_Type'Mod (ID.clock_seq_low);
         Result := Result xor Hash_Type'Mod (ID.node);
      end return;
   end;
   
   --
   -- Binary IO
   --
   
   -- On the wire, the UUID needs to be packed into a 128-bit value. Since
   -- ultimately going to be translating into contigious octets, we really need
   -- to construct each one at a time.
   --
   -- For reference, the conceptual in-memory representation of the full
   -- value would look like this, Bit_Order => high High_Order_First
   --
   -- for Wire_UUID use
   --   record
   --     time_low                  at 0 range 0  .. 31;
   --     time_mid                  at 0 range 32 .. 47;
   --     time_hi_and_version       at 0 range 48 .. 63;
   --     clock_seq_hi_and_reserved at 8 range 0  .. 7;
   --     clock_seq_low             at 8 range 8  .. 15;
   --     node                      at 8 range 16 .. 63;
   --   end record;
   
   pragma Assert 
     (Check => Ada.Streams.Stream_Element'Modulus 
        = Interfaces.Unsigned_8'Modulus,
      Message => "Warning Stream output might not be contiguous.");
   
   ---------------
   -- To_Binary --
   ---------------
   
   function To_Binary (ID: in UUID) return Binary_UUID is
      use Interfaces;
      
      Accumulator: Unsigned_8;
      Arranger_32: Unsigned_32;
      Arranger_64: Unsigned_64;
      
      Current_Octet: Integer := UUID_Binary_MSB;
      
      procedure Push_Accumulator (Target: in out Binary_UUID) 
      with Inline is
      begin
         Target (Current_Octet) := Accumulator;
         Current_Octet := Current_Octet - 1;
      end;
   begin
      -- We'll construct it in big-endian order, just to be logically
      -- consistent
      
      return Bin: Binary_UUID do
         -- Take advantage of build-in-place, if available
         
         Arranger_32 := Unsigned_32 (ID.time_low);
         for I in reverse 0 .. 3 loop
            Accumulator := Unsigned_8
              (Shift_Right (Arranger_32, 8 * I) and 16#ff#);
            Push_Accumulator (Bin);
         end loop;
         
         Arranger_32 := Unsigned_32 (ID.time_mid);
         for I in reverse 0 .. 1 loop
            Accumulator := Unsigned_8
              (Shift_Right (Arranger_32, 8 * I) and 16#ff#);
            Push_Accumulator (Bin);
         end loop;
         
         Arranger_32 := Unsigned_32 (ID.time_hi_and_version);
         for I in reverse 0 .. 1 loop
            Accumulator := Unsigned_8
              (Shift_Right (Arranger_32, 8 * I) and 16#ff#);
            Push_Accumulator (Bin);
         end loop;
         
         Accumulator := Unsigned_8 (ID.clock_seq_hi_and_reserved);
         Push_Accumulator (Bin);
         
         Accumulator := Unsigned_8 (ID.clock_seq_low);
         Push_Accumulator (Bin);

         Arranger_64 := Unsigned_64 (ID.node);
         for I in reverse 0 .. 5 loop
            Accumulator := Unsigned_8
              (Shift_Right (Arranger_64, 8 * I) and 16#ff#);
            Push_Accumulator (Bin);
         end loop;
      end return;
      
   end To_Binary;
   
   -----------------
   -- From_Binary --
   -----------------
   
   function From_Binary (ID: in Binary_UUID) return UUID is
      use Interfaces;
      
      Current_Octet: Integer := UUID_Binary_MSB;
      
      function Pop_Accumulator return Unsigned_8 with Inline is
      begin
         Current_Octet := Current_Octet - 1;
         return ID (Current_Octet + 1);
      end;
      
      Accumulator: Unsigned_8;
      Arranger_32: Unsigned_32;
      Arranger_64: Unsigned_64;
   begin
      return ID: UUID do
         -- Simply reverse To_Binary
         Arranger_32 := 0;
         
         for I in 0 .. 3 loop
            Accumulator := Pop_Accumulator;
            Arranger_32 := Shift_Left (Arranger_32, 8);
            Arranger_32 := Arranger_32 + Unsigned_32 (Accumulator);
         end loop;
         ID.time_low := Bitfield_32 (Arranger_32);
         
         Arranger_32 := 0;
         for I in 0 .. 1 loop
            Accumulator := Pop_Accumulator;
            Arranger_32 := Shift_Left (Arranger_32, 8);
            Arranger_32 := Arranger_32 + Unsigned_32 (Accumulator);
         end loop;
         ID.time_mid := Bitfield_16 (Arranger_32);
         
         Arranger_32 := 0;
         for I in 0 .. 1 loop
            Accumulator := Pop_Accumulator;
            Arranger_32 := Shift_Left (Arranger_32, 8);
            Arranger_32 := Arranger_32 + Unsigned_32 (Accumulator);
         end loop;
         ID.time_hi_and_version := Bitfield_16 (Arranger_32);
         
         Accumulator := Pop_Accumulator;
         ID.clock_seq_hi_and_reserved := Bitfield_8 (Accumulator);
         
         Accumulator := Pop_Accumulator;
         ID.clock_seq_low := Bitfield_8 (Accumulator);
         
         Arranger_64 := 0;
         for I in 0 .. 5 loop
            Accumulator := Pop_Accumulator;
            Arranger_64 := Shift_Left (Arranger_64, 8);
            Arranger_64 := Arranger_64 + Unsigned_64 (Accumulator);
         end loop;
         ID.node := Bitfield_48 (Arranger_64);
      end return;
   end From_Binary;
   
   -----------
   -- Write --
   -----------
   
   procedure Write (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
                    ID    : in UUID)
   is
      Bin: constant Binary_UUID := To_Binary (ID);
   begin
      for Octet of reverse Bin loop
         Interfaces.Unsigned_8'Write (Stream, Octet);
      end loop;
   end;
   
   ----------
   -- Read --
   ----------
   
   procedure Read (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
                   ID    : out UUID)
   is 
      Bin: Binary_UUID;
   begin
      for Octet of reverse Bin loop
         Interfaces.Unsigned_8'Read (Stream, Octet);
      end loop;
      
      ID := From_Binary (Bin);
   end;
   
   -------------
   -- Version --
   -------------
   
   function Version (ID: UUID) return UUID_Version is
   begin
      
      return UUID_Version((ID.time_hi_and_version and 16#f000#) / 2 ** 12);
      -- This will do a run-time check, and if it fails, Constraint_Error will
      -- be raised into this function, which we can then catch in a catch-all
      -- handler
      
   exception
      when others => return 0;
              
   end Version;
   
end UUIDs;
