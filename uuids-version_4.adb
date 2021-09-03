------------------------------------------------------------------------------
--                                                                          --
--                       Common UUID Handling Package                       --
--                       - RFC 4122  Implementation -                       --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--                Version 4 (Random) UUID Generation Package                --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2021 ANNEXI-STRAYLINE Trans-Human Ltd.                    --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai (ANNEXI-STRAYLINE)                                        --
--                                                                          --
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

package body UUIDs.Version_4 is
   
   -- State Management --
   
   ----------------
   -- Initialize --
   ----------------
   
   function Initialize (Seed: Natural) return Generator is
   begin
      return Gen: Generator do
         Gen_64.Reset (Gen => Gen.Gen, Initiator => Seed);
      end return;
   end;
   
   ----------------------------------------------------------------------
   
   function Initialize (From_State: State) return Generator is
   begin
      return Gen: Generator do
         Gen_64.Reset (Gen => Gen.Gen, From_State => From_State.Gen_State);
      end return;
   end;
   
   ----------
   -- Save --
   ----------
   
   procedure Save (Gen: in Generator; To_State: out State) is
   begin
      Gen_64.Save (Gen => Gen.Gen, To_State => To_State.Gen_State);
   end;
   
   -----------
   -- Reset --
   -----------
   
   procedure Reset (Gen: in out Generator; Seed: in Integer) is
   begin
      Gen_64.Reset (Gen => Gen.Gen, Initiator => Seed);
   end;
   
   ----------------------------------------------------------------------
   
   procedure Reset (Gen: in out Generator; From_State: in State) is
   begin
      Gen_64.Reset (Gen => Gen.Gen, From_State => From_State.Gen_State);
   end;
   
   -----------------
   -- Random_UUID --
   -----------------
   
   function Random_UUID (Gen: Generator) return UUID is
      use Interfaces;
      
      Rand_Hi: Unsigned_64 := Gen_64.Random (Gen.Gen);
      Rand_Lo: Unsigned_64 := Gen_64.Random (Gen.Gen);
      
      ID: UUID;
   begin
      -- The basic strategy is to generate two 64-bit values, and them overlay
      -- them onto the UUID value, before editing the specific bits we need to
      -- as per RFC 4122
      
      ID.time_low := Bitfield_32 (Rand_Hi and 16#ffff_ffff#);
      Rand_Hi := Shift_Right (Rand_Hi, 32);
      
      ID.time_mid := Bitfield_16 (Rand_Hi and 16#ffff#);
      Rand_Hi := Shift_Right (Rand_Hi, 16);
      
      
      -- Set the four most significant bits (bits 12 through 15) of the
      -- time_hi_and_version field to the 4-bit version number from
      -- Section 4.1.3. (2#0100# i.e. 4)
      
      ID.time_hi_and_version := Bitfield_16 (Rand_Hi and 16#0fff#);
      ID.time_hi_and_version := ID.time_hi_and_version or 16#4000#;
      
      
      -- RFC4122: "Set the two most significant bits (bits 6 and 7) of the
      -- clock_seq_hi_and_reserved to zero and one, respectively.
      
      ID.clock_seq_hi_and_reserved := Bitfield_8 (Rand_Lo and 2#0011_1111#);
      ID.clock_seq_hi_and_reserved 
        := ID.clock_seq_hi_and_reserved or 2#1000_0000#;
      Rand_Lo := Shift_Right (Rand_Lo, 8);
      
      ID.clock_seq_low := Bitfield_8 (Rand_Lo and 16#ff#);
      Rand_Lo := Shift_Right (Rand_Lo, 8);
      
      ID.node := Bitfield_48 (Rand_Lo and 16#ffff_ffff_ffff#);
      
      return ID;
      
   end Random_UUID;
   
end UUIDs.Version_4;
