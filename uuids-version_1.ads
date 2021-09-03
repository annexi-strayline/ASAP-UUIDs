------------------------------------------------------------------------------
--                                                                          --
--                       Common UUID Handling Package                       --
--                       - RFC 4122  Implementation -                       --
--                                                                          --
--                               Version 1.0                                --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--              Version 1 (Time-based) UUID Generation Package              --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018, 2020-2021 ANNEXI-STRAYLINE Trans-Human Ltd.         --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai, Ensi Martini, Aninda Poddar, Noshen Atashe               --
--    (ANNEXI-STRAYLINE)                                                    --
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

with Hex;

package UUIDs.Version_1 is

   -------------
   -- Node ID --
   -------------
   
   subtype Node_ID_Hex is String (1 .. 12);
   
   -- Represents a 48 bit value in hexadecimal form
   
   
   --------------------
   -- Clock Sequence --
   --------------------
   
   type Clock_Sequence is mod 2**14;
   
   -- Represents a Version 1 UUID Clock Sequence value. Used to initialize new
   -- Version_1_States, and should be provided as a randomized value during
   -- instantiation of generator packages

   
   ----------------------
   -- Sequence Overrun --
   ----------------------
   
   type Sequence_Overrun is (Delay_or_Error, Delay_Until, Error);
   
   -- Selected action in case of the UUID of a clock sequence overrun event.
   --
   -- Delay_or_Error: New generation will be delayed by a 2 * System.Tick.
   --                 If that is insufficient, Generation_Failed will be 
   --                 raised.
   --
   -- Delay_Until   : New generation will be delayed by a loop of System.Tick
   --                 delays until the timestamp value changes.
   --
   -- Error         : A Generation_Failed exception is raised until the
   --                 the system clock advances.
   
   
   --------------------------
   -- Timestamp Generation --
   --------------------------
   
   type Timestamp is mod 2**60;
   
   -- Represents the number of 100ns periods since 0 UTC, Oct 15, 1582, as per
   -- RFC 4122, Section 4.1.4
   
   function Standard_Source return Timestamp;
   
   -- Generates an RFC 4122 Version 1 UUID timestamp based on the reported 
   -- system time, via the Ada.Calendar package.
   
   
   -------------------------------
   -- Version_1 Generator State --
   -------------------------------
   
   type Version_1_State(<>) is limited private;
   
   function New_State 
     (Node_ID               : Node_ID_Hex; 
      Initial_Clock_Sequence: Clock_Sequence;
      Initial_Timestamp     : Timestamp      := Standard_Source;
      Fail_Active           : Boolean        := False) 
     return Version_1_State;
   
   -- Generates a Version 1 UUID with the provided Node ID, the current time-
   -- stamp, and an (assumed) random clock sequence. This can be used to
   -- initialize the state of a Generator package. For long-lived systems, the 
   -- State should be saved and restored as appropriate.
   --
   -- If Fail_Active is True, any exception raised during state generation is
   -- suppressed, and the returned state has three properties:
   -- 1. Node ID is set as provided
   -- 2. Clock sequence is set as provided
   -- 3. Initial timestamp is set to zero
   
   function Save_State    (State: Version_1_State) return UUID;
   
   function Restore_State (ID: UUID) return Version_1_State 
   with Pre => Version (ID) = 1;
   
   -- Generates a Version 1 State from an existing Version 1 UUID. Overrun
   -- protection only applies to the clock sequence restored. Saved states
   -- should always be advanced appropriately (as per RFC 4122, Section 
   
   function Extract_Node_ID (State: Version_1_State) return Node_ID_Hex;
   
   -- Returns the Node_ID Hex string for the Node ID as set for State
   
   
   ---------------------------------
   -- Version_1 Generic Generator --
   ---------------------------------
   
   generic
      with function Timestamp_Source return Timestamp;
   function Generic_Generator
     (State           : in out Version_1_State;
      Overrun         : in     Sequence_Overrun)
     return UUID;
   
   -- Returns a new Version 1 UUID and modifies the State accordingly.
   -- If any error is encountered during generation, or the clock is not
   -- available, or the Overrun setting causes an error condition, 
   -- a Generation_Failed exception is raised.
   --
   -- -- Duplicate Generation Avoidance and Timestamp Collision Behavior  --
   -- If successive calls to generate for the same State encounter a current
   -- timestamp from Timestamp_Source which is the same as or less than the
   -- last issued timestamp (in the previous UUID), Generate takes one of two
   -- possible actions to ensure no duplicate UUIDs, both of which are pursuant
   -- to the permitted techniques described in RFC 4122
   --
   -- 1. If the sourced timestamp has not changed since the previous generation,
   --    a synthetic tick of a single 100ns period is applied to the last-used
   --    timestamp, and the clock sequence is not changed.
   --
   -- 2. If the source timestamp has changed since the previous generation, but
   --    is less than the last used timestamp, the synthetic timestamp has
   --    overrun the system reported (sourced) timestamp, and the issued
   --    timestamp is held until the sourced timestamp regains the lead.
   --
   --    During a held timestamp period, the clock sequence is incremented for
   --    each generated UUID. The generator records the sequence at entry to
   --    the held period, and monitors for sequence overrun. If generation of a
   --    UUID will cause a sequence overrun, Generate will take action
   --    according to the Overrun parameter, who's options are described with
   --    the Sequence_Overrun type specification, above
   --
   -- -- All Possible Exception --
   -- *  Generation_Failed: Timestamp generation failure, overflow, or
   --                       clock sequence overrun
   

   ---------------------
   -- Local_Generator --
   ---------------------
   
   -- A local generator is not Task-safe, and is intended for Task-specific
   -- UUID generation. For global/task-safe UUID generation, use the related 
   -- Global_Generator generic package below.
   
   generic
      Default_Overrun: in Sequence_Overrun;
      
      with function Timestamp_Source return Timestamp is Standard_Source;
   package Local_Generator is
      
      function Generate (Overrun: Sequence_Overrun := Default_Overrun)
                        return UUID;
      -- Returns a new Version 1 UUID, according to RFC 4122
      -- Not task-safe.
      
      function  Export_State (Advance: Duration := 0.0) return Version_1_State;
      -- Returns the current state of the generator, with the time-stamp
      -- advanced according to Delay.
      
      procedure Import_State (New_State: Version_1_State);
      -- Sets a new state
      
   end Local_Generator;
   
   
   ----------------------
   -- Global_Generator --
   ----------------------
   -- Calling Generate from a Global_Generator package is Task-Safe
   
   generic
      Default_Overrun: in Sequence_Overrun;
      
      with function Timestamp_Source return Timestamp is Standard_Source;
      
   package Global_Generator is
      function Generate (Overrun: Sequence_Overrun := Default_Overrun)
                        return UUID;
      -- Returns a new Version 1 UUID, according to RFC 4122
      -- Fully task-safe.
      
      function Export_State (Advance: Duration := 0.0) return Version_1_State;
      -- Fully task-safe.
      
      procedure Import_State (New_State: Version_1_State);
      -- Fully task-safe
      
   end Global_Generator;
   
private
   -- Helper function expression to extract the Clock_Sequence specifically
   -- Not exposed to the user as this shouldn't have much of a use.
   -- New Version_1_States shall always be initialized with a random sequence,
   -- making this user-accessible might give the wrong idea..
   function Extract_Sequence (ID: UUID) return Clock_Sequence is
      ((Clock_Sequence
         (ID.clock_seq_hi_and_reserved and 2#0011_1111#) * 2**8)
         or Clock_Sequence (ID.clock_seq_low));
      -- Mask out lowest 6 bits of clock_seq_hi and shift left 8 bits,
      -- then or in clock_seq_low
      
      
   ---------------------
   -- Version_1_State --
   ---------------------
   type Version_1_State is
      record
         Last_ID     : UUID           := Nil_UUID;
         Base_TS     : Timestamp      := 0;
         Period_Start: Clock_Sequence := 0;
      end record;
   -- The actual Version 1 UUID state is simply the last ID issued. However, we
   -- track extra information during generation to ensure we can detect sequence
   -- overruns if the generation rate is greater than 16,383 per system clock
   -- tick
   
   function Save_State (State: Version_1_State) return UUID is
      (State.Last_ID);
   
   function Restore_State (ID: UUID) return Version_1_State is
      (Version_1_State'(Last_ID      => ID,
                        Base_TS      => 0,
                        Period_Start => Extract_Sequence (ID)));
      -- This configuration ensures that restoring advanced states (ie UUIDs 
      -- from "the future" always result in clock sequence advancement, instead
      -- of clock tick synthesis, by setting Base_TS to zero. This conforms with
      -- RFC 4122
      
end UUIDs.Version_1;
