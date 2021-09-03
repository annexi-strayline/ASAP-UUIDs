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
--  Copyright (C) 2018, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contibutors:                                                   --
--  * Aninda Poddar (ANNEXI-STRAYLINE)                                      --
--                                                                          --
--  First release review and refinement                                     --
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

with System;
with Ada.Calendar;                 use Ada.Calendar;
with Ada.Calendar.Formatting;      use Ada.Calendar.Formatting;
with Ada.Calendar.Arithmetic;
with Ada.Exceptions;               use Ada;

with Hex.Modular_Codec;

package body UUIDs.Version_1 is
   
   type Bitfield_4  is mod 2**4  with Size => 4;
   type Bitfield_6  is mod 2**6  with Size => 6;
   type Bitfield_12 is mod 2**12 with Size => 12;
   type Bitfield_14 is mod 2**14 with Size => 14;
   type Bitfield_60 is mod 2**60 with Size => 60;
   
   --
   -- State Management Utilities
   --
   
   package Node_ID_Hex_Codec is new Hex.Modular_Codec (Bitfield_48, 48);
   
   -----------------
   -- Format_Time --
   -----------------
   -- Takes a 60-bit UUID Version 1 timestamp (RFC 4122, Section 4.1.4), and
   -- breaks it out into the UUID standard fields, in a portable way which is
   -- machine-endian independent.
   
   procedure Format_Time (From_Timestamp: in     Timestamp;
                          Time_High     :    out Bitfield_12; 
                          Time_Low      :    out Bitfield_32; 
                          Time_Mid      :    out Bitfield_16)
     with Inline => True
   is
   begin      
      -- To extract the high 12 bits of the total time field 
      -- (Total_100_Nano_Seconds) mask everything other than the first 12 bits 
      -- and shift the bitfield 60  by 48 bits  
      -- to get the 12 most significant bits into Time_High
      Time_High                    := (Bitfield_12((From_Timestamp and 
                                      16#fff0_0000_0000_000#) / 2**48));    
      Time_Mid                     := (Bitfield_16((From_Timestamp and 
                                      16#000f_fff0_0000_000#) / 2**32));
      Time_Low                     := (Bitfield_32((From_Timestamp and 
                                      16#0000_000f_ffff_fff#)));    
   end Format_Time;
   
   -----------------------
   -- Extract_Timestamp --
   -----------------------
   -- Extract the UUID Version 1 timestamp (RFC 4122, Section 4.1.4) from the
   -- current State (itself just a Version 1 UUID)
   function Extract_Timestamp  (State: Version_1_State)
                               return  Timestamp 
     with Inline => True
   is
      -- Extract the last time stamp to later compare with the current 
      -- time generated according to RFC 4122
   begin     
      
      return UUID_timestamp: Timestamp := 0 do
        
        -- Mask the first 4 bits containing the version number, leaving
        -- the high bits of the timestamp, then shift left by 48 bits, 
        -- putting it at the most significant 12 bits of the timestamp.
        UUID_timestamp := 
          Timestamp (State.Last_ID.time_hi_and_version and 16#0fff#) * 2**48;

        -- Insert the time_mid in the last time stamp by shifting left
        -- 32 bits, and dropping it in with an or operation
        UUID_timestamp := (Timestamp (State.Last_ID.time_mid) * 2**32)
          or UUID_timestamp;
        
        -- The low bits get dropped in as-is
        UUID_timestamp := UUID_timestamp + Timestamp (State.Last_ID.time_low);
      end return;
      
   end Extract_Timestamp;
   
   
   --
   -- Implementation of Specification
   --
   
   ---------------------
   -- Standard_Source --
   ---------------------
   -- Generates a 60-bit unsigned modular timestamp value as specified in 
   -- RFC 4122, Section 4.1.4:
   -- "The timestamp is a 60-bit value.  For UUID version 1, this is represented
   --  by Coordinated Universal Time (UTC) as a count of 100-nanosecond 
   --  intervals since 00:00:00.00, 15 October 1582 (the date of Gregorian 
   --  reform to the Christian calendar)."
   
   function Standard_Source return Timestamp is
      -- Generate a current timestamp from the system clock
      use Ada.Calendar.Arithmetic;
      Old_1901               : Time;                  
      Days_1901_Now          : Day_Count;
      Seconds                : Duration;
      Leap_Seconds           : Leap_Seconds_Count;
      Total_Days             : Timestamp;
      Total_Seconds          : Timestamp;
      Now                    : Time         := Clock;
      Days_1582_1901         : constant     := 116225;
   begin            
      Old_1901:= Time_Of (Year   => 1901,
                          Month  => 1, 
                          Day    => 1, 
                          Hour   => 0,
                          Minute => 0, 
                          Second => 0);
      -- At the time of writing, the Ada 2012 Reference Manual states this is
      -- the earliest possible time which may be represented by a value of type
      -- Time. We need to use this as a base point, from which we will add on
      -- the days from 1582/01/01:00:00:00.00 until 1900/12/31:00:00:00.00 UTC
      
      -- Get the time difference from now to Jan 1st 1901
      Difference (Left         => Now, 
                  Right        => Old_1901,  
                  Days         => Days_1901_Now, 
                  Seconds      => Seconds, 
                  Leap_Seconds => Leap_Seconds);
      -- Though the Ada RM doesn't specifically state that Clock should be in
      -- UTC, it does suggest as much. For most uses, this will likely be true.
      -- RFC 4122 does not require the time to be UTC, but rather that it is 
      -- consistent. Implementations may want to audit UUID generation to ensure
      -- that clock is set correctly, if this is a cause for concern.
      
      -- Get the current time as a 60-bit count of 100-nanosecond intervals 
      -- since 00:00:00.00, 15 October 1582 
      Total_Days         := Timestamp (Days_1901_Now + Days_1582_1901);
      
      -- Add all the seconds
      Total_Seconds      := (Total_Days * 24 * 60 * 60) + 
        Timestamp (Seconds) + Timestamp (Leap_Seconds);
      
      -- Multiply to get 100 ns (10_000_000 100 ns intervals/s) and return
      return (Total_Seconds * 10_000_000);
      
   end Standard_Source;
   
   ---------------
   -- New_State --
   ---------------
   function New_State 
     (Node_ID               : Node_ID_Hex; 
      Initial_Clock_Sequence: Clock_Sequence;
      Initial_Timestamp     : Timestamp      := Standard_Source;
      Fail_Active           : Boolean        := False) 
     return Version_1_State
   is
      Node_48_Bits: Bitfield_48;
      
      Time_High   : Bitfield_12;
      Time_Low    : Bitfield_32;
      Time_Mid    : Bitfield_16;
      
      Clock_Low   : Bitfield_8;
      Clock_High  : Bitfield_6;
 
   begin     
      -- Convert node_id_hex to 48 bits. This may be needed for handling an
      -- exception, and so is placed out here
      if not Hex.Valid_Hex_String (Node_Id) then
         raise Constraint_Error with "Node ID is not a valid hexadecimal";
      end if;
      
      Node_48_Bits := Node_ID_Hex_Codec.Decode (Node_ID);
      
      return State: Version_1_State do
        -- Period start set to the current (initial) clock sequence
        State.Period_Start := Initial_Clock_Sequence;
        
        -- Masking everything other that the 6 most significant bits of 
        -- clock_sequence for clock_seq_high and shift richt2e  by 8 bits 
        -- to keep only the 6 bits
        Clock_High                
          := Bitfield_6 
          ((Initial_Clock_Sequence and 2#1111_1100_0000_00#) / 2**8);
        
        Clock_Low                 
          := Bitfield_8 
          ((Initial_Clock_Sequence and 2#0000_0011_1111_11#));
        
        State.Last_ID.clock_seq_low := Bitfield_8 (Clock_Low);
        
        -- Insert the variant number into the clock_seq_hi_and_reserved part
        State.Last_ID.clock_seq_hi_and_reserved 
          := Bitfield_8 (Clock_High) or 2#1000_0000#;
        
        -----
        -- The above statements should not possibly result in an exception,
        -- whereas the below very well may. If Fail_Active is enabled, we can
        -- catch this and at least return a valid state, with a zeroed timestamp
        
        -- Get the current time as a 60-bit count of 100-nanosecond intervals
        -- since 00:00:00.00, 15 October 1582, and set it as the "Previous 
        -- TS" for the state
        State.Base_TS := Initial_Timestamp;
        
        -- Set-up the value for "Last_ID"
        State.Last_ID.node := Node_48_Bits;
        
        -- Split our 60-bit full timestamp into the requisite individual 
        -- bitfields for insertion into the actual UUID
        Format_Time (From_Timestamp => State.Base_TS, 
                     Time_High      => Time_High, 
                     Time_Mid       => Time_Mid, 
                     Time_Low       => Time_Low);
        
        State.Last_ID.time_low            := Time_Low;
        State.Last_ID.time_mid            := Time_Mid;
        State.Last_ID.time_hi_and_version 
          := (Bitfield_16 (Time_High)) or 16#1000#;
        -- RFC 4122 states that the high bit is set.
           
      exception
         when others => 
            if Fail_Active then 
               State.Last_ID.time_low            := 0;
               State.Last_ID.time_mid            := 0;
               State.Last_ID.time_hi_and_version := 16#1000#;
            else
                 raise;
            end if;
      end return;
      
   exception
      
      when others => 
         if Fail_Active then 
            return Version_1_State'
              (Last_ID => (node => Node_48_Bits, 
                           others => <>),
               others  => <>);
         else
            raise;
         end if;
              
   end New_State;
   
   
   ---------------------
   -- Extract_Node_ID --
   ---------------------
   function Extract_Node_ID (State: Version_1_State) return Node_ID_Hex is
   begin
      return Node_ID: Node_ID_Hex do
        Node_ID_Hex_Codec.Encode (Value  => State.Last_ID.node,
                                  Buffer => Node_ID);
      end return;
   end Extract_Node_ID;
   
   
   -----------------------
   -- Generic_Generator --
   -----------------------
   function Generic_Generator (State  : in out Version_1_State;
                               Overrun: in     Sequence_Overrun) 
                              return UUID 
   is
      UUID_Last_Node_ID  : Bitfield_48;
      UUID_Last_Timestamp: Timestamp;
      Current_Timestamp  : Timestamp;   
      
      Time_High          : Bitfield_12;
      Time_Low           : Bitfield_32;
      Time_Mid           : Bitfield_16;
      
      This_Sequence      : Clock_Sequence;
      Clock_Low          : Bitfield_8;
      Clock_High         : Bitfield_6;      
      
      Overrun_Decision   : Sequence_Overrun := Overrun;
   begin    
      -- From the last UUID generator state read the values of:
      -- timestamp, clock sequence, and node ID 
         
      UUID_Last_Node_ID   := State.Last_ID.node;
        
      UUID_Last_Timestamp := Extract_Timestamp (State);
         
      -- Get the current time as a 60-bit count of 100-nanosecond intervals
      -- since 00:00:00.00, 15 October 1582
      -- Get current node ID
          
      -- Extracting the last clock sequence from State, to be the assumed
      -- current sequence, unless we absolutely need to increment it
      This_Sequence := Extract_Sequence (State.Last_ID);

      -- An interesting problem occurs when generating Version 1 UUIDs on
      -- typical server hardware: the generation rate is very high, but the
      -- system clock tick is often not very fine-grain.
      --
      -- We will attempt to be smart about this by synthesizing clock ticks,
      -- and backing off said synthesis when we find that we have outrun the
      -- system clock, at which point we will fall back to clock sequence
      -- incrementation.
      --
      -- We achieve this by recording the last valid "Base" timestamp in the
      -- state, which lets us detect an actual system clock advancement.
      --
      -- Note that for new or restored states, the "Base timestamp" is zero,
      -- and so we are assured that we will see a "new clock tick" this time.
      -- If the state was restored from an "advanced" state, then the "last"
      -- timestamp will be in the future, which simulates a clock overrun
      -- condition, and causes us to increment the clock sequence until the
      -- clock catches-up. This is conformant with RFC 4122.
      
      Current_Timestamp := Timestamp_Source;
      -- First-off, we take a look at
      
      if Current_Timestamp = State.Base_TS then
         -- This means the system clock has not advanced since last time we
         -- generated an ID. So we will therefore simply synthesize a clock
         -- tick, and skip to the end
         Current_Timestamp := UUID_Last_Timestamp + 1;
         
         -- It also marks a new clock sequence period for the state
         State.Period_Start := This_Sequence;
         
      else
         -- Otherwise, we now that we have a new clock tick. We need to decide
         -- if we can use it (and update State.Base_TS), or otherwise it means
         -- we have overrun the system clock, and we need to wait for it to
         -- catch up by incrementing the clock sequence. This leaves a
         -- possibility that we will need to implement an explicit delay and
         -- retry. We will put all the logic into a loop, so that we can
         -- appropriately handle a clock sequence overrun, should it happen.
         
         -- Note that for newly Restored states, the initial Base_TS is always
         -- zero, which works out perfectly by ensuring only the clock sequence
         -- increments until the system clock catches up
         
         loop
            if Current_Timestamp > UUID_Last_Timestamp then
               -- This means we have a new valid (advanced) timestamp, and
               -- we can safely use this and commit it as the new Base_TS
               State.Base_TS := Current_Timestamp;
               
               -- Also marks a new clock sequence period
               State.Period_Start := This_Sequence;
               exit;
               
            else
               -- Assume last timestamp
               Current_Timestamp := UUID_Last_Timestamp;
               
               -- This is the classic use-case for clock sequence 
               -- incrementation. We are unable to increment the last used 
               -- timestamp, and so we must make the UUID unique by changing the
               -- clock sequence.
               if (This_Sequence + 1) = State.Period_Start then
                  -- We have an clock sequence overrun!
                  case Overrun_Decision is
                     when Delay_or_Error =>
                        delay (System.Tick * 2);
                        
                        -- This is the only delay, so next iteration, we need
                        -- to act as if we are configured for Error on overrun
                        Overrun_Decision := Error;
                        
                     when Delay_Until =>
                        delay System.Tick;
                        
                     when Error =>
                        raise Generation_Failed with
                          "Clock sequence overrun";
                  end case;
                  
               else
                  -- Clock sequence is OK for increment
                  This_Sequence := This_Sequence + 1;
                  exit;
                  
               end if;
            end if;
            
            -- If we get here, it means we are trying again (after a delay)
            Current_Timestamp := Timestamp_Source;
            
         end loop;
      end if;
      
      -- Finally, we have a valid timestamp and clock sequence with which to
      -- generate our new UUID
         
      -- Format a UUID from the current timestamp, clock sequence, and node
      Format_Time (From_Timestamp => Current_Timestamp, 
                   Time_High      => Time_High, 
                   Time_Mid       => Time_Mid, 
                   Time_Low       => Time_Low);
         
      return Generated_UUID: UUID do
        -- Format time
        Generated_UUID.time_low            := Time_Low;
        Generated_UUID.time_mid            := Time_Mid;
        Generated_UUID.time_hi_and_version := (Bitfield_16 (Time_High)) or 
                                              16#1000#;
        
        -- Masking everything other that the 6 most significant bits of 
        -- clock_sequence for clock_seq_high and shift left  by 8 bits 
        -- to keep only the 6 bits
        Clock_High                
          := Bitfield_6 
          ((This_Sequence and 2#11_1111_0000_0000#) / 2**8);     
        
        Clock_Low                 
          := Bitfield_8 
          (This_Sequence and  2#00_0000_1111_1111#);
        
        Generated_UUID.clock_seq_low          
          := Bitfield_8 (Clock_Low);
        
        -- Insert the variant number into the clock_seq_hi_and_reserved part
        Generated_UUID.clock_seq_hi_and_reserved 
          := Bitfield_8 (Clock_High) or 2#1000_0000#;
        
        -- Copy in the node ID
        Generated_UUID.node := UUID_Last_Node_ID;
        
        -- Save the id to the State
        State.Last_ID := Generated_UUID;
        
      end return;
      
   exception
      when e: others =>
         raise Generation_Failed with
           "Generation raised an exception: " & 
           Exceptions.Exception_Information (e);
   end Generic_Generator;
   
   
   ---------------------
   -- Local_Generator --
   ---------------------
   
   package body Local_Generator is
            
      State: Version_1_State;
      
      function Generate_Actual is new Generic_Generator
        (Timestamp_Source);
      
      --------------
      -- Generate --
      --------------
      
      function Generate (Overrun: Sequence_Overrun := Default_Overrun)
                        return UUID is (Generate_Actual (State   => State,
                                                         Overrun => Overrun));

                        
      ------------------
      -- Export_State --
      ------------------
      
      function Export_State (Advance: Duration := 0.0) return Version_1_State
      is
         UUID_Last_timestamp     : Timestamp;
         Time_High               : Bitfield_12;
         Time_Low                : Bitfield_32;
         Time_Mid                : Bitfield_16;
         
      begin
         -- Check  for a default Advance of 0.0
         if Advance = 0.0 then            
            return State; 
         end if;
         
         UUID_Last_Timestamp := Extract_Timestamp (State);   
         
         -- Add the advance nanoseconds (10,000,000 100ns/s)
         UUID_Last_Timestamp
           := UUID_Last_Timestamp + Timestamp (Advance * 10_000_000);
         
         return Advanced_State: Version_1_State := State do
           -- We copy out the current State, only updating the advance
           -- timestamp, and clearing the Base_TS;
           Format_Time (From_Timestamp => UUID_Last_timestamp,
                        Time_High      => Time_High,
                        Time_Mid       => Time_Mid , 
                        Time_Low       => Time_Low);
           
           Advanced_State.Last_ID.time_low := Time_Low;
           Advanced_State.Last_ID.time_mid := Time_Mid;
           
           -- Insert the version number of 1 into the time_hi_and_version part
           Advanced_State.Last_ID.time_hi_and_version
             := Bitfield_16 (Time_High) or 16#1000#;                  
           
           Advanced_State.Base_TS := 0;
           
         end return;                  
      end Export_State;
      
      
      ------------------
      -- Import_State --
      ------------------
      
      procedure Import_State (New_State: Version_1_State) is
      begin
         State := New_State;
      end Import_State;
      
   end Local_Generator;
   
   
   ----------------------
   -- Global_Generator --
   ----------------------
   
   package body Global_Generator is
      
      package Generator is new Local_Generator 
        (Default_Overrun  => Default_Overrun,
         Timestamp_Source => Timestamp_Source);
      
      -- Protected body from which to call The Local Generator 
      -- Global Lock
      protected Protected_Generator is         
         procedure Generate (ID: out UUID; Overrun: in Sequence_Overrun);
         function  Export_State (Advance: duration) return Version_1_State;     
         procedure Import_State (New_State: Version_1_State);         
      end Protected_Generator;      
      
      protected body Protected_Generator is       
         
         procedure Generate (ID: out UUID; Overrun: in Sequence_Overrun) is
         begin
            ID :=  Generator.Generate (Overrun);
         end Generate; 
     
         function Export_State (Advance: duration) return Version_1_State is  
         begin
            return Generator.Export_State (Advance);                        
         end Export_State;
            
         procedure Import_State (New_State: Version_1_State) is
         begin            
            Generator.Import_State (New_State);            
         end Import_State;
         
      end Protected_Generator;
      
      
      --------------
      -- Generate --
      --------------
      
      function Generate (Overrun: Sequence_Overrun := Default_Overrun) 
                        return UUID 
      is         
      begin                 
         return ID: UUID do           
           Protected_Generator.Generate (ID => ID, Overrun => Overrun);
         end return;               
      end Generate;
      
      
      ------------------
      -- Export_State --
      ------------------
      
      function Export_State (Advance: Duration := 0.0) return Version_1_State is
      begin            
         return ID: Version_1_State do           
           ID :=  Protected_Generator.Export_State(Advance);           
         end return;        
      end Export_State;
      
      
      ------------------
      -- Import_State --
      ------------------
      
      procedure Import_State (New_State: Version_1_State) is
      begin        
         Protected_Generator.Import_State(New_State);         
      end Import_State;
      
   end Global_Generator;
   
end UUIDs.Version_1;

   
