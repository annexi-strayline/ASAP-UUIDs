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
--                    Throttled Timestamp Source Package                    --
--                      for high performance generation                     --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                   --
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

-- This generic package creates a highly performant, task-safe timestamp source
-- which is designed to limit OS system calls to a the rate specified by 
-- Update_Quantum. The generated timestamp will not change during the period of
-- the Update_Quantum. This allows for very highspeed bursted, and parallel
-- UUID generation which would otherwise be rate-limited by syscall saturation.
--
-- Management of the timestamp is achieved through a task, which can be
-- terminated through the Shutdown procedure
--
-- -- WARNING --
-- Care should be taken using this source in sustained high-rate applications.
--
-- If Update_Quantum is too high, and UUID generation is sustained at a high-
-- rate during the period, there is potential for the generated UUIDs to
-- overrun the system timestamp, which once updated, may cause further UUIDs to
-- be held at the last used timestamp until the clock "catches-up", which may
-- cause clock sequence overrun. See the Generate function in the Version_1
-- package for further discussion on timestamp collision behavior.
--
-- Theoretically, this should only be a serious consideration if a single node
-- generates UUIDs at 1/ 100ns or greater, causing the synthetic tick to be 
-- faster than the timestamp representation of time itself


generic
   with function Actual_Source return Timestamp is Standard_Source;
   
   Update_Quantum: Duration;
package UUIDs.Version_1.Throttled_TS is
   
   task Timestamp_Monitor is
      entry Kill;
   end Timestamp_Monitor;
   -- Allows direct querying of the task state by the user
   
   function  Source   return  Timestamp;
   procedure Shutdown renames Timestamp_Monitor.Kill;
   
private
   protected Current_Timestamp is
      function  Get return Timestamp;
      procedure Set (New_TS: in Timestamp);
      
   private
      TS: Timestamp := Actual_Source;
   end Current_Timestamp;
   
   function Source return Timestamp is (Current_Timestamp.Get);
   
end UUIDs.Version_1.Throttled_TS;

