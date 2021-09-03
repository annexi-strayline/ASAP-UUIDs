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

package body UUIDs.Version_1.Throttled_TS is
   
   -----------------------
   -- Current_Timestamp --
   -----------------------
   
   protected body Current_Timestamp is
      function Get return Timestamp is (TS);
      
      procedure Set (New_TS: in Timestamp) is
      begin
         TS := New_TS;
      end Set;
   end Current_Timestamp;
   
   
   -----------------------
   -- Timestamp_Monitor --
   -----------------------
   
   task body Timestamp_Monitor is
      Alive: Boolean := True;
      TS   : Timestamp;
   begin
      while Alive loop
         select
            accept Kill do
               Alive := False;
            end Kill;
               
            or
               delay Update_Quantum;
               TS := Actual_Source;
               Current_Timestamp.Set (TS);
            
         end select;
      end loop;
   end Timestamp_Monitor;
   
end UUIDs.Version_1.Throttled_TS;
