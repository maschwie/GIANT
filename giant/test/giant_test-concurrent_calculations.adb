-----------------------------------------------------------------------------
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License version 2 as
--  published by the Free Software Foundation.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
--  As a special exception, if you link this unit with the Bauhaus toolkit
--  to produce an executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
--  First Author: Steffen Keul
--
--  $RCSfile: giant_test-concurrent_calculations.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Giant_Test.Concurrent_Calculations is

   function Create
     (Calculation_Number : in Natural;
      Number_Of_Steps    : in Natural)
      return Counter_Access is

      Pointer : Counter_Access;
   begin
      Pointer := new Counter;
      Giant.Evolutions.Initialize
        (Giant.Evolutions.Evolution_Class_Access (Pointer),
         0); --  Number_Of_Steps);
      Pointer.Number := Calculation_Number;
      return Pointer;
   end Create;

   procedure Step
     (Individual  : access Counter;
      Next_Action :    out Giant.Evolutions.Evolution_Action) is

      J : Integer := 0;
   begin
      for I in 1 .. 1_000_000 loop
         J := (J + I) mod 17;
      end loop;
      if J > 0 then
         null;
      else
         null;
      end if;
      Giant.Evolutions.Advance_Progress (Individual, 1);
      if Giant.Evolutions.Get_Progress_Count (Individual) >=
        100 then --Giant.Evolutions.Get_Complexity (Individual) then

         Next_Action := Giant.Evolutions.Finish;
      else
         if Giant.Evolutions.Get_Progress_Count (Individual)
           mod (100 / 2 + 10) = 0 then

            Next_Action := Giant.Evolutions.Synchronize;
         else
            Next_Action := Giant.Evolutions.Run;
         end if;
      end if;
   end Step;

   procedure Synchronized_Step
     (Individual   : access Counter;
      Next_Action  :    out Giant.Evolutions.Evolution_Action) is
   begin
      Put_Line ("Now doing synchronized action.");
      Next_Action := Giant.Evolutions.Run;
   end Synchronized_Step;

   procedure Finish
     (Individual : access Counter;
      Canceled   : in     Boolean) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Counter, Counter_Access);

      Local_Copy : Counter_Access;
   begin
      Put ("Action" & Natural'Image (Individual.Number));
      if Canceled then
         Put_Line (" was canceled.");
      else
         Put_Line (" has finished.");
      end if;

      Local_Copy := Counter_Access (Individual);
      Free (Local_Copy);
   end Finish;

end Giant_Test.Concurrent_Calculations;
