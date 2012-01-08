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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-basic_evolutions.adb,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with Glib;
with Gtk.Main;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);
with Giant.Progress_Dialog;
use type Giant.Progress_Dialog.Progress_Dialog_Access;

package body Giant.Basic_Evolutions is

   Update_Interval : constant := 1_000;

   package Logger is new Giant.Logger
     (Name => "Giant.Basic_Evolutions");

   package Basic_Evolution_Dialog_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type => Progress_Dialog.Progress_Dialog_Record,
      User_Type   => Basic_Evolution_Access);

   procedure Cancel_Callback
     (Dialog     : access Progress_Dialog.Progress_Dialog_Record'Class;
      Individual : in     Basic_Evolution_Access) is
   begin
      Individual.Cancelled := True;
   end Cancel_Callback;

   function Create
     (Dialog : in Progress_Dialog.Progress_Dialog_Access)
     return Basic_Evolution_Access
     is
      Individual : Basic_Evolution_Access;
   begin
      Individual := new Basic_Evolution;
      Individual.Dialog := Dialog;
      if (Dialog /= null) then
         Individual.Cancel_Handler :=
           Basic_Evolution_Dialog_Cb.Connect
           (Widget    => Dialog,
            Name      => "cancelled",
            Marsh     => Basic_Evolution_Dialog_Cb.To_Marshaller
            (Cancel_Callback'Access),
            User_Data => Individual);
         Progress_Dialog.Set_Activity_Mode (Dialog, True);
         Progress_Dialog.Show_All (Dialog);
      end if;
      return Individual;
   end Create;

   procedure Free is new Ada.Unchecked_Deallocation
     (Basic_Evolution, Basic_Evolution_Access);

   procedure Destroy
     (Individual : in out Basic_Evolution_Access)
   is
   begin
      if (Individual.Dialog /= null) then
         Progress_Dialog.Destroy (Individual.Dialog);
      end if;
      Free (Individual);
   end Destroy;

   function Iterate_Main
     (Individual   : in Basic_Evolution_Access;
      Force_Update : in Boolean)
     return Boolean
   is
     Dead : Boolean;
   begin
     if (Force_Update) then
        while Gtk.Main.Events_Pending loop
           Dead := Gtk.Main.Main_Iteration;
           exit when Dead;
        end loop;
     end if;

      return Individual.Cancelled;
   end Iterate_Main;

   function Should_Update
     (Individual   : in Basic_Evolution_Access)
     return Boolean
   is
     use type Ada.Real_Time.Time;
   begin
      if (Ada.Real_Time.Clock >= Individual.Last_Main_Iteration
          + Ada.Real_Time.Milliseconds (Update_Interval))
      then
         Individual.Last_Main_Iteration := Ada.Real_Time.Clock;
         return True;
      else
         return False;
      end if;
   end Should_Update;

   function Step
     (Individual       : in Basic_Evolution_Access;
      Delta_Complexity : in Natural                := 1)
      return Boolean
   is
      Update : Boolean;
   begin
      if (Individual = null) then
         return False;
      end if;

      Individual.Complexity := Individual.Complexity + Delta_Complexity;
      Update := Should_Update (Individual);
      if (Individual.Dialog /= null and then Update) then
         Progress_Dialog.Set_Value (Individual.Dialog,
                                    Glib.Gdouble (Individual.Complexity));
      end if;

      return Iterate_Main (Individual, Update);
   end Step;

   procedure Set_Cancel_Enabled
     (Individual : in Basic_Evolution_Access;
      Enabled    : in Boolean)
   is
   begin
      if (Individual = null) then
         return;
      end if;

      if (Individual.Dialog /= null) then
         Progress_Dialog.Set_Cancel_Enabled (Individual.Dialog, Enabled);
      end if;
   end Set_Cancel_Enabled;

   function Set_Percentage
     (Individual : in Basic_Evolution_Access;
      Percentage : in Float;
      Text       : in String := "")
     return Boolean
   is
   begin
      if (Individual = null) then
         return False;
      end if;

      if (Individual.Dialog /= null) then
         Progress_Dialog.Set_Percentage (Individual.Dialog,
                                         Glib.Gdouble (Percentage));
         Progress_Dialog.Set_Activity_Mode (Individual.Dialog, False);
         Progress_Dialog.Set_Progress_Text (Individual.Dialog, Text);
      end if;

      return Iterate_Main (Individual, Force_Update => True);
   end Set_Percentage;

   procedure Set_Text
     (Individual : in Basic_Evolution_Access;
      Text       : in String)
   is
   begin
      if (Individual = null) then
         return;
      end if;

      if (Individual.Dialog /= null) then
         Progress_Dialog.Set_Progress_Text (Individual.Dialog, Text);
      end if;
   end Set_Text;

   procedure Set_Total_Complexity
     (Individual       : in Basic_Evolution_Access;
      Total_Complexity : in     Natural)
   is
      Cancel : Boolean;
   begin
      if (Individual = null) then
         return;
      end if;

      Individual.Complexity := 0;
      if (Individual.Dialog /= null) then
         Progress_Dialog.Set_Upper (Individual.Dialog,
                                    Glib.Gdouble (Total_Complexity));
         Progress_Dialog.Set_Activity_Mode
           (Individual.Dialog, Total_Complexity = 0);
      end if;

      Cancel := Iterate_Main (Individual, Force_Update => True);
   end Set_Total_Complexity;

end Giant.Basic_Evolutions;
