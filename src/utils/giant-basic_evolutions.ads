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
--  $RCSfile: giant-basic_evolutions.ads,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  This package provides a basic framework for user-cancelable
--  operations. The main difference to the Giant.Evolutions package is
--  that the control flow remains within the operation.
--
--  The operation periodically needs to call Step() or Set_Percentage().
--
--  All procedures except for Create and Destroy can be called with
--  Individual = null. In that case no operation is performed.
--
--  See:
--    Giant.Evolutions

with Ada.Real_Time;

with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Giant.Progress_Dialog;

package Giant.Basic_Evolutions is

   type Basic_Evolution is private;

   type Basic_Evolution_Access is access Basic_Evolution;

   function Create
     (Dialog : in Progress_Dialog.Progress_Dialog_Access)
     return Basic_Evolution_Access;

   procedure Destroy
     (Individual : in out Basic_Evolution_Access);

   ---------------------------------------------------------------------------
   --  Advances the operation by Delta_Complexity. If enough time has
   --  gone by, the progress dialog is updated accordingly.
   --
   --  Parameters:
   --    Individual - Subject of the evolution process
   --    Delta_Complexity - The operation is advanced this much
   --  Returns:
   --    True, if the operation was cancelled
   --  See:
   --    Set_Total_Complexity
   function Step
     (Individual       : in Basic_Evolution_Access;
      Delta_Complexity : in Natural                := 1)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Enables or disables the cancel button.
   procedure Set_Cancel_Enabled
     (Individual : in Basic_Evolution_Access;
      Enabled    : in Boolean);

   ---------------------------------------------------------------------------
   --  Advances the progress to Percentage and updates the progress
   --  dialog.
   --
   --  Parameters:
   --    Individual - Subject of the evolution process
   --    Percentage - A value between 0.0 and 1.0.
   --    Text - The text to display on the progress bar.
   --  Returns:
   --    True, if the operation was cancelled
   function Set_Percentage
     (Individual : in Basic_Evolution_Access;
      Percentage : in Float;
      Text       : in String := "")
     return Boolean;

   ----------------------------------------------------------------------------
   --  Sets a format string to be displayed on the progress bar.
   --
   --  The following Texts can be replaced inside the String:
   --    %v - current complexity
   --    %u - total complexity
   --    %p - current progress percentage
   procedure Set_Text
     (Individual : in Basic_Evolution_Access;
      Text       : in String);

   ---------------------------------------------------------------------------
   --  Sets the total complexity of the operation. If complexity is
   --  unknown 0 should be passed to set the progress dialog to
   --  activity mode.
   --
   --  Parameters
   --    Individual - Subject of the evolution process
   --    Total_Complexity - Number of items processed when this evolution has
   --                       finished, or 0 if unknown
   procedure Set_Total_Complexity
     (Individual       : in Basic_Evolution_Access;
      Total_Complexity : in Natural);

private

   function Iterate_Main
     (Individual   : in Basic_Evolution_Access;
      Force_Update : in Boolean)
     return Boolean;

   type Basic_Evolution is
      record
         --  A monitoring dialog or null
         Dialog              : Progress_Dialog.Progress_Dialog_Access;
         Cancel_Handler      : Gtk.Handlers.Handler_Id;
         Cancelled           : Boolean := False;
         Complexity          : Natural := 0;
         Last_Main_Iteration : Ada.Real_Time.Time;
      end record;

end Giant.Basic_Evolutions;
