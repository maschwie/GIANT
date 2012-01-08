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
--  $RCSfile: giant-progress_dialog.ads,v $, $Revision: 1.9 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
-- Provides a progress dialog.
--
-- Emits the "cancelled" callback when the cancel button is pressed.
--

with Glib;
with Gtk.Adjustment;
with Gtk.Button;
with Gtk.Label;
with Gtk.Progress_Bar;

with Gtk.Window;

with Giant.Default_Dialog;

package Giant.Progress_Dialog is

   type Progress_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Progress_Dialog_Access is access all Progress_Dialog_Record'Class;

   procedure Create
     (Dialog  :    out Progress_Dialog_Access;
      Title   : in     String;
      Message : in     String);

   procedure Initialize
     (Dialog  : access Progress_Dialog_Record'Class;
      Title   : in     String;
      Message : in     String);

   function Can_Hide
     (Dialog : access Progress_Dialog_Record)
     return Boolean;

   function Get_Activity_Mode
     (Dialog : access Progress_Dialog_Record)
     return Boolean;

   procedure Set_Activity_Mode
     (Dialog        : access Progress_Dialog_Record;
      Activity_Mode : in     Boolean);

   procedure Set_Cancel_Enabled
     (Dialog  : access Progress_Dialog_Record;
      Enabled : in     Boolean);

   procedure Set_Lower
     (Dialog : access Progress_Dialog_Record;
      Lower  : in     Glib.Gdouble);

   -------------------------------------------------------------------------
   --  Sets the message that is displayed above the progress bar.
   procedure Set_Message
     (Dialog  : access Progress_Dialog_Record;
      Message : in     String);

   procedure Set_Percentage
     (Dialog     : access Progress_Dialog_Record;
      Percentage : in     Glib.Gdouble);

   -------------------------------------------------------------------------
   --  Sets a format string used to display text indicating the
   --  current progress. The string can contain the following
   --  substitution characters:
   --
   --  %v - the current progress value.
   --  %l - the lower bound for the progress value.
   --  %u - the upper bound for the progress value.
   --  %p - the current progress percentage.
   --
   --  Text is only displayed in activity mode.
   procedure Set_Progress_Text
     (Dialog : access Progress_Dialog_Record;
      Text   : in     String);

   procedure Set_Upper (Dialog : access Progress_Dialog_Record;
                        Upper  : in     Glib.Gdouble);

   -------------------------------------------------------------------------
   --  Sets the current value. If value is higher than upper_bound,
   --  value mod upper_bound is set.
   procedure Set_Value (Dialog : access Progress_Dialog_Record;
                        Value  : in     Glib.Gdouble);

private
   type Progress_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Cancel_Button : Gtk.Button.Gtk_Button;
        Progress_Bar : Gtk.Progress_Bar.Gtk_Progress_Bar;
        Progress_Bar_Adjustment : Gtk.Adjustment.Gtk_Adjustment;
        Progress_Label : Gtk.Label.Gtk_Label;
     end record;

end Giant.Progress_Dialog;
