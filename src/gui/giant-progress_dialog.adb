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
--  $RCSfile: giant-progress_dialog.adb,v $, $Revision: 1.15 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Interfaces.C.Strings;
with System;

with Glib.Object;
with Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Object;
with Gtk.Progress_Bar;
with Gtk.Stock;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.Types;

with Giant.Default_Dialog;
with Giant.Gui_Utils;

package body Giant.Progress_Dialog is

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => Interfaces.C.Strings.New_String ("cancelled"));

   package Progress_Dialog_Callback is new
     Gtk.Handlers.Callback (Progress_Dialog_Record);

   procedure On_Cancel_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Default_Dialog.Hide (Source, Default_Dialog.Response_Cancel);
   end On_Cancel_Button_Clicked;

   procedure Create
     (Dialog  :    out Progress_Dialog_Access;
      Title   : in     String;
      Message : in     String)

   is
   begin
      Dialog := new Progress_Dialog_Record;
      Initialize (Dialog, Title, Message);
   end Create;

   procedure Initialize
     (Dialog  : access Progress_Dialog_Record'Class;
      Title   : in     String;
      Message : in     String)
   is
      Box : Gtk.Box.Gtk_Vbox;
   begin
      Default_Dialog.Initialize (Dialog, Title, Default_Dialog.Button_None);
      Set_Modal (Dialog, True);

      --  provide signals
      Glib.Object.Initialize_Class_Record
        (Object       => Dialog,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "Progress_Dialog");

      Box := Get_Center_Box (Dialog);

      Gtk.Label.Gtk_New (Dialog.Progress_Label, Message);
      Gtk.Box.Pack_Start (Box, Dialog.Progress_Label, Expand => False,
                          Fill => False, Padding => Gui_Utils.DEFAULT_SPACING);

      Gtk.Adjustment.Gtk_New (Dialog.Progress_Bar_Adjustment,
                              Value => 0.0, Lower => 0.0,
                              Upper => 100.0, Step_Increment => 1.0,
                              Page_Increment => 1.0, Page_Size => 1.0);

      Dialog.Cancel_Button := Gui_Utils.New_Stock_Button
        (Gtk.Stock.Stock_Cancel,
         On_Cancel_Button_Clicked'Access,
         Dialog);
      Add_Button (Dialog, Dialog.Cancel_Button);

      Gtk.Progress_Bar.Gtk_New (Dialog.Progress_Bar);
      Gtk.Progress_Bar.Set_Adjustment
        (Dialog.Progress_Bar,
         Dialog.Progress_Bar_Adjustment);
      Gtk.Progress_Bar.Set_Pulse_Step (Dialog.Progress_Bar, Glib.Gdouble (0.01));
      Gtk.Box.Pack_Start (Box, Dialog.Progress_Bar, Expand => False,
                          Fill => False, Padding => Gui_Utils.DEFAULT_SPACING);
   end;

   function Can_Hide
     (Dialog : access Progress_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;

      -- the reason for closing the dialog
      Response : Default_Dialog.Response_Type;
   begin
      Response := Get_Response (Dialog);

      if (Get_Response (Dialog) = Default_Dialog.Response_Cancel) then
         -- the cancel button was pressed
         Progress_Dialog_Callback.Emit_By_Name (Dialog, "cancelled");
      end if;

      return False;
   end Can_Hide;

   function Get_Activity_Mode
     (Dialog        : access Progress_Dialog_Record)
     return Boolean
   is
   begin
      return Gtk.Progress_Bar.Get_Activity_Mode (Dialog.Progress_Bar);
   end Get_Activity_Mode;

   procedure Set_Activity_Mode
     (Dialog        : access Progress_Dialog_Record;
      Activity_Mode : in     Boolean)
   is
   begin
      Gtk.Progress_Bar.Set_Activity_Mode (Dialog.Progress_Bar, Activity_Mode);
   end Set_Activity_Mode;

   procedure Set_Cancel_Enabled
     (Dialog  : access Progress_Dialog_Record;
      Enabled : in     Boolean)
   is
   begin
      Gtk.Button.Set_Sensitive (Dialog.Cancel_Button, Enabled);
   end Set_Cancel_Enabled;

   procedure Set_Lower
     (Dialog : access Progress_Dialog_Record;
      Lower  : in     Glib.Gdouble)
   is
   begin
      Gtk.Adjustment.Set_Lower (Dialog.Progress_Bar_Adjustment,
                                Glib.Gdouble (Lower));
   end Set_Lower;

   procedure Set_Message
     (Dialog  : access Progress_Dialog_Record;
      Message : in     String)
   is
   begin
      Gtk.Label.Set_Text (Dialog.Progress_Label, Message);
   end Set_Message;

   procedure Set_Percentage
     (Dialog     : access Progress_Dialog_Record;
      Percentage : in     Glib.Gdouble)
   is
   begin
      Gtk.Progress_Bar.Set_Percentage (Dialog.Progress_Bar,
                                       Glib.Gdouble (Percentage));
   end Set_Percentage;

   procedure Set_Progress_Text
     (Dialog : access Progress_Dialog_Record;
      Text   : in     String)
   is
   begin
      Gtk.Progress_Bar.Set_Show_Text (Dialog.Progress_Bar, True);
      Gtk.Progress_Bar.Set_Format_String (Dialog.Progress_Bar, Text);
   end Set_Progress_Text;

   procedure Set_Upper (Dialog : access Progress_Dialog_Record;
                        Upper  : in     Glib.Gdouble)
   is
   begin
      Gtk.Adjustment.Set_Upper (Dialog.Progress_Bar_Adjustment,
                                Glib.Gdouble (Upper));
   end Set_Upper;

   procedure Set_Value (Dialog : access Progress_Dialog_Record;
                        Value  : in     Glib.Gdouble)
   is
      use type Glib.Gdouble;
      Upper_Bound : Glib.Gdouble;
      Mod_Value : Glib.Gdouble := Value;
   begin
      --  adjust value
      Upper_Bound := abs Gtk.Adjustment.Get_Upper
        (Dialog.Progress_Bar_Adjustment);
      if (Upper_Bound = 0.0) then
         Gtk.Adjustment.Set_Value (Dialog.Progress_Bar_Adjustment, 0.0);
      else
         -- map value to bounds
         while (Mod_Value > Upper_Bound) loop
            Mod_Value := Mod_Value - Upper_Bound;
         end loop;

         Gtk.Adjustment.Set_Value (Dialog.Progress_Bar_Adjustment, Mod_Value);
      end if;
   end Set_Value;

end Giant.Progress_Dialog;
