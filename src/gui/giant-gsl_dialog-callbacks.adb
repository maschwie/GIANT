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
--  $RCSfile: giant-gsl_dialog-callbacks.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Ada.Exceptions;

with Gtk.Text;

with Giant.Config_Settings;
with Giant.Controller;
with Giant.Dialogs;
with Giant.File_Management;
pragma Elaborate_All (Giant.File_Management);

package body Giant.Gsl_Dialog.Callbacks is

   ---------------------------------------------------------------------------
   --  File Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_File_Insert
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      Show_Open_Dialog (Dialog, Delete => False);
   end;

   procedure On_File_New
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      if (Save_Changes (Dialog)) then
         Set_Filename (Dialog, "");
         Gtk.Text.Delete_Text (Dialog.Text_Area);
         Dialog.Text_Has_Changed := False;
      end if;
   end;

   procedure On_File_Open
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      Show_Open_Dialog (Dialog, Delete => True);
   end;

   procedure On_File_Open_External
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      File_Management.Execute_External_Editor
        (Command  => Config_Settings.Get_Setting_As_String ("Editor.Source"),
         Filename => Get_Filename (Dialog),
         Line     => 0,
         Column   => 0);
   end;

   procedure On_File_Revert
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use type Default_Dialog.Response_Type;
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
      Response : Default_Dialog.Response_Type;
      Success : Boolean;
   begin
      if (Get_Filename (Dialog) /= "") then
         if (Dialog.Text_Has_Changed) then
            Response := Dialogs.Show_Confirmation_Dialog
              (-"The text was modified. Really revert?",
               Default_Dialog.Button_Yes_No);
            if (Response = Default_Dialog.Response_No) then
               --  do not revert
               return;
            end if;
         end if;

         Gtk.Text.Delete_Text (Dialog.Text_Area);
         Success := Read (Dialog, Get_Filename (Dialog));
      end if;
   end;

   procedure On_File_Save
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
      Success : Boolean;
   begin
      if (Get_Filename (Dialog) = "") then
         On_File_Save_As (Source);
      else
         Success := Write (Dialog, Get_Filename (Dialog));
      end if;
   end;

   procedure On_File_Save_As
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
      Success : Boolean;
   begin
      Success := Show_Save_As_Dialog (Dialog);
   end;

   ---------------------------------------------------------------------------
   --  Edit Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Edit_Clear
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      Gtk.Text.Delete_Text (Dialog.Text_Area);
   end;

   procedure On_Edit_Copy
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      Gtk.Text.Copy_Clipboard (Dialog.Text_Area, 0);
   end;

   procedure On_Edit_Cut
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      Gtk.Text.Cut_Clipboard (Dialog.Text_Area, 0);
   end;

   procedure On_Edit_Paste
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      Gtk.Text.Paste_Clipboard (Dialog.Text_Area, 0);
   end;

   ---------------------------------------------------------------------------
   --  Button Callbacks
   ---------------------------------------------------------------------------

   procedure On_Run_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
      Success : Boolean;
   begin
      if (Save_Changes (Dialog)) then
         if (Dialog.Filename
             = Ada.Strings.Unbounded.Null_Unbounded_String) then
            --  write to a temporary file
            Set_Filename (Dialog, Temp_Gsl_Filename);
            Success := Write
              (Dialog, Ada.Strings.Unbounded.To_String (Dialog.Filename));
         end if;

         declare
            Filename : String
              := Ada.Strings.Unbounded.To_String (Dialog.Filename);
         begin
            Controller.Execute_GSL (Filename);

            --  the script was executed successfully
            --  close the dialog?
         exception
           when E : others =>
              Dialogs.Show_Error_Dialog
                (-"Error during execution"
                 & " (" & Ada.Exceptions.Exception_Message (E)
                 & ").");
              -- FIX: if syntax error: jump to position
         end;
      end if;
   end On_Run_Button_Clicked;

   ---------------------------------------------------------------------------
   --  Other Callbacks
   ---------------------------------------------------------------------------

   function On_Focus
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event)
     return Boolean
   is
      use type Default_Dialog.Response_Type;
      use type File_Management.OS_Time;
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
      Filename : constant String := Get_Filename (Dialog);
      Response : Default_Dialog.Response_Type;
      Success : Boolean;
   begin
      if (Filename /= "" and then
          Dialog.Last_Save_Date
          /= File_Management.File_Time_Stamp (Filename)) then

         Response := Dialogs.Show_Confirmation_Dialog
           (-"The file has changed on disk. Revert?",
            Default_Dialog.Button_Yes_No);
         if (Response = Default_Dialog.Response_Yes) then
            Gtk.Text.Delete_Text (Dialog.Text_Area);
            Success := Read (Dialog, Get_Filename (Dialog));
         else
            --  do not bother user again
            Dialog.Last_Save_Date := File_Management.File_Time_Stamp (Filename);
         end if;
      end if;
      return False;
   end On_Focus;

end Giant.Gsl_Dialog.Callbacks;
