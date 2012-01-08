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
--  $RCSfile: giant-dialogs.adb,v $, $Revision: 1.11 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Gtk.Box;
with Gtk.Check_Button;
with Gtkada.Pixmaps;

with Giant.Config_Settings;

package body Giant.Dialogs is

   function Show_Delete_Confirmation_Dialog
     (Message : in String := -"Do you really want to delete?")
     return Boolean
   is
      use type Default_Dialog.Response_Type;

      Dialog : Default_Dialog.Default_Dialog_Access;
      Box : Gtk.Box.Gtk_Hbox;
      Check : Gtk.Check_Button.Gtk_Check_Button;
      Response : Default_Dialog.Response_Type;
   begin
      if (Config_Settings.Get_Setting_As_Boolean
           ("Confirm.Delete") = False) then
         return True;
      end if;

      Default_Dialog.Create (Dialog, -"Giant Delete Confirmation",
                             Default_Dialog.Button_Yes_No);
      Box := Default_Dialog.Add_Icon_Box
        (Dialog, Gtkada.Pixmaps.Confirmation_Xpm, Message);

      Gtk.Check_Button.Gtk_New
        (Check, -"Do not show delete confirmations again");
      Gtk.Box.Add (Default_Dialog.Get_Center_Box (Dialog), Check);

      Default_Dialog.Show_Modal (Dialog);

      --  save confirm setting
      Config_Settings.Set_Setting
        ("Confirm.Delete",
         Boolean'Image (not Gtk.Check_Button.Get_Active (Check)));

      Response := Default_Dialog.Get_Response (Dialog);
      Default_Dialog.Destroy (Dialog);

      return (Response = Default_Dialog.Response_Yes);
   end Show_Delete_Confirmation_Dialog;

   function Show_Confirmation_Dialog
     (Message : in String;
      Buttons : in Default_Dialog.Button_Type := Default_Dialog.Button_Yes_No)
     return Default_Dialog.Response_Type
   is
      Dialog : Default_Dialog.Default_Dialog_Access;
      Box : Gtk.Box.Gtk_Hbox;
      Response : Default_Dialog.Response_Type;
   begin
      Default_Dialog.Create (Dialog, -"Giant Question", Buttons);
      Box := Default_Dialog.Add_Icon_Box
        (Dialog, Gtkada.Pixmaps.Confirmation_Xpm, Message);

      Default_Dialog.Show_Modal (Dialog);

      Response := Default_Dialog.Get_Response (Dialog);
      Default_Dialog.Destroy (Dialog);

      return Response;
   end Show_Confirmation_Dialog;

   procedure Show_Error_Dialog
     (Message : in String;
      Title   : in String := -"Giant Error")
   is
      Dialog : Default_Dialog.Default_Dialog_Access;
      Box : Gtk.Box.Gtk_Hbox;
   begin
      Default_Dialog.Create (Dialog, Title, Default_Dialog.Button_Close);
      Box := Default_Dialog.Add_Icon_Box
        (Dialog, Gtkada.Pixmaps.Error_Xpm, Message);

      Default_Dialog.Show_Modal (Dialog);

      Default_Dialog.Destroy (Dialog);
   end Show_Error_Dialog;

   procedure Show_Info_Dialog
     (Message : in String;
      Title   : in String := -"Giant Information")
   is
      Dialog : Default_Dialog.Default_Dialog_Access;
      Box : Gtk.Box.Gtk_Hbox;
   begin
      Default_Dialog.Create (Dialog, Title, Default_Dialog.Button_Close);
      Box := Default_Dialog.Add_Icon_Box
        (Dialog, Gtkada.Pixmaps.Information_Xpm, Message);

      Default_Dialog.Show_Modal (Dialog);

      Default_Dialog.Destroy (Dialog);
   end Show_Info_Dialog;

   function Show_Input_Dialog
     (Message         : in String;
      Title           : in String                                   := -"Giant Input";
      Default_Input   : in String                                   := "";
      Input_Validator : in Gtk_Object_Input_Dialog.Input_Validator_Type := null)
      return String
   is
   begin
      return Gtk_Object_Input_Dialog.Show (Message, Title, Default_Input,
                                           Input_Validator, null);
   end Show_Input_Dialog;

end Giant.Dialogs;
