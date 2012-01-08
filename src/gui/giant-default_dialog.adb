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
--  $RCSfile: giant-default_dialog.adb,v $, $Revision: 1.24 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded;

with Glib;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Gentry;
with Gtk.Label;
with Gtk.Main;
with Gtk.Pixmap;
with Gtk.Separator;
with Gtk.Stock;
with Gtkada.Types;

with Giant.Gui_Utils;
with Giant.Main_Window;
--with Giant.Utils; use Giant.Utils;

package body Giant.Default_Dialog is

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   procedure On_Cancel_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Hide (Source, Response_Cancel);
   end On_Cancel_Button_Clicked;

   procedure On_Close_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Hide (Source, Response_Close);
   end On_Close_Button_Clicked;

   function On_Delete
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Boolean
   is
   begin
      Hide (Source, Response_Close);
      return True;
   end On_Delete;

   procedure On_No_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Hide (Source, Response_No);
   end On_No_Button_Clicked;

   procedure On_Okay_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Hide (Source, Response_Okay);
   end On_Okay_Button_Clicked;

   procedure On_Yes_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Hide (Source, Response_Yes);
   end On_Yes_Button_Clicked;

   procedure On_Close_Project
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : Default_Dialog_Access
        := Default_Dialog_Access (Source);
   begin
      Hide (Dialog);
   end On_Close_Project;

   function Can_Hide
     (Dialog : access Default_Dialog_Record)
     return Boolean
   is
   begin
      return True;
   end Can_Hide;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog  :    out Default_Dialog_Access;
      Title   : in     String;
      Buttons : in     Button_Type)
   is
   begin
      Dialog := new Default_Dialog_Record;
      Initialize (Dialog, Title, Buttons);
   end Create;

   procedure Initialize
     (Dialog  : access Default_Dialog_Record'class;
      Title   : in     String;
      Buttons : in     Button_Type)
   is
      use Gui_Utils;

      Button : Gtk.Button.Gtk_Button;
   begin
      Gtk.Window.Initialize (Dialog, Window_Toplevel);
      Set_Title (Dialog, Title);

      --  center box
      Gtk.Box.Gtk_New_Vbox (Dialog.Center_Box);
      Gtk.Box.Set_Border_Width (Dialog.Center_Box, DEFAULT_SPACING);
      Add (Dialog, Dialog.Center_Box);

      --  button box
      Gtk.Hbutton_Box.Gtk_New (Dialog.Button_Box);
      Gtk.Hbutton_Box.Set_Spacing (Dialog.Button_Box, BUTTON_SPACING);
      Gtk.Hbutton_Box.Set_Layout (Dialog.Button_Box, Buttonbox_Spread);
      Gtk.Box.Pack_End (Dialog.Center_Box, Dialog.Button_Box,
                        Expand => False, Fill => True,
                        Padding => DEFAULT_SPACING);

      --  buttons
      if (Buttons = Button_Close) then
         Button := New_Stock_Button
           (Gtk.Stock.Stock_Close,
            On_Close_Button_Clicked'access);
         Add_Button (Dialog, Button, False);
         Gtk.Button.Grab_Default (Button);
      elsif (Buttons = Button_Okay
             or else Buttons = Button_Okay_Cancel) then
         Button := New_Stock_Button
           (Gtk.Stock.Stock_Ok,
            On_Okay_Button_Clicked'access);
         Add_Button (Dialog, Button, False);
         Gtk.Button.Grab_Default (Button);
      elsif (Buttons = Button_Yes_No
             or else Buttons = Button_Yes_No_Cancel) then
         Button := New_Stock_Button
           (Gtk.Stock.Stock_Yes,
            On_Yes_Button_Clicked'access);
         Add_Button (Dialog, Button, False);
         Gtk.Button.Grab_Default (Button);
         Add_Button (Dialog,
                     New_Stock_Button (Gtk.Stock.Stock_No,
                                       On_No_Button_Clicked'access),
                     False);
      end if;

      if (Buttons = Button_Cancel
          or else Buttons = Button_Okay_Cancel
          or else Buttons = Button_Yes_No_Cancel) then
         Add_Button (Dialog,
                     New_Stock_Button (Gtk.Stock.Stock_Cancel,
                                       On_Cancel_Button_Clicked'access),
                     False);
      end if;

      --  horizontal separator
      Gtk.Box.Pack_End (Dialog.Center_Box, New_Hseperator, Expand => False,
                        Fill => True, Padding => DEFAULT_SPACING);

      --  set position
      Set_Position (Dialog, Win_Pos_Mouse);

      --  connect close button
      Widget_Boolean_Callback.Connect
        (Dialog, "delete_event",
         Widget_Boolean_Callback.To_Marshaller (On_Delete'Access));

      --  connect close project
      Main_Window.Connect_Close_Project (On_Close_Project'Access, Dialog);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Public Methods
   ---------------------------------------------------------------------------

   procedure Add_Button
     (Dialog   : access Default_Dialog_Record;
      Button   : in     Gtk.Button.Gtk_Button;
      Add_Left : in     Boolean               := True)
   is
   begin
      Gtk.Hbutton_Box.Add (Dialog.Button_Box, Button);
      if (Add_Left) then
         --  the button box seems to ignore pack_end and pack_start
         --  therefore reordering is used as a workaround
         Gtk.Hbutton_Box.Reorder_Child (Dialog.Button_Box, Button, 0);
      end if;
   end;

   function Add_Icon_Box
     (Dialog        : access Default_Dialog_Record;
      Pixmap        : in     Gtk.Pixmap.Gtk_Pixmap;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Box.Gtk_Hbox
   is
      Box : Gtk.Box.Gtk_Hbox;
   begin
      Gtk.Box.Gtk_New_Hbox (Box);
      Set_Center_Widget (Dialog, Box);

      --  icon
      --Gtk.Pixmap.Set_Alignment (Dialog.Confirmation_Msg_Pixmap, 0.5, 0.5);
      Gtk.Box.Pack_Start (Box, pixmap, expand => False, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);

      --  widget
      Gtk.Box.Pack_Start (Box, Widget, Expand => True, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);

      return Box;
   end Add_Icon_Box;

   function Add_Icon_Box
     (Dialog        : access Default_Dialog_Record;
      Icon_Filename : in     String;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Box.Gtk_Hbox
   is
   begin
      return Add_Icon_Box (Dialog, Gtk.Pixmap.Create_Pixmap
                           (Gui_Utils.Get_Icon (Icon_Filename), Dialog), Widget);
   end Add_Icon_Box;

   function Add_Icon_Box
     (Dialog : access Default_Dialog_Record;
      Pixmap : in     Gtkada.Types.Chars_Ptr_Array;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Box.Gtk_Hbox
   is
   begin
      return Add_Icon_Box (Dialog, Gtk.Pixmap.Create_Pixmap
                           (Pixmap, Dialog), Widget);
   end Add_Icon_Box;

   function Add_Icon_Box
     (Dialog  : access Default_Dialog_Record;
      Pixmap  : in     Gtkada.Types.Chars_Ptr_Array;
      Message : in     String                       := "")
     return Gtk.Box.Gtk_Hbox
   is
      Label : Gtk.Label.Gtk_Label;
   begin
      Gtk.Label.Gtk_New (Label, Message);
      --Set_Alignment (Dialog.Confirmation_Message, 0.5, 0.5);
      Gtk.Label.Set_Justify (Label, Justify_Center);
      Gtk.Label.Set_Line_Wrap (Label, False);

      return Add_Icon_Box (Dialog, Pixmap, Label);
   end Add_Icon_Box;

   function Get_Center_Box
     (Dialog : access Default_Dialog_Record)
     return Gtk.Box.Gtk_Vbox
   is
   begin
      return Dialog.Center_Box;
   end Get_Center_Box;

   function Get_Response
     (Dialog : access Default_Dialog_Record)
     return Response_Type
   is
   begin
      return Dialog.Response;
   end Get_Response;

   procedure Hide
     (Dialog : access Default_Dialog_Record)
   is
   begin
      if (Dialog.Is_Modal) then
         Gtk.Main.Main_Quit;
      else
         Destroy (Dialog);
      end if;
   end;

   procedure Hide
     (Source   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Response : in     Response_Type)
   is
      Dialog : Default_Dialog_Access
        := Default_Dialog_Access (Gtk.Widget.Get_Toplevel (Source));
   begin
      Dialog.Response := Response;

      if (Can_Hide (Default_Dialog_Access (Dialog))) then
         Hide (Dialog);
      end if;
   end;

   function Is_Modal
     (Dialog : access Default_Dialog_Record)
     return Boolean
   is
   begin
      return Dialog.Is_Modal;
   end;

   function Is_Response_Okay
     (Dialog : access Default_Dialog_Record)
     return Boolean
   is
   begin
      return (Dialog.Response = Response_Okay);
   end Is_Response_Okay;

   procedure Set_Center_Widget
     (Dialog : access Default_Dialog_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Gtk.Box.Pack_Start (Dialog.Center_Box, Widget,
                          Expand => True, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);
   end Set_Center_Widget;

   procedure Show_Modal
     (Dialog : access Default_Dialog_Record)
   is
   begin
      Dialog.Is_Modal := True;
      Set_Modal (Dialog, Dialog.Is_Modal);

      Show_All (Dialog);
      Gtk.Main.Main;
   end Show_Modal;

end Giant.Default_Dialog;
