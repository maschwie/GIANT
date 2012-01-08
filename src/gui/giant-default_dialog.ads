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
--  $RCSfile: giant-default_dialog.ads,v $, $Revision: 1.15 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
------------------------------------------------------------------------------
--
-- Provides a default dialog with a content area and a button panel.
--

with Gtk.Box;
with Gtk.Button;
with Gtk.Hbutton_Box;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.Types;

package Giant.Default_Dialog is

   type Input_Callback is access function (S: in String) return Boolean;

   type Response_Type is
      (Response_Okay, Response_Cancel, Response_Close, Response_Yes,
       Response_No);

   type Button_Type is
     (Button_None, Button_Cancel, Button_Close, Button_Okay,
      Button_Okay_Cancel, Button_Yes_No, Button_Yes_No_Cancel);

   type Default_Dialog_Record is new Gtk.Window.Gtk_Window_Record with private;

   type Default_Dialog_Access is access all Default_Dialog_Record'Class;

   procedure Create
     (Dialog  :    out Default_Dialog_Access;
      Title   : in     String;
      Buttons : in     Button_Type);

   procedure Initialize
     (Dialog  : access Default_Dialog_Record'class;
      Title   : in     String;
      Buttons : in     Button_Type);

   procedure Add_Button
     (Dialog   : access Default_Dialog_Record;
      Button   : in     Gtk.Button.Gtk_Button;
      Add_Left : in     Boolean               := True);


   function Add_Icon_Box
     (Dialog        : access Default_Dialog_Record;
      Icon_Filename : in     String;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Box.Gtk_Hbox;

   function Add_Icon_Box
     (Dialog : access Default_Dialog_Record;
      Pixmap : in     Gtkada.Types.Chars_Ptr_Array;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Box.Gtk_Hbox;

   ---------------------------------------------------------------------------
   --  Invoked by Giant.Dialogs to create the default dialogs.
   --
   --  The message is displayed in a Gtk_Label.
   --
   --  Returns:
   --    The created box.
   function Add_Icon_Box
     (Dialog  : access Default_Dialog_Record;
      Pixmap  : in     Gtkada.Types.Chars_Ptr_Array;
      Message : in     String                       := "")
     return Gtk.Box.Gtk_Hbox;

   ---------------------------------------------------------------------------
   --  Called if one of the default buttons is pressed.
   --
   --  Sub-classes can overwrite this method.
   --
   --  Returns:
   --    True
   function Can_Hide
     (Dialog : access Default_Dialog_Record)
     return Boolean;

   function Get_Center_Box
     (Dialog : access Default_Dialog_Record)
     return Gtk.Box.Gtk_Vbox;

   function Get_Response
     (Dialog : access Default_Dialog_Record)
     return Response_Type;

   ---------------------------------------------------------------------------
   --  If dialog is not modal, the dialog is destroyed.
   --
   procedure Hide
     (Dialog : access Default_Dialog_Record);

   ---------------------------------------------------------------------------
   --  Called by the button callbacks.
   procedure Hide
     (Source   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Response : in     Response_Type);

   ---------------------------------------------------------------------------
   --  Returns true, if the dialog runs in a separate gtk event loop.
   function Is_Modal
     (Dialog : access Default_Dialog_Record)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Returns true, if the okay button was pressed.
   function Is_Response_Okay
     (Dialog : access Default_Dialog_Record)
     return Boolean;

   procedure Set_Center_Widget
     (Dialog : access Default_Dialog_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Show_Modal
     (Dialog : access Default_Dialog_Record);

private

   type Default_Dialog_Record is new Gtk.Window.Gtk_Window_Record with record
      Center_Box : Gtk.Box.Gtk_Vbox;
      Button_Box : Gtk.Hbutton_Box.Gtk_Hbutton_Box;
      Response : Response_Type;
      Is_Modal : Boolean := False;
   end record;

end Giant.Default_Dialog;
