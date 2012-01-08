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
--  $RCSfile: giant-clists.adb,v $, $Revision: 1.11 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Ada.Strings.Unbounded;
with Interfaces.C.Strings;

with Glib; use type Glib.Gint;
with Gdk.Event;
with Gdk.Types;
with Gtk.Arguments;
with Gtk.Enums;
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.TearOff_Menu_Item;
with Gtk.Widget;
with Gtkada.Types;

package body Giant.Clists is

   package Clist_Callback is new
     Gtk.Handlers.Callback (Gtk.Clist.Gtk_Clist_Record);

   package Clist_Menu_Return_Callback is new
     Gtk.Handlers.User_Return_Callback (Gtk.Clist.Gtk_Clist_Record, Boolean,
                                        Gtk.Menu.Gtk_Menu);

   package Clist_Menu_Callback is new
     Gtk.Handlers.User_Callback (Gtk.Clist.Gtk_Clist_Record,
                                 Gtk.Menu.Gtk_Menu);

   Sensitive: Boolean;

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   procedure Set_Children_Sensitive
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      --  do not disable tear off items
      if (Widget.all
          in Gtk.TearOff_Menu_Item.Gtk_Tearoff_Menu_Item_Record) then
         return;
      end if;

      Gtk.Widget.Set_Sensitive (Widget, Sensitive);
   end;

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   function Select_Clicked_Row
     (Source : access Gtk.Clist.Gtk_Clist_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event)
     return Boolean
   is
      Row : Glib.Gint;
      Column : Glib.Gint;
      Is_Valid : Boolean;
   begin
      Gtk.Clist.Get_Selection_Info (Source,
                                    Glib.Gint (Gdk.Event.Get_X (Event)),
                                    Glib.Gint (Gdk.Event.Get_Y (Event)),
                                    Row, Column, Is_Valid);
      if (Is_Valid) then
         --  select clicked row
         Gtk.Clist.Select_Row (Source, Row, Column);
         return True;
      end if;
      return False;
   end Select_Clicked_Row;

   function On_Clist_Button_Press
     (Source : access Gtk.Clist.Gtk_Clist_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event;
      Menu   : in     Gtk.Menu.Gtk_Menu)
     return Boolean
   is
      use type Glib.Guint;
      use type Gdk.Event.Gdk_Event_Type;

      function Activate
        (Children : in Gtk.Widget.Widget_List.Glist;
         Index    : in Glib.Guint)
        return Boolean
      is
         use type Glib.Guint;
         Widget : Gtk.Widget.Gtk_Widget;
      begin
         if (Gtk.Widget.Widget_List.Length (Children) > Index) then
            Widget := Gtk.Widget.Widget_List.Nth_Data (Children, Index);
            if (Widget.all in Gtk.Menu_Item.Gtk_Menu_Item_Record) then
               Gtk.Menu_Item.Activate
                 (Gtk.Menu_Item.Gtk_Menu_Item (Widget));
               return True;
            end if;
         end if;
         return False;
      end;

   begin
      if Gdk.Event.Get_Button (Event) = 3
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Button_Press
      then
         --  right single click
         if (Select_Clicked_Row (Source, Event)) then
            --  show popup menu
            Gtk.Menu.Show_All (Menu);
            Gtk.Menu.Popup (Menu,
                            Button => Gdk.Event.Get_Button (Event),
                            Activate_Time => Gdk.Event.Get_Time (Event));
            return True;
         end if;
      elsif Gdk.Event.Get_Button (Event) = 1
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Gdk_2Button_Press
      then
         --  left double click
         if (Select_Clicked_Row (Source, Event)) then
            declare
               Children : Gtk.Widget.Widget_List.Glist;
               Activated : Boolean;
            begin
              Children := Gtk.Menu.Children (Menu);
              if (not Activate (Children, 0)) then
                 --  activate second popup menu item (first is tear off)
                 Activated := Activate (Children, 1);
              end if;
              Gtk.Widget.Widget_List.Free (Children);
              return True;
           end;
        end if;
      end if;

      return False;
   end On_Clist_Button_Press;

   procedure On_Clist_Click_Column
     (List   : access Gtk.Clist.Gtk_Clist_Record'Class;
      Args   : in     Gtk.Arguments.Gtk_Args)
   is
      Column : constant Glib.Gint := Gtk.Arguments.To_Gint (Args, 1);

      use type Glib.Gint;
      use type Gtk.Clist.Gtk_Sort_Type;
   begin
      if (Column = Gtk.Clist.Get_Sort_Column (List)
          and then Gtk.Clist.Get_Sort_Type (List)
          = Gtk.Clist.Ascending) then
         Gtk.Clist.Set_Sort_Type (List, Gtk.Clist.Descending);
      else
         Gtk.Clist.Set_Sort_Type (List, Gtk.Clist.Ascending);
      end if;

      Gtk.Clist.Set_Sort_Column (List, Column);
      Gtk.Clist.Sort (List);
   end On_Clist_Click_Column;

   procedure On_Clist_Select_Row
     (List : access Gtk.Clist.Gtk_Clist_Record'Class;
      Args : in     Gtk.Arguments.Gtk_Args;
      Menu : in     Gtk.Menu.Gtk_Menu)
   is
   begin
      Sensitive := True;
      Gtk.Menu.Forall (Menu, Set_Children_Sensitive'Access);
   end On_Clist_Select_Row;

   procedure On_Clist_Unselect_Row
     (List : access Gtk.Clist.Gtk_Clist_Record'Class;
      Args : in     Gtk.Arguments.Gtk_Args;
      Menu : in     Gtk.Menu.Gtk_Menu)
   is
   begin
      Sensitive := False;
      Gtk.Menu.Forall (Menu, Set_Children_Sensitive'Access);
   end On_Clist_Unselect_Row;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (List    :    out Giant_Clist;
      Columns : in     Glib.Gint)
   is
   begin
      List := new Giant_Clist_Record;
      Initialize (List, Columns);
   end Create;

   procedure Initialize
     (List    : access Giant_Clist_Record'Class;
      Columns : in     Glib.Gint)
   is
   begin
      Gtk.Clist.Initialize (List, Columns);

      Set_Selection_Mode (List, Gtk.Enums.Selection_Single);
      Set_Show_Titles (List, True);
      Column_Titles_Active (List);

      --  sort rows on column header click
      Clist_Callback.Connect
        (List, "click_column", On_Clist_Click_Column'Access);
   end;

   ---------------------------------------------------------------------------
   --  Public Methods
   ---------------------------------------------------------------------------

   function Get_Selected_Row
     (List : access Giant_Clist_Record)
     return Glib.Gint
   is
      use type Gtk.Enums.Gint_List.Glist;
      Selection : constant Gtk.Enums.Gint_List.Glist := Get_Selection (List);
   begin
      if Selection /= Gtk.Enums.Gint_List.Null_List then
         return Gtk.Enums.Gint_List.Get_Data
           (Gtk.Enums.Gint_List.First (Selection));
      end if;

      return -1;
   end Get_Selected_Row;

   procedure Columns_Autosize
     (List  : access Giant_Clist_Record)
   is
      Width : Glib.Gint;
   begin
      Width := Columns_Autosize (List);
   end Columns_Autosize;

   procedure Connect_Popup_Menu
     (List : access Giant_Clist_Record;
      Menu : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
   begin
      Clist_Menu_Return_Callback.Connect
        (List, "button_press_event",
         Clist_Menu_Return_Callback.To_Marshaller
         (On_Clist_Button_Press'Access), Gtk.Menu.Gtk_Menu (Menu));

      Clist_Menu_Callback.Connect
        (List, "select_row",
         On_Clist_Select_Row'Access, Gtk.Menu.Gtk_Menu (Menu));
      Clist_Menu_Callback.Connect
        (List, "unselect_row",
         On_Clist_Unselect_Row'Access, Gtk.Menu.Gtk_Menu (Menu));
   end Connect_Popup_Menu;

end Giant.Clists;
