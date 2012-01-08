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
--  $RCSfile: giant-menu_factory.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Ada.Strings.Unbounded;

with Gtk.Handlers;
with Gtk.Menu_Item;

with String_Lists;

with Giant.Gui_Utils;
with Giant.String_Split;

package body Giant.Menu_Factory is

   procedure Generate
     (Labels    : in     String;
      Separator : in     String;
      Menu      : in     Gtk.Menu.Gtk_Menu;
      Callback  : in     Widget_User_Callback.Marshallers.Void_Marshaller.Handler;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Item : Ada.Strings.Unbounded.Unbounded_String;
      Data : Script_Event_Access;
      Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      List := String_Split.Split_String (Labels, Separator);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Item);

         declare
            Label : constant String
              := Ada.Strings.Unbounded.To_String (Item);
         begin
            if (Label = "-") then
               Gtk.Menu.Append (Menu, Gui_Utils.New_Menu_Separator);
            else
               Data := new Script_Event(Label'Length);
               Data.Widget := Gtk.Widget.Gtk_Widget (Widget);
               Data.Label := Label;

               Gtk.Menu_Item.Gtk_New (Menu_Item, Data.Label);
               Widget_User_Callback.Connect
                 (Menu_Item,
                  "activate",
                  Widget_User_Callback.Marshallers.Void_Marshaller.To_Marshaller
                  (Callback),
                  Data.all);

               Gtk.Menu.Append (Menu, Menu_Item);
            end if;
         end;
      end loop;
      String_Lists.Destroy (List);
   end Generate;

end Giant.Menu_Factory;
