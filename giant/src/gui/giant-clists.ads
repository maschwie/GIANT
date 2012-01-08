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
--  $RCSfile: giant-clists.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
--  Provides an enhanced Gtk.Clist.
--
--  Pattern:
--    ADT
--

with Glib;
with Gtk.Clist;
with Gtk.Menu;

package Giant.Clists is

   type Giant_Clist_Record is new Gtk.Clist.Gtk_Clist_Record with private;

   type Giant_Clist is access all Giant_Clist_Record'Class;

   procedure Create
     (List    :    out Giant_Clist;
      Columns : in     Glib.Gint);

   procedure Initialize
     (List    : access Giant_Clist_Record'Class;
      Columns : in     Glib.Gint);

   procedure Columns_Autosize
     (List  : access Giant_Clist_Record);

   procedure Connect_Popup_Menu
     (List : access Giant_Clist_Record;
      Menu : access Gtk.Menu.Gtk_Menu_Record'Class);

   function Get_Selected_Row
     (List : access Giant_Clist_Record)
     return Glib.Gint;

private

   type Giant_Clist_Record is new Gtk.Clist.Gtk_Clist_Record with null
      record;

end Giant.Clists;
