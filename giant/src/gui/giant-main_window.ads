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
--  $RCSfile: giant-main_window.ads,v $, $Revision: 1.17 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
--  Provides the main window. The main window is only instanciated once.
--

with Gdk.Event;
with Gtk.Widget;
with Gtk.Window;

with Giant.Graph_Window;
with Giant.Gui_Manager;
with Giant.Gui_Utils;
with Giant.Vis;
with Giant.Vis_Windows;
with Giant.Valid_Names;

package Giant.Main_Window is

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record with private;

   type Main_Window_Access is access all Main_Window_Record'Class;

   ---------------------------------------------------------------------------
   --  Window Methods
   ---------------------------------------------------------------------------

   procedure Add_Window
     (Name : in String);

   procedure Update_Window
     (Name : in String);

   procedure Remove_Window
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Subgraphs
   ---------------------------------------------------------------------------
   procedure Add_Subgraph
     (Name : in String);

   procedure Update_Subgraph
     (Name : in String);

   procedure Remove_Subgraph
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Other Methods
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Called by listeners of the "can_close_project" signal to
   --  cancel the close operation.
   --
   --  This is a ugly workaround because it seems to be impossible to
   --  define custom signals that have a return value.
   procedure Cancel_Close_Project;

   function Close_Project
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Connect_Can_Close_Project
     (Callback : in     Gui_Utils.Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget   : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Connect_Close_Project
     (Callback : in     Gui_Utils.Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget   : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Sets windows visible.
   --
   procedure Show;

   ---------------------------------------------------------------------------
   --  Hides the application.
   --
   --  Return:
   --    True, if window was hidden.
   function Hide
     (Ask_For_Confirmation: Boolean)
     return Boolean;

   procedure Initialize_Project;

   procedure Set_Status
     (Text : in String);

   procedure Set_Graph_Filename
     (Text : in String);

   procedure Update_Column_Sizes;

private

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record
     with null record;

end Giant.Main_Window;
