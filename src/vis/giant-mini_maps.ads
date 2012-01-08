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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-mini_maps.ads,v $, $Revision: 1.9 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  This package contains the mini map used to display the current position
--  and size of the visual area within a graph widget.
--


with Gdk.Color;
with Gtk.Drawing_Area;
with Gdk.GC;
with Gdk.Pixmap;
with Gtk.Handlers;
with Giant.Graph_Widgets;
with Giant.Vis;

package Giant.Mini_Maps is

   type Mini_Map_Record is new
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;

   type Mini_Map is access all Mini_Map_Record'Class;

   procedure Create
     (Widget  :    out Mini_Map;
      Watched : in     Graph_Widgets.Graph_Widget := null);

   procedure Initialize
     (Widget  : access Mini_Map_Record;
      Watched : in     Graph_Widgets.Graph_Widget);

   procedure Set_Graph_Widget
     (Widget  : access Mini_Map_Record;
      Watched : in     Graph_Widgets.Graph_Widget);

private

   procedure Update
     (Widget : access Mini_Map_Record);

   procedure Draw_Mini_Map
     (Widget : access Mini_Map_Record);

   Default_Width  : constant := 100;
   Default_Height : constant :=  70;

   --  must start at index 0, Mini_Map_Colors'Images (E) must be
   --  recognized by Gdk.Color.Parse for every E : Mini_Map_Colors
   type Mini_Map_Colors is (Black, Red, White);

   type Mini_Map_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         Watched              : Graph_Widgets.Graph_Widget := null;
         Logical_Area_Handler : Gtk.Handlers.Handler_Id;
         Visible_Area_Handler : Gtk.Handlers.Handler_Id;
         Transformation       : Vis.Transformation_Type;
         Buffer               : Gdk.Pixmap.Gdk_Pixmap      :=
                                  Gdk.Pixmap.Null_Pixmap;
         Polluted             : Boolean                    := True;
         Background_Gc        : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Logical_Area_Gc      : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Visible_Border_Gc    : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Visible_Fill_Gc      : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Colors               : Gdk.Color.Gdk_Color_Array
           (Mini_Map_Colors'Pos (Mini_Map_Colors'First) ..
            Mini_Map_Colors'Pos (Mini_Map_Colors'Last));
      end record;

end Giant.Mini_Maps;
