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
--  $RCSfile: giant-main_window-actions.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
--  Provides the main window. The main window is only instanciated once.
--

with Gdk.Event;

with Giant.Graph_Window;
with Giant.Graph_Widgets;
with Giant.Graph_Widgets.Handlers;
with Giant.Vis;

package Giant.Main_Window.Actions is

   type Create_Selection_Action_Type (Name_Length : Positive) is
     new Graph_Window.Actions.Graph_Window_Action_Type with record
        Subgraph_Name : String(1 .. Name_Length);
     end record;

   type Create_Selection_Action_Access is
     access all Create_Selection_Action_Type'Class;

   function Create
     (Subgraph_Name : in String)
     return Create_Selection_Action_Access;

   procedure Cancel
     (Action : access Create_Selection_Action_Type);

   function Execute
     (Action   : access Create_Selection_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
     return Boolean;

end Giant.Main_Window.Actions;
