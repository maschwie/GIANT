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
--  $RCSfile: giant-main_window-actions.adb,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--


with Giant.Controller;

package body Giant.Main_Window.Actions is

   ---------------------------------------------------------------------------
   --  Create Selection From Subgraph
   ---------------------------------------------------------------------------

   function Create
     (Subgraph_Name : in String)
     return Create_Selection_Action_Access
   is
      Action : Create_Selection_Action_Access;
   begin
      Action := new Create_Selection_Action_Type (Subgraph_Name'Length);
      Action.Subgraph_Name := Subgraph_Name;
      return Action;
   end Create;

   procedure Cancel
     (Action : access Create_Selection_Action_Type)
   is
   begin
      null;
   end Cancel;

   function Execute
     (Action   : access Create_Selection_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
     return Boolean
   is
   begin
      Controller.Create_Selection_From_Subgraph
        (Action.Subgraph_Name,
         Vis_Windows.Get_Name (Graph_Window.Get_Vis_Window (Window)),
         Action.Subgraph_Name);
      return True;
   end Execute;

end Giant.Main_Window.Actions;
