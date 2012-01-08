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
--  $RCSfile: giant-gui_manager-actions.ads,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
--  Manages the actions that have global visiblity i.e. are pending
--  for all graph windows.
--

with Gdk.Event;

with Giant.Graph_Widgets;
with Giant.Graph_Widgets.Handlers;
with Giant.Graph_Window;
with Giant.Vis;

package Giant.Gui_Manager.Actions is

   procedure Set_Global_Action
     (Action : access Graph_Window.Actions.Graph_Window_Action_Type'Class);

   procedure Set_Local_Action
     (Window_Name : in     String;
      Action      : access Graph_Window.Actions.Graph_Window_Action_Type'Class);

   function Is_Action_Pending
     return Boolean;

   procedure Cancel;

   procedure Trigger
     (Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action);

private

   Pending_Action : Graph_Window.Actions.Graph_Window_Action_Access;

end Giant.Gui_Manager.Actions;

