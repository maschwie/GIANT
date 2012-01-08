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
--  $RCSfile: giant-layout_dialog-actions.adb,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Giant.Controller;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Layout_Dialog.Actions is

   package Logger is new Giant.Logger("giant.layout_dialog.actions");

   ---------------------------------------------------------------------------
   --  Create Selection From Subgraph
   ---------------------------------------------------------------------------

   function Create
     (Dialog : Layout_Dialog_Access)
     return Set_Position_Action_Access
   is
      Action : Set_Position_Action_Access;
   begin
      Action := new Set_Position_Action_Type;
      Action.Dialog := Dialog;
      return Action;
   end Create;

   procedure Cancel
     (Action : access Set_Position_Action_Type)
   is
   begin
      Logger.Warn ("Destroying layout dialog.");
      Destroy (Action.Dialog);
   end Cancel;

   function Execute
     (Action   : access Set_Position_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
     return Boolean
   is
   begin
      Layout_Dialog.Apply_Layout (Action.Dialog, Event.Location);
      return True;
   end Execute;

end Giant.Layout_Dialog.Actions;
