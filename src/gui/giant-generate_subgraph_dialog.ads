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
--  $RCSfile: giant-generate_subgraph_dialog.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Giant.Default_Dialog;
with Giant.Gui_Utils;
with Giant.Graph_Lib.Subgraphs;

with Gtk.Gentry;

package Giant.Generate_Subgraph_Dialog is

   type Generate_Subgraph_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Generate_Subgraph_Dialog_Access is
      access all Generate_Subgraph_Dialog_Record'Class;

   procedure Show
     (The_Subgraph : in Graph_Lib.Subgraphs.Subgraph);

private
   type Generate_Subgraph_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
         The_Subgraph      : Graph_Lib.Subgraphs.Subgraph;
         Class_Set_List    : Gui_Utils.String_Clists.Giant_Data_Clist;
         New_Subgraph_Name : Gtk.Gentry.Gtk_Entry;
     end record;

end Giant.Generate_Subgraph_Dialog;
