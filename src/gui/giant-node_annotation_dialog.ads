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
--  $RCSfile: giant-node_annotation_dialog.ads,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Ada.Strings.Unbounded;

with Gtk.Text;

with Giant.Default_Dialog;
with Giant.Graph_Lib;

package Giant.Node_Annotation_Dialog is

   type Node_Annotation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Node_Annotation_Dialog_Access is
      access all Node_Annotation_Dialog_Record'Class;

   procedure Create
     (Dialog :    out Node_Annotation_Dialog_Access);

   procedure Initialize
     (Dialog : access Node_Annotation_Dialog_Record'Class);

   function Can_Hide
     (Dialog : access Node_Annotation_Dialog_Record)
     return Boolean;

   procedure Set_Node
     (Dialog : access Node_Annotation_Dialog_Record'Class;
      Node   : in     Graph_Lib.Node_Id);

   procedure Show
     (Node : in Graph_Lib.Node_Id);

private
   type Node_Annotation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Text_Area : Gtk.Text.Gtk_Text;
        Node : Graph_Lib.Node_Id;
     end record;

end Giant.Node_Annotation_Dialog;
