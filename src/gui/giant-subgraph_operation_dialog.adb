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
--  $RCSfile: giant-subgraph_operation_dialog.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Ada.Strings.Unbounded;

with Gtk.Combo;
with Gtk.Enums;
with Gtk.Gentry;

with String_Lists;

with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Projects;

package body Giant.Subgraph_Operation_Dialog is

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   function Can_Hide
     (Dialog : access Subgraph_Operation_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;
      use type Set_Operation_Dialog.Operation_Type;
	  
	  Operation : Set_Operation_Dialog.Operation_Type;
   begin
      if (Get_Response (Dialog) = Default_Dialog.Response_Okay
		  and then Validate (Dialog)) then
		 
		 Operation := Get_Operation (Dialog);
		 if (Operation = Set_Operation_Dialog.Difference) then
			Controller.Subgraph_Difference (Get_Left_Source (Dialog),
											Get_Right_Source (Dialog),
											Get_Target (Dialog));
		 elsif (Operation = Set_Operation_Dialog.Intersection) then
			Controller.Subgraph_Intersection (Get_Left_Source (Dialog),
											  Get_Right_Source (Dialog),
											  Get_Target (Dialog));
		 elsif (Operation = Set_Operation_Dialog.Union) then
			Controller.Subgraph_Union (Get_Left_Source (Dialog),
									   Get_Right_Source (Dialog),
									   Get_Target (Dialog));
		 end if;
	  end if;
	  
	  return True;
   exception
	 when Projects.Subgraph_Is_Not_Part_Of_Project_Exception =>
		Controller.Show_Error (-"Please select valid sources.");
		return False;
	 when Projects.Subgraph_Is_Already_Part_Of_Project_Exception =>
		Controller.Show_Error
		  (-"The target name is already used. Please try a different name.");
		return False;
   end Can_Hide;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog : out Subgraph_Operation_Dialog_Access)
   is
   begin
      Dialog := new Subgraph_Operation_Dialog_Record;
      Initialize (Dialog);
   end Create;

   function Get_Subgraphs
     return Gtk.Enums.String_List.Glist
   is
      Source : String_Lists.List;
      Target : Gtk.Enums.String_List.Glist;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Source := Projects.Get_All_Subgraphs (Controller.Get_Project);
      Iterator := String_Lists.MakeListIter (Source);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         Gtk.Enums.String_List.Append
		   (Target, Ada.Strings.Unbounded.To_String (Name));
      end loop;
      String_Lists.Destroy (Source);

      return Target;
   end;

   procedure Initialize
     (Dialog  : access Subgraph_Operation_Dialog_Record'Class)
   is
   begin
	  Set_Operation_Dialog.Initialize (Dialog, Get_Subgraphs);
   end;

end Giant.Subgraph_Operation_Dialog;

