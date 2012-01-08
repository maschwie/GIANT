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
--  $RCSfile: giant-selection_operation_dialog.adb,v $, $Revision: 1.3 $
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
with Giant.Vis_Windows;

package body Giant.Selection_Operation_Dialog is

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   function Can_Hide
     (Dialog : access Selection_Operation_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;
      use type Set_Operation_Dialog.Operation_Type;
	  
	  type Selection_Operation_Type is access procedure
		(Window_Name : in String;
		 Left		 : in String;
		 Right		 : in String;
		 Target_Name : in String);

	  Operation : Set_Operation_Dialog.Operation_Type;
	  Window_Name : constant String 
		:= Ada.Strings.Unbounded.To_String (Dialog.Window_Name);
	  
	  function Exists 
		(Selection_Name : in String)
		return Boolean
	  is
	  begin
		 return Controller.Exists_Selection (Window_Name, Selection_Name);
	  end Exists;
	  
	  procedure Execute
		(Operation_Procedure : in Selection_Operation_Type)
	  is
	  begin 
		 Operation_Procedure (Window_Name,
							  Get_Left_Source (Dialog),
							  Get_Right_Source (Dialog),
							  Get_Target (Dialog));
	  end Execute;
   begin
      if (Get_Response (Dialog) = Default_Dialog.Response_Okay
		  and then Validate (Dialog)) then
		 
		 if (not Exists (Get_Left_Source (Dialog))
			 or else not Exists (Get_Right_Source (Dialog))) then
			Controller.Show_Error (-"Please select valid sources.");
			return False;
		 end if;		
		 
		 if (Exists (Get_Target (Dialog))) then
			Controller.Show_Error
			  (-"The target name is already used. Please try a different name.");
			return False;
		 end if;
		 
		 Operation := Get_Operation (Dialog);
		 if (Operation = Set_Operation_Dialog.Difference) then
			Execute (Controller.Selection_Difference'Access);
		 elsif (Operation = Set_Operation_Dialog.Intersection) then
			Execute (Controller.Selection_Intersection'Access);
		 elsif (Operation = Set_Operation_Dialog.Union) then
			Execute (Controller.Selection_Union'Access);
		 end if;
	  end if;
	  
	  return True;
   end Can_Hide;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog	  :    out Selection_Operation_Dialog_Access;
	  Window_Name : in     String)
   is
   begin
      Dialog := new Selection_Operation_Dialog_Record;
      Initialize (Dialog, Window_Name);
   end Create;

   function Get_Selections
	 (Window_Name : in String)
     return Gtk.Enums.String_List.Glist
   is
      Source : String_Lists.List;
      Target : Gtk.Enums.String_List.Glist;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Source := Vis_Windows.Get_All_Selections
		(Controller.Get_Window (Window_Name));
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
     (Dialog	  : access Selection_Operation_Dialog_Record'Class;
	  Window_Name : in     String)
   is
   begin
	  Set_Operation_Dialog.Initialize (Dialog, Get_Selections (Window_Name));
	  
	  Dialog.Window_Name
		:= Ada.Strings.Unbounded.To_Unbounded_String (Window_Name);
   end;

end Giant.Selection_Operation_Dialog;

