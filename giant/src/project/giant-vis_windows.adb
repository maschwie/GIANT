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
--  First Author: Martin Schwienbacher
--
--  $RCSfile: giant-vis_windows.adb,v $, $Revision: 1.39 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
with Ada.Unchecked_Deallocation;

with Unbounded_String_Hash; -- from Bauhaus IML "Reuse.src"

with Giant.Config;            -- from GIANT
with Giant.Config.Vis_Styles; -- from GIANT
with Giant.Graph_Lib;         -- from GIANT

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);


package body Giant.Vis_Windows is

   package Logger is new Giant.Logger("Giant.Vis_Windows");

   ---------------------------------------------------------------------------
   --  The default name used for standard selections
   --  (when creating a new project). The
   --  Name should correspond to Giant.Valid_Names.Standard_Name
   --  (only if used in GIANT). If you use this package separately
   --  you do not need a name that corresponds to Standard_Name.
   Standard_Selection_Name : constant String := "Default";


   ---------------------------------------------------------------------------
   --  0.1
   --  Internal subprograms
   ---------------------------------------------------------------------------

   --  needed for platform independent persistence
   ---------------------------------------------------------------------------
   procedure Selection_Data_Elemet_Read
     (Stream  : in  Bauhaus_IO.In_Stream_Type;
      Element : out Selection_Data_Elemet) is

      Highlight_Integer_Id : Integer;
   begin

      --  Read the Selection
      Graph_Lib.Selections.Selection_Read (Stream, Element.The_Selection);

      --  Read Highlight Status
      Bauhaus_IO.Read_Integer (Stream, Highlight_Integer_Id);
      Element.Highlight_Status :=
        Selection_Highlight_Status'Val (Highlight_Integer_Id);

      --  Read fading status
      Bauhaus_IO.Read_Boolean (Stream, Element.Is_Faded_Out);
   end Selection_Data_Elemet_Read;

   ---------
   procedure Selection_Data_Sets_Read is new Selection_Data_Sets.Read_Set
     (Read_Element => Selection_Data_Elemet_Read);


   --  needed for platform independent persistence
   ---------------------------------------------------------------------------
   procedure Selection_Data_Elemet_Write
     (Stream  : in Bauhaus_IO.Out_Stream_Type;
      Element : in Selection_Data_Elemet) is

      Highlight_Integer_Id : Integer;
   begin

      --  Stream the selection
      Graph_Lib.Selections.Selection_Write (Stream, Element.The_Selection);

      --  Stream the enumeration type via conversion to integer
      Highlight_Integer_Id :=
        Selection_Highlight_Status'Pos (Element.Highlight_Status);
      Bauhaus_IO.Write_Integer (Stream, Highlight_Integer_Id);

      -- Stream fading status flag
      Bauhaus_IO.Write_Boolean (Stream, Element.Is_Faded_Out);
   end Selection_Data_Elemet_Write;

   ---------
   procedure Selection_Data_Sets_Write is new Selection_Data_Sets.Write_Set
     (Write_Element => Selection_Data_Elemet_Write);


   --  needed for platform independent persistence
   ---------------------------------------------------------------------------
   procedure Pin_Read
     (Stream  : in  Bauhaus_IO.In_Stream_Type;
      Element : out Pin) is
   begin
      Bauhaus_IO.Read_Unbounded_String (Stream, Element.Pin_Name);
      Vis.Logic.Read_Vector (Stream, Element.Pin_Pos);
      Bauhaus_IO.Read_Float (Stream, Element.Pin_Zoom);
   end Pin_Read;

   ---------
   procedure Pin_Sets_Read is new Pin_Sets.Read_Set
     (Read_Element => Pin_Read);


   --  needed for platform independent persistence
   ---------------------------------------------------------------------------
   procedure Pin_Write
     (Stream  : in Bauhaus_IO.Out_Stream_Type;
      Element : in Pin) is
   begin
      Bauhaus_IO.Write_Unbounded_String (Stream, Element.Pin_Name);
      Vis.Logic.Write_Vector (Stream, Element.Pin_Pos);
      Bauhaus_IO.Write_Float (Stream, Element.Pin_Zoom);
   end Pin_Write;

   ---------
   procedure Pin_Sets_Write is new Pin_Sets.Write_Set
     (Write_Element => Pin_Write);

   ---------------------------------------------------------------------------
   -- for internal use
   procedure Internal_Set_Highlight_Status_Unchecked
     (Vis_Window           : in Visual_Window_Access;
      Selection_Name       : in String;
      New_Highlight_Status : in Selection_Highlight_Status) is

      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Change_Data_Element : Selection_Data_Elemet;
   begin

      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
      Dummy_Data_Element.The_Selection := Dummy_Selection;

      Change_Data_Element := Selection_Data_Sets.Get
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);

      Change_Data_Element.Highlight_Status := New_Highlight_Status;

      Selection_Data_Sets.Remove
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);

      Selection_Data_Sets.Insert
        (Vis_Window.All_Managed_Selections, Change_Data_Element);

      Graph_Lib.Selections.Destroy (Dummy_Selection);
   end Internal_Set_Highlight_Status_Unchecked;


   ---------------------------------------------------------------------------
   --  A
   --  Initialisation, Finalisation and Persistence
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Create_New
     (Vis_Window_Name : in String;
      Annotations     : in Node_Annotations.Node_Annotation_Access :=
        Node_Annotations.Create_Empty)
     return Visual_Window_Access is

      New_Window_Ac     : Visual_Window_Access;
      New_Standard_Sel  : Graph_Lib.Selections.Selection;
      New_Graph_Widget  : Graph_Widgets.Graph_Widget;
      Standard_Sel_Data : Selection_Data_Elemet;
   begin

      New_Window_Ac := new Visual_Window_Element;
      -- set name
      New_Window_Ac.Vis_Window_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name);

      -- set default visualisation style
      New_Window_Ac.The_Visualisation_Style :=
        Config.Vis_Styles.Get_Name_Of_Vis_Style
          (Config.Vis_Styles.Get_Default_Vis_Style);

      -- create empty pin set
      New_Window_Ac.Set_Of_All_Pins := Pin_Sets.Empty_Set;
      New_Window_Ac.All_Managed_Selections := Selection_Data_Sets.Empty_Set;

      --  Initialize new Graph_Widget
      Graph_Widgets.Create
        (New_Graph_Widget,
         Config.Vis_Styles.Get_Default_Vis_Style,
         Annotations);

      --  Increases the GTK Reference Counter - needed to keep the graph
      --  widget persistent in this data structure
      Graph_Widgets.Ref (New_Graph_Widget);

      New_Window_Ac.The_Graph_Widget := New_Graph_Widget;

      --  Create empty standard selection and make it to the current selection
      --  -> this behaviour is demanded by the Specification
      --  (see Spec 3.4.2. Standard-Selektion).
      New_Standard_Sel :=
        Graph_Lib.Selections.Create (Standard_Selection_Name);

      New_Window_Ac.Standard_Selection :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Graph_Lib.Selections.Get_Name (New_Standard_Sel));

      New_Window_Ac.Current_Selection :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Graph_Lib.Selections.Get_Name (New_Standard_Sel));

      --  Build management data for standard selection
      Standard_Sel_Data.The_Selection    := New_Standard_Sel;
      Standard_Sel_Data.Highlight_Status := Current_Selection;
      Standard_Sel_Data.Is_Faded_Out     := False;

      -- Insert standard selection
      Selection_Data_Sets.Insert
        (New_Window_Ac.All_Managed_Selections, Standard_Sel_Data);

      return New_Window_Ac;
   end Create_New;

   ---------------------------------------------------------------------------
   procedure Visual_Window_Access_Read
     (Stream      : in      Bauhaus_IO.In_Stream_Type;
      Vis_Window  :     out Visual_Window_Access;
      Annotations : in      Node_Annotations.Node_Annotation_Access :=
        Node_Annotations.Create_Empty) is
   begin

      Vis_Window := new Visual_Window_Element;

      --  Read Vis_Window_Name
      Bauhaus_IO.Read_Unbounded_String (Stream, Vis_Window.Vis_Window_Name);

      --  Read the_Visualisation_Style (name only)
      Bauhaus_IO.Read_Unbounded_String
        (Stream, Vis_Window.The_Visualisation_Style);

      Graph_Widgets.Read_Graph_Widget
        (Stream, Vis_Window.The_Graph_Widget, Config.Vis_Styles.Get_Default_Vis_Style, Annotations);
      --  Increases the GTK Reference Counter - needed to keep the graph
      --  widget persistent in this data structure
      Graph_Widgets.Ref (Vis_Window.The_Graph_Widget);

      --  Read the Set of all Pins
      Pin_Sets_Read (Stream, Vis_Window.Set_Of_All_Pins);

      --  Read the Standard_Selection
      Bauhaus_IO.Read_Unbounded_String
        (Stream, Vis_Window.Standard_Selection);

      --  Read the Current_Selection
      Bauhaus_IO.Read_Unbounded_String (Stream, Vis_Window.Current_Selection);

      --  Read the set of All_Managed_Selections
      Selection_Data_Sets_Read (Stream, Vis_Window.All_Managed_Selections);

      --  Check whether there is a visualisation style with the read name,
      --  else take the default vis style.
      if not Config.Vis_Styles.Does_Vis_Style_Exist
        (Ada.Strings.Unbounded.To_String
          (Vis_Window.The_Visualisation_Style)) then

        Vis_Window.The_Visualisation_Style :=
          Config.Vis_Styles.Get_Name_Of_Vis_Style
            (Config.Vis_Styles.Get_Default_Vis_Style);
      end if;
   end Visual_Window_Access_Read;

   ---------------------------------------------------------------------------
   procedure Visual_Window_Access_Write
     (Stream     : in Bauhaus_IO.Out_Stream_Type;
      Vis_Window : in Visual_Window_Access) is
   begin

      --  Write Vis_Window_Name
      Bauhaus_IO.Write_Unbounded_String (Stream, Vis_Window.Vis_Window_Name);

      --  Write the_Visualisation_Style (name only)
      Bauhaus_IO.Write_Unbounded_String
        (Stream, Vis_Window.The_Visualisation_Style);

      --  Write the_Graph_Widget
      Graph_Widgets.Write_Graph_Widget (Stream, Vis_Window.The_Graph_Widget);

      --  Write the Set of all Pins
      Pin_Sets_Write (Stream, Vis_Window.Set_Of_All_Pins);

      --  Write the Standard_Selection
      Bauhaus_IO.Write_Unbounded_String
        (Stream, Vis_Window.Standard_Selection);

      --  Write the Current_Selection
      Bauhaus_IO.Write_Unbounded_String (Stream, Vis_Window.Current_Selection);

      --  Write the set of All_Managed_Selections
      Selection_Data_Sets_Write (Stream, Vis_Window.All_Managed_Selections);
   end Visual_Window_Access_Write;

   ---------------------------------------------------------------------------
   procedure Deallocate_Vis_Window_Deep
     (Vis_Window : in out Visual_Window_Access) is

      --------------------------------------------
      --  Deallocates all selections that are part of the elements stored
      --  in a Selection_Data_Set.
      procedure Deallocate_All_Selections_Part_Of_Elements_In_Set
        (The_Set : in Selection_Data_Sets.Set) is

         Set_Iter : Selection_Data_Sets.Iterator;
         A_Selection_Data_Element : Selection_Data_Elemet;
      begin

         Set_Iter := Selection_Data_Sets.Make_Iterator (The_Set);

         while Selection_Data_Sets.More (Set_Iter) loop
          Selection_Data_Sets.Next (Set_Iter, A_Selection_Data_Element);
          Graph_Lib.Selections.Destroy
            (A_Selection_Data_Element.The_Selection);
         end loop;

         Selection_Data_Sets.Destroy (Set_Iter);
      end Deallocate_All_Selections_Part_Of_Elements_In_Set;

      ---------------------------------------------
      procedure Free_Visual_Window_Access is new Ada.Unchecked_Deallocation
        (Visual_Window_Element, Visual_Window_Access);

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      --  Decreases the GTK Reference Counter.
      --  The Graph widget will be deallocated automatically
      --  (only if there are no other references that have
      --  increased the Reference Counter).
      --  Graph_Widgets.Unref (Vis_Window.The_Graph_Widget);
      Graph_Widgets.Destroy (Vis_Window.The_Graph_Widget);

      Pin_Sets.Destroy (Vis_Window.Set_Of_All_Pins);

      -- deep deallocation of all selections managed by the set.
      Deallocate_All_Selections_Part_Of_Elements_In_Set
        (Vis_Window.All_Managed_Selections);

      Selection_Data_Sets.Destroy (Vis_Window.All_Managed_Selections);

      Free_Visual_Window_Access (Vis_Window);
   end Deallocate_Vis_Window_Deep;


   ---------------------------------------------------------------------------
   -- B
   -- General access on Visual_Window_Access
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Name
     (Vis_Window : in Visual_Window_Access)
     return String is
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded.To_String (Vis_Window.Vis_Window_Name);
   end Get_Name;

   ---------------------------------------------------------------------------

   procedure Change_Name
     (Vis_Window : in Visual_Window_Access;
      New_Name   : in String) is
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      Vis_Window.Vis_Window_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (New_Name);
   end Change_Name;

   ---------------------------------------------------------------------------
   function Is_Equal
     (Left  : in Visual_Window_Access;
      Right : in Visual_Window_Access)
     return Boolean is

   begin

      if (Left = null) or (Right = null) then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded."="
        (Left.Vis_Window_Name, Right.Vis_Window_Name);
   end Is_Equal;

   ---------------------------------------------------------------------------
   function Get_Hash_Value
     (Vis_Window  : in Visual_Window_Access)
     return Integer is

   begin

      if Vis_Window = null then
          raise Visual_Window_Access_Not_Initialized_Exception;
       end if;

      return Unbounded_String_Hash (Vis_Window.Vis_Window_Name);
   end Get_Hash_Value;

   ---------------------------------------------------------------------------
   function Get_Graph_Widget
     (Vis_Window : in Visual_Window_Access)
     return Graph_Widgets.Graph_Widget is

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      return Vis_Window.The_Graph_Widget;
   end Get_Graph_Widget;


   ---------------------------------------------------------------------------
   -- C
   -- Management of the Selections that belong to a visualisation window
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Selection_Exist
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Boolean is

     Dummy_Selection    : Graph_Lib.Selections.Selection;
     Dummy_Data_Element : Selection_Data_Elemet;
     Found              : Boolean := False;

   begin

       if Vis_Window = null then
          raise Visual_Window_Access_Not_Initialized_Exception;
       end if;

       Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
       Dummy_Data_Element.The_Selection := Dummy_Selection;

       if Selection_Data_Sets.Is_Member
         (Vis_Window.All_Managed_Selections, Dummy_Data_Element) then
          Found := True;
       end if;

       Graph_Lib.Selections.Destroy (Dummy_Selection);

       return Found;
   end Does_Selection_Exist;

   ---------------------------------------------------------------------------
   function Get_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
      return Graph_Lib.Selections.Selection is

      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Return_Data_Element : Selection_Data_Elemet;

   begin

       if Vis_Window = null then
          raise Visual_Window_Access_Not_Initialized_Exception;
       end if;

       if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
          raise Selection_With_Passed_Name_Not_Found_Exception;
       end if;

       Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
       Dummy_Data_Element.The_Selection := Dummy_Selection;

       Return_Data_Element := Selection_Data_Sets.Get
         (Vis_Window.All_Managed_Selections, Dummy_Data_Element);
       Graph_Lib.Selections.Destroy (Dummy_Selection);

       return Return_Data_Element.The_Selection;
   end Get_Selection;

   ---------------------------------------------------------------------------
   function Get_All_Selections
     (Vis_Window : in Visual_Window_Access)
     return String_Lists.List is

      The_List                 : String_Lists.List;
      Set_Iter                 : Selection_Data_Sets.Iterator;
      A_Selection_Data_Element : Selection_Data_Elemet;

   begin

       if Vis_Window = null then
          raise Visual_Window_Access_Not_Initialized_Exception;
       end if;

       The_List := String_Lists.Create;
       Set_Iter := Selection_Data_Sets.Make_Iterator
         (Vis_Window.All_Managed_Selections);

       -- append standard selection
       String_Lists.Attach (The_List, Vis_Window.Standard_Selection);

       while Selection_Data_Sets.More (Set_Iter) loop

          Selection_Data_Sets.Next (Set_Iter, A_Selection_Data_Element);

          -- do not append standard selection once again
          if not Ada.Strings.Unbounded."="
            (Graph_Lib.Selections.Get_Name
              (A_Selection_Data_Element.The_Selection),
             Vis_Window.Standard_Selection)
            then

            String_Lists.Attach
              (The_List, Ada.Strings.Unbounded.To_Unbounded_String
                (Graph_Lib.Selections.Get_Name
                  (A_Selection_Data_Element.The_Selection)));
           end if;
       end loop;

       Selection_Data_Sets.Destroy (Set_Iter);

       return The_List;
   end Get_All_Selections;

   ---------------------------------------------------------------------------
   procedure Change_Selection_Name
     (Vis_Window         : in Visual_Window_Access;
      Selection_Name     : in String;
      New_Selection_Name : in String) is

      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Change_Data_Element : Selection_Data_Elemet;
   begin
      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if Does_Selection_Exist (Vis_Window, New_Selection_Name) then
         raise New_Selection_Name_Does_Already_Exist_Exception;
      end if;

      if Ada.Strings.Unbounded."="
        (Vis_Window.Standard_Selection, Selection_Name) then

         raise Standard_Selection_Name_May_Not_Be_Changed_Exception;
      end if;

      -- update set
      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
      Dummy_Data_Element.The_Selection := Dummy_Selection;

      Change_Data_Element := Selection_Data_Sets.Get
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);

      Selection_Data_Sets.Remove
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);

      Graph_Lib.Selections.Destroy (Dummy_Selection);

      Graph_Lib.Selections.Rename
        (Change_Data_Element.The_Selection, New_Selection_Name);

      Selection_Data_Sets.Insert
        (Vis_Window.All_Managed_Selections, Change_Data_Element);

      -- change current selection entry if necessary
      if Ada.Strings.Unbounded."="
        (Vis_Window.Current_Selection, Selection_Name) then

         Vis_Window.Current_Selection :=
           Ada.Strings.Unbounded.To_Unbounded_String (New_Selection_Name);
      end if;
   end Change_Selection_Name;

   ---------------------------------------------------------------------------
   procedure Add_Selection
     (Vis_Window : in Visual_Window_Access;
      Selection  : in Graph_Lib.Selections.Selection) is

      New_Sel_Data_Element : Selection_Data_Elemet;

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Selection_Exist
        (Vis_Window,
         Graph_Lib.Selections.Get_Name (Selection)) = True) then
         raise Selection_Is_Already_Part_Of_Window_Exception;
      end if;

      -- Build management data for new selection
      New_Sel_Data_Element.The_Selection := Selection;
      New_Sel_Data_Element.Highlight_Status := None;
      New_Sel_Data_Element.Is_Faded_Out := False;

      Selection_Data_Sets.Insert
        (Vis_Window.All_Managed_Selections, New_Sel_Data_Element);
   end Add_Selection;

   ---------------------------------------------------------------------------
   procedure Remove_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String) is

     Dummy_Selection    : Graph_Lib.Selections.Selection;
     Dummy_Data_Element : Selection_Data_Elemet;

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;

      if Ada.Strings.Unbounded."="
        (Selection_Name, Vis_Window.Standard_Selection) then
         raise Standard_Selection_May_Not_Be_Removed_Exception;
      end if;

      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
      Dummy_Data_Element.The_Selection := Dummy_Selection;

      --  Must happen before removal.
      --
      --  On removal of current selection the standard selection becomes
      --  the current selection.
      if Ada.Strings.Unbounded."="
        (Selection_Name, Vis_Window.Current_Selection) then
         Set_Current_Selection
           (Vis_Window,
            Ada.Strings.Unbounded.To_String
             (Vis_Window.Standard_Selection));
      end if;

      Selection_Data_Sets.Remove
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);

      Graph_Lib.Selections.Destroy (Dummy_Selection);

      Logger.Debug
        ("Rem Selectio - Stand Sel: "
         & Ada.Strings.Unbounded.To_String (Vis_Window.Standard_Selection));
   end Remove_Selection;

   ---------------------------------------------------------------------------
   function Get_Current_Selection
     (Vis_Window : in Visual_Window_Access)
     return String is
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded.To_String (Vis_Window.Current_Selection);
   end Get_Current_Selection;

   ---------------------------------------------------------------------------
   -- Assumes that a current selection always exists
   procedure Set_Current_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String) is

      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Change_Data_Element : Selection_Data_Elemet;

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;

      if Is_Faded_Out (Vis_Window, Selection_Name) = True then
         raise Illegal_Current_Selection_Exception;
      end if;

      if Get_Highlight_Status (Vis_Window, Selection_Name) /= None then
         raise Illegal_Current_Selection_Exception;
      end if;

      --  reset highlight status of former current selection
      Internal_Set_Highlight_Status_Unchecked
        (Vis_Window,
         Ada.Strings.Unbounded.To_String (Vis_Window.Current_Selection),
         Selection_Highlight_Status'(None));

      Vis_Window.Current_Selection :=
        Ada.Strings.Unbounded.To_Unbounded_String (Selection_Name);

      --  update highlight status of current selection
      Internal_Set_Highlight_Status_Unchecked
        (Vis_Window,
         Selection_Name,
         Selection_Highlight_Status'(Current_Selection));

   end Set_Current_Selection;

   ---------------------------------------------------------------------------
   function Get_Standard_Selection
     (Vis_Window : in Visual_Window_Access)
     return String is
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded.To_String (Vis_Window.Standard_Selection);
   end Get_Standard_Selection;

   ---------------------------------------------------------------------------
   function Get_Highlight_Status
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Selection_Highlight_Status is

      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Return_Data_Element : Selection_Data_Elemet;

   begin

       if Vis_Window = null then
          raise Visual_Window_Access_Not_Initialized_Exception;
       end if;

       if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
          raise Selection_With_Passed_Name_Not_Found_Exception;
       end if;

       Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
       Dummy_Data_Element.The_Selection := Dummy_Selection;

       Return_Data_Element := Selection_Data_Sets.Get
         (Vis_Window.All_Managed_Selections, Dummy_Data_Element);

       Graph_Lib.Selections.Destroy (Dummy_Selection);

       return Return_Data_Element.Highlight_Status;
   end Get_Highlight_Status;

   ---------------------------------------------------------------------------
   function May_Highlight_Status_Be_Changed
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Boolean
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;

      return (Vis_Window.Current_Selection /= Selection_Name);
   end May_Highlight_Status_Be_Changed;

   ---------------------------------------------------------------------------
   procedure Set_Highlight_Status
     (Vis_Window           : in Visual_Window_Access;
      Selection_Name       : in String;
      New_Highlight_Status : in Changable_Highlight_Status) is

      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Change_Data_Element : Selection_Data_Elemet;
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;

      if not May_Highlight_Status_Be_Changed (Vis_Window, Selection_Name) then
         raise Highlight_Status_Of_Selection_May_Not_Be_Changed_Exception;
      end if;

      Internal_Set_Highlight_Status_Unchecked
        (Vis_Window, Selection_Name, New_Highlight_Status);
   end Set_Highlight_Status;


   --------------------------------------------------------------------------
   -- D
   -- Filters
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   function May_Be_Faded_Out
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Boolean is
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;

      -- check
      if Ada.Strings.Unbounded."="
        (Vis_Window.Current_Selection, Selection_Name) then

         return False;
      end if;

      if Ada.Strings.Unbounded."="
        (Vis_Window.Standard_Selection, Selection_Name) then

         return False;
      end if;

      return True;
   end May_Be_Faded_Out;

   --------------------------------------------------------------------------
   function Is_Faded_Out
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Boolean is

      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Return_Data_Element : Selection_Data_Elemet;

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;

      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
      Dummy_Data_Element.The_Selection := Dummy_Selection;

      Return_Data_Element := Selection_Data_Sets.Get
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);

      Graph_Lib.Selections.Destroy (Dummy_Selection);
      return Return_Data_Element.Is_Faded_Out;
   end Is_Faded_Out;

   --------------------------------------------------------------------------
   procedure Fade_Out_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String) is

      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Change_Data_Element : Selection_Data_Elemet;

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;

      if (May_Be_Faded_Out (Vis_Window, Selection_Name) = False) then
         raise Selection_May_Not_Be_Faded_Out_Exception;
      end if;

      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
      Dummy_Data_Element.The_Selection := Dummy_Selection;
      Change_Data_Element := Selection_Data_Sets.Get
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);
      Change_Data_Element.Is_Faded_Out := True;

      Selection_Data_Sets.Remove
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);
      Selection_Data_Sets.Insert
        (Vis_Window.All_Managed_Selections, Change_Data_Element);

      Graph_Lib.Selections.Destroy (Dummy_Selection);
   end Fade_Out_Selection;

   --------------------------------------------------------------------------
   procedure Fade_In_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String) is

      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Change_Data_Element : Selection_Data_Elemet;

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;

      if (Is_Faded_Out (Vis_Window, Selection_Name) = False) then
         raise Selection_Is_Not_Faded_Out_Exception;
      end if;

      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
      Dummy_Data_Element.The_Selection := Dummy_Selection;
      Change_Data_Element := Selection_Data_Sets.Get
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);
      Change_Data_Element.Is_Faded_Out := False;

      Selection_Data_Sets.Remove
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);
      Selection_Data_Sets.Insert
        (Vis_Window.All_Managed_Selections, Change_Data_Element);

      Graph_Lib.Selections.Destroy (Dummy_Selection);
   end Fade_In_Selection;


   --------------------------------------------------------------------------
   -- E
   -- Pin Management
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   function Does_Pin_Exist
     (Vis_Window : in Visual_Window_Access;
      Pin_Name   : in String)
     return Boolean is

      Dummy_Pin : Pin;
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      Dummy_Pin.Pin_Name := Ada.Strings.Unbounded.To_Unbounded_String
        (Pin_Name);

      return Pin_Sets.Is_Member
        (Vis_Window.Set_Of_All_Pins, Dummy_Pin);
   end Does_Pin_Exist;

   --------------------------------------------------------------------------
   function Get_Position
     (Vis_Window : in Visual_Window_Access;
      Pin_Name   : in String)
     return Vis.Logic.Vector_2d is

     Dummy_Pin  : Pin;
     Return_Pin : Pin;
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Pin_Exist (Vis_Window, Pin_Name) = False) then
         raise Pin_With_Passed_Name_Not_Found_Exception;
      end if;

      Dummy_Pin.Pin_Name := Ada.Strings.Unbounded.To_Unbounded_String
        (Pin_Name);

      Return_Pin := Pin_Sets.Get
        (Vis_Window.Set_Of_All_Pins, Dummy_Pin);

      return Return_Pin.Pin_Pos;
   end Get_Position;

   --------------------------------------------------------------------------
   function Get_Zoom
     (Vis_Window : in Visual_Window_Access;
      Pin_Name   : in String)
     return Vis.Zoom_Level is

     Dummy_Pin  : Pin;
     Return_Pin : Pin;
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Pin_Exist (Vis_Window, Pin_Name) = False) then
         raise Pin_With_Passed_Name_Not_Found_Exception;
      end if;

      Dummy_Pin.Pin_Name := Ada.Strings.Unbounded.To_Unbounded_String
        (Pin_Name);

      Return_Pin := Pin_Sets.Get
        (Vis_Window.Set_Of_All_Pins, Dummy_Pin);

      return Return_Pin.Pin_Zoom;
   end Get_Zoom;

   --------------------------------------------------------------------------
   function Get_All_Pins
     (Vis_Window : in Visual_Window_Access)
     return String_Lists.List is

      The_List     : String_Lists.List;
      Pin_Set_Iter : Pin_Sets.Iterator;
      A_Pin        : Pin;

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      The_List := String_Lists.Create;
      Pin_Set_Iter := Pin_Sets.Make_Iterator (Vis_Window.Set_Of_All_Pins);

      while Pin_Sets.More (Pin_Set_Iter) loop
         Pin_Sets.Next (Pin_Set_Iter, A_Pin);
         String_Lists.Attach (The_List, A_Pin.Pin_Name);
      end loop;

      Pin_Sets.Destroy (Pin_Set_Iter);

      return The_List;
   end Get_All_Pins;

   ---------------------------------------------------------------------------
   procedure Add_Pin
     (Vis_Window : in Visual_Window_Access;
      Name       : in String;
      Position   : in Vis.Logic.Vector_2d;
      Zoom_Level : in Vis.Zoom_Level) is

      New_Pin : Pin;
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Pin_Exist (Vis_Window, Name) = True) then
         raise Pin_Does_Already_Exist_Exception;
      end if;

      New_Pin.Pin_Name := Ada.Strings.Unbounded.To_Unbounded_String
        (Name);
      New_Pin.Pin_Pos  := Position;
      New_Pin.Pin_Zoom := Zoom_Level;

      Pin_Sets.Insert (Vis_Window.Set_Of_All_Pins, New_Pin);
   end Add_Pin;


   ---------------------------------------------------------------------------
   procedure Change_Pin_Name
      (Vis_Window   : in Visual_Window_Access;
       Pin_Name     : in String;
       New_Pin_Name : in String) is

      Change_Pin : Pin;
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if Does_Pin_Exist (Vis_Window, New_Pin_Name) then
         raise Pin_Does_Already_Exist_Exception;
      end if;

      Change_Pin.Pin_Name := Ada.Strings.Unbounded.To_Unbounded_String
        (Pin_Name);

      -- Get all data stored in Pin
      Change_Pin := Pin_Sets.Get (Vis_Window.Set_Of_All_Pins, Change_Pin);

      Pin_Sets.Remove (Vis_Window.Set_Of_All_Pins, Change_Pin);

      Change_Pin.Pin_Name := Ada.Strings.Unbounded.To_Unbounded_String
        (New_Pin_Name);

      Pin_Sets.Insert (Vis_Window.Set_Of_All_Pins, Change_Pin);
   end Change_Pin_Name;

   ---------------------------------------------------------------------------
   procedure Remove_Pin
     (Vis_Window : in Visual_Window_Access;
      Pin_Name   : in String) is

      Dummy_Pin  : Pin;
   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Does_Pin_Exist (Vis_Window, Pin_Name) = False) then
         raise Pin_With_Passed_Name_Not_Found_Exception;
      end if;

      Dummy_Pin.Pin_Name :=  Ada.Strings.Unbounded.To_Unbounded_String
        (Pin_Name);
      Pin_Sets.Remove (Vis_Window.Set_Of_All_Pins, Dummy_Pin);
   end Remove_Pin;


   ---------------------------------------------------------------------------
   -- F
   -- Visualisation Styles
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Vis_Style
     (Vis_Window : in Visual_Window_Access)
     return String is

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded.To_String
        (Vis_Window.The_Visualisation_Style);
   end Get_Vis_Style;

   ---------------------------------------------------------------------------
   procedure Set_Vis_Style
     (Vis_Window     : in Visual_Window_Access;
      Vis_Style_Name : in String) is

   begin

      if Vis_Window = null then
         raise Visual_Window_Access_Not_Initialized_Exception;
      end if;

      if (Config.Vis_Styles.Does_Vis_Style_Exist (Vis_Style_Name) = False)
        then
         raise Vis_Style_Does_Not_Exist_Exception;
      end if;

      Vis_Window.The_Visualisation_Style :=
        Ada.Strings.Unbounded.To_Unbounded_String (Vis_Style_Name);
   end Set_Vis_Style;


   ---------------------------------------------------------------------------
   --  Implementation of subprograms from the private part of the
   --  specification (*.ads file) of this package.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Pin_Equal (Left : in Pin; Right : in Pin) return Boolean is
   begin

     return Ada.Strings.Unbounded."=" (Left.Pin_Name, Right.Pin_Name);
   end Pin_Equal;

   ---------------------------------------------------------------------------
   function Pin_Less_Than (Left : in Pin; Right : in Pin) return Boolean is
   begin

     return Ada.Strings.Unbounded."<" (Left.Pin_Name, Right.Pin_Name);
   end Pin_Less_Than;

   ---------------------------------------------------------------------------
   function Selection_Data_Equal
     (Left  : in Selection_Data_Elemet;
      Right : in Selection_Data_Elemet)
     return Boolean is
   begin

      return (Graph_Lib.Selections.Get_Name (Left.The_Selection) =
        Graph_Lib.Selections.Get_Name (Right.The_Selection));
   end Selection_Data_Equal;

   ---------------------------------------------------------------------------
   function Selection_Data_Less_Than
     (Left  : in Selection_Data_Elemet;
      Right : in Selection_Data_Elemet)
     return Boolean is
   begin

     return (Graph_Lib.Selections.Get_Name (Left.The_Selection) <
        Graph_Lib.Selections.Get_Name (Right.The_Selection));
   end Selection_Data_Less_Than;

end Giant.Vis_Windows;



