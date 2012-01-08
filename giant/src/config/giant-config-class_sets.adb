------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
-- Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- First Author: Martin Schwienbacher
--
-- $RCSfile: giant-config-class_sets.adb,v $, $Revision: 1.15 $
-- $Author: koppor $
-- $Date: 2003-10-06 17:44:30 $
--

--  from GIANT
with Giant.File_Management;
with Giant.XML_File_Access;
with Giant.Edge_Class_Proc;
with Giant.IML_Class_Inheritance_Proc;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

-- from Bauhaus IML "Reuse.src"
with Unbounded_String_Hash;

-- from xmlada
with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with Tree_Readers;

package body Giant.Config.Class_Sets is

   package Logger is new Giant.Logger("Giant.Config.Class_Sets");

   ---------------------------------------------------------------------------
   -- 0.1
   -- The internal data structure of the ADO
   ---------------------------------------------------------------------------

   -- used to organize all known class sets
   package Class_Sets_Hashed_Mappings is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Hash       => Unbounded_String_Hash,
      Value_Type => Class_Set_Access);

   -- holds all known class sets - ADO not initialized at this point
   -- Do not call "Class_Sets_Hashed_Mappings.Create" at this point.
   Class_Sets_Map : Class_Sets_Hashed_Mappings.Mapping;
   ADO_Initialized : Boolean := False;


   ---------------------------------------------------------------------------
   -- 0.2
   -- Internal subprograms
   ---------------------------------------------------------------------------

   -- deallocates a whole class set entry
   procedure Deallocate_Class_Set (Class_Set : in out Class_Set_Access) is

     procedure Free_Class_Set_Access is new Ada.Unchecked_Deallocation
     (Class_Set_Data, Class_Set_Access);

   begin
      -- deep deallocation
      Node_Class_Look_Up_Hashed_Mappings.Destroy (Class_Set.Node_Classes);
      Edge_Class_Look_Up_Hashed_Mappings.Destroy (Class_Set.Edge_Classes);

      -- deallocate class set itself
      Free_Class_Set_Access (Class_Set);
   end Deallocate_Class_Set;


   ------------------------------------------------------------------------
   -- insert all edge classes from a edge class id set into the class set
   procedure Insert_Edge_Class_Id_Set_Into_Class_Set
     (The_Class_Set_Access : in Class_Set_Access;
      Edge_Class_Id_Set : in Graph_Lib.Edge_Class_Id_Set) is

      -- insert the element into the class set if it not already
      -- exists
      procedure Process_Element (Item : in Graph_Lib.Edge_Class_Id) is

      begin

         if not Edge_Class_Look_Up_Hashed_Mappings.Is_Bound
           (The_Class_Set_Access.Edge_Classes, Item) then

            Edge_Class_Look_Up_Hashed_Mappings.Bind
              (The_Class_Set_Access.Edge_Classes, Item, Item);
         end if;
      end Process_Element;

      procedure Add_All_Elements_To_Class_Set is new
        Graph_Lib.Edge_Class_Id_Sets.Apply
        (Execute => Process_Element);

   begin

      Add_All_Elements_To_Class_Set (Edge_Class_Id_Set);
   end Insert_Edge_Class_Id_Set_Into_Class_Set;

   ------------------------------------------------------------------------
   -- insert all node classes from a edge class id set into the class set
   procedure Insert_Node_Class_Id_Set_Into_Class_Set
     (The_Class_Set_Access : in Class_Set_Access;
      Node_Class_Id_Set : in Graph_Lib.Node_Class_Id_Set) is

      -- insert the element into the class set if it not already
      -- exists
      procedure Process_Element (Item : in Graph_Lib.Node_Class_Id) is

      begin

         if not Node_Class_Look_Up_Hashed_Mappings.Is_Bound
           (The_Class_Set_Access.Node_Classes, Item) then

            Node_Class_Look_Up_Hashed_Mappings.Bind
              (The_Class_Set_Access.Node_Classes, Item, Item);
         end if;
      end Process_Element;

      procedure Add_All_Elements_To_Class_Set is new
        Graph_Lib.Node_Class_Id_Sets.Apply
        (Execute => Process_Element);

   begin

      Add_All_Elements_To_Class_Set (Node_Class_Id_Set);
   end Insert_Node_Class_Id_Set_Into_Class_Set;


   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and deallocation of the internal datastructure
   -- that holds all known class sets.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Side Effect - Changed global variables:
   --   - Class_Sets_Map;
   procedure Initialize_Class_Sets
     (GIANT_Class_Sets_Directory : in String) is

      -- needed to insert new class sets
      New_Class_Set_Access : Class_Set_Access := null;

      -- needed for deallocation while replacing
      -- an old class set with a new (later read) one
      Remove_Class_Set_Access : Class_Set_Access := null;

      -- Needed for deallocation
      The_Tree_Reader  : Tree_Readers.Tree_Reader;
      -- top level node (document node)
      The_XML_Document : Dom.Core.Document;

      XML_Nodes_List : DOM.Core.Node_List;
      XML_Node : DOM.Core.Node;

      A_Node_Class_ID     : Graph_Lib.Node_Class_Id;
      A_Node_Class_ID_Set : Graph_Lib.Node_Class_Id_Set;

      An_Edge_Class_ID_Set : Graph_Lib.Edge_Class_Id_Set;

      File_List : String_Lists.List;
      File_List_Iter : String_Lists.ListIter;
      A_File_Name : Ada.Strings.Unbounded.Unbounded_String;

      -- needed to determine which files should be ignored
      Ignore_File : Boolean := False;

   begin

      -- Get all xml files in passed dir
      begin
         File_List := File_Management.Get_Filtered_Files_From_Directory
           (GIANT_Class_Sets_Directory, True, ".xml");
      exception
         when File_Management.Invalid_Directory_Exception =>
            raise Invalid_Class_Set_Directory_Exception;
      end;
      -- Now we have all absolute paths for the files holding
      -- the class sets

      -- Load each class set into internal datastructure
      File_List_Iter := String_Lists.MakeListIter (File_List);

      -- INITIALIZATION
      -- Create new empty top level hash map holding all class sets and
      -- initializes the ADO
      Class_Sets_Map := Class_Sets_Hashed_Mappings.Create;

      -- Begin: Processing all xml files for class sets
      while String_Lists.More (File_List_Iter) loop
         -- Think positive - process files until security check says
         -- something else
         Ignore_File := False;

         -- get a file to process
         String_Lists.Next (File_List_Iter, A_File_Name);

         --  Logger.Debug (Ada.Strings.Unbounded.To_String (A_File_Name));

         -- ignore not readable xml files
         begin
            XML_File_Access.Load_XML_File_Validated
              (Ada.Strings.Unbounded.To_String (A_File_Name),
               The_Tree_Reader,
               The_XML_Document);
         exception
            when XML_File_Access.XML_File_Access_Error_Exception =>
               Logger.Error ("XML_File_Access_Error_Exception");
               Ignore_File := True;
            when XML_File_Access.XML_File_Parse_Fatal_Error_Exception =>
               Logger.Error ("XML_File_Parse_Fatal_Error_Exception");
               Ignore_File := True;
         end;

         --  Logger.Debug (Boolean'Image (Ignore_File));

         -- ignore files that do not describe a class set
         if not Ignore_File and then
            not (XML_File_Access.Does_XML_Document_Belong_To_Type
                 ("giant_class_set_file", The_XML_Document)) then
            Logger.Info ("XML-File does not belong to giant_class_set_file");
            Tree_Readers.Free (The_Tree_Reader);
            Ignore_File := True;
         end if;

         --  Logger.Debug (Boolean'Image (Ignore_File));

         if not Ignore_File then
            -- create a new class set with existing data
            ---------------------------------------------
            New_Class_Set_Access := new Class_Set_Data;
            New_Class_Set_Access.Node_Classes :=
              Node_Class_Look_Up_Hashed_Mappings.Create;
            New_Class_Set_Access.Edge_Classes :=
              Edge_Class_Look_Up_Hashed_Mappings.Create;

            -- get name of class_set (the file name without ending and path)
            -- file name: "\class_sets\my_set.xml" -->
            -- name of class set: "my_set";
            -- an already existing class set with the same name will be
            --  replaced.
            New_Class_Set_Access.Class_Set_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (File_Management.Calculate_Name_For_File
                   (Ada.Strings.Unbounded.To_String (A_File_Name)));

            -- process node classes
            ------------------------------------------------------------------

            -- Get_All_Node_Classes (only one class)
            XML_Nodes_List :=
              DOM.Core.Documents.Get_Elements_By_Tag_Name
              (The_XML_Document, "node_class");

            for I in 0 .. DOM.Core.Nodes.Length (XML_Nodes_List) - 1 loop

               XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);

               Logger.Debug ("current node: "
                 & """"
                 & DOM.Core.Elements.Get_Attribute
                     (XML_Node, "node_class_name")
                 & """");

               -- IGNORE node classes not known by the IML Reflection
               if Graph_Lib.Does_Node_Class_Exist
                 (DOM.Core.Elements.Get_Attribute
                  (XML_Node, "node_class_name")) then

                  -- Calculate node class ID
                  A_Node_Class_ID := Graph_Lib.Convert_Node_Class_Name_To_Id
                    (DOM.Core.Elements.Get_Attribute
                     (XML_Node, "node_class_name"));

                 -- Check whether the node class is already in the set
                 if not Node_Class_Look_Up_Hashed_Mappings.Is_Bound
                   (New_Class_Set_Access.Node_Classes, A_Node_Class_ID) then

                    -- Add node class to Class set
                    Node_Class_Look_Up_Hashed_Mappings.Bind
                      (New_Class_Set_Access.Node_Classes,
                       A_Node_Class_ID,
                       A_Node_Class_ID);

                  end if;
               end if;
            end loop;

            DOM.Core.Free (XML_Nodes_List);

            -- Get_All_Node_Classes regarding inheritance hierarchy
            XML_Nodes_List :=
              DOM.Core.Documents.Get_Elements_By_Tag_Name
              (The_XML_Document, "super_node_class");

            for I in 0 .. DOM.Core.Nodes.Length (XML_Nodes_List) - 1 loop

               XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);

               -- IGNORE node classes not known by the IML Reflection
               if Graph_Lib.Does_Node_Class_Exist
                 (DOM.Core.Elements.Get_Attribute
                  (XML_Node, "super_node_class_name")) then

                  -- get all inheriting sub classes incl. the super class
                  A_Node_Class_ID_Set :=
                    IML_Class_Inheritance_Proc.Get_All_Sub_Node_Classes
                      (XML_Node);

                  -- check whether returned set is empty
                  if Graph_Lib.Node_Class_Id_Sets.Is_Empty
                    (A_Node_Class_ID_Set) then

                     Graph_Lib.Edge_Class_Id_Sets.Destroy
                       (An_Edge_Class_ID_Set);

                  else
                     -- insert all node class id's from the calculated set
                     -- into the class set
                     Insert_Node_Class_Id_Set_Into_Class_Set
                       (New_Class_Set_Access, A_Node_Class_ID_Set);

                     -- deallocate returned node class id set
                     Graph_Lib.Node_Class_Id_Sets.Destroy
                       (A_Node_Class_ID_Set);
                  end if;
               end if;
            end loop;

            DOM.Core.Free (XML_Nodes_List);


            -- process edge classes
            ------------------------------------------------------------------
            -- get all entries for edge classes (no inheritance)
            XML_Nodes_List :=
              DOM.Core.Documents.Get_Elements_By_Tag_Name
              (The_XML_Document, "edge_class");

            -- process all entries
            for I in 0 .. DOM.Core.Nodes.Length (XML_Nodes_List) - 1 loop

               XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);

               -- calculate set of edge classes specified by this node
               An_Edge_Class_ID_Set :=
                 Edge_Class_Proc.Process_Edge_Class_Entry (XML_Node);

               -- check whether returned set is empty
               if Graph_Lib.Edge_Class_Id_Sets.Is_Empty
                 (An_Edge_Class_ID_Set) then

                  Graph_Lib.Edge_Class_Id_Sets.Destroy
                    (An_Edge_Class_ID_Set);

               else

                  -- insert all edge class id's from the calculated set into
                  -- the class set
                  Insert_Edge_Class_Id_Set_Into_Class_Set
                    (New_Class_Set_Access, An_Edge_Class_ID_Set);

                  -- deallocate returned edge class id set
                  Graph_Lib.Edge_Class_Id_Sets.Destroy
                    (An_Edge_Class_ID_Set);

               end if;

            end loop;

            DOM.Core.Free (XML_Nodes_List);

            -- get all entries for edge classes regarding inheritance
            XML_Nodes_List :=
              DOM.Core.Documents.Get_Elements_By_Tag_Name
              (The_XML_Document, "super_edge_class");

            for I in 0 .. DOM.Core.Nodes.Length (XML_Nodes_List) - 1 loop

               XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);

               -- calculate set of edge classes specified by this node
               An_Edge_Class_ID_Set :=
                 IML_Class_Inheritance_Proc.
                   Get_All_Sub_Edge_Classes (XML_Node);

               -- check whether returned set is empty
               if Graph_Lib.Edge_Class_Id_Sets.Is_Empty
                 (An_Edge_Class_ID_Set) then

                  Graph_Lib.Edge_Class_Id_Sets.Destroy
                    (An_Edge_Class_ID_Set);
               else
                  -- insert all edge class id's from the calculated set into
                  -- the class set
                  Insert_Edge_Class_Id_Set_Into_Class_Set
                    (New_Class_Set_Access, An_Edge_Class_ID_Set);

                  -- deallocate returned edge class id set
                  Graph_Lib.Edge_Class_Id_Sets.Destroy
                    (An_Edge_Class_ID_Set);
               end if;

            end loop;

            DOM.Core.Free (XML_Nodes_List);

            -- Free memory needed for the xml dom tree
            Tree_Readers.Free(The_Tree_Reader);

            -- Insert the complete new class set into the ADO's data structure
            ------------------------------------------------------------------

            -- Check whether the new Class_Set_Name already exists and
            -- remove the old one then necessary.
            if (Class_Sets_Hashed_Mappings.Is_Bound
                (Class_Sets_Map, New_Class_Set_Access.Class_Set_Name)) then

               -- get already inserted class set with same name
               Remove_Class_Set_Access :=
                 Class_Sets_Hashed_Mappings.Fetch
                   (Class_Sets_Map, New_Class_Set_Access.Class_Set_Name);

               -- remove old class set from ADO
               Class_Sets_Hashed_Mappings.Unbind
                 (Class_Sets_Map, New_Class_Set_Access.Class_Set_Name);

               -- deallocate old class set
               Deallocate_Class_Set (Remove_Class_Set_Access);
            end if;

            -- insert new class set
            Class_Sets_Hashed_Mappings.Bind
              (Class_Sets_Map,
               New_Class_Set_Access.Class_Set_Name,
               New_Class_Set_Access);
         end if; -- if (not Ignore_File)

      end loop; -- End: Processing all xml files for class sets

      -- Deallocate File_List
      String_Lists.Destroy (File_List);

      ADO_Initialized := True;
   end Initialize_Class_Sets;

   ---------------------------------------------------------------------------
   procedure Clear_Class_Sets is

      -- needed for deeop deallocation
      Class_Sets_Iter : Class_Sets_Hashed_Mappings.Values_Iter;
      Dealloc_Class_Set_Access : Class_Set_Access;
   begin

      if not ADO_Initialized then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      Class_Sets_Iter := Class_Sets_Hashed_Mappings.Make_Values_Iter
        (Class_Sets_Map);

      -- deep deallocation for each class set
      while Class_Sets_Hashed_Mappings.More (Class_Sets_Iter) loop
         Class_Sets_Hashed_Mappings.Next
           (Class_Sets_Iter,
            Dealloc_Class_Set_Access);

         Deallocate_Class_Set (Dealloc_Class_Set_Access);
      end loop;

      -- deallocate top level hash map needed to manage all class sets
      Class_Sets_Hashed_Mappings.Destroy (Class_Sets_Map);

      -- mark ADO as not initialized
      ADO_Initialized := False;

   end Clear_Class_Sets;


   ---------------------------------------------------------------------------
   -- B
   -- Access to class sets.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_All_Existing_Class_Sets
     return String_Lists.List is

      Names_List : String_Lists.List;
      -- Iterate over all known class sets;
      Class_Sets_Iter : Class_Sets_Hashed_Mappings.Values_Iter;

      A_Class_Set_Access : Class_Set_Access;
   begin

      if not ADO_Initialized then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      Class_Sets_Iter :=
        Class_Sets_Hashed_Mappings.Make_Values_Iter (Class_Sets_Map);
      Names_List := String_Lists.Create;

      while Class_Sets_Hashed_Mappings.More (Class_Sets_Iter) loop
         Class_Sets_Hashed_Mappings.Next
           (Class_Sets_Iter,
            A_Class_Set_Access);
         String_Lists.Attach (Names_List, A_Class_Set_Access.Class_Set_Name);
      end loop;

      return Names_List;
   end Get_All_Existing_Class_Sets;


   ---------------------------------------------------------------------------
   function Does_Class_Set_Exist
     (Class_Set_Name : in String)
     return Boolean is

   begin

      if not ADO_Initialized then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      return Class_Sets_Hashed_Mappings.Is_Bound
        (Class_Sets_Map,
         Ada.Strings.Unbounded.To_Unbounded_String(Class_Set_Name));
   end Does_Class_Set_Exist;

   ---------------------------------------------------------------------------
   function Get_Class_Set_Access
     (Class_Set_Name : in String)
     return Class_Set_Access is

   begin

      if not ADO_Initialized then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      if (Does_Class_Set_Exist (Class_Set_Name) = False) then
         raise Class_Set_Does_Not_Exist_Exception;
      end if;

      return Class_Sets_Hashed_Mappings.Fetch
        (Class_Sets_Map,
         Ada.Strings.Unbounded.To_Unbounded_String(Class_Set_Name));
   end Get_Class_Set_Access;


   ---------------------------------------------------------------------------
   -- D
   -- Queries to class sets.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Is_Node_Class_Element_Of_Class_Set
     (Class_Set  : in Class_Set_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Boolean is

   begin

      if not ADO_Initialized then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      if (Class_Set = null) then
         raise Class_Sets_Access_Not_Initialized_Exception;
      end if;

      return
        Node_Class_Look_Up_Hashed_Mappings.Is_Bound
        (Class_Set.Node_Classes, Node_Class);
   end Is_Node_Class_Element_Of_Class_Set;

   ---------------------------------------------------------------------------
   function Is_Edge_Class_Element_Of_Class_Set
     (Class_Set  : in Class_Set_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Boolean is

   begin

      if not ADO_Initialized then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      if (Class_Set = null) then
         raise Class_Sets_Access_Not_Initialized_Exception;
      end if;

      return
        Edge_Class_Look_Up_Hashed_Mappings.Is_Bound
        (Class_Set.Edge_Classes, Edge_Class);
   end Is_Edge_Class_Element_Of_Class_Set;

   ---------------------------------------------------------------------------
   -- F
   -- Meta Class Sets
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Is_Empty (Meta : Meta_Class_Set_Access) return Boolean is

   begin
      if Node_Class_Look_Up_Hashed_Mappings.Is_Empty
        (Meta.Node_Classes)
        and Edge_Class_Look_Up_Hashed_Mappings.Is_Empty
        (Meta.Edge_Classes) then

        return True;
      else

         return False;
      end if;

   end Is_Empty;

   ---------------------------------------------------------------------------
   function Build (Elements : in Class_Set_Array)
     return Meta_Class_Set_Access is

      New_Meta        : Meta_Class_Set_Access;
      Node_Class_Iter : Node_Class_Look_Up_Hashed_Mappings.Values_Iter;
      A_Node_Class    : Graph_Lib.Node_Class_Id;
      Edge_Class_Iter : Edge_Class_Look_Up_Hashed_Mappings.Values_Iter;
      An_Edge_Class    : Graph_Lib.Edge_Class_Id;
   begin

      New_Meta := new Class_Set_Data;
      New_Meta.Node_Classes := Node_Class_Look_Up_Hashed_Mappings.Create;
      New_Meta.Edge_Classes := Edge_Class_Look_Up_Hashed_Mappings.Create;

      for I in Elements'Range loop

         if (Elements (I) /= null) then

            -- Process node classes
            -----------------------
            Node_Class_Iter :=
              Node_Class_Look_Up_Hashed_Mappings.Make_Values_Iter
                (Elements (I).Node_Classes);

            while Node_Class_Look_Up_Hashed_Mappings.More
              (Node_Class_Iter) loop

               Node_Class_Look_Up_Hashed_Mappings.Next
                 (Node_Class_Iter, A_Node_Class);

               if not Node_Class_Look_Up_Hashed_Mappings.Is_Bound
                 (New_Meta.Node_Classes, A_Node_Class) then

                  Node_Class_Look_Up_Hashed_Mappings.Bind
                    (New_Meta.Node_Classes,
                     A_Node_Class,
                     A_Node_Class);
               end if;
            end loop;

            -- Process edge classes
            -----------------------
            Edge_Class_Iter :=
              Edge_Class_Look_Up_Hashed_Mappings.Make_Values_Iter
                (Elements (I).Edge_Classes);

            while Edge_Class_Look_Up_Hashed_Mappings.More
              (Edge_Class_Iter) loop

               Edge_Class_Look_Up_Hashed_Mappings.Next
                 (Edge_Class_Iter, An_Edge_Class);

               if not Edge_Class_Look_Up_Hashed_Mappings.Is_Bound
                 (New_Meta.Edge_Classes, An_Edge_Class) then

                  Edge_Class_Look_Up_Hashed_Mappings.Bind
                    (New_Meta.Edge_Classes,
                     An_Edge_Class,
                     An_Edge_Class);
               end if;
            end loop;

         end if; -- end if (Elements (I) /= null)

      end loop;

      return New_Meta;
   end Build;

   ---------------------------------------------------------------------------
   function Build (Elements : in Class_Sets_Lists.List)
     return Meta_Class_Set_Access is

      Class_Sets_Array : Class_Set_Array
        (1..Class_Sets_Lists.Length (Elements));
      Iter             : Class_Sets_Lists.ListIter;
   begin

      Iter := Class_Sets_Lists.MakeListIter (Elements);

      for I in Class_Sets_Array'Range loop
        Class_Sets_Lists.Next (Iter, Class_Sets_Array (I));
      end loop;

      return Build (Class_Sets_Array);
   end Build;

   ---------------------------------------------------------------------------
   procedure Destroy (Target : in out Meta_Class_Set_Access) is

   begin

      if (Target = null) then
         raise Class_Sets_Access_Not_Initialized_Exception;
      end if;

      Deallocate_Class_Set (Target);
   end Destroy;

end Giant.Config.Class_Sets;
