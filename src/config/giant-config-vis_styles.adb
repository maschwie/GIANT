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
-- $RCSfile: giant-config-vis_styles.adb,v $, $Revision: 1.24 $
-- $Author: keulsn $
-- $Date: 2003-09-12 20:30:11 $
--
with Ada.Unchecked_Deallocation;

with Unbounded_String_Hash; -- from Bauhaus IML "Reuse.src"

with DOM.Core.Documents; -- from xmlada
with DOM.Core.Elements;  -- from xmlada
with DOM.Core.Nodes;     -- from xmlada
with Tree_Readers;

with Giant.XML_File_Access;            -- from GIANT
with Giant.File_Management;            -- from GIANT
with Giant.Edge_Class_Proc;            -- from GIANT
with Giant.IML_Class_Inheritance_Proc; -- from GIANT
with Giant.Logger;                     -- from GIANT
pragma Elaborate_All (Giant.Logger);

package body Giant.Config.Vis_Styles is

   package Logger is new Giant.Logger("Giant.Config.Vis_Styles");

   ---------------------------------------------------------------------------
   -- 0.1
   -- Types describing the internal data structure
   ---------------------------------------------------------------------------

   -- needed to manage all  xpm files for icons - ensures that each
   -- each file holding an icon is only registered once duiring
   -- parsing of all visualisation styles.
   --
   -- As key the absolute path of a icon file is used.
   -- Value: An integer that encodes an icon file
   -- (Range 1 .. Ammount of Icon Files).
   package Icons_Encoding_Mappings is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Hash       => Unbounded_String_Hash,
      Value_Type => Integer);

   type Color_Encoding_Data_Element is record
      Color    : Config.Color_Access;
      Encoding : Integer := 0;
   end record;

   -- needed to manage the colors - ensures that for two equal colors
   -- the pointers "Config.Color_Access" are equal too.
   --
   -- Also used for deallocation.
   -- As key the unbounded string describing a color is used.
   package Colors_Encoding_Hashed_Mappings is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Hash       => Unbounded_String_Hash,
      Value_Type => Color_Encoding_Data_Element);

   -- needed to manage the visualisation styles - each style is identified
   -- by its unique name
   package All_Vis_Styles_Hashed_Mappings is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Hash       => Unbounded_String_Hash,
      Value_Type => Visualisation_Style_Access);


   ---------------------------------------------------------------------------
   -- 0.2
   -- Global variables forming the internal datastructure
   ---------------------------------------------------------------------------

   -- Marks the ADO as initialized or not
   ADO_Initialized : Boolean := False;

   -- needed to encode the icons
   Global_Icon_Counter : Integer := 1;
   -- holds all icons
   Icons_Encoding_Map  : Icons_Encoding_Mappings.Mapping;
   -- holds the array that provides access to the files using the
   -- integer encoding as index
   Encoded_Icons_Array : Node_Icons_Array_Access;

   Global_Color_Counter : Integer := 1;
   Colors_Encoding_Map  : Colors_Encoding_Hashed_Mappings.Mapping;
   Encoded_Colors_Array : Color_Access_Array_Access;

   -- holds all visualisation styles (incl. default vis style)
   All_Vis_Styles_Map : All_Vis_Styles_Hashed_Mappings.Mapping;
   -- The name of the default visualisation style
   Global_Default_Vis_Style_Name : Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.Null_Unbounded_String;


   ---------------------------------------------------------------------------
   -- 0.3
   -- Internal subDoes_Vis_Style_Existprograms
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Builds the internal encoding array out of the hash map.
   procedure Build_Encoded_Icons_Array is

      Iter : Icons_Encoding_Mappings.Bindings_Iter;
      Size : Integer := 0;

      Icon_File : Ada.Strings.Unbounded.Unbounded_String;
      Encoding  : Integer;
   begin

      Iter := Icons_Encoding_Mappings.Make_Bindings_Iter (Icons_Encoding_Map);

      -- determine Size
      -----------------
      Size := 0;
      while Icons_Encoding_Mappings.More (Iter) loop

         Icons_Encoding_Mappings.Next (Iter, Icon_File, Encoding);
         Size := Size + 1;
      end loop;

      Encoded_Icons_Array := new Node_Icons_Array'
        (1 .. Size => Ada.Strings.Unbounded.Null_Unbounded_String);

      -- isert entries
      ----------------
      Iter := Icons_Encoding_Mappings.Make_Bindings_Iter (Icons_Encoding_Map);

      while Icons_Encoding_Mappings.More (Iter) loop

         Icons_Encoding_Mappings.Next (Iter, Icon_File, Encoding);

         Encoded_Icons_Array (Encoding) := Icon_File;
      end loop;

   end Build_Encoded_Icons_Array;

   ---------------------------------------------------------------------------
   -- Registers an Icon file -
   -- Ensures that each icon is only once registered.
   --
   -- "All_Icons_Map" must be initialized before using this
   -- subprogram.
   -- "The_Icon_File" has to be an absolute path.
   procedure Register_Icon_File_Encoding
     (The_Icon_File       : in     Ada.Strings.Unbounded.Unbounded_String;
      Encoded_Icon_Index  :    out Integer) is

   begin

      -- check whether icon is already loeaded
      if Icons_Encoding_Mappings.Is_Bound
        (Icons_Encoding_Map, The_Icon_File) then

         -- return pointer to alrady loaded icon
         Encoded_Icon_Index :=
           Icons_Encoding_Mappings.Fetch
             (Icons_Encoding_Map, The_Icon_File);
      else

         -- generate new encoding
         Encoded_Icon_Index := Global_Icon_Counter;
         Global_Icon_Counter := Global_Icon_Counter + 1;

         -- add encoding to hash map
         Icons_Encoding_Mappings.Bind
           (Icons_Encoding_Map,
            The_Icon_File,
            Encoded_Icon_Index);
      end if;
   end Register_Icon_File_Encoding;

   ---------------------------------------------------------------------------
   procedure Build_Encoded_Colors_Array is

      Iter : Colors_Encoding_Hashed_Mappings.Bindings_Iter;
      Size : Integer := 0;

      Color_String : Ada.Strings.Unbounded.Unbounded_String;
      Data_Element : Color_Encoding_Data_Element;
   begin

      Iter := Colors_Encoding_Hashed_Mappings.Make_Bindings_Iter
        (Colors_Encoding_Map);

      -- determine Size
      -----------------
      Size := 0;
      while Colors_Encoding_Hashed_Mappings.More (Iter) loop

         Colors_Encoding_Hashed_Mappings.Next
           (Iter, Color_String, Data_Element);
         Size := Size + 1;
      end loop;

      Encoded_Colors_Array :=
        new Color_Access_Array'(1 .. Size => null);

      -- isert entries
      ----------------
      Iter := Colors_Encoding_Hashed_Mappings.Make_Bindings_Iter
        (Colors_Encoding_Map);

      while Colors_Encoding_Hashed_Mappings.More (Iter) loop

         Colors_Encoding_Hashed_Mappings.Next
           (Iter, Color_String, Data_Element);

         Encoded_Colors_Array (Data_Element.Encoding) := Data_Element.Color;
      end loop;

   end Build_Encoded_Colors_Array;

   ---------------------------------------------------------------------------
   -- Decides how to insert a new color string into the global data model
   -- and does the ecoding to integer id's.
   --
   -- Ensures that each different color is only loaded once into the
   -- main memory.
   procedure Register_Color_Encoding
     (The_Color_String    : in     Ada.Strings.Unbounded.Unbounded_String;
      Encoded_Color_Index :    out Integer) is

      New_Color_Access : Config.Color_Access := null;
      New_Data : Color_Encoding_Data_Element;
   begin

      -- Check whether the string already exists and return a reference
      -- to that string if so.
      if Colors_Encoding_Hashed_Mappings.Is_Bound
        (Colors_Encoding_Map, The_Color_String) then

         -- get existing encoding for strings
         Encoded_Color_Index := Colors_Encoding_Hashed_Mappings.Fetch
           (Colors_Encoding_Map, The_Color_String).Encoding;


      else
         -- create entry for new color string
         New_Color_Access :=
           new Ada.Strings.Unbounded.Unbounded_String'(The_Color_String);
         New_Data.Color := New_Color_Access;

         New_Data.Encoding := Global_Color_Counter;
         Global_Color_Counter := Global_Color_Counter + 1;

         Colors_Encoding_Hashed_Mappings.Bind
           (Colors_Encoding_Map,
            The_Color_String,
            New_Data);

         Encoded_Color_Index := New_Data.Encoding;
      end if;
   end Register_Color_Encoding;

   ---------------------------------------------------------------------------
   -- Calculates all attribute names from all <node_attribute> - subnodes
   -- for a passed node.
   --
   -- The passed node must be a <node default_node_setting> or a
   -- <node_class_specific_setting> - node
   function Get_All_Attribute_Names
     (XML_Node_Setting_Node : in DOM.Core.Node) return String_Lists.List is

      The_List       : String_Lists.List := String_Lists.Create;
      XML_Nodes_List : DOM.Core.Node_List;
      XML_Node       : DOM.Core.Node;

   begin

      XML_Nodes_List := DOM.Core.Elements.Get_Elements_By_Tag_Name
        (XML_Node_Setting_Node, "node_attribute");

      for I in 0 ..  DOM.Core.Nodes.Length(XML_Nodes_List) - 1 loop

         XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);

         -- get name of attribute and add it to the list
         String_Lists.Attach
           (The_List,
            Ada.Strings.Unbounded.To_Unbounded_String
            (DOM.Core.Elements.Get_Attribute
             (XML_Node, "attribute_name")));

      end loop;

      DOM.Core.Free (XML_Nodes_List);

      return The_List;
   end Get_All_Attribute_Names;

   ---------------------------------------------------------------------------
   -- Calculates all node class names from all <node_class> - subnodes
   -- for a passed node <node_class_specific_setting> and
   -- for all <super_node_class> nodes.
   --
   -- The passed node must be a <node_class_specific_setting> - node.
   function Get_All_Node_Class_Names
     (XML_Node_Setting_Node : in DOM.Core.Node) return String_Lists.List is

      The_List       : String_Lists.List := String_Lists.Create;
      XML_Nodes_List : DOM.Core.Node_List;
      XML_Node       : DOM.Core.Node;

      Node_Class_Set      : Graph_Lib.Node_Class_Id_Set;
      Node_Class_Set_Iter : Graph_Lib.Node_Class_Id_Sets.Iterator;
      A_Node_Class_ID     : Graph_Lib.Node_Class_Id;

   begin

      -- No inheritance
      -----------------
      XML_Nodes_List := DOM.Core.Elements.Get_Elements_By_Tag_Name
        (XML_Node_Setting_Node, "node_class");

      for I in 0 ..  DOM.Core.Nodes.Length(XML_Nodes_List) - 1 loop

         XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);

         -- get name of class name attribute and add it to the list
         String_Lists.Attach
           (The_List,
            Ada.Strings.Unbounded.To_Unbounded_String
            (DOM.Core.Elements.Get_Attribute
             (XML_Node, "node_class_name")));

      end loop;
      DOM.Core.Free (XML_Nodes_List);

      -- with inheritance
      -------------------
      XML_Nodes_List := DOM.Core.Elements.Get_Elements_By_Tag_Name
        (XML_Node_Setting_Node, "super_node_class");

      for I in 0 ..  DOM.Core.Nodes.Length(XML_Nodes_List) - 1 loop

         XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);

         -- get all inheriting node classes incl. super class
         Node_Class_Set :=
           IML_Class_Inheritance_Proc.Get_All_Sub_Node_Classes (XML_Node);

         Node_Class_Set_Iter := Graph_Lib.Node_Class_Id_Sets.Make_Iterator
           (Node_Class_Set);

         while Graph_Lib.Node_Class_Id_Sets.More (Node_Class_Set_Iter) loop

            Graph_Lib.Node_Class_Id_Sets.Next
              (Node_Class_Set_Iter, A_Node_Class_ID);

            String_Lists.Attach
              (The_List,
               Ada.Strings.Unbounded.To_Unbounded_String
                 (Graph_Lib.Get_Node_Class_Tag (A_Node_Class_ID)));
         end loop;

         Graph_Lib.Node_Class_Id_Sets.Destroy (Node_Class_Set_Iter);
         Graph_Lib.Node_Class_Id_Sets.Destroy (Node_Class_Set);
      end loop;
      DOM.Core.Free (XML_Nodes_List);

      return The_List;
   end Get_All_Node_Class_Names;

   ---------------------------------------------------------------------------
   -- Processes node settings -
   -- must be a <node default_node_setting> or a
   -- <node_class_specific_setting> - node
   --
   -- DOES NOT BUILD THE FILTER as the Attribute_Filter is special
   -- for each node class.
   --
   function Build_Node_Class_Vis_Data_Without_Filter
     (XML_Node_Setting_Node : in DOM.Core.Node;
      -- directory of the vis style file that holds "XML_Node_Setting_Node"
      Vis_Style_Dir         : in String;
      Resources_Root_Dir    : in String)
     return Node_Class_Vis_Data is

      Node_Vis_Data : Node_Class_Vis_Data;

      -- needed for calculation of the position of the icon
      Node_Icon_Full_Path : Ada.Strings.Unbounded.Unbounded_String;
      Icon_Encoding : Integer := 0;

      File_Found : Boolean := True;

   begin

      begin
         -- try calculate full path for icon file (root is "Vis_Style_Path")
         Node_Icon_Full_Path := Ada.Strings.Unbounded.To_Unbounded_String
           (
            File_Management.Get_Absolute_Path_To_File_From_Relative
            (Vis_Style_Dir,
             DOM.Core.Elements.Get_Attribute (XML_Node_Setting_Node, "icon"))
           );
      exception
         when File_Management.File_Does_Not_Exist_Exception =>
            File_Found := False;
      end;

      if not File_Found then
         begin
            -- try calculate file name based on Resources_Root_Dir
            Node_Icon_Full_Path := Ada.Strings.Unbounded.To_Unbounded_String
              (File_Management.Get_Absolute_Path_To_File_From_Relative
                (Resources_Root_Dir,
                 DOM.Core.Elements.Get_Attribute
                  (XML_Node_Setting_Node, "icon")));
         exception
            when File_Management.File_Does_Not_Exist_Exception =>
               File_Found := False;
         end;
      end if;

      if File_Found then
         Register_Icon_File_Encoding (Node_Icon_Full_Path, Icon_Encoding);
      else
         -- no icon file exists
         Icon_Encoding := 0;
      end if;

      -- Icon
      Node_Vis_Data.Icon_Encoding := Icon_Encoding;

      -- Attribute_Filter
      -- needs to be set up for each node class separately

      -- Border_Color
      Register_Color_Encoding
        (Ada.Strings.Unbounded.To_Unbounded_String
         (DOM.Core.Elements.Get_Attribute
          (XML_Node_Setting_Node, "border_color")),
         Node_Vis_Data.Border_Color_Encoding);

      -- Fill_Color
      Register_Color_Encoding
        (Ada.Strings.Unbounded.To_Unbounded_String
         (DOM.Core.Elements.Get_Attribute
          (XML_Node_Setting_Node, "fill_color")),
         Node_Vis_Data.Fill_Color_Encoding);

      -- Text_Color
      Register_Color_Encoding
        (Ada.Strings.Unbounded.To_Unbounded_String
         (DOM.Core.Elements.Get_Attribute
          (XML_Node_Setting_Node, "text_color")),
         Node_Vis_Data.Text_Color_Encoding);

      return Node_Vis_Data;

   end Build_Node_Class_Vis_Data_Without_Filter;

   -------------------------------------------------
   -- Processes edge settings -
   -- must be a <default_edge_setting> or a
   -- <edge_class_specific_setting> - node
   --
   function Build_Edge_Class_Vis_Data
     (XML_Edge_Setting_Node : in DOM.Core.Node)
     return Edge_Class_Vis_Data is

      Edge_Vis_Data : Edge_Class_Vis_Data;

      Line_Style_Val : Ada.Strings.Unbounded.Unbounded_String;
      Label_Val : Ada.Strings.Unbounded.Unbounded_String;

   begin

      -- Line_Color
      Register_Color_Encoding
        (Ada.Strings.Unbounded.To_Unbounded_String
         (DOM.Core.Elements.Get_Attribute
          (XML_Edge_Setting_Node, "line_color")),
         Edge_Vis_Data.Line_Color_Encoding);

      -- Text_Color
      Register_Color_Encoding
        (Ada.Strings.Unbounded.To_Unbounded_String
         (DOM.Core.Elements.Get_Attribute
          (XML_Edge_Setting_Node, "text_color")),
         Edge_Vis_Data.Text_Color_Encoding);

      -- Line_Style
      Line_Style_Val := Ada.Strings.Unbounded.To_Unbounded_String
        (DOM.Core.Elements.Get_Attribute
         (XML_Edge_Setting_Node, "line_style"));

      if Ada.Strings.Unbounded."="(Line_Style_Val, "dashed_line") then
         Edge_Vis_Data.Line_Style := Dashed_Line;
      elsif Ada.Strings.Unbounded."="(Line_Style_Val, "dotted_line") then
         Edge_Vis_Data.Line_Style := Dotted_Line;
      else
         Edge_Vis_Data.Line_Style := Continuous_Line;
      end if;

      -- Label
      Label_Val := Ada.Strings.Unbounded.To_Unbounded_String
        (DOM.Core.Elements.Get_Attribute
         (XML_Edge_Setting_Node, "show_label"));

      if  Ada.Strings.Unbounded."="(Label_Val, "yes") then
         Edge_Vis_Data.Label := True;
      else
         Edge_Vis_Data.Label := False;
      end if;

      return Edge_Vis_Data;
   end Build_Edge_Class_Vis_Data;

   ---------------------------------------------------------------------------
   -- Creates a new data structure describing a visualisation style
   --
   -- Does not catch any exceptions
   function Process_XML_Vis_Style
     (Vis_Style_XML_Document : in Dom.Core.Document;
      -- the directory of the vis style file "Vis_Style_Name"
      Resources_Root_Dir     : in String;
      Vis_Style_Dir          : in String;
      Vis_Style_Name         : in Ada.Strings.Unbounded.Unbounded_String)

     return Visualisation_Style_Access is

      ------------------------------------------------------------------------
      -- insert the data for edge classes from a edge class id set
      -- into a vis style
      procedure Insert_Edge_Class_Id_Data_From_Set_Into_Vis_Style
        (The_Vis_Style_Access : in Visualisation_Style_Access;
         Edge_Class_Id_Set    : in Graph_Lib.Edge_Class_Id_Set;
         Edge_Data            : in Edge_Class_Vis_Data) is

         procedure Process_Element (Item : in Graph_Lib.Edge_Class_Id) is

         begin

            -- Unbind must always be possible as each edge class
            -- has a default setting that was already set
            Edge_Class_Id_Hashed_Mappings.Unbind
              (The_Vis_Style_Access.Edge_Class_Specific_Vis,
               Item);

            -- insert new config setting for edge class
            Edge_Class_Id_Hashed_Mappings.Bind
              (The_Vis_Style_Access.Edge_Class_Specific_Vis,
               Item,
               Edge_Data);

         end Process_Element;

         procedure Add_All_Element_Settings_To_Hash_Map is new
           Graph_Lib.Edge_Class_Id_Sets.Apply
           (Execute => Process_Element);

      begin

         Add_All_Element_Settings_To_Hash_Map (Edge_Class_Id_Set);
      end Insert_Edge_Class_Id_Data_From_Set_Into_Vis_Style;
      ------------------------------------------------------------------------


      -- this dataobject will be initialized and returned
      New_Vis_Style_Access : Visualisation_Style_Access := null;

      -- describes settings
      Node_Setting : Node_Class_Vis_Data;
      Edge_Setting : Edge_Class_Vis_Data;

      -- needed to process the xml files
      XML_Nodes_List_Top_Level : DOM.Core.Node_List;
      XML_Node_Top_Level       : DOM.Core.Node;

      -- needed to process node classes
      Node_Class_Names_List     : String_Lists.List;
      Node_Class_Names_ListIter : String_Lists.ListIter;
      A_Node_Class_Name         : Ada.Strings.Unbounded.Unbounded_String;

      A_Node_Class_Id           : Graph_Lib.Node_Class_Id;

      -- needed to process edge classes
      XML_Nodes_List_Edge_Classes : DOM.Core.Node_List;
      XML_Node_Edge_Class         : DOM.Core.Node;

      -- holds all attributes that should be visualized
      Attribute_Names_List : String_Lists.List;

      -- describes a set of node classes
      A_Node_Class_Id_Set : Graph_Lib.Node_Class_Id_Set;
      A_Node_Class_Id_SetIter :
        Graph_Lib.Node_Class_Id_Sets.Iterator;

      -- sets of edge classes
      A_Edge_Class_Id_Set : Graph_Lib.Edge_Class_Id_Set;
      A_Edge_Class_Id_SetIter :
        Graph_Lib.Edge_Class_Id_Sets.Iterator;

      A_Edge_Class_Id : Graph_Lib.Edge_Class_Id;

      -- needed for visualisation data
      A_Filter : Graph_Lib.Node_Attribute_Filters.Filter;

      -- needed for deallocation
      Dealloc_Filter : Graph_Lib.Node_Attribute_Filters.Filter;

   begin

      -- Data object describing a visualisation style
      New_Vis_Style_Access := new Vis_Style_Data;

      New_Vis_Style_Access.Style_Name := Vis_Style_Name;

      New_Vis_Style_Access.Node_Class_Specific_Vis :=
        Node_Class_Id_Hashed_Mappings.Create;

      New_Vis_Style_Access.Edge_Class_Specific_Vis :=
        Edge_Class_Id_Hashed_Mappings.Create;


      -- Step 0 global data
      ------------------------------------------------------------------------

      -- get <vis_window_background_color> color node
      XML_Nodes_List_Top_Level :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Vis_Style_XML_Document, "global_settings");
      -- lists only holds one node
      XML_Node_Top_Level :=
        DOM.Core.Nodes.Item (XML_Nodes_List_Top_Level, 0);

      --  Visualisation_Window_Background_Color
      Register_Color_Encoding
        (Ada.Strings.Unbounded.To_Unbounded_String
         (DOM.Core.Elements.Get_Attribute
          (XML_Node_Top_Level, "vis_window_background_color")),
         New_Vis_Style_Access.Global_Vis_Data.
         Visualisation_Window_Background_Color_Encoding);

      -- deallocation
      DOM.Core.Free (XML_Nodes_List_Top_Level);

      -- Step 1 process node classes default setting
      ------------------------------------------------------------------------
      XML_Nodes_List_Top_Level :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Vis_Style_XML_Document, "default_node_setting");

      -- Only one <default_node_setting> - node exists
      XML_Node_Top_Level := DOM.Core.Nodes.Item (XML_Nodes_List_Top_Level, 0);

      -- a "general node setting that does not yet hold a filter
      Node_Setting := Build_Node_Class_Vis_Data_Without_Filter
        (XML_Node_Top_Level, Vis_Style_Dir, Resources_Root_Dir);

      -- get all node classes known by the iml
      A_Node_Class_Id_Set := Graph_Lib.Get_All_Node_Class_Ids;

      -- get all attribute names
      Attribute_Names_List := Get_All_Attribute_Names (XML_Node_Top_Level);

      -- create the filter for each node class and insert the setting

      A_Node_Class_Id_SetIter :=
        Graph_Lib.Node_Class_Id_Sets.Make_Iterator
        (A_Node_Class_Id_Set);

      -- iterate over all node classes of the iml
      -- create filter and insert data into the vis style inernal
      -- data model.
      while Graph_Lib.Node_Class_Id_Sets.More
        (A_Node_Class_Id_SetIter) loop

         Graph_Lib.Node_Class_Id_Sets.Next
           (A_Node_Class_Id_SetIter, A_Node_Class_Id);

         -- Create Filter - for actaul node class id
         -------------------------

         -- ignores all attributes that the actual node class does not have
         A_Filter := Graph_Lib.Node_Attribute_Filters.Create
           (A_Node_Class_Id,
            Attribute_Names_List);

         -- add node class specific filter
         Node_Setting.Attribute_Filter := A_Filter;

         -- add setting to hash_map
         Node_Class_Id_Hashed_Mappings.Bind
           (New_Vis_Style_Access.Node_Class_Specific_Vis,
            A_Node_Class_Id,
            Node_Setting);

      end loop;

      -- deallocation
      String_Lists.Destroy (Attribute_Names_List);
      Graph_Lib.Node_Class_Id_Sets.Destroy (A_Node_Class_Id_SetIter);
      Graph_Lib.Node_Class_Id_Sets.Destroy (A_Node_Class_Id_Set);
      DOM.Core.Free (XML_Nodes_List_Top_Level);


      -- Step 2 process all node class specific settings
      -- Entries from Step 1 will be overwritten
      ------------------------------------------------------------------------
      XML_Nodes_List_Top_Level :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Vis_Style_XML_Document, "node_class_specific_setting");

      -- Several or no <node_class_specific_setting> - nodes may exists
      -- for each node_class the setting least read will be taken
      for I in 0 ..  DOM.Core.Nodes.Length(XML_Nodes_List_Top_Level) - 1 loop

         XML_Node_Top_Level :=
           DOM.Core.Nodes.Item (XML_Nodes_List_Top_Level, I);

         -- a "general" node setting that does not yet hold a filter
         Node_Setting := Build_Node_Class_Vis_Data_Without_Filter
           (XML_Node_Top_Level, Vis_Style_Dir, Resources_Root_Dir);

         -- get all attribute names that should be directly visualized
         Attribute_Names_List :=
           Get_All_Attribute_Names (XML_Node_Top_Level);

         -- create the filter for each node class and insert the setting

         -- 2.1 Get all node classes the actual setting should be used for
         -----------------------------------------------------------------
         Node_Class_Names_List :=
           Get_All_Node_Class_Names (XML_Node_Top_Level);

         Node_Class_Names_ListIter :=
           String_Lists.MakeListIter (Node_Class_Names_List);

         while String_Lists.More (Node_Class_Names_ListIter) loop

            -- get a file to process
            String_Lists.Next (Node_Class_Names_ListIter, A_Node_Class_Name);

            -- ignore not existing node classes
            if Graph_Lib.Does_Node_Class_Exist
              (Ada.Strings.Unbounded.To_String (A_Node_Class_Name)) then

               -- Get Node Class ID
               A_Node_Class_Id := Graph_Lib.Convert_Node_Class_Name_To_Id
                 (Ada.Strings.Unbounded.To_String (A_Node_Class_Name));

               -- Create Filter - for actual node class
               -------------------------

               -- ignores all attributes that the actual node class does not
               -- have
               A_Filter := Graph_Lib.Node_Attribute_Filters.Create
                 (A_Node_Class_Id, Attribute_Names_List);

               -- add node class specific filter
               Node_Setting.Attribute_Filter := A_Filter;

               -- replace setting, if already exists
               -- (it must exist, as a setting for each node class id
               -- was defined in Step 1).
               --
               -- if setting is not found, this will
               -- indicate an error in the graph_lib.

               -- Deallocation of Attribute Filter
               Dealloc_Filter := Node_Class_Id_Hashed_Mappings.Fetch
                 (New_Vis_Style_Access.Node_Class_Specific_Vis,
                   A_Node_Class_Id).Attribute_Filter;

               Graph_Lib.Node_Attribute_Filters.Destroy
                 (Dealloc_Filter);

               Node_Class_Id_Hashed_Mappings.Unbind
                 (New_Vis_Style_Access.Node_Class_Specific_Vis,
                   A_Node_Class_Id);

               ------
               -- add new setting to hash_map
               Node_Class_Id_Hashed_Mappings.Bind
                 (New_Vis_Style_Access.Node_Class_Specific_Vis,
                  A_Node_Class_Id,
                  Node_Setting);

            end if;
         end loop; -- end while

         -- deallocation of data allocated for each
         -- <node_class_specific_setting> - node
         String_Lists.Destroy (Node_Class_Names_List);
         String_Lists.Destroy (Attribute_Names_List);
      end loop; -- end for

      -- deallocation list of <node_class_specific_setting> - nodes
      DOM.Core.Free (XML_Nodes_List_Top_Level);


      -- Step 3 process edge classes default setting
      ------------------------------------------------------------------------
      XML_Nodes_List_Top_Level :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Vis_Style_XML_Document, "default_edge_setting");

      -- Only one <default_edge_setting> - node exists
      XML_Node_Top_Level := DOM.Core.Nodes.Item (XML_Nodes_List_Top_Level, 0);


      Edge_Setting := Build_Edge_Class_Vis_Data
        (XML_Node_Top_Level);

      -- get all edge classes known by the iml
      A_Edge_Class_Id_Set := Graph_Lib.Get_All_Edge_Class_Ids;

      A_Edge_Class_Id_SetIter :=
        Graph_Lib.Edge_Class_Id_Sets.Make_Iterator
        (A_Edge_Class_Id_Set);

      -- iterate over all edge classes of the iml
      -- and insert setting data into the vis style inernal
      -- data model.
      while Graph_Lib.Edge_Class_Id_Sets.More
        (A_Edge_Class_Id_SetIter) loop

         Graph_Lib.Edge_Class_Id_Sets.Next
           (A_Edge_Class_Id_SetIter, A_Edge_Class_Id);

         -- add setting to hash_map
         Edge_Class_Id_Hashed_Mappings.Bind
           (New_Vis_Style_Access.Edge_Class_Specific_Vis,
            A_Edge_Class_Id,
            Edge_Setting);

      end loop;

      -- deallocation
      Graph_Lib.Edge_Class_Id_Sets.Destroy (A_Edge_Class_Id_Set);
      Graph_Lib.Edge_Class_Id_Sets.Destroy (A_Edge_Class_Id_SetIter);
      DOM.Core.Free (XML_Nodes_List_Top_Level);

      -- Step 4 process all edge class specific settings
      -- Entries from Step 3 will be overwritten
      ------------------------------------------------------------------------

      XML_Nodes_List_Top_Level :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Vis_Style_XML_Document, "edge_class_specific_setting");

      -- Several or no <edge_class_specific_setting> - nodes may exists
      -- for each node_class the setting least read will be taken
      for I in 0 ..  DOM.Core.Nodes.Length(XML_Nodes_List_Top_Level) - 1 loop

         -- a <edge_class_specific_setting> node
         XML_Node_Top_Level :=
           DOM.Core.Nodes.Item (XML_Nodes_List_Top_Level, I);

         Edge_Setting := Build_Edge_Class_Vis_Data
           (XML_Node_Top_Level);

         -- get all edge classes definitions the actual setting should be
         -- taken for (each definition may represent several edge classes
         -- by using the vildcard "*");
         --
         -- does not regard inheritance
         XML_Nodes_List_Edge_Classes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
           (XML_Node_Top_Level, "edge_class");

         for I in 0 ..
           DOM.Core.Nodes.Length(XML_Nodes_List_Edge_Classes) - 1 loop

            -- a <edge_class> node
            XML_Node_Edge_Class := DOM.Core.Nodes.Item
              (XML_Nodes_List_Edge_Classes, I);

            -- calculate set of edge classes specified by this node
            A_Edge_Class_ID_Set :=
              Edge_Class_Proc.Process_Edge_Class_Entry (XML_Node_Edge_Class);

            -- check whether returned set is empty
            if Graph_Lib.Edge_Class_Id_Sets.Is_Empty
              (A_Edge_Class_ID_Set) then

               Graph_Lib.Edge_Class_Id_Sets.Destroy
                 (A_Edge_Class_ID_Set);
            else

               -- insert settings for edge classes into vis style
               -- the already existing setting for affected edge classes
               -- wille be overwritten
               Insert_Edge_Class_Id_Data_From_Set_Into_Vis_Style
                 (New_Vis_Style_Access, A_Edge_Class_Id_Set, Edge_Setting);

               -- deallocate returned edge class id set
               Graph_Lib.Edge_Class_Id_Sets.Destroy
                 (A_Edge_Class_ID_Set);
            end if;
         end loop;  -- end for

         -- deallocate list of <edge_class> - nodes
         DOM.Core.Free(XML_Nodes_List_Edge_Classes);


         -- get all edge classes definitions the actual setting should be
         -- taken for regarding inheritance
         XML_Nodes_List_Edge_Classes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
           (XML_Node_Top_Level, "super_edge_class");

         for I in 0 ..
           DOM.Core.Nodes.Length(XML_Nodes_List_Edge_Classes) - 1 loop

            -- a <super_edge_class> node
            XML_Node_Edge_Class := DOM.Core.Nodes.Item
              (XML_Nodes_List_Edge_Classes, I);

            -- calculate set of edge classes specified by this node
            -- regarding inheritance hierarchy
            A_Edge_Class_ID_Set :=
              IML_Class_Inheritance_Proc.Get_All_Sub_Edge_Classes
                (XML_Node_Edge_Class);

            -- check whether returned set is empty
            if Graph_Lib.Edge_Class_Id_Sets.Is_Empty
              (A_Edge_Class_ID_Set) then

               Graph_Lib.Edge_Class_Id_Sets.Destroy
                 (A_Edge_Class_ID_Set);
            else

               -- insert settings for edge classes into vis style
               -- the already existing setting for affected edge classes
               -- wille be overwritten
               Insert_Edge_Class_Id_Data_From_Set_Into_Vis_Style
                 (New_Vis_Style_Access, A_Edge_Class_Id_Set, Edge_Setting);

               -- deallocate returned edge class id set
               Graph_Lib.Edge_Class_Id_Sets.Destroy
                 (A_Edge_Class_ID_Set);
            end if;
         end loop;  -- end for

         -- deallocate list of <edge_class> - nodes
         DOM.Core.Free(XML_Nodes_List_Edge_Classes);

      end loop; -- end for

      -- deallocate list of <edge_class_specific_setting> - nodes
      DOM.Core.Free (XML_Nodes_List_Top_Level);

      -- now all data describing a visualisation style is read
      return New_Vis_Style_Access;
   end Process_XML_Vis_Style;

   ---------------------------------------------------------------------------
   -- Deallocates a visualisation style.
   --
   -- The node attribute filters are deallocated too.
   --
   -- Does not deallocate memory for the icon file names
   -- (Icons_Encoding_Map) and colors (cColors_Encoding_Map)
   -- as they may be used by other vis styles.
   --
   -- Icons (the file names) or colors my only be deallocated by calling
   -- "Clear_Config_Vis_Styles" i.e. by destroying the hole ADO.
   procedure Deallocate_Vis_Style_Access
     (The_Vis_Style_Access : in out Visualisation_Style_Access) is

      procedure Free_Visualisation_Style_Access is new
        Ada.Unchecked_Deallocation
        (Vis_Style_Data, Visualisation_Style_Access);

      -- needed to deallocate
      Node_Class_Specific_Vis_Iter :
        Node_Class_Id_Hashed_Mappings.Values_Iter;
      Dealloc_Setting : Node_Class_Vis_Data;

   begin

      -- deallocate the Attribute Filters for each node class
      Node_Class_Specific_Vis_Iter :=
        Node_Class_Id_Hashed_Mappings.Make_Values_Iter
        (The_Vis_Style_Access.Node_Class_Specific_Vis);

      while Node_Class_Id_Hashed_Mappings.More
        (Node_Class_Specific_Vis_Iter) loop

         Node_Class_Id_Hashed_Mappings.Next
           (Node_Class_Specific_Vis_Iter,
            Dealloc_Setting);

         Graph_Lib.Node_Attribute_Filters.Destroy
           (Dealloc_Setting.Attribute_Filter);
      end loop;

      -- deep deallocation
      Node_Class_Id_Hashed_Mappings.Destroy
        (The_Vis_Style_Access.Node_Class_Specific_Vis);
      Edge_Class_Id_Hashed_Mappings.Destroy
        (The_Vis_Style_Access.Edge_Class_Specific_Vis);

      Free_Visualisation_Style_Access (The_Vis_Style_Access);
   end Deallocate_Vis_Style_Access;


   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation of the internal data structure
   -- that holds all known visualisation styles (each visualisation style
   -- is regarded as an ADO).
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   procedure Initialize_Config_Vis_Styles

     (Resources_Root_Dir     : in String;
      GIANT_Vis_Directory    : in String;
      User_Vis_Directory     : in String;
      Default_Vis_Style_File : in String) is

      -- needed to ignore not correct xml files for visualisation styles
      Ignore_File : Boolean := False;

      File_List             : String_Lists.List;
      File_List_Iter        : String_Lists.ListIter;
      A_Vis_Style_File_Name : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      -- used to iterate over xml documets
      A_Vis_Style_Tree_Reader  : Tree_Readers.Tree_Reader;
      A_Vis_Style_XML_Document : Dom.Core.Document;
      A_Vis_Style_Name         : Ada.Strings.Unbounded.Unbounded_String;

      -- default vis style
      Default_Vis_Style_Tree_Reader  : Tree_Readers.Tree_Reader;
      Default_Vis_Style_XML_Document : Dom.Core.Document;

      -- Used to handle new vis styles - do not deallocate
      New_Vis_Style : Visualisation_Style_Access := null;
      Old_Vis_Style : Visualisation_Style_Access := null;

   begin

      -- reset counters:
      Global_Icon_Counter  := 1;
      Global_Color_Counter := 1;

      File_List := String_Lists.Create;

      -- get all xml files from GIANT_Vis_Directory and User_Vis_Directory
      -- (not the default vis style "Default_Vis_Style_File"
      begin
         if (GIANT_Vis_Directory /= "") then
            -- Get all xml files in GIANT_Vis_Directory
            String_Lists.Attach
              (File_List,
               File_Management.Get_Filtered_Files_From_Directory
               (GIANT_Vis_Directory, True, ".xml"));
         end if;
      exception
         when File_Management.Invalid_Directory_Exception =>
            null; --  ignore
      end;

      begin
         if (User_Vis_Directory /= "") then
            -- Get all xml files in User_Vis_Directory
            String_Lists.Attach
              (File_List,
               File_Management.Get_Filtered_Files_From_Directory
               (User_Vis_Directory, True, ".xml"));
         end if;
      exception
         when File_Management.Invalid_Directory_Exception =>
            null; --  ignore
      end;

      ------------------------------------------------
      -- get default vis_style - this style must exist
      begin
         XML_File_Access.Load_XML_File_Validated
           (Default_Vis_Style_File,
            Default_Vis_Style_Tree_Reader,
            Default_Vis_Style_XML_Document);
      exception
         when XML_File_Access.XML_File_Access_Error_Exception =>
            String_Lists.Destroy (File_List);
            raise Illegal_Default_Vis_Style_Exception;
         when XML_File_Access.XML_File_Parse_Fatal_Error_Exception =>
            String_Lists.Destroy (File_List);
            raise Illegal_Default_Vis_Style_Exception;
      end;

      -- check for correct type of xml file
      if (XML_File_Access.Does_XML_Document_Belong_To_Type
          ("giant_vis_style_file",
           Default_Vis_Style_XML_Document) = False) then

         Tree_Readers.Free(Default_Vis_Style_Tree_Reader);
         String_Lists.Destroy (File_List);
         raise Illegal_Default_Vis_Style_Exception;
      end if;

      -- get name of default vis style (the file name without ending and path)
      -- file name: "\def_vis\my_def_vis.xml" -->
      -- name of default vis style: "my_def_vis";

      -- INITIALIZE - The name of the default visualisation style
      Global_Default_Vis_Style_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (File_Management.Calculate_Name_For_File
            (Default_Vis_Style_File));

      -------------------------------
      -- INITIALIZE internal data strucuture
      Icons_Encoding_Map := Icons_Encoding_Mappings.Create;
      Colors_Encoding_Map := Colors_Encoding_Hashed_Mappings.Create;
      All_Vis_Styles_Map := All_Vis_Styles_Hashed_Mappings.Create;

      -------------------------------
      -- Process all files from GIANT_Vis_Directory and User_Vis_Directory
      -- not suitable (resp. valid) files will be ignored.
      -- does not read the default vis style)
      File_List_Iter := String_Lists.MakeListIter (File_List);

      while String_Lists.More (File_List_Iter) loop

         -- when a security check sets ignore = true the actual file will
         -- file "A_Vis_Style_File_Name" will no longer be processed
         Ignore_File := False;

         -- get a file to process
         String_Lists.Next (File_List_Iter, A_Vis_Style_File_Name);

         -- ignore not readable xml files
         begin

            XML_File_Access.Load_XML_File_Validated
              (Ada.Strings.Unbounded.To_String (A_Vis_Style_File_Name),
               A_Vis_Style_Tree_Reader,
               A_Vis_Style_XML_Document);

            -- ignore files that do not describe a visualisation style
            if (XML_File_Access.Does_XML_Document_Belong_To_Type
                ("giant_vis_style_file",
                 A_Vis_Style_XML_Document) = False) then

               Tree_Readers.Free(A_Vis_Style_Tree_Reader);
               Ignore_File := True;
            else

               -- calculate name
               ------------------
               A_Vis_Style_Name :=
                 Ada.Strings.Unbounded.To_Unbounded_String
                   (File_Management.Calculate_Name_For_File
                     (Ada.Strings.Unbounded.To_String
                       (A_Vis_Style_File_Name)));
            end if;

         exception
            when XML_File_Access.XML_File_Access_Error_Exception =>
               Ignore_File := True;
            when XML_File_Access.XML_File_Parse_Fatal_Error_Exception =>
               Ignore_File := True;
         end;

         -- Insert Vis_Style into internal data structure
         ---------------------------------------------------------------------
         if (Ignore_File = False) then
            begin
               -- build new vis style
               -- ignore vis style if reading failes
               New_Vis_Style := Process_XML_Vis_Style
                 (A_Vis_Style_XML_Document,
                  Resources_Root_Dir,
                  File_management.Return_Dir_Path_For_File_Path
                   (Ada.Strings.Unbounded.To_String (A_Vis_Style_File_Name)),
                  A_Vis_Style_Name) ;

            exception
               when others =>

                  -- ignore not correct vis styles (may cause the loss of
                  -- heap memory allocated by "Process_XML_Vis_Style").
                  -- proecess next vis style if exists
                  Ignore_File := True;
            end;
         end if;

         if (Ignore_File = False) then

            -- remove older vis_style (with same name) if exists
            if All_Vis_Styles_Hashed_Mappings.Is_Bound
              (All_Vis_Styles_Map, A_Vis_Style_Name) then

               Old_Vis_Style := All_Vis_Styles_Hashed_Mappings.Fetch
                 (All_Vis_Styles_Map, A_Vis_Style_Name);

               -- Remove old vis style from hash map
               All_Vis_Styles_Hashed_Mappings.Unbind
                 (All_Vis_Styles_Map, A_Vis_Style_Name);

               -- Deallocate old vis style
               Deallocate_Vis_Style_Access (Old_Vis_Style);
            end if;

            -- add new vis style to hash map
            All_Vis_Styles_Hashed_Mappings.Bind
              (All_Vis_Styles_Map, A_Vis_Style_Name, New_Vis_Style);

            -- deallocate Dom Tree for actual processed vis style
            Tree_Readers.Free(A_Vis_Style_Tree_Reader);
         end if;

      end loop; -- end while String_Lists.More (File_List_Iter)

      -- deallocation
      String_Lists.Destroy (File_List);

      ---------------
      -- Insert Default vis style - ensure that another vis style
      -- with the same name that was read befor will be replaced
      -- by the default vis style.

      -- build default vis style
      -- this style must be correct


      begin

         New_Vis_Style := Process_XML_Vis_Style
           (Default_Vis_Style_XML_Document,
            Resources_Root_Dir,
            File_management.Return_Dir_Path_For_File_Path
             (Default_Vis_Style_File),
            Global_Default_Vis_Style_Name);

      exception
         when others =>
            raise Illegal_Default_Vis_Style_Exception;
      end;

      -- remove other vis style with same name if necessary
      if All_Vis_Styles_Hashed_Mappings.Is_Bound
        (All_Vis_Styles_Map, Global_Default_Vis_Style_Name) then

         Old_Vis_Style := All_Vis_Styles_Hashed_Mappings.Fetch
           (All_Vis_Styles_Map, Global_Default_Vis_Style_Name);

         -- Remove old vis style from hash map
         All_Vis_Styles_Hashed_Mappings.Unbind
           (All_Vis_Styles_Map, Global_Default_Vis_Style_Name);

         -- Deallocate old vis style
         Deallocate_Vis_Style_Access (Old_Vis_Style);
      end if;

      -- add default vis style to hash map
      All_Vis_Styles_Hashed_Mappings.Bind
        (All_Vis_Styles_Map, Global_Default_Vis_Style_Name, New_Vis_Style);

      -- deallocate DOM Tree for default visualisation style
      Tree_Readers.Free (Default_Vis_Style_Tree_Reader);

      -- build data arrays
      Build_Encoded_Icons_Array;
      Build_Encoded_Colors_Array;

      -- mark ADO as initialized
      ADO_Initialized := True;
   end Initialize_Config_Vis_Styles;

   ---------------------------------------------------------------------------
   procedure Free_Node_Icons_Array_Access is new Ada.Unchecked_Deallocation
     (Node_Icons_Array, Node_Icons_Array_Access);

   ---------------------------------------------------------------------------
   procedure Free_Color_Access_Array_Access is new Ada.Unchecked_Deallocation
     (Color_Access_Array, Color_Access_Array_Access);


   procedure Clear_Config_Vis_Styles is

      All_Vis_Styles_Map_Iter  : All_Vis_Styles_Hashed_Mappings.Values_Iter;
      Dealloc_Vis_Style_Access : Visualisation_Style_Access;

      Colors_Encoding_Map_Iter : Colors_Encoding_Hashed_Mappings.Values_Iter;
      Dealloc_Data             : Color_Encoding_Data_Element;

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;


      -- "deinitialize" Default_Vis_Style_Name
      -----------------------------------------
      Global_Default_Vis_Style_Name :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      -- Deallocate all vis styles
      ------------------------------
      All_Vis_Styles_Map_Iter :=
        All_Vis_Styles_Hashed_Mappings.Make_Values_Iter
        (All_Vis_Styles_Map);

      -- deep deallocation for each vis style
      while All_Vis_Styles_Hashed_Mappings.More
        (All_Vis_Styles_Map_Iter) loop

         All_Vis_Styles_Hashed_Mappings.Next
           (All_Vis_Styles_Map_Iter,
            Dealloc_Vis_Style_Access);

         -- deallocates a visualisation style
         Deallocate_Vis_Style_Access (Dealloc_Vis_Style_Access);
      end loop;

      -- deallocate global hash map for vis styles
      All_Vis_Styles_Hashed_Mappings.Destroy (All_Vis_Styles_Map);


      -- deallocates memory for all loaded icons file names
      ----------------------------------
      Icons_Encoding_Mappings.Destroy (Icons_Encoding_Map);
      Free_Node_Icons_Array_Access (Encoded_Icons_Array);


      -- deallocates all colors (color strings)
      ------------------------------------------
      Colors_Encoding_Map_Iter :=
        Colors_Encoding_Hashed_Mappings.Make_Values_Iter
        (Colors_Encoding_Map);

      while Colors_Encoding_Hashed_Mappings.More
        (Colors_Encoding_Map_Iter) loop

         Colors_Encoding_Hashed_Mappings.Next
           (Colors_Encoding_Map_Iter,
            Dealloc_Data);

         -- Free memory used to store the color string
         Config.Free_Color_Access (Dealloc_Data.Color);

      end loop;

      -- deallocate global hash map
      Colors_Encoding_Hashed_Mappings.Destroy (Colors_Encoding_Map);
      Free_Color_Access_Array_Access (Encoded_Colors_Array);

      -- mark ADO as not initialized
      --------------------------------
      ADO_Initialized := False;

   end Clear_Config_Vis_Styles;


   ---------------------------------------------------------------------------
   -- B
   -- Mangement of Visualisation Styles
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Number_Of_Known_Vis_Styles return Integer is

      -- Iterate over all known vis styles
      All_Vis_Styles_Map_Iter : All_Vis_Styles_Hashed_Mappings.Values_Iter;
      Vis_Style_Access : Visualisation_Style_Access;
      Vis_Style_Count : Integer := 0;

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      All_Vis_Styles_Map_Iter :=
        All_Vis_Styles_Hashed_Mappings.Make_Values_Iter
        (All_Vis_Styles_Map);

      -- count visualisation styles
      while All_Vis_Styles_Hashed_Mappings.More
        (All_Vis_Styles_Map_Iter) loop

         All_Vis_Styles_Hashed_Mappings.Next
           (All_Vis_Styles_Map_Iter,
            Vis_Style_Access);

         Vis_Style_Count := Vis_Style_Count + 1;
      end loop;

      return Vis_Style_Count;
   end Get_Number_Of_Known_Vis_Styles;

   ---------------------------------------------------------------------------
   function Get_All_Vis_Styles return String_Lists.List is

      -- holds the names of all known vis styles
      Names_List : String_Lists.List;

      -- Iterate over all known vis styles
      All_Vis_Styles_Map_Iter : All_Vis_Styles_Hashed_Mappings.Values_Iter;
      Vis_Style_Access : Visualisation_Style_Access;

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      Names_List := String_Lists.Create;

      All_Vis_Styles_Map_Iter :=
        All_Vis_Styles_Hashed_Mappings.Make_Values_Iter
        (All_Vis_Styles_Map);

      -- get names
      while All_Vis_Styles_Hashed_Mappings.More
        (All_Vis_Styles_Map_Iter) loop

         All_Vis_Styles_Hashed_Mappings.Next
           (All_Vis_Styles_Map_Iter,
            Vis_Style_Access);

         String_Lists.Attach
           (Names_List, Get_Name_Of_Vis_Style (Vis_Style_Access));
      end loop;

      return Names_List;
   end Get_All_Vis_Styles;

   ---------------------------------------------------------------------------
   function Does_Vis_Style_Exist
     (Vis_Style_Name : in String)
     return Boolean is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      return All_Vis_Styles_Hashed_Mappings.Is_Bound
        (All_Vis_Styles_Map,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Style_Name));
   end Does_Vis_Style_Exist;

   ---------------------------------------------------------------------------
   function Initialize_Vis_Style_By_Name
     (Vis_Style_Name : in String)
     return Visualisation_Style_Access is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Does_Vis_Style_Exist (Vis_Style_Name) = False) then
         raise Visualisation_Style_Not_Found_Exception;
      end if;

      return All_Vis_Styles_Hashed_Mappings.Fetch
        (All_Vis_Styles_Map,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Style_Name));

   end Initialize_Vis_Style_By_Name;

   ---------------------------------------------------------------------------
   function Get_Name_Of_Vis_Style
     (Vis_Style : in Visualisation_Style_Access)
     return Ada.Strings.Unbounded.Unbounded_String is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Vis_Style.Style_Name;
   end Get_Name_Of_Vis_Style;

   ---------------------------------------------------------------------------
   function Get_Default_Vis_Style return Visualisation_Style_Access is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
         return null;
      end if;

      return Initialize_Vis_Style_By_Name
        (Ada.Strings.Unbounded.To_String (Global_Default_Vis_Style_Name));
   end Get_Default_Vis_Style;


   ---------------------------------------------------------------------------
   -- C.0
   -- Encoded Colors
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_All_Colors return Color_Access_Array_Access is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      return Encoded_Colors_Array;
   end Get_All_Colors;

   ---------------------------------------------------------------------------
   -- C.1
   -- Global visualisation data
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Vis_Window_Background_Color
     (Vis_Style : in Visualisation_Style_Access)
     return Integer is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Vis_Style.Global_Vis_Data.
         Visualisation_Window_Background_Color_Encoding;
   end Get_Vis_Window_Background_Color;


   ---------------------------------------------------------------------------
   -- C.2
   -- Node Class specific visualisation data
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_All_Node_Icons return Node_Icons_Array_Access is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      return Encoded_Icons_Array;
   end Get_All_Node_Icons;

   ---------------------------------------------------------------------------
   function Get_Node_Icon_Encoding
     (Vis_Style  : in Visualisation_Style_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Integer is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Node_Class_Id_Hashed_Mappings.Fetch
        (Vis_Style.Node_Class_Specific_Vis, Node_Class).Icon_Encoding;
   end Get_Node_Icon_Encoding;

   ---------------------------------------------------------------------------
   function Get_Attribute_Filter
     (Vis_Style  : in Visualisation_Style_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Graph_Lib.Node_Attribute_Filters.Filter is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Node_Class_Id_Hashed_Mappings.Fetch
        (Vis_Style.Node_Class_Specific_Vis, Node_Class).Attribute_Filter;
   end Get_Attribute_Filter;

   ---------------------------------------------------------------------------
   function Get_Border_Color
     (Vis_Style  : in Visualisation_Style_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Integer is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Node_Class_Id_Hashed_Mappings.Fetch
        (Vis_Style.Node_Class_Specific_Vis, Node_Class).Border_Color_Encoding;
   end Get_Border_Color;

   ---------------------------------------------------------------------------
   function Get_Fill_Color
     (Vis_Style  : in Visualisation_Style_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Integer is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Node_Class_Id_Hashed_Mappings.Fetch
        (Vis_Style.Node_Class_Specific_Vis, Node_Class).Fill_Color_Encoding;
   end Get_Fill_Color;

   ---------------------------------------------------------------------------
   function Get_Text_Color
     (Vis_Style  : in Visualisation_Style_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Integer is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Node_Class_Id_Hashed_Mappings.Fetch
        (Vis_Style.Node_Class_Specific_Vis, Node_Class).Text_Color_Encoding;
   end Get_Text_Color;

   ---------------------------------------------------------------------------
   -- C.3
   -- Edge Class specific visualisation data
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Line_Color
     (Vis_Style  : in Visualisation_Style_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Integer is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Edge_Class_Id_Hashed_Mappings.Fetch
        (Vis_Style.Edge_Class_Specific_Vis, Edge_Class).Line_Color_Encoding;
   end Get_Line_Color;

   ---------------------------------------------------------------------------
   function Get_Text_Color
     (Vis_Style  : in Visualisation_Style_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Integer is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Edge_Class_Id_Hashed_Mappings.Fetch
        (Vis_Style.Edge_Class_Specific_Vis, Edge_Class).Text_Color_Encoding;
   end Get_Text_Color;

   ---------------------------------------------------------------------------
   function Get_Line_Style
     (Vis_Style  : in Visualisation_Style_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Edge_Line_Style is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Edge_Class_Id_Hashed_Mappings.Fetch
        (Vis_Style.Edge_Class_Specific_Vis, Edge_Class).Line_Style;
   end Get_Line_Style;

   ---------------------------------------------------------------------------
   function Show_Label_For_Edge_Class_Name
     (Vis_Style  : in Visualisation_Style_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Boolean is

   begin

      if (ADO_Initialized = False) then
         raise Config_Vis_Styles_Not_Initialized_Exception;
      end if;

      if (Vis_Style = null) then
         raise Visualisation_Style_Access_Not_Initialized_Exception;
      end if;

      return Edge_Class_Id_Hashed_Mappings.Fetch
        (Vis_Style.Edge_Class_Specific_Vis, Edge_Class).Label;
   end Show_Label_For_Edge_Class_Name;

end Giant.Config.Vis_Styles;
