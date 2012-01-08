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
--  $RCSfile: giant-projects.adb,v $, $Revision: 1.51 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib; -- from GNAT
with GNAT.Directory_Operations; -- from GNAT

with Bauhaus_IO; -- from Bauhaus IML "Reuse.src"

with Tree_Readers;       -- from xmlada
with Dom.Core;           -- from xmlada
with Dom.Core.Nodes;     -- from xmlada
with Dom.Core.Documents; -- from xmlada
with Dom.Core.Elements;  -- from xmlada

with Giant.File_Management; -- from GIANT
with Giant.XML_File_Access; -- from GIANT
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);


package body Giant.Projects is

   package Logger is new Giant.Logger("giant.projects");

   ---------------------------------------------------------------------------
   -- Note:
   --  The Management files for subgarphs, vis windows and projects
   --  always are named this way:
   --  "path" & "name of project /vis window / subgraph" & "ending"
   -- You should not change this.

   ---------------------------------------------------------------------------
   --  Name used for the file holding the node annotations
   --  - only used when a new project is created.
   Const_Node_Annotations_File_Name : constant String :=
     "node_annotations.xml";

   ---------------------------------------------------------------------------
   --  Ending of Management file for a visual window
   Const_Vis_Window_File_Ending : constant String := ".viswin";

   --  Security files ending
   Const_Vis_Window_Security_File_Ending : constant String := ".viswin~";

   ---------------------------------------------------------------------------
   --  Ending of Management file for a subgraph
   Const_Subgraph_File_Ending : constant String := ".subgraph";

   --  Security files ending
   Const_Subgraph_Security_File_Ending : constant String := ".subgraph~";


   ---------------------------------------------------------------------------
   --  0.1
   --  Streaming functionality for platform independent streams.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Does not read all parts of the record !!!
   procedure Subgraph_Data_Element_Read
     (Stream  : in     Bauhaus_IO.In_Stream_Type;
      Element :    out Subgraph_Data_Element) is

      Highlight_Integer_Id : Integer;
   begin

      Giant.Graph_Lib.Subgraphs.Subgraph_Read (Stream, Element.Subgraph);

      --  Read Highlight Status
      Bauhaus_IO.Read_Integer (Stream, Highlight_Integer_Id);
      Element.Highlight_Status :=
        Subgraph_Highlight_Status'Val (Highlight_Integer_Id);
   end Subgraph_Data_Element_Read;

   ---------------------------------------------------------------------------
   --  Does not write all parts of the record !!!
   procedure Subgraph_Data_Element_Write
     (Stream  : in Bauhaus_IO.Out_Stream_Type;
      Element : in Subgraph_Data_Element) is

      Highlight_Integer_Id : Integer;
   begin

      Giant.Graph_Lib.Subgraphs.Subgraph_Write (Stream, Element.Subgraph);

      --  Write Highlight Status (via Conversion to integer)
      Highlight_Integer_Id :=
        Subgraph_Highlight_Status'Pos (Element.Highlight_Status);
      Bauhaus_IO.Write_Integer (Stream, Highlight_Integer_Id);
   end Subgraph_Data_Element_Write;


   ---------------------------------------------------------------------------
   --  0.2
   --  Internal Subprograms
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Checks whether the project's iml graph matches the iml graph that
   --  is actually loaded by the graph_lib.
   --  The check is based on an hash id value calculated for the iml graph.
   function Is_Correct_IML_Graph_Loaded (Graph_ID : in Integer)
                                        return Boolean is
   begin
      if (Graph_Lib.Get_Graph_Hash = Graph_ID) then
         return True;
      else
         return False;
      end if;
   end Is_Correct_IML_Graph_Loaded;

   ---------------------------------------------------------------------------
   --  Used to create absolute file names (with absolute path) for
   --  files regarding the name and the ending.
   function Create_Name_For_File
     (Directory : in String;
      Name      : in String;
      Ending    : in String)
     return Ada.Strings.Unbounded.Unbounded_String is

      use Ada.Strings.Unbounded;

      Absolute_Dir_Path : Ada.Strings.Unbounded.Unbounded_String;
   begin

      -- calculate absolute path for dir based on the current execution
      -- environment directory.
      -- Does no changes if "Directory" already is an absolute path.
      Absolute_Dir_Path := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Absolute_Path_To_Directory_From_Relative
         (GNAT.Directory_Operations.Get_Current_Dir,
          Directory));

      -- build file name for thefile
      return Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Append_Dir_Separator_If_Necessary
         (Ada.Strings.Unbounded.To_String (Absolute_Dir_Path))
         & Name
         & Ending);
   end Create_Name_For_File;

   ---------------------------------------------------------------------------
   --  Deletes all files in the directory
   --  with the corresponding endings regardeless of the content.
   procedure Kill_All_Security_Files (Directory : in String) is

      File_List      : String_Lists.List;
      File_List_Iter : String_Lists.ListIter;
      A_File_Name    : Ada.Strings.Unbounded.Unbounded_String;
   begin

      File_List := String_Lists.Create;

      -- Security Files for subgraphs
      String_Lists.Attach
        (File_List,
         File_Management.Get_Filtered_Files_From_Directory
         (Directory,
          True,
          Const_Subgraph_Security_File_Ending));

      -- Security Files for visualisation windows
      String_Lists.Attach
        (File_List,
         File_Management.Get_Filtered_Files_From_Directory
         (Directory,
          True,
          Const_Vis_Window_Security_File_Ending));

      File_List_Iter := String_Lists.MakeListIter (File_List);

      while String_Lists.More (File_List_Iter) loop

         String_Lists.Next (File_List_Iter, A_File_Name);
         File_Management.Delete_File (Ada.Strings.Unbounded.To_String
                                      (A_File_Name));
      end loop;

      String_Lists.Destroy (File_List);
   end Kill_All_Security_Files;

   ---------------------------------------------------------------------------
   -- Increases fault tolerance:
   --   Ensures that the Name of the vis window corresponds to the
   --   file's name from that it is loaded (user may have changed this name).
   function Load_Vis_Window_Into_Main_Memory
     (File_Path   : in String;
      Annotations : in Node_Annotations.Node_Annotation_Access)
     return Vis_Windows.Visual_Window_Access is

      Stream_File       : Ada.Streams.Stream_IO.File_Type;
      Ada_Stream        : Ada.Streams.Stream_IO.Stream_Access;
      Bauhaus_In_Stream : Bauhaus_IO.In_Stream_Type;
      New_Vis_Window    : Vis_Windows.Visual_Window_Access;
   begin

      Ada.Streams.Stream_IO.Open
        (Stream_File,
         Ada.Streams.Stream_IO.In_File,
         File_Path);

      Ada_Stream := Ada.Streams.Stream_IO.Stream (Stream_File);
      Bauhaus_In_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);

      Vis_Windows.Visual_Window_Access_Read
        (Bauhaus_In_Stream, New_Vis_Window, Annotations);

      -- close resources
      Bauhaus_IO.Release (Bauhaus_In_Stream);
      Ada.Streams.Stream_IO.Close (Stream_File);

      -- ENFORCE Name consistency
      Vis_Windows.Change_Name
        (New_Vis_Window,
         File_Management.Calculate_Name_For_File (File_Path));

      return New_Vis_Window;
   end Load_Vis_Window_Into_Main_Memory;

   ---------------------------------------------------------------------------
   procedure Write_Vis_Window_To_File
     (File_Path  : in String;
      Vis_Window : in Vis_Windows.Visual_Window_Access) is

      Stream_File        : Ada.Streams.Stream_IO.File_Type;
      Ada_Stream         : Ada.Streams.Stream_IO.Stream_Access;
      Bauhaus_Out_Stream : Bauhaus_IO.Out_Stream_Type;
   begin

     Logger.Debug("Sec File: " & File_Path);

      -- test if file exists, create new one if necessary
      begin
         Ada.Streams.Stream_IO.Open
           (Stream_File,
            Ada.Streams.Stream_IO.Out_File,
            File_Path);
      exception
         when Ada.Streams.Stream_IO.Name_Error =>
         Ada.Streams.Stream_IO.Create
           (Stream_File,
            Ada.Streams.Stream_IO.Out_File,
            File_Path);
      end;

      Ada_Stream := Ada.Streams.Stream_IO.Stream (Stream_File);
      Bauhaus_Out_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);

      Vis_Windows.Visual_Window_Access_Write
        (Bauhaus_Out_Stream, Vis_Window);

      -- close resources
      Bauhaus_IO.Release (Bauhaus_Out_Stream);
      Ada.Streams.Stream_IO.Close (Stream_File);
   end Write_Vis_Window_To_File;

   ---------------------------------------------------------------------------
   -- Increases fault tolerance:
   --   Ensures that the Name of the subgraph corresponds to the
   --   file's name from that it is loaded
   --   (user may have changed this name).
   function Load_Sub_Graph_Data_Into_Main_Memory
     (File_Path : String)
     return Subgraph_Data_Element is

      Stream_File        : Ada.Streams.Stream_IO.File_Type;
      Ada_Stream         : Ada.Streams.Stream_IO.Stream_Access;
      Bauhaus_In_Stream  : Bauhaus_IO.In_Stream_Type;
      New_Sub_Graph_Data : Subgraph_Data_Element;
   begin

      Ada.Streams.Stream_IO.Open
        (Stream_File,
         Ada.Streams.Stream_IO.In_File,
         File_Path);

      Ada_Stream        := Ada.Streams.Stream_IO.Stream (Stream_File);
      Bauhaus_In_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);

      Subgraph_Data_Element_Read
        (Bauhaus_In_Stream, New_Sub_Graph_Data);

      -- TODO Name Consistency CHECK

      --  close resources
      Bauhaus_IO.Release (Bauhaus_In_Stream);
      Ada.Streams.Stream_IO.Close (Stream_File);

      --  -----
      New_Sub_Graph_Data.Is_File_Linked := True;
      New_Sub_Graph_Data.Existing_Subgraph_File :=
        Ada.Strings.Unbounded.To_Unbounded_String (File_Path);

      -- ENFORCE Name consistency
      Graph_Lib.Subgraphs.Rename
        (New_Sub_Graph_Data.Subgraph,
         File_Management.Calculate_Name_For_File (File_Path));

      return New_Sub_Graph_Data;
   end Load_Sub_Graph_Data_Into_Main_Memory;

   ---------------------------------------------------------------------------
   procedure Write_Sub_Graph_Data_To_File
     (File_Path     : in String;
      Subgraph_Data : in Subgraph_Data_Element) is

      Stream_File        : Ada.Streams.Stream_IO.File_Type;
      Ada_Stream         : Ada.Streams.Stream_IO.Stream_Access;
      Bauhaus_Out_Stream : Bauhaus_IO.Out_Stream_Type;

   begin

      -- check if file exists, create new one if necessary
      if (not GNAT.OS_Lib.Is_Writable_File (File_Path)) then

         Ada.Streams.Stream_IO.Create
           (Stream_File,
            Ada.Streams.Stream_IO.Out_File,
            File_Path);
      else
         Ada.Streams.Stream_IO.Open
           (Stream_File,
            Ada.Streams.Stream_IO.Out_File,
            File_Path);
      end if;

      Ada_Stream := Ada.Streams.Stream_IO.Stream (Stream_File);
      Bauhaus_Out_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);

      Subgraph_Data_Element_Write
        (Bauhaus_Out_Stream, Subgraph_Data);

      --  close resources
      Bauhaus_IO.Release (Bauhaus_Out_Stream);
      Ada.Streams.Stream_IO.Close (Stream_File);
   end Write_Sub_Graph_Data_To_File;


   ---------------------------------------------------------------------------
   --  0.3
   --  Management of project related xml files
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Writes an external DTD into the directory where the xml project file
   --  is located.
   procedure Write_DTD_To_Directory
     (Project_Directory : in String) is

      use Ada.Strings.Unbounded;

      Abs_DTD_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      DTD_File          : Ada.Text_IO.File_Type;
   begin

      --  calculate absolute dtd file name
      Abs_DTD_File_Name := Create_Name_For_File
        (Project_Directory,
         "giant_project_file",
         ".dtd");

      Ada.Text_IO.Create
        (DTD_File,
         Ada.Text_IO.Out_File,
         Ada.Strings.Unbounded.To_String (Abs_DTD_File_Name));
      Ada.Text_IO.Set_Output(DTD_File);

      -- Write content of dtd file
      Ada.Text_IO.Put_Line
        ("<!ELEMENT giant_project_file "
         &"(global_data, visualisation_windows, subgraphs)>");

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("  <!ELEMENT global_data EMPTY>");
      Ada.Text_IO.Put_Line
        ("  <!ATTLIST global_data");
      Ada.Text_IO.Put_Line
        ("    iml_graph_file_path        CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line
        ("    iml_graph_checksum         CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line
        ("    node_annotations_file_name CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line
        ("  >");

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("  <!ELEMENT visualisation_windows (a_vis_window_file)*>");

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("    <!ELEMENT a_vis_window_file EMPTY>");
      Ada.Text_IO.Put_Line
        ("    <!ATTLIST a_vis_window_file");
      Ada.Text_IO.Put_Line
        ("      file_path  CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line
        ("    >");

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("  <!ELEMENT subgraphs (a_subgraph_file)*>");

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("    <!ELEMENT a_subgraph_file EMPTY>");
      Ada.Text_IO.Put_Line
        ("    <!ATTLIST a_subgraph_file");
      Ada.Text_IO.Put_Line
        ("      file_path  CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line
        ("    >");

      -- close resources
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (DTD_File);
   end Write_DTD_To_Directory;

   ---------------------------------------------------------------------------
   --  If you use this function to store a project you should first
   --  write the visualisation windows into the stream files, as this
   --  procedure only writes entries for vis windows into the
   --  project file that are already "file linked" (same counts for
   --  subgraphs).
   procedure Write_Project_XML_File (The_Project : Project_Access) is

      Project_File : Ada.Text_IO.File_Type;
      Abs_Project_File_Name : Ada.Strings.Unbounded.Unbounded_String;

      Vis_Window_Iter : Known_Vis_Windows_Hashs.Values_Iter;
      A_Vis_Window_Data_Element : Vis_Window_Data_Element;

      Subgraphs_Iter : Subgraph_Data_Hashs.Values_Iter;
      A_Subgraph_Data_Element : Subgraph_Data_Element;

      Rel_File_Path : Ada.Strings.Unbounded.Unbounded_String;

   begin

      Abs_Project_File_Name := Create_Name_For_File
        (Ada.Strings.Unbounded.To_String (The_Project.Abs_Project_Directory),
         Ada.Strings.Unbounded.To_String (The_Project.Project_Name),
         ".xml");

      --  create the file
      -------------------
      Ada.Text_IO.Create
        (Project_File,
         Ada.Text_IO.Out_File,
         Ada.Strings.Unbounded.To_String (Abs_Project_File_Name));
      Ada.Text_IO.Set_Output (Project_File);

      --  write xml file header for external dtd
      ------------------------------------------
      Ada.Text_IO.Put_Line
        ("<?xml version=""1.0"" encoding=""ISO-8859-1""?>");
      Ada.Text_IO.Put_Line
        ("<!DOCTYPE giant_project_file");
      Ada.Text_IO.Put_Line
        ("  SYSTEM ""giant_project_file.dtd"">");
      Ada.Text_IO.New_Line;

      --  open top level xml node (document node)
      Ada.Text_IO.Put_Line
        ("<giant_project_file>");

      --  write entry for global project data
      ---------------------------------------
      Ada.Text_IO.Put_Line
        ("  <global_data");

      Rel_File_Path := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Relative_Path_From_Absolute
          (Ada.Strings.Unbounded.To_String
            (The_Project.Abs_Project_Directory),
           Ada.Strings.Unbounded.To_String
            (The_Project.Abs_Bauhaus_IML_Graph_File)));


      Ada.Text_IO.Put_Line
        ("    iml_graph_file_path = """
         & Ada.Strings.Unbounded.To_String
         (Rel_File_Path)
         & """");
      Ada.Text_IO.Put_Line
        ("    iml_graph_checksum = """
         & Integer'Image (The_Project.Bauhaus_IML_Graph_File_Checksum)
         & """") ;
      -- node annotations file

      -- Calculate relative path if possible
      Rel_File_Path := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Relative_Path_From_Absolute
          (Ada.Strings.Unbounded.To_String
            (The_Project.Abs_Project_Directory),
           Ada.Strings.Unbounded.To_String
            (The_Project.Node_Annotations_File)));

      Ada.Text_IO.Put_Line
        ("    node_annotations_file_name = """
         & Ada.Strings.Unbounded.To_String
         (Rel_File_Path)
         & """ />");

      --  write entries for the files holding the
      --  streamed visualisation windows
      --  tries to figure out a relative path
      -------------------------------------------
      Ada.Text_IO.Put_Line
        ("  <visualisation_windows>");

      --  iterate over all visualisation windows
      --  only writes windows that are FILE_Linked  !!!
      Vis_Window_Iter := Known_Vis_Windows_Hashs.Make_Values_Iter
        (The_Project.All_Vis_Windows);

      while Known_Vis_Windows_Hashs.More (Vis_Window_Iter) loop

         Known_Vis_Windows_Hashs.Next
           (Vis_Window_Iter, A_Vis_Window_Data_Element);

         -- Calculate relative path if possible
         Rel_File_Path := Ada.Strings.Unbounded.To_Unbounded_String
           (File_Management.Get_Relative_Path_From_Absolute
             (Ada.Strings.Unbounded.To_String
                (The_Project.Abs_Project_Directory),
              Ada.Strings.Unbounded.To_String
                (A_Vis_Window_Data_Element.Existing_Vis_Window_File)));

         if (A_Vis_Window_Data_Element.Is_File_Linked = True) then

            Ada.Text_IO.Put_Line
              ("    <a_vis_window_file file_path = """
               & Ada.Strings.Unbounded.To_String
                   (Rel_File_Path)
               & """ />");
         end if;
      end loop;

      Ada.Text_IO.Put_Line
        ("  </visualisation_windows>");

      --  write entries for the files holding the iml subgraphs
      ---------------------------------------------------------
      Ada.Text_IO.Put_Line
        ("  <subgraphs>");

      Subgraphs_Iter := Subgraph_Data_Hashs.Make_Values_Iter
        (The_Project.All_Subgraphs);

      -- only writes entries for subgraphs that have an management file
      -- (and are "file linked") !!!
      while Subgraph_Data_Hashs.More (Subgraphs_Iter) loop

         Subgraph_Data_Hashs.Next
           (Subgraphs_Iter, A_Subgraph_Data_Element);

         Rel_File_Path := Ada.Strings.Unbounded.To_Unbounded_String
           (File_Management.Get_Relative_Path_From_Absolute
             (Ada.Strings.Unbounded.To_String
                (The_Project.Abs_Project_Directory),
              Ada.Strings.Unbounded.To_String
                (A_Subgraph_Data_Element.Existing_Subgraph_File)));

         if (A_Subgraph_Data_Element.Is_File_Linked = True) then

            Ada.Text_IO.Put_Line
              ("    <a_subgraph_file file_path = """
               & Ada.Strings.Unbounded.To_String
               (Rel_File_Path)
               & """ />");
         end if;
      end loop;

      Ada.Text_IO.Put_Line
        ("  </subgraphs>");

      -- last entry in file - close top level xml node (document node)
      Ada.Text_IO.Put_Line
        ("</giant_project_file>");

      --  close down resources
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (Project_File);
   end Write_Project_XML_File;

   ----------------------------------------------------------------------------
   procedure Free_Project_Access is new Ada.Unchecked_Deallocation
     (Project_Element, Project_Access);


   ---------------------------------------------------------------------------
   -- A
   -- General Project Management
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Project_Exist
     (Project_Name      : in String;
      Project_Directory : in String)
     return Boolean is

      Absolute_Project_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      A_File : Ada.Text_IO.File_Type;
      Project_Exists : Boolean := False;

      A_Tree_Reader  : Tree_Readers.Tree_Reader;
      A_XML_Document : Dom.Core.Document;
   begin

      -- Check whether the directory exists
      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;

      -- build file name
      Absolute_Project_File_Name := Create_Name_For_File
        (Project_Directory,
         Project_Name,
         ".xml");

      -- check whether the xml file is a file that describes project
      begin

         XML_File_Access.Load_XML_File_Validated
           (Ada.Strings.Unbounded.To_String
            (Absolute_Project_File_Name),
            A_Tree_Reader,
            A_XML_Document);

         -- check for project files
         if (XML_File_Access.Does_XML_Document_Belong_To_Type
             ("giant_project_file", A_XML_Document) = True) then

            Project_Exists := True;
         end if;

         Tree_Readers.Free (A_Tree_Reader);
      exception
         when Error : others =>
            Logger.Error ("error parsing project file");
            Logger.Error (Error);

            Project_Exists := False;
      end;

      return Project_Exists;
   end Does_Project_Exist;

   ---------------------------------------------------------------------------
   function Does_Project_Exist_File
     (Project_File_Name : in String)
      return Boolean is
   begin

      return Does_Project_Exist
        (Project_Name =>
           File_Management.Calculate_Name_For_File (Project_File_Name),
         Project_Directory =>
           File_Management.Return_Dir_Path_For_File_Path
         (Project_File_Name));
   exception
     when File_Management.Directory_Could_Not_Be_Calculated_Exception =>
        return False;
     when Invalid_Project_Directory_Excpetion =>
        return False;
   end Does_Project_Exist_File;

   ---------------------------------------------------------------------------
   function Is_Already_A_Project_File_In_Directory
     (Project_Directory : in String)
     return Boolean is

      Project_File_Found : Boolean := False;

      File_List          : String_Lists.List;
      File_List_Iter     : String_Lists.ListIter;

      A_Tree_Reader      : Tree_Readers.Tree_Reader;
      A_XML_Document     : Dom.Core.Document;

      A_File_Name : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
   begin

      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;

      -- process all xml files in the directory and check whether one is a
      -- project file
      File_List := String_Lists.Create;

      String_Lists.Attach
        (File_List,
         File_Management.Get_Filtered_Files_From_Directory
         (Project_Directory, True, ".xml"));

      File_List_Iter := String_Lists.MakeListIter (File_List);

      while String_Lists.More (File_List_Iter) loop

         String_Lists.Next (File_List_Iter, A_File_Name);

         begin
            XML_File_Access.Load_XML_File_Validated
              (Ada.Strings.Unbounded.To_String (A_File_Name),
               A_Tree_Reader,
               A_XML_Document);

            -- check for project files
            if (XML_File_Access.Does_XML_Document_Belong_To_Type
                ("giant_project_file", A_XML_Document) = True) then

               Project_File_Found := True;
            end if;

            Tree_Readers.Free(A_Tree_Reader);
         exception
            when others =>
               null;
         end;
      end loop;

      String_Lists.Destroy (File_List);

      return Project_File_Found;
   end Is_Already_A_Project_File_In_Directory;

   --------------------------------------------------------------------------
   procedure Get_Bauhaus_IML_Graph_Data
     (Project_Name           : in String;
      Project_Directory      : in String;
      Bauhaus_IML_Graph_File : out Ada.Strings.Unbounded.Unbounded_String;
      Bauhaus_IML_Graph_File_Checksum : out Integer) is

      Absolute_Project_File_Name : Ada.Strings.Unbounded.Unbounded_String;

      Project_Tree_Reader  : Tree_Readers.Tree_Reader;
      Project_XML_Document : Dom.Core.Document;

      XML_Nodes_List : DOM.Core.Node_List;
      Data_XML_Node  : DOM.Core.Node;
   begin

      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;

      if (Does_Project_Exist (Project_Name, Project_Directory) = False) then
         raise Project_Does_Not_Exist_Exception;
      end if;

      Absolute_Project_File_Name := Create_Name_For_File
        (Project_Directory,
         Project_Name,
         ".xml");

      --  it is already certain that the file describes a project
      XML_File_Access.Load_XML_File_Validated
        (Ada.Strings.Unbounded.To_String (Absolute_Project_File_Name),
         Project_Tree_Reader,
         Project_XML_Document);

      --  get the global setting node
      XML_Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Project_XML_Document, "global_data");

      --  the list holds only one node
      Data_XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, 0);

      -- expand path relative to the project directory.
      Bauhaus_IML_Graph_File  :=
        Ada.Strings.Unbounded.To_Unbounded_String
         (File_Management.Get_Absolute_Path_To_File_From_Relative
          (Project_Directory,
           DOM.Core.Elements.Get_Attribute
            (Data_XML_Node, "iml_graph_file_path")));

      Bauhaus_IML_Graph_File_Checksum :=
        Integer'Value (
                       (DOM.Core.Elements.Get_Attribute
                        (Data_XML_Node, "iml_graph_checksum")));

      --  deallocate storrage
      DOM.Core.Free (XML_Nodes_List);
      Tree_Readers.Free(Project_Tree_Reader);
   end Get_Bauhaus_IML_Graph_Data;

   ---------------------------------------------------------------------------
   procedure Get_Bauhaus_IML_Graph_Data_For_File
     (Project_File_Name      : in     String;
      Bauhaus_IML_Graph_File :    out Ada.Strings.Unbounded.Unbounded_String;
      Bauhaus_IML_Graph_File_Checksum : out Integer) is
   begin

      Get_Bauhaus_IML_Graph_Data
        (Project_Name =>
           File_Management.Calculate_Name_For_File (Project_File_Name),
         Project_Directory =>
           File_Management.Return_Dir_Path_For_File_Path (Project_File_Name),
         Bauhaus_IML_Graph_File => Bauhaus_IML_Graph_File,
         Bauhaus_IML_Graph_File_Checksum => Bauhaus_IML_Graph_File_Checksum);
   end Get_Bauhaus_IML_Graph_Data_For_File;

   ---------------------------------------------------------------------------
   function Get_Bauhaus_IML_Graph_File
     (Project_File_Name : in String)
     return String is

      Checksum : Integer;
      File     : Ada.Strings.Unbounded.Unbounded_String;
   begin

      Get_Bauhaus_IML_Graph_Data
        (Project_Name =>
           File_Management.Calculate_Name_For_File (Project_File_Name),
         Project_Directory =>
           File_Management.Return_Dir_Path_For_File_Path (Project_File_Name),
         Bauhaus_IML_Graph_File => File,
         Bauhaus_IML_Graph_File_Checksum => Checksum);

      return Ada.Strings.Unbounded.To_String (File);
   end Get_Bauhaus_IML_Graph_File;

   ---------------------------------------------------------------------------
   function Load_Project_File
     (Project_File_Name : in String)
     return Project_Access is
   begin

      return Load_Project
        (Project_Name =>
           File_Management.Calculate_Name_For_File (Project_File_Name),
         Project_Directory =>
           File_Management.Return_Dir_Path_For_File_Path (Project_File_Name));
   exception
      when File_Management.Directory_Could_Not_Be_Calculated_Exception =>
         raise Invalid_Project_Directory_Excpetion;
   end Load_Project_File;

   ---------------------------------------------------------------------------
   function Load_Project
     (Project_Name      : in String;
      Project_Directory : in String)
     return Project_Access is

      use Ada.Strings.Unbounded;

      New_Project_Access : Project_Access;
      Absolute_Project_File_Name : Ada.Strings.Unbounded.Unbounded_String;

      Abs_Node_Annotations_File : Ada.Strings.Unbounded.Unbounded_String;

      Project_Tree_Reader  : Tree_Readers.Tree_Reader;
      Project_XML_Document : Dom.Core.Document;

      XML_Nodes_List       : DOM.Core.Node_List;
      Data_XML_Node        : DOM.Core.Node;
      Vis_Window_XML_Node  : Dom.Core.Node;
      Subgraphs_XML_Node   : Dom.Core.Node;
      A_XML_Node           : DOM.Core.Node;

      A_Subgraph_File_Name        : Ada.Strings.Unbounded.Unbounded_String;
      New_Subgraph_Data_Element   : Subgraph_Data_Element;

      A_Vis_Window_File_Name      : Ada.Strings.Unbounded.Unbounded_String;
      New_Vis_Window_Data_Element : Vis_Window_Data_Element;
      Test_Window_Acc             : Vis_Windows.Visual_Window_Access;

      -- needed for fault tolerance
      Ignore_Subgraph         : Boolean := False;
      Ignore_Vis_Win          : Boolean := False;
      Ignore_Node_Annotations : Boolean := False;
   begin

      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;

      if (Does_Project_Exist (Project_Name, Project_Directory) = False) then
         raise Project_Does_Not_Exist_Exception;
      end if;

      Absolute_Project_File_Name := Create_Name_For_File
        (Project_Directory,
         Project_Name,
         ".xml");

      XML_File_Access.Load_XML_File_Validated
        (Ada.Strings.Unbounded.To_String (Absolute_Project_File_Name),
         Project_Tree_Reader,
         Project_XML_Document);

      --  build new project
      ---------------------
      New_Project_Access := new Project_Element;

      --  read global settings
      ------------------------
      XML_Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Project_XML_Document, "global_data");
      -- list only holds one element
      Data_XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, 0);

      New_Project_Access.Project_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Project_Name);

      New_Project_Access.Abs_Project_Directory :=
        Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Return_Dir_Path_For_File_Path
         (Ada.Strings.Unbounded.To_String
          (Absolute_Project_File_Name)));

      New_Project_Access.Abs_Bauhaus_IML_Graph_File :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (File_Management.Get_Absolute_Path_To_File_From_Relative
           (Ada.Strings.Unbounded.To_String
             (New_Project_Access.Abs_Project_Directory),
           (DOM.Core.Elements.Get_Attribute
            (Data_XML_Node, "iml_graph_file_path"))));

      New_Project_Access.Bauhaus_IML_Graph_File_Checksum :=
        Integer'Value (
                       (DOM.Core.Elements.Get_Attribute
                        (Data_XML_Node, "iml_graph_checksum")));

      ------------------------------------------------------------------------
      -- initialize hash maps
      New_Project_Access.All_Subgraphs := Subgraph_Data_Hashs.Create;
      New_Project_Access.All_Vis_Windows := Known_Vis_Windows_Hashs.Create;


      --  check whether correct iml graph is loaded
      ---------------------------------------------
      if not Is_Correct_IML_Graph_Loaded
        (New_Project_Access.Bauhaus_IML_Graph_File_Checksum) then

         --  now deep deallocation necessary
         Free_Project_Access (New_Project_Access);
         raise Wrong_IML_Graph_Loaded_Exception;
      end if;

      -- calculate path relative to project directory if necessary
      Ignore_Node_Annotations := False;

      New_Project_Access.Node_Annotations_File :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (DOM.Core.Elements.Get_Attribute
           (Data_XML_Node, "node_annotations_file_name"));

      begin
         Abs_Node_Annotations_File :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (File_Management.Get_Absolute_Path_To_File_From_Relative
                (Ada.Strings.Unbounded.To_String
                  (New_Project_Access.Abs_Project_Directory),
                 Ada.Strings.Unbounded.To_String
                  (New_Project_Access.Node_Annotations_File)));
      exception
         when File_Management.File_Does_Not_Exist_Exception =>
            Ignore_Node_Annotations := True;
            Logger.Info
              ("Node_Annotation_File as defined in the "
               & "project file not found -> new one will be created");
      end;

      -- list holding global data node
      DOM.Core.Free (XML_Nodes_List);

      -- Load all subgraphs into main memory
      --------------------------------------

      XML_Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Project_XML_Document, "subgraphs");
      -- list only holds one element
      Subgraphs_XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, 0);
      DOM.Core.Free (XML_Nodes_List);

      XML_Nodes_List := Dom.Core.Elements.Get_Elements_By_Tag_Name
        (Subgraphs_XML_Node, "a_subgraph_file");

      -- process subgraph entries - the <a_subgraph_file> nodes
      for I in 0 .. DOM.Core.Nodes.Length (XML_Nodes_List) - 1 loop

         Ignore_Subgraph := False;

         A_XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);

         -- calculate absolute path if necessary
         begin
            A_Subgraph_File_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (File_Management.Get_Absolute_Path_To_File_From_Relative
                (Ada.Strings.Unbounded.To_String
                (New_Project_Access.Abs_Project_Directory),
                (DOM.Core.Elements.Get_Attribute
                (A_XML_Node, "file_path"))));

         exception
            -- ignore subgraphs where management file not exits
            when File_Management.File_Does_Not_Exist_Exception =>
              Ignore_Subgraph := True;
         end;

         if not Ignore_Subgraph then
            New_Subgraph_Data_Element :=
              Load_Sub_Graph_Data_Into_Main_Memory
              (Ada.Strings.Unbounded.To_String
               (A_Subgraph_File_Name));

            Subgraph_Data_Hashs.Bind
              (New_Project_Access.All_Subgraphs,
               Ada.Strings.Unbounded.To_Unbounded_String
               (Graph_Lib.Subgraphs.Get_Name
                (New_Subgraph_Data_Element.Subgraph)),
               New_Subgraph_Data_Element);
         end if;

      end loop;

      DOM.Core.Free (XML_Nodes_List);

      -- Load information about all visual windows (not the windows itself)
      -- in order to check consistency each vis window will be loaded
      -- into the main memory (only one window at once) and closed afterwards.
      ---------------------------------------------------------------------

      XML_Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Project_XML_Document, "visualisation_windows");
      -- list only holds one element
      Vis_Window_XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, 0);
      DOM.Core.Free (XML_Nodes_List);

      XML_Nodes_List := Dom.Core.Elements.Get_Elements_By_Tag_Name
        (Vis_Window_XML_Node, "a_vis_window_file");

      -- process visulisation window entries - the <a_vis_window_file> nodes
      for I in 0 .. DOM.Core.Nodes.Length (XML_Nodes_List) - 1 loop

         Ignore_Vis_Win := False;

         A_XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);


         begin
            -- calculate absolute path if necessary
            A_Vis_Window_File_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (File_Management.Get_Absolute_Path_To_File_From_Relative
               (Ada.Strings.Unbounded.To_String
                (New_Project_Access.Abs_Project_Directory),
                (DOM.Core.Elements.Get_Attribute
                 (A_XML_Node, "file_path"))));
         exception
            when File_Management.File_Does_Not_Exist_Exception =>
            Ignore_Vis_Win := True;
         end;

         if not Ignore_Vis_Win then

            -- build new vis window data element
            New_Vis_Window_Data_Element.Vis_Window_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (File_Management.Calculate_Name_For_File
               (Ada.Strings.Unbounded.To_String
                (A_Vis_Window_File_Name)));

            New_Vis_Window_Data_Element.Is_File_Linked := True;
            New_Vis_Window_Data_Element.Existing_Vis_Window_File :=
              A_Vis_Window_File_Name;

            New_Vis_Window_Data_Element.Is_Memory_Loaded := False;
            -------------

            -- FIX Martin - not necessary due first checck
            ------------- security check
            --  try opening file - check whether vis window file really exists
            --  Test_Window_Acc := Load_Vis_Window_Into_Main_Memory
            --  (Ada.Strings.Unbounded.To_String (A_Vis_Window_File_Name),
            --  Node_Annotations.Create_Empty);

            --  Vis_Windows.Deallocate_Vis_Window_Deep (Test_Window_Acc);
            -------------

            --  insert window data element
            Known_Vis_Windows_Hashs.Bind
              (New_Project_Access.All_Vis_Windows,
               New_Vis_Window_Data_Element.Vis_Window_Name,
               New_Vis_Window_Data_Element);
         end if;
      end loop;

      DOM.Core.Free (XML_Nodes_List);

      -- Load node annotations
      -- Fault tolerancy - if not annotations file was destroyed or is not
      -- correct. --> New empty file will be created, a probably
      -- existing old one will be saved as a security file.
      -----------------------------------------------------

      if not Ignore_Node_Annotations then

         begin
            New_Project_Access.The_Node_Annotations :=
              Node_Annotations.Load_From_File
              (Ada.Strings.Unbounded.To_String
               (Abs_Node_Annotations_File));
         exception
            when Node_Annotations.Node_Annotations_File_Not_Found_Exception =>
               Ignore_Node_Annotations := True;
               Logger.Info
                 ("Node_Annotation_File as defined in the "
                  & "project file not found -> new one will be created");

            when
              Node_Annotations.Node_Annotations_File_Not_Correct_Exception =>

               Ignore_Node_Annotations := True;
               Logger.Info
                 ("Node_Annotation_File """
                  & Ada.Strings.Unbounded.To_String
                    (Abs_Node_Annotations_File)
                  & """ not correct - file ignored - security file "
                  & """<file_name>&~"" created.");

               -- create security file (old incorrect version)
               File_Management.Copy_File
                 (Ada.Strings.Unbounded.To_String
                   (Abs_Node_Annotations_File),
                  Ada.Strings.Unbounded.To_String
                   (Abs_Node_Annotations_File) & "~");
         end;
      end if;

      if Ignore_Node_Annotations then

         -- create new empty node annotations file
         Abs_Node_Annotations_File :=
           File_Management.Append_Dir_Separator_If_Necessary
            (Ada.Strings.Unbounded.To_String
              (New_Project_Access.Abs_Project_Directory))
           & Ada.Strings.Unbounded.To_Unbounded_String
            (Const_Node_Annotations_File_Name);

         New_Project_Access.Node_Annotations_File :=
           Ada.Strings.Unbounded.To_Unbounded_String
            (Const_Node_Annotations_File_Name);

         New_Project_Access.The_Node_Annotations :=
           Node_Annotations.Create_Empty;

         Node_Annotations.Write_To_File
           (New_Project_Access.The_Node_Annotations,
            Ada.Strings.Unbounded.To_String (Abs_Node_Annotations_File));

         Logger.Info
           ("New empty Node_Annotations_File created in project directory");
      end if;

      --  deallocate storrage
      -----------------------
      Tree_Readers.Free(Project_Tree_Reader);

      return New_Project_Access;
   end Load_Project;

   ---------------------------------------------------------------------------
   function Create_Empty_Project
     (Project_Name                    : in String;
      Project_Directory               : in String;
      Bauhaus_IML_Graph_File          : in String;
      Bauhaus_IML_Graph_File_Checksum : in Integer)
     return Project_Access is

      use Ada.Strings.Unbounded;

      New_Project_Access        : Project_Access;
      Abs_Node_Annotations_File : Ada.Strings.Unbounded.Unbounded_String;

      Abs_Project_Directory     : Ada.Strings.Unbounded.Unbounded_String;
      Abs_IML_Graph_File        : Ada.Strings.Unbounded.Unbounded_String;
   begin

      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;

      if Is_Already_A_Project_File_In_Directory (Project_Directory) then
         raise Directory_Holds_Already_A_Project_File_Exception;
      end if;

      -- check for correct iml Graph
      if not Is_Correct_IML_Graph_Loaded (Bauhaus_IML_Graph_File_Checksum)
      then

         raise Wrong_IML_Graph_Loaded_Exception;
      end if;

      --  calculate absolute paths
      Abs_Project_Directory := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Absolute_Path_To_Directory_From_Relative
         (GNAT.Directory_Operations.Get_Current_Dir, Project_Directory));

      Abs_Project_Directory :=
        Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Append_Dir_Separator_If_Necessary
         (Ada.Strings.Unbounded.To_String (Abs_Project_Directory)));

      Abs_Node_Annotations_File :=
        File_Management.Append_Dir_Separator_If_Necessary
        (Ada.Strings.Unbounded.To_String (Abs_Project_Directory))
        & Ada.Strings.Unbounded.To_Unbounded_String
        (Const_Node_Annotations_File_Name);

      Abs_IML_Graph_File := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Absolute_Path_To_File_From_Relative
         (GNAT.Directory_Operations.Get_Current_Dir, Bauhaus_IML_Graph_File));

      --  build new project
      New_Project_Access := new Project_Element;

      -- for new projects this file is always located in the
      -- project directory
      New_Project_Access.Node_Annotations_File :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Const_Node_Annotations_File_Name);

      New_Project_Access.Project_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Project_Name);

      --  stored as an absolute path  (not written to the xml file)
      New_Project_Access.Abs_Project_Directory := Abs_Project_Directory;

      --  stored as an absolute path  (also written to the xml file)
      New_Project_Access.Abs_Bauhaus_IML_Graph_File :=
        Abs_IML_Graph_File;

      New_Project_Access.Bauhaus_IML_Graph_File_Checksum :=
        Bauhaus_IML_Graph_File_Checksum;

      --  Stored as an relative path (towards Project_Directory)
      --  - only while creating a new project (also written to xml file);
      --  Per default this file is loacted in the project directory.
      New_Project_Access.Node_Annotations_File :=
        Ada.Strings.Unbounded.To_Unbounded_String
        (Const_Node_Annotations_File_Name);

      New_Project_Access.All_Vis_Windows :=
        Known_Vis_Windows_Hashs.Create;

      New_Project_Access.All_Subgraphs :=
        Subgraph_Data_Hashs.Create;

      New_Project_Access.The_Node_Annotations :=
        Node_Annotations.Create_Empty;

      --  write project files for new (empty) project into project directory
      --  (a xml project file and a dtd).
      Write_DTD_To_Directory
        (Ada.Strings.Unbounded.To_String
         (New_Project_Access.Abs_Project_Directory));
      Write_Project_XML_File (New_Project_Access);

      --  write empty xml file for node annotations
      Node_Annotations.Write_To_File
        (New_Project_Access.The_Node_Annotations,
         Ada.Strings.Unbounded.To_String (Abs_Node_Annotations_File));

      return New_Project_Access;
   end Create_Empty_Project;

   ----------------------------------------------------------------------------
   function Create_Empty_Project_For_File
     (Project_File_Name               : in String;
      Bauhaus_IML_Graph_File          : in String;
      Bauhaus_IML_Graph_File_Checksum : in Integer)
     return Project_Access is

   begin

      return Create_Empty_Project
        (Project_Name =>
           File_Management.Calculate_Name_For_File (Project_File_Name),
         Project_Directory =>
           File_Management.Return_Dir_Path_For_File_Path (Project_File_Name),
         Bauhaus_IML_Graph_File          => Bauhaus_IML_Graph_File,
         Bauhaus_IML_Graph_File_Checksum => Bauhaus_IML_Graph_File_Checksum);
   end Create_Empty_Project_For_File;

   ----------------------------------------------------------------------------
   procedure Deallocate_Project_Deep (Project : in out Project_Access) is

      Vis_Iter : Known_Vis_Windows_Hashs.Values_Iter;
      A_Vis_Window_Data_Element : Vis_Window_Data_Element;

      Subgraphs_Iter : Subgraph_Data_Hashs.Values_Iter;
      A_Subgraph_Data_Element : Subgraph_Data_Element;

   begin
      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      --  deep deallocation of all memory loaded visualisation windows
      ----------------------------------------------------------------
      Vis_Iter := Known_Vis_Windows_Hashs.Make_Values_Iter
        (Project.All_Vis_Windows);

      while Known_Vis_Windows_Hashs.More (Vis_Iter) loop

         Known_Vis_Windows_Hashs.Next
           (Vis_Iter, A_Vis_Window_Data_Element);

         if Is_Vis_Window_Memory_Loaded
           (Project,
            Ada.Strings.Unbounded.To_String
            (A_Vis_Window_Data_Element.Vis_Window_Name)) then

            Vis_Windows.Deallocate_Vis_Window_Deep
              (A_Vis_Window_Data_Element.Vis_Window);
         end if;
      end loop;

      --  deallocate All_Project_Vis_Windows
      --------------------------------------
      Known_Vis_Windows_Hashs.Destroy (Project.All_Vis_Windows);

      --  deep deallocation of subgraphs
      ----------------------------------
      Subgraphs_Iter := Subgraph_Data_Hashs.Make_Values_Iter
        (Project.All_Subgraphs);

      while Subgraph_Data_Hashs.More (Subgraphs_Iter) loop

         Logger.Debug ("Killed Subgraph");

         Subgraph_Data_Hashs.Next (Subgraphs_Iter, A_Subgraph_Data_Element);
         Graph_Lib.Subgraphs.Destroy (A_Subgraph_Data_Element.Subgraph);
      end loop;

      Subgraph_Data_Hashs.Destroy (Project.All_Subgraphs);

      --  deep deallocation of the data structure for the node annotations
      --------------------------------------------------------------------
      Node_Annotations.Deallocate (Project.The_Node_Annotations);

      --  deallocate data object itself
      ---------------------------------
      Free_Project_Access (Project);
   end;

   ---------------------------------------------------------------------------
   -- Stores a whole project and optionally "migrates" the project to
   -- a new project directory (incl. new project name).
   --
   -- Change_Project_Dir - determines whether the old Project should be
   --   stored (False) or if the Project should be stored under a new name
   --   in a new directory (True)
   --
   -- New_Project_Directory - An absolute Path that ends with a
   --   directory separator is required !!!
   procedure General_Store_Whole_Project
     (Project                   : in Project_Access;
      Change_Project_Files      : in Boolean;
      New_Project_Name          : in String;
      New_Abs_Project_Directory : in String) is

      use Ada.Strings.Unbounded;

      ----------------------------
      -- calculates file name and writes the file
      -- used then project is saved unter a new project
      -- new prject name and path must already have been set.
      procedure Process_Vis_Window
        (P_Project         : in     Project_Access;
         P_Vis_Window      : Vis_Windows.Visual_Window_Access;
         P_Vis_Window_Key  : in     Ada.Strings.Unbounded.Unbounded_String;
         P_Vis_Window_Data : in out Vis_Window_Data_Element) is

         P_Vis_Window_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      begin



         -- calculate new file name
         P_Vis_Window_File_Name :=
           Create_Name_For_File
           (Ada.Strings.Unbounded.To_String
            (P_Project.Abs_Project_Directory),
            Ada.Strings.Unbounded.To_String
            (P_Vis_Window_Data.Vis_Window_Name),
            Const_Vis_Window_File_Ending);

         Write_Vis_Window_To_File
           (Ada.Strings.Unbounded.To_String
            (P_Vis_Window_File_Name),
            P_Vis_Window);

         -- change vis window status
         P_Vis_Window_Data.Is_File_Linked := True;
         P_Vis_Window_Data.Existing_Vis_Window_File :=
           P_Vis_Window_File_Name;
         Known_Vis_Windows_Hashs.Update_Value
           (P_Project.All_Vis_Windows,
            P_Vis_Window_Key,
            P_Vis_Window_Data);
      end Process_Vis_Window;

      Abs_Node_Annotations_File : Ada.Strings.Unbounded.Unbounded_String;

      Known_Vis_Iter : Known_Vis_Windows_Hashs.Bindings_Iter;
      A_Vis_Window_Data_Element : Vis_Window_Data_Element;
      A_Vis_Window              : Vis_Windows.Visual_Window_Access;
      A_Vis_Window_Key          : Ada.Strings.Unbounded.Unbounded_String;
      A_Vis_Window_File_Name    : Ada.Strings.Unbounded.Unbounded_String;

      Subgraphs_Iter            : Subgraph_Data_Hashs.Bindings_Iter;
      A_Subgraph_Data_Element   : Subgraph_Data_Element;
      A_Subgraph_Key            : Ada.Strings.Unbounded.Unbounded_String;
      A_Subgraph_File_Name      : Ada.Strings.Unbounded.Unbounded_String;

   begin

      -- migrate project (must be done before anything else is migrated)
      ------------------------------------------------------------------
      if Change_Project_Files then

         -- after migration the node annotations file will always be
         -- located in the project directory
         Project.Node_Annotations_File :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Const_Node_Annotations_File_Name);
         --  "Migrate the project"
         --  (must be done before processing vis windows and subgraphs).
         Project.Project_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String (New_Project_Name);

         Project.Abs_Project_Directory :=
           Ada.Strings.Unbounded.To_Unbounded_String
           (New_Abs_Project_Directory);
      end if;

      --  Process visualisation windows
      -----------------------------------------------
      Known_Vis_Iter :=
        Known_Vis_Windows_Hashs.Make_Bindings_Iter
        (Project.All_Vis_Windows);

      while Known_Vis_Windows_Hashs.More (Known_Vis_Iter) loop

         Known_Vis_Windows_Hashs.Next
           (Known_Vis_Iter, A_Vis_Window_Key, A_Vis_Window_Data_Element);

         --  migrate ALL (incl. not memory loaded)
         --  vis windows to new project directory
         -----------------------------------------
         if Change_Project_Files then
            -- avoids loading of all not already loaded
            -- vis windows into main memory at once
            if not A_Vis_Window_Data_Element.Is_Memory_Loaded then

               -- will load the window into the main memory
               -- changes a vis window data element
               A_Vis_Window := Get_Visualisation_Window
                 (Project,
                  Ada.Strings.Unbounded.To_String
                  (A_Vis_Window_Data_Element.Vis_Window_Name));

               -- reload as changed
               A_Vis_Window_Data_Element :=
                 Known_Vis_Windows_Hashs.Fetch
                   (Project.All_Vis_Windows, A_Vis_Window_Key);

               Process_Vis_Window
                 (Project,
                  A_Vis_Window,
                  A_Vis_Window_Key,
                  A_Vis_Window_Data_Element);

               Free_Memory_For_Vis_Window
                 (Project,
                  Ada.Strings.Unbounded.To_String
                  (A_Vis_Window_Data_Element.Vis_Window_Name));
            elsif A_Vis_Window_Data_Element.Is_Memory_Loaded then

               --  window is already memory loaded
               A_Vis_Window := Get_Visualisation_Window
                 (Project,
                  Ada.Strings.Unbounded.To_String
                  (A_Vis_Window_Data_Element.Vis_Window_Name));

               Process_Vis_Window
                 (Project,
                  A_Vis_Window,
                  A_Vis_Window_Key,
                  A_Vis_Window_Data_Element);
            end if;
         end if; -- end if Change_Project_Files

         -- do not migrate vis windows into new project dir
         --------------------------------------------------
         if not Change_Project_Files then

            -- ignore not memory loaded windows
            if A_Vis_Window_Data_Element.Is_Memory_Loaded then

               if A_Vis_Window_Data_Element.Is_File_Linked then
                  A_Vis_Window_File_Name :=
                    A_Vis_Window_Data_Element.Existing_Vis_Window_File;
               else

                  -- calculate new file name if window if not file linked
                  A_Vis_Window_File_Name :=
                    Create_Name_For_File
                    (Ada.Strings.Unbounded.To_String
                     (Project.Abs_Project_Directory),
                     Ada.Strings.Unbounded.To_String
                     (A_Vis_Window_Data_Element.Vis_Window_Name),
                     Const_Vis_Window_File_Ending);
               end if;

               Write_Vis_Window_To_File
                 (Ada.Strings.Unbounded.To_String
                  (A_Vis_Window_File_Name),
                  A_Vis_Window_Data_Element.Vis_Window);

               -- change vis window status
               A_Vis_Window_Data_Element.Is_File_Linked := True;
               A_Vis_Window_Data_Element.Existing_Vis_Window_File :=
                 A_Vis_Window_File_Name;
               Known_Vis_Windows_Hashs.Update_Value
                 (Project.All_Vis_Windows,
                  A_Vis_Window_Key,
                  A_Vis_Window_Data_Element);
            end if;
         end if; -- end if not Change_Project_Files
      end loop;

      --  Process iml subgraphs (all subgraphs are  loaded into the
      --  main memory)
      --------------------------------------------------------------
      Subgraphs_Iter := Subgraph_Data_Hashs.Make_Bindings_Iter
        (Project.All_Subgraphs);

      while Subgraph_Data_Hashs.More (Subgraphs_Iter) loop

         Subgraph_Data_Hashs.Next
           (Subgraphs_Iter, A_Subgraph_Key, A_Subgraph_Data_Element);

         -- write not file linked subgraphs into the project dir
         -- (as for migratation the project dir is already changed
         -- there are no differences between saving to a new project
         -- directory or to the old one).
         if (A_Subgraph_Data_Element.Is_File_Linked
           and Change_Project_Files)
           or not A_Subgraph_Data_Element.Is_File_Linked then

            A_Subgraph_File_Name :=
              Create_Name_For_File
              (Ada.Strings.Unbounded.To_String
               (Project.Abs_Project_Directory),
               Graph_Lib.Subgraphs.Get_Name
               (A_Subgraph_Data_Element.Subgraph),
               Const_Subgraph_File_Ending);
         end if;

         -- Keep existing file (do not migrate)
         if A_Subgraph_Data_Element.Is_File_Linked
           and not Change_Project_Files then

            A_Subgraph_File_Name :=
              A_Subgraph_Data_Element.Existing_Subgraph_File;
         end if;

         Write_Sub_Graph_Data_To_File
           (Ada.Strings.Unbounded.To_String
            (A_Subgraph_File_Name),
            A_Subgraph_Data_Element);

         -- change subgraph status
         A_Subgraph_Data_Element.Is_File_Linked := True;
         A_Subgraph_Data_Element.Existing_Subgraph_File :=
           A_Subgraph_File_Name;

         Subgraph_Data_Hashs.Update_Value
           (Project.All_Subgraphs,
            A_Subgraph_Key,
            A_Subgraph_Data_Element);
      end loop;

      -- write dtd (overwritten if already exists)
      Write_DTD_To_Directory
        (Ada.Strings.Unbounded.To_String
         (Project.Abs_Project_Directory));

      -- Write Node_Annotations (file name has already be changed if project
      -- is migrated). - expand toward project dir if necessary
      --------------------------------------
      Abs_Node_Annotations_File :=
        File_Management.Append_Dir_Separator_If_Necessary
        (Ada.Strings.Unbounded.To_String (Project.Abs_Project_Directory))
        & Project.Node_Annotations_File;

      Node_Annotations.Write_To_File
        (Project.The_Node_Annotations,
         Ada.Strings.Unbounded.To_String
         (Abs_Node_Annotations_File));

      -- Kill all Security save files
      -------------------------------
      Kill_All_Security_Files
        (Ada.Strings.Unbounded.To_String
          (Project.Abs_Project_Directory));

      -- Update Project XML File (MUST happen at the end - not before)
      ---------------------------
      Write_Project_XML_File (Project);
   end General_Store_Whole_Project;


   ----------------------------------------------------------------------------
   procedure Store_Whole_Project (Project : in Project_Access) is

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      -- do not migrate
      General_Store_Whole_Project
        (Project                   => Project,
         Change_Project_Files      => False,
         New_Project_Name          => "",
         New_Abs_Project_Directory => "");
   end Store_Whole_Project;

   ---------------------------------------------------------------------------
   -- Change_Project_Dir determines whether the old Project should be stored
   --   (False) or if the Project should be stored under a new name
   --   in a new directory (True)
   procedure Store_Whole_Project_As
     (Project               : in Project_Access;
      New_Project_Name      : in String;
      New_Project_Directory : in String) is

      Abs_Project_Directory     : Ada.Strings.Unbounded.Unbounded_String;
   begin

      -- Security checks
      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if (GNAT.OS_Lib.Is_Directory (New_Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;

      if Is_Already_A_Project_File_In_Directory (New_Project_Directory) then
         raise Directory_Holds_Already_A_Project_File_Exception;
      end if;

      ----------------------------
      --  calculate absloute paths for new files
      Abs_Project_Directory := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Absolute_Path_To_Directory_From_Relative
         (GNAT.Directory_Operations.Get_Current_Dir, New_Project_Directory));

      Abs_Project_Directory := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Append_Dir_Separator_If_Necessary
         (Ada.Strings.Unbounded.To_String (Abs_Project_Directory)));

      -- migrate
      General_Store_Whole_Project
        (Project                   => Project,
         Change_Project_Files      => True,
         New_Project_Name          => New_Project_Name,
         New_Abs_Project_Directory =>
           Ada.Strings.Unbounded.To_String
         (Abs_Project_Directory));
   end Store_Whole_Project_As;

   ---------------------------------------------------------------------------
   procedure Store_Whole_Project_As_For_File
     (Project               : in Project_Access;
      New_Project_File_Name : in String) is
   begin

      Store_Whole_Project_As
        (Project           => Project,
         New_Project_Name      =>
           File_Management.Calculate_Name_For_File (New_Project_File_Name),
         New_Project_Directory =>
           File_Management.Return_Dir_Path_For_File_Path
         (New_Project_File_Name));
   end Store_Whole_Project_As_For_File;

   ---------------------------------------------------------------------------
   function Get_Graph_Filename
     (Project : in Project_Access)
     return String is
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded.To_String
        (Project.Abs_Bauhaus_IML_Graph_File);
   end Get_Graph_Filename;

   ---------------------------------------------------------------------------
   function Get_Project_Name
     (Project : in Project_Access)
     return String is
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded.To_String
        (Project.Project_Name);
   end Get_Project_Name;

   ---------------------------------------------------------------------------
   function Get_Project_File_Name
     (Project : in Project_Access)
     return String is

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      return  Ada.Strings.Unbounded.To_String
        (Create_Name_For_File
         (Ada.Strings.Unbounded.To_String (Project.Abs_Project_Directory),
          Ada.Strings.Unbounded.To_String (Project.Project_Name),
          ".xml"));
   end Get_Project_File_Name;

   ---------------------------------------------------------------------------
   function Get_Project_Directory
     (Project : in Project_Access)
     return String is

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded.To_String
        (Project.Abs_Project_Directory);
   end Get_Project_Directory;


   ---------------------------------------------------------------------------
   -- B
   -- Visualisation Windows
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Vis_Window_Exist
     (Project         : in Project_Access;
      Vis_Window_Name : in String)
     return Boolean is
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      return Known_Vis_Windows_Hashs.Is_Bound
        (Project.All_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name));
   end Does_Vis_Window_Exist;

   ---------------------------------------------------------------------------
   function Is_Vis_Window_Memory_Loaded
     (Project         : in Project_Access;
      Vis_Window_Name : in String)
     return Boolean is

      A_Vis_Window_Data_Element : Vis_Window_Data_Element;

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if not Known_Vis_Windows_Hashs.Is_Bound
        (Project.All_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name)) then

         return False;
      else

         A_Vis_Window_Data_Element := Known_Vis_Windows_Hashs.Fetch
           (Project.All_Vis_Windows,
            Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name));

         return A_Vis_Window_Data_Element.Is_Memory_Loaded;
      end if;

   end Is_Vis_Window_Memory_Loaded;

   ---------------------------------------------------------------------------
   function Get_All_Visualisation_Window_Names
     (Project : in Project_Access)
     return String_Lists.List is

      Names_List : String_Lists.List;
      Vis_Iter   : Known_Vis_Windows_Hashs.Values_Iter;
      A_Vis_Window_Data_Element : Vis_Window_Data_Element;
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      Names_List := String_Lists.Create;
      Vis_Iter := Known_Vis_Windows_Hashs.Make_Values_Iter
        (Project.All_Vis_Windows);

      while Known_Vis_Windows_Hashs.More (Vis_Iter) loop

         Known_Vis_Windows_Hashs.Next (Vis_Iter, A_Vis_Window_Data_Element);
         String_Lists.Attach
           (Names_List, A_Vis_Window_Data_Element.Vis_Window_Name);
      end loop;

      return Names_List;
   end Get_All_Visualisation_Window_Names;

   ---------------------------------------------------------------------------
   function Get_Visualisation_Window
     (Project         : in Project_Access;
      Vis_Window_Name : in String)
     return Vis_Windows.Visual_Window_Access is

      Vis_Window_Data     : Vis_Window_Data_Element;
      New_Vis_Window_Inst : Vis_Windows.Visual_Window_Access;

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if (Does_Vis_Window_Exist (Project, Vis_Window_Name) = False) then
         raise Visualisation_Window_Is_Not_Part_Of_Project_Exception;
      end if;


      --  Check if already loaded and return the instance if that is the case
      if (Is_Vis_Window_Memory_Loaded (Project, Vis_Window_Name)) then

         return Known_Vis_Windows_Hashs.Fetch
           (Project.All_Vis_Windows,
            Ada.Strings.Unbounded.To_Unbounded_String
            (Vis_Window_Name)).Vis_Window;
         --  load vis window into main memory
      else

         Vis_Window_Data := Known_Vis_Windows_Hashs.Fetch
           (Project.All_Vis_Windows,
            Ada.Strings.Unbounded.To_Unbounded_String
            (Vis_Window_Name));

         Logger.Debug ("Load file" & Ada.Strings.Unbounded.To_String
            (Vis_Window_Data.Existing_Vis_Window_File));

         New_Vis_Window_Inst := Load_Vis_Window_Into_Main_Memory
           (Ada.Strings.Unbounded.To_String
            (Vis_Window_Data.Existing_Vis_Window_File),
            Project.The_Node_Annotations);

         -- update data entry
         Vis_Window_Data := Known_Vis_Windows_Hashs.Fetch
           (Project.All_Vis_Windows,
            Ada.Strings.Unbounded.To_Unbounded_String
            (Vis_Window_Name));

         Vis_Window_Data.Vis_Window := New_Vis_Window_Inst;
         Vis_Window_Data.Is_Memory_Loaded := True;

         Known_Vis_Windows_Hashs.Update_Value
           (Project.All_Vis_Windows,
            Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name),
            Vis_Window_Data);

         return New_Vis_Window_Inst;
      end if;
   end Get_Visualisation_Window;

   ---------------------------------------------------------------------------
   procedure Change_Vis_Window_Name
     (Project             : in Project_Access;
      Vis_Window_Name     : in String;
      New_Vis_Window_Name : in String) is

      A_Vis_Window_Data_Element : Vis_Window_Data_Element;
      New_Vis_Window_File_Name : Ada.Strings.Unbounded.Unbounded_String;
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if not Does_Vis_Window_Exist
        (Project, Vis_Window_Name) then
         raise Visualisation_Window_Is_Not_Part_Of_Project_Exception;
      end if;

      if Does_Vis_Window_Exist
        (Project, New_Vis_Window_Name) then
         raise New_Vis_Window_Name_Does_Already_Exist_Exception;
      end if;

      A_Vis_Window_Data_Element := Known_Vis_Windows_Hashs.Fetch
        (Project.All_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name));

      if not A_Vis_Window_Data_Element.Is_Memory_Loaded then
         raise Visualisation_Window_Is_Not_Memory_Loaded_Exception;
      end if;

      -- change name of Vis_Window
      -----------------------------------
      Vis_Windows.Change_Name
        (A_Vis_Window_Data_Element.Vis_Window,
         New_Vis_Window_Name);

      A_Vis_Window_Data_Element.Vis_Window_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (New_Vis_Window_Name);

      -- rename old management file
      ------------------------------------------
      if A_Vis_Window_Data_Element.Is_File_Linked then

         New_Vis_Window_File_Name :=
           Create_Name_For_File
           (File_Management.Return_Dir_Path_For_File_Path
            (Ada.Strings.Unbounded.To_String
             (A_Vis_Window_Data_Element.Existing_Vis_Window_File)),
            Ada.Strings.Unbounded.To_String
            (A_Vis_Window_Data_Element.Vis_Window_Name),
            Const_Vis_Window_File_Ending);

         -- copy old file
         File_Management.Copy_File
           (Ada.Strings.Unbounded.To_String
            (A_Vis_Window_Data_Element.Existing_Vis_Window_File),
            Ada.Strings.Unbounded.To_String
            (New_Vis_Window_File_Name));

         -- delete old file
         File_Management.Delete_File
           (Ada.Strings.Unbounded.To_String
            (A_Vis_Window_Data_Element.Existing_Vis_Window_File));

         -- update status
         A_Vis_Window_Data_Element.Existing_Vis_Window_File :=
           New_Vis_Window_File_Name;
      end if;

      -- update hash map
      ---------------------------------
      Known_Vis_Windows_Hashs.Unbind
        (Project.All_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name));

      Known_Vis_Windows_Hashs.Bind
        (Project.All_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (New_Vis_Window_Name),
         A_Vis_Window_Data_Element);

      -- update xml file
      -- necessary to keep track on the new file name for
      -- file linked vis windows.
      ----------------------------------------------------------
      Write_Project_XML_File (Project);

   end Change_Vis_Window_Name;

   ---------------------------------------------------------------------------
   procedure Add_Visualisation_Window
     (Project    : in Project_Access;
      Vis_Window : in Vis_Windows.Visual_Window_Access) is

      New_Vis_Window_Data : Vis_Window_Data_Element;
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if (Does_Vis_Window_Exist
          (Project,
           Vis_Windows.Get_Name (Vis_Window)) = True) then

         raise Visualisation_Window_Is_Already_Part_Of_Project_Exception;
      end if;

      New_Vis_Window_Data.Vis_Window_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
        (Vis_Windows.Get_Name (Vis_Window));

      New_Vis_Window_Data.Is_File_Linked := False;
      New_Vis_Window_Data.Existing_Vis_Window_File :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      New_Vis_Window_Data.Is_Memory_Loaded := True;
      New_Vis_Window_Data.Vis_Window := Vis_Window;

      Known_Vis_Windows_Hashs.Bind
        (Project.All_Vis_Windows,
         New_Vis_Window_Data.Vis_Window_Name,
         New_Vis_Window_Data);
   end Add_Visualisation_Window;

   ---------------------------------------------------------------------------
   procedure Store_Single_Visualisation_Window
     (Project         : in Project_Access;
      Vis_Window_Name : in String) is

      A_Vis_Window_Data_Element : Vis_Window_Data_Element;
      A_Vis_Window_File_Name : Ada.Strings.Unbounded.Unbounded_String;

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if not Does_Vis_Window_Exist
        (Project, Vis_Window_Name) then
         raise Visualisation_Window_Is_Not_Part_Of_Project_Exception;
      end if;

      A_Vis_Window_Data_Element := Known_Vis_Windows_Hashs.Fetch
        (Project.All_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name));

      if not A_Vis_Window_Data_Element.Is_Memory_Loaded then
         raise Visualisation_Window_Is_Not_Memory_Loaded_Exception;
      end if;

      if A_Vis_Window_Data_Element.Is_File_Linked then
         A_Vis_Window_File_Name :=
           A_Vis_Window_Data_Element.Existing_Vis_Window_File;
      else

         -- calculate new file name if window if not file linked
         A_Vis_Window_File_Name :=
           Create_Name_For_File
           (Ada.Strings.Unbounded.To_String
            (Project.Abs_Project_Directory),
            Ada.Strings.Unbounded.To_String
            (A_Vis_Window_Data_Element.Vis_Window_Name),
            Const_Vis_Window_File_Ending);
      end if;

      Write_Vis_Window_To_File
        (Ada.Strings.Unbounded.To_String
         (A_Vis_Window_File_Name),
         A_Vis_Window_Data_Element.Vis_Window);

      A_Vis_Window_Data_Element.Is_File_Linked := True;
      A_Vis_Window_Data_Element.Existing_Vis_Window_File :=
        A_Vis_Window_File_Name;

      Known_Vis_Windows_Hashs.Update_Value
        (Project.All_Vis_Windows,
         A_Vis_Window_Data_Element.Vis_Window_Name,
         A_Vis_Window_Data_Element);

      -- update xml file
      Write_Project_XML_File (Project);
   end Store_Single_Visualisation_Window;

   ---------------------------------------------------------------------------
   procedure Free_Memory_For_Vis_Window
     (Project         : in Project_Access;
      Vis_Window_Name : in String) is

      A_Vis_Window_Data_Element : Vis_Window_Data_Element;
      Security_File_Name : Ada.Strings.Unbounded.Unbounded_String;

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if not Does_Vis_Window_Exist
        (Project, Vis_Window_Name) then
         raise Visualisation_Window_Is_Not_Part_Of_Project_Exception;
      end if;

      A_Vis_Window_Data_Element := Known_Vis_Windows_Hashs.Fetch
        (Project.All_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name));

      if not A_Vis_Window_Data_Element.Is_Memory_Loaded then
         raise Visualisation_Window_Is_Not_Memory_Loaded_Exception;
      end if;

      --  create security file (always in project directory)
      ------------------------------------------------------
      Security_File_Name := Create_Name_For_File
        (Ada.Strings.Unbounded.To_String
         (Project.Abs_Project_Directory),
         Ada.Strings.Unbounded.To_String
         (A_Vis_Window_Data_Element.Vis_Window_Name),
         Const_Vis_Window_Security_File_Ending);

      Write_Vis_Window_To_File
        (Ada.Strings.Unbounded.To_String
         (Security_File_Name),
         A_Vis_Window_Data_Element.Vis_Window);

      --  deallocate memory needed for vis window
      -------------------------------------------
      Vis_Windows.Deallocate_Vis_Window_Deep
        (A_Vis_Window_Data_Element.Vis_Window);

      --  change vis window status
      ----------------------------
      A_Vis_Window_Data_Element.Is_Memory_Loaded := False;

      Logger.Debug ("File nach Free : " & Ada.Strings.Unbounded.To_String
        (Known_Vis_Windows_Hashs.Fetch
        (Project.All_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name)).
         Existing_Vis_Window_File));

      if A_Vis_Window_Data_Element.Is_File_Linked then
         Known_Vis_Windows_Hashs.Update_Value
           (Project.All_Vis_Windows,
            A_Vis_Window_Data_Element.Vis_Window_Name,
            A_Vis_Window_Data_Element);
      else
         Known_Vis_Windows_Hashs.Unbind
           (Project.All_Vis_Windows,
            A_Vis_Window_Data_Element.Vis_Window_Name);
      end if;

   end Free_Memory_For_Vis_Window;

   ---------------------------------------------------------------------------
   procedure Remove_Visualisation_Window
     (Project         : in Project_Access;
      Vis_Window_Name : in String) is

      A_Vis_Window_Data_Element : Vis_Window_Data_Element;
      Security_File_Name : Ada.Strings.Unbounded.Unbounded_String;

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if not Does_Vis_Window_Exist
        (Project, Vis_Window_Name) then
         raise Visualisation_Window_Is_Not_Part_Of_Project_Exception;
      end if;

      A_Vis_Window_Data_Element := Known_Vis_Windows_Hashs.Fetch
        (Project.All_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name));

      -- create security file for memory loaded windows
      -------------------------------------------------
      if A_Vis_Window_Data_Element.Is_Memory_Loaded then
         Security_File_Name := Create_Name_For_File
           (Ada.Strings.Unbounded.To_String
            (Project.Abs_Project_Directory),
            Ada.Strings.Unbounded.To_String
            (A_Vis_Window_Data_Element.Vis_Window_Name),
            Const_Vis_Window_Security_File_Ending);

         Write_Vis_Window_To_File
           (Ada.Strings.Unbounded.To_String
            (Security_File_Name),
            A_Vis_Window_Data_Element.Vis_Window);
      end if;

      -- remove management file if exists
      -----------------------------------
      if A_Vis_Window_Data_Element.Is_File_Linked then

         File_Management.Delete_File
           (Ada.Strings.Unbounded.To_String
            (A_Vis_Window_Data_Element.Existing_Vis_Window_File));
      end if;

      -- remove vis window from project (no deallocation)
      ---------------------------------------------------
      Known_Vis_Windows_Hashs.Unbind
        (Project.All_Vis_Windows,
         A_Vis_Window_Data_Element.Vis_Window_Name);

      -- update xml file
      ------------------
      Write_Project_XML_File (Project);
   end Remove_Visualisation_Window;


   ---------------------------------------------------------------------------
   -- C Subgraphs
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Subgraph_Exist
     (Project       : in Project_Access;
      Subgraph_Name : in String)
     return Boolean is

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      return Subgraph_Data_Hashs.Is_Bound
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String
         (Subgraph_Name));
   end Does_Subgraph_Exist;

   ---------------------------------------------------------------------------
   function Get_Subgraph
     (Project       : in Project_Access;
      Subgraph_Name : in String)
     return Graph_Lib.Subgraphs.Subgraph is

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if not Does_Subgraph_Exist (Project, Subgraph_Name) then
         raise Subgraph_Is_Not_Part_Of_Project_Exception;
      end if;

      return Subgraph_Data_Hashs.Fetch
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String
         (Subgraph_Name)).Subgraph;
   end Get_Subgraph;

   ---------------------------------------------------------------------------
   function Get_All_Subgraphs
     (Project : in Project_Access)
     return String_Lists.List is

      Names_List              : String_Lists.List;
      Subgraph_Iter           : Subgraph_Data_Hashs.Values_Iter;
      A_Subgraph_Data_Element : Subgraph_Data_Element;
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      Names_List := String_Lists.Create;
      Subgraph_Iter := Subgraph_Data_Hashs.Make_Values_Iter
        (Project.All_Subgraphs);

      while Subgraph_Data_Hashs.More (Subgraph_Iter) loop

         Subgraph_Data_Hashs.Next
           (Subgraph_Iter, A_Subgraph_Data_Element);
         String_Lists.Attach
           (Names_List,
            Ada.Strings.Unbounded.To_Unbounded_String
            (Graph_Lib.Subgraphs.Get_Name
             (A_Subgraph_Data_Element.Subgraph)));
      end loop;

      return Names_List;
   end Get_All_Subgraphs;

   ---------------------------------------------------------------------------
   procedure Change_Subgraph_Name
     (Project           : in Project_Access;
      Subgraph_Name     : in String;
      New_Subgraph_Name : in String) is

      A_Subgraph_Data_Element : Subgraph_Data_Element;
      New_Subgraph_File_Name : Ada.Strings.Unbounded.Unbounded_String;
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if not Does_Subgraph_Exist
        (Project, Subgraph_Name) then
         raise Subgraph_Is_Not_Part_Of_Project_Exception;
      end if;

      if Does_Subgraph_Exist
        (Project, New_Subgraph_Name) then
         raise New_Subgraph_Name_Does_Already_Exist_Exception;
      end if;

      A_Subgraph_Data_Element := Subgraph_Data_Hashs.Fetch
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String (Subgraph_Name));

      -- change name of subgraph
      --------------------------
      Graph_Lib.Subgraphs.Rename
        (A_Subgraph_Data_Element.Subgraph,
         New_Subgraph_Name);

      -- rename old management file
      ------------------------------------------
      if A_Subgraph_Data_Element.Is_File_Linked then

         New_Subgraph_File_Name :=
           Create_Name_For_File
           (File_Management.Return_Dir_Path_For_File_Path
            (Ada.Strings.Unbounded.To_String
             (A_Subgraph_Data_Element.Existing_Subgraph_File)),
            Graph_Lib.Subgraphs.Get_Name
            (A_Subgraph_Data_Element.Subgraph),
            Const_Subgraph_File_Ending);

         -- copy old file
         File_Management.Copy_File
           (Ada.Strings.Unbounded.To_String
            (A_Subgraph_Data_Element.Existing_Subgraph_File),
            Ada.Strings.Unbounded.To_String
            (New_Subgraph_File_Name));

         -- delete old file
         File_Management.Delete_File
           (Ada.Strings.Unbounded.To_String
            (A_Subgraph_Data_Element.Existing_Subgraph_File));

         -- update subgraph status
         A_Subgraph_Data_Element.Existing_Subgraph_File :=
           New_Subgraph_File_Name;
      end if;

      -- update hash map and data model
      ---------------------------------
      Subgraph_Data_Hashs.Unbind
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String (Subgraph_Name));

      Subgraph_Data_Hashs.Bind
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String (New_Subgraph_Name),
         A_Subgraph_Data_Element);

      -- update xml file - necessary to keep track on new file name
      -------------------------------------------------------------
      Write_Project_XML_File (Project);
   end Change_Subgraph_Name;

   ---------------------------------------------------------------------------
   procedure Add_Subgraph
     (Project  : in Project_Access;
      Subgraph : in Graph_Lib.Subgraphs.Subgraph) is

      New_Subgraph_Data_Element : Subgraph_Data_Element;
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if Does_Subgraph_Exist
        (Project,
         Graph_Lib.Subgraphs.Get_Name (Subgraph)) then
         raise Subgraph_Is_Already_Part_Of_Project_Exception;
      end if;

      New_Subgraph_Data_Element.Subgraph               := Subgraph;
      New_Subgraph_Data_Element.Highlight_Status       := None;
      New_Subgraph_Data_Element.Is_File_Linked         := False;
      New_Subgraph_Data_Element.Existing_Subgraph_File :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      Subgraph_Data_Hashs.Bind
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String
         (Graph_Lib.Subgraphs.Get_Name (Subgraph)),
         New_Subgraph_Data_Element);
   end Add_Subgraph;

   ---------------------------------------------------------------------------
   procedure Remove_Subgraph
     (Project       : in Project_Access;
      Subgraph_Name : in String) is

      A_Subgraph_Data_Element : Subgraph_Data_Element;
      Security_File_Name : Ada.Strings.Unbounded.Unbounded_String;

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if not Does_Subgraph_Exist
        (Project, Subgraph_Name) then
         raise Subgraph_Is_Not_Part_Of_Project_Exception;
      end if;

      A_Subgraph_Data_Element := Subgraph_Data_Hashs.Fetch
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String (Subgraph_Name));

      -- create security file
      -----------------------
      Security_File_Name := Create_Name_For_File
        (Ada.Strings.Unbounded.To_String
         (Project.Abs_Project_Directory),
         Subgraph_Name,
         Const_Subgraph_Security_File_Ending);

      Write_Sub_Graph_Data_To_File
        (Ada.Strings.Unbounded.To_String
         (Security_File_Name),
         A_Subgraph_Data_Element);

      -- remove management file if exists
      -----------------------------------
      if A_Subgraph_Data_Element.Is_File_Linked then

         File_Management.Delete_File
           (Ada.Strings.Unbounded.To_String
            (A_Subgraph_Data_Element.Existing_Subgraph_File));
      end if;

      -- remove subgraph from project (no deallocation)
      ---------------------------------------------------
      Subgraph_Data_Hashs.Unbind
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String (Subgraph_Name));

      -- update xml file
      -------------------------------------------------------------
      Write_Project_XML_File (Project);
   end Remove_Subgraph;

   ---------------------------------------------------------------------------
   function Get_Highlight_Status
     (Project       : in Project_Access;
      Subgraph_Name : in String)
     return Subgraph_Highlight_Status is

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if (Does_Subgraph_Exist (Project, Subgraph_Name) = False) then
         raise Subgraph_Is_Not_Part_Of_Project_Exception;
      end if;

      return Subgraph_Data_Hashs.Fetch
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String
         (Subgraph_Name)).Highlight_Status;
   end Get_Highlight_Status;

   ---------------------------------------------------------------------------
   procedure Change_Highlight_Status
     (Project              : in Project_Access;
      Subgraph_Name        : in String;
      New_Highlight_Status : in Subgraph_Highlight_Status) is

      A_Subgraph_Data_Element   : Subgraph_Data_Element;
      New_Subgraph_Data_Element : Subgraph_Data_Element;

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if (Does_Subgraph_Exist (Project, Subgraph_Name) = False) then
         raise Subgraph_Is_Not_Part_Of_Project_Exception;
      end if;

      A_Subgraph_Data_Element := Subgraph_Data_Hashs.Fetch
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String (Subgraph_Name));

      -- change highlight status
      New_Subgraph_Data_Element := A_Subgraph_Data_Element;
      New_Subgraph_Data_Element.Highlight_Status :=
        New_Highlight_Status;

      Subgraph_Data_Hashs.Unbind
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String
         (Graph_Lib.Subgraphs.Get_Name
          (A_Subgraph_Data_Element.Subgraph)));

      Subgraph_Data_Hashs.Bind
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String
         (Graph_Lib.Subgraphs.Get_Name
          (New_Subgraph_Data_Element.Subgraph)),
         New_Subgraph_Data_Element);
   end Change_Highlight_Status;

   ---------------------------------------------------------------------------
   -- D Node Annotations
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Node_Annotations
     (Project : in Project_Access)
     return Node_Annotations.Node_Annotation_Access is
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      return Project.The_Node_Annotations;
   end Get_Node_Annotations;

   function Exists_Name
     (Project : in Project_Access;
      Name    : in String)
     return Boolean
   is
   begin
      return Does_Vis_Window_Exist (Project, Name)
        or else Projects.Does_Subgraph_Exist (Project, Name);
   end Exists_Name;

end Giant.Projects;






