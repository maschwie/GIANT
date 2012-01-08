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
--  $RCSfile: giant-projects-test.adb,v $, $Revision: 1.17 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Gtk.Main;

with Gnat.OS_Lib;

with Giant.Config.Vis_Styles;
with Giant.File_Management;
with Giant.Logger;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Subgraphs;
with Giant.Vis_Windows;
with Giant.Node_Annotations;

with Giant.Projects;

package body Giant.Projects.Test is
  
   ---------------------------------------------------------------------------
   --  Deletes all files in the directory
   procedure Kill_Files_In_Dir (Directory : in String) is

      File_List      : String_Lists.List;
      File_List_Iter : String_Lists.ListIter;
      A_File_Name    : Ada.Strings.Unbounded.Unbounded_String;
   begin

      File_List := String_Lists.Create;

      -- Security Files for visualisation windows
      String_Lists.Attach
        (File_List,
         File_Management.Get_Filtered_Files_From_Directory
         (Directory,
          False,
          ""));

      File_List_Iter := String_Lists.MakeListIter (File_List);

      while String_Lists.More (File_List_Iter) loop

         String_Lists.Next (File_List_Iter, A_File_Name);
         File_Management.Delete_File (Ada.Strings.Unbounded.To_String
                                      (A_File_Name));
      end loop;

      String_Lists.Destroy (File_List);
   end Kill_Files_In_Dir;
   
   ---------------------------------------------------------------------------
   function Create_Test_Project_1 return Giant.Projects.Project_Access is 
   
      Test_Project_1 : Giant.Projects.Project_Access;
      A_Subgraph     : Giant.Graph_Lib.Subgraphs.Subgraph;  
      A_Vis_Window   : Giant.Vis_Windows.Visual_Window_Access;
      
   begin    
         
      Test_Project_1 := Create_Empty_Project
        (Project_Name => "Test_Project_1",
         Project_Directory => "resources/test_project_directory/dir_1",
         Bauhaus_IML_Graph_File => "resources/rfg_examp.iml",
         Bauhaus_IML_Graph_File_Checksum => Giant.Graph_Lib.Get_Graph_Hash);
            
      -- add subgraphs
      A_Subgraph := Giant.Graph_Lib.Subgraphs.Create ("Sub_Graph_1");
      Giant.Projects.Add_Subgraph (Test_Project_1, A_Subgraph);   
        
      A_Subgraph := Giant.Graph_Lib.Subgraphs.Create ("Sub_Graph_2");
      Giant.Projects.Add_Subgraph (Test_Project_1, A_Subgraph);         

      A_Subgraph := Giant.Graph_Lib.Subgraphs.Create ("Sub_Graph_3");
      Giant.Projects.Add_Subgraph (Test_Project_1, A_Subgraph);  

      A_Subgraph := Giant.Graph_Lib.Subgraphs.Create ("Sub_Graph_4");
      Giant.Projects.Add_Subgraph (Test_Project_1, A_Subgraph);  

      -- add vis windows
      A_Vis_Window := Giant.Vis_Windows.Create_New ("Vis_Window_1");
      Giant.Projects.Add_Visualisation_Window
        (Test_Project_1, A_Vis_Window);
        
      A_Vis_Window := Giant.Vis_Windows.Create_New ("Vis_Window_2");
      Giant.Projects.Add_Visualisation_Window
        (Test_Project_1, A_Vis_Window);
        
      A_Vis_Window := Giant.Vis_Windows.Create_New ("Vis_Window_3");
      Giant.Projects.Add_Visualisation_Window
        (Test_Project_1, A_Vis_Window);
        
      A_Vis_Window := Giant.Vis_Windows.Create_New ("Vis_Window_4");
      Giant.Projects.Add_Visualisation_Window
        (Test_Project_1, A_Vis_Window);

      return Test_Project_1;
   end Create_Test_Project_1;
                         
   ---------------------------------------------------------------------------
   procedure Check_Test_Project_1       
     (Project : in Giant.Projects.Project_Access) is 
   
     Test_List : String_Lists.List;
     
     Annotats : Node_Annotations.Node_Annotation_Access;
     Nod_List : Graph_Lib.Node_Id_Lists.List;
   begin
   
      -- check project status
      Assert 
        (Projects.Get_Project_Name (Project) = "Test_Project_1",
         "Check correct project name after initialisation");

      -- check subgraphs
      Assert (Projects.Does_Subgraph_Exist
        (Project, "Sub_Graph_1"),
         "Check whether Sub_Graph_1 exits");
         
      Assert (Projects.Does_Subgraph_Exist
        (Project, "Sub_Graph_2"),
         "Check whether Sub_Graph_2 exits");
         
      Assert (Projects.Does_Subgraph_Exist
        (Project, "Sub_Graph_3"),
         "Check whether Sub_Graph_3 exits");
         
      Assert (Projects.Does_Subgraph_Exist
        (Project, "Sub_Graph_4"),
         "Check whether Sub_Graph_4 exits");

      Assert (not Projects.Does_Subgraph_Exist
        (Project, "Donald"),
         "Check whether Sub_Graph Donald not exits");
         
      Test_List := 
        Projects.Get_All_Subgraphs (Project);
      Assert (String_Lists.Length (Test_List) = 4,
              "Test Lenght of Subgraph Names list");
      String_Lists.Destroy (Test_List);   


      -- check vis windows
      Assert (Projects.Does_Vis_Window_Exist
        (Project, "Vis_Window_1"),
         "Check whether Vis_Window_1 exists");

      Assert (Projects.Does_Vis_Window_Exist
        (Project, "Vis_Window_2"),
         "Check whether Vis_Window_2 exists");

      Assert (Projects.Does_Vis_Window_Exist
        (Project, "Vis_Window_3"),
         "Check whether Vis_Window_3 exists");

      Assert (Projects.Does_Vis_Window_Exist
        (Project, "Vis_Window_4"),
         "Check whether Vis_Window_4 exists");

      Assert (not Projects.Does_Vis_Window_Exist
        (Project, "Durchsicht"),
         "Check whether Vis_Window Durchsicht not exists");  
      
      Test_List := 
        Projects.Get_All_Visualisation_Window_Names (Project);
      Assert (String_Lists.Length (Test_List) = 4,
              "Test Lenght of Vis Window Names list");
      String_Lists.Destroy (Test_List);

      Annotats := 
        Projects.Get_Node_Annotations (Project);
        
      Nod_List := Giant.Node_Annotations.Get_All_Annotated_Nodes (Annotats);
      Assert (Graph_Lib.Node_Id_Lists.Length (Nod_List) = 0,
              "Test whether node annotations are empty");
      Graph_Lib.Node_Id_Lists.Destroy (Nod_List);
   
   end Check_Test_Project_1;
   
   ---------------------------------------------------------------------------
   procedure Check_Whether_All_T_Project_1_Files_Written
     (Dir : in String) is
     
      Test_List : String_Lists.List;      
   begin
       
      -- check dir
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        (Dir,
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 12,
         "Gen Test correct ammount of files in " 
         & Dir);
      String_Lists.Destroy (Test_List);   
      
      -- project files
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir & "Test_Project_1.xml"),
         "Test Project_File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir & "/giant_project_file.dtd"),
         "Test Project DTD File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "node_annotations.xml"),
         "Test Node Annotartions File written correctly");                  
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "giant_node_annotations_file.dtd"),
         "Test Node Annotartions DTD File written correctly");
      
      -- Subgraph Stream files
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Sub_Graph_1.subgraph"),
         "Test Sub_Graph_1 File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Sub_Graph_2.subgraph"),
         "Test Sub_Graph_2 File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Sub_Graph_3.subgraph"),
         "Test Sub_Graph_3 File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Sub_Graph_4.subgraph"),
         "Test Sub_Graph_4 File written correctly");
         
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Sub_Graph_4.subgraph"),      
         "Test Sub_Graph_4 File written correctly");
     
      -- Vis Window Stream files
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Vis_Window_1.viswin"),
         "Test Vis_Window_1 File written correctly");         
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Vis_Window_2.viswin"),
         "Test Vis_Window_2 File written correctly");   
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Vis_Window_3.viswin"),
         "Test Vis_Window_3 File written correctly");   
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Vis_Window_4.viswin"),
         "Test Vis_Window_4 File written correctly");   
   
   end Check_Whether_All_T_Project_1_Files_Written;

   ---------------------------------------------------------------------------
   procedure Test_Initialize
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Project_1 : Giant.Projects.Project_Access;

   begin

      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");      
      Test_Project_1 := Create_Test_Project_1;            
      Check_Test_Project_1 (Test_Project_1);     
      Projects.Deallocate_Project_Deep (Test_Project_1);
   end Test_Initialize;

   ---------------------------------------------------------------------------
   procedure Basic_File_Mangement_Test
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Project_1 : Giant.Projects.Project_Access;
      Test_List      : String_Lists.List; 
      
      Bauhaus_IML_Graph_File : Ada.Strings.Unbounded.Unbounded_String;
      Bauhaus_IML_Graph_File_Checksum : Integer;     

   begin
   
      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");
      Test_Project_1 := Create_Test_Project_1;            
      Check_Test_Project_1 (Test_Project_1);
      
      -- test directory content after initialisation
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/Test_Project_1.xml"),
         "Test Project_File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/giant_project_file.dtd"),
         "Test Project DTD File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "node_annotations.xml"),
         "Test Node Annotartions File written correctly");                  
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "giant_node_annotations_file.dtd"),
         "Test Node Annotartions DTD File written correctly");
              
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1/",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 4,
         "Test correct ammount of files in dir_1 - new project created");
      String_Lists.Destroy (Test_List);   

      -- test content after storing 
      Projects.Store_Whole_Project (Test_Project_1);
      
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_1/");
                
      -- advanced storing test
      Kill_Files_In_Dir ("resources/test_project_directory/dir_2");      
      Projects.Store_Whole_Project_As_For_File
      (Test_Project_1,
       "resources/test_project_directory/dir_2/Test_Project_1.xml");      
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_2/");
                  
      Kill_Files_In_Dir ("resources/test_project_directory/dir_2");      
      Projects.Store_Whole_Project_As_For_File
      (Test_Project_1,
       "resources/test_project_directory/dir_2/Test_Project_1");      
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_2/");        
        
      Kill_Files_In_Dir ("resources/test_project_directory/dir_2");      
      Projects.Store_Whole_Project_As
      (Test_Project_1,
       "Test_Project_1",
       "resources/test_project_directory/dir_2/");      
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_2/");   

      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_1/");
                      
      -- check load store
      -------------------
      Projects.Deallocate_Project_Deep (Test_Project_1);            
      Test_Project_1 := Projects.Load_Project_File
        ("resources/test_project_directory/dir_2/Test_Project_1.xml");
      Check_Test_Project_1 (Test_Project_1);
      
      Kill_Files_In_Dir ("resources/test_project_directory/dir_3");      
      Projects.Store_Whole_Project_As_For_File
      (Test_Project_1,
       "resources/test_project_directory/dir_3/Test_Project_3");
             
      -- Project_Does_Not_Exist_Exception
      begin       
         Test_Project_1 := Projects.Load_Project_File
           ("resources/test_project_directory/dir_2/Test_Project_3.xml");  
         Assert 
           (False,
            "Check Load_Project_File - Project_Does_Not_Exist_Exception");                     
      exception
         when Project_Does_Not_Exist_Exception =>
            null;
      end;
      
      -- Invalid_Project_Directory_Excpetion
      begin       
         Test_Project_1 := Projects.Load_Project_File
           ("resources/test_project_directory/dir_5/Test_Project_3.xml");  
         Assert 
           (False,
            "Check Load_Project_File - Invalid_Project_Directory_Excpetion");                     
      exception
         when Invalid_Project_Directory_Excpetion =>
            null;
      end;
                    
      Projects.Deallocate_Project_Deep (Test_Project_1);            
      Test_Project_1 := Projects.Load_Project_File
        ("resources/test_project_directory/dir_3/Test_Project_3.xml");      
      
      Assert 
        (Projects.Get_Project_Name (Test_Project_1) = "Test_Project_3",
         "Check project renamed correctly");
      
      Kill_Files_In_Dir ("resources/test_project_directory/dir_2");      
      Projects.Store_Whole_Project_As_For_File
      (Test_Project_1,
       "resources/test_project_directory/dir_2/Test_Project_1");
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_2/");  
  
      Projects.Deallocate_Project_Deep (Test_Project_1);            
      Test_Project_1 := Projects.Load_Project_File
        ("resources/test_project_directory/dir_2/Test_Project_1.xml"); 
      Check_Test_Project_1 (Test_Project_1);        
                                                
      -- check project directoty content test                              
      Projects.Deallocate_Project_Deep (Test_Project_1);
            
      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");
      Test_Project_1 := Create_Test_Project_1;            
      Check_Test_Project_1 (Test_Project_1);
      
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/Test_Project_1.xml"),
         "Test Project_File written correctly");
                 
      Assert 
        (Projects.Does_Project_Exist
          ("Test_Project_1",
           "resources/test_project_directory/dir_1"),
         "Check Procedure Does_Project_Exist - project found");

      Assert 
        (Projects.Is_Already_A_Project_File_In_Directory
          ("resources/test_project_directory/dir_1"),
         "Check Procedure Is_Already_A_Project_File_In_Directory "
         &" - project found");

      -- Directory_Holds_Already_A_Project_File_Exception
      begin 
         Projects.Store_Whole_Project_As
           (Test_Project_1,
            "Test_Project_1",
            "resources/test_project_directory/dir_1/");     
         Assert 
           (False,
            "Check Store_Whole_Project_As "
            & "Directory_Holds_Already_A_Project_File_Exception");
      exception
         when Directory_Holds_Already_A_Project_File_Exception =>
         Assert 
           (True,
            "Check Store_Whole_Project_As "
            & "Directory_Holds_Already_A_Project_File_Exception");                                   
      end;
            
      Assert 
        (Projects.Get_Bauhaus_IML_Graph_File 
          ("resources/test_project_directory/dir_1/Test_Project_1.xml") =
         File_Management.Get_Absolute_Path_To_File_From_Relative
           (".", "resources/rfg_examp.iml"),
         "Check whether correct iml graph file retrieved - 1");
         
      Projects.Get_Bauhaus_IML_Graph_Data_For_File
        ("resources/test_project_directory/dir_1/Test_Project_1.xml",
         Bauhaus_IML_Graph_File,
         Bauhaus_IML_Graph_File_Checksum);
      
      Assert (Ada.Strings.Unbounded.To_String (Bauhaus_IML_Graph_File)
        = File_Management.Get_Absolute_Path_To_File_From_Relative
            (".", "resources/rfg_examp.iml"),
         "Check whether correct iml graph file retrieved - 2");
      
      Assert
        (Bauhaus_IML_Graph_File_Checksum = Giant.Graph_Lib.Get_Graph_Hash,
         "Check whether coorect IML Graph Checksum is stored in Project");
         
      Assert 
        (Projects.Get_Project_File_Name (Test_Project_1)
         = File_Management.Get_Absolute_Path_To_File_From_Relative
           (".", "resources/test_project_directory/dir_1/Test_Project_1.xml"),
         "Check correct project file name returned");

      Assert 
        (Projects.Get_Project_Directory (Test_Project_1)
         = File_Management.Get_Absolute_Path_To_Directory_From_Relative
           (".", "resources/test_project_directory/dir_1"),
         "Check correct project directory is returned");
      
      -----   
      File_Management.Delete_File  
        ("resources/test_project_directory/dir_1/Test_Project_1.xml");
        
      Assert 
        (not Projects.Does_Project_Exist
          ("Test_Project_1",
           "resources/test_project_directory/dir_1"),
         "Check Procedure Does_Project_Exist - project not found");
              
      Assert 
        (not Projects.Is_Already_A_Project_File_In_Directory
          ("resources/test_project_directory/dir_1"),
         "Check Procedure Is_Already_A_Project_File_In_Directory "
         &" - project not found");
      
      Projects.Deallocate_Project_Deep (Test_Project_1);   
         
   end Basic_File_Mangement_Test;
   
   ---------------------------------------------------------------------------
   procedure Changing_Test_Only_Memory
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Project_1 : Giant.Projects.Project_Access;
      Test_List      : String_Lists.List; 
      
      A_Subgraph     : Giant.Graph_Lib.Subgraphs.Subgraph;  
      A_Vis_Window   : Giant.Vis_Windows.Visual_Window_Access;
   begin
   
      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");
      Test_Project_1 := Create_Test_Project_1;            
      Check_Test_Project_1 (Test_Project_1);
      
       -- check dir
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 4,
         "Test correct ammount of files in dir_1 - before changing");
      String_Lists.Destroy (Test_List); 
      
      -- do some changes 
      ------------------     
      Projects.Change_Vis_Window_Name
        (Test_Project_1,
         "Vis_Window_1",
         "Vis_Window_1_New_Name");
         
      Projects.Change_Subgraph_Name
        (Test_Project_1,
         "Sub_Graph_1",
         "Sub_Graph_1_New_Name");   
                   
      -- New_Vis_Window_Name_Does_Already_Exist_Exception
      begin 
         Projects.Change_Vis_Window_Name
           (Test_Project_1,
            "Vis_Window_2",
            "Vis_Window_3");     
         Assert 
           (False,
            "Check Change_Vis_Window_Name "
            & "New_Vis_Window_Name_Does_Already_Exist_Exception");
      exception
         when New_Vis_Window_Name_Does_Already_Exist_Exception =>
           null;                                
      end;   

      -- Visualisation_Window_Is_Already_Part_Of_Project_Exception
      begin
         A_Vis_Window := Giant.Vis_Windows.Create_New ("Vis_Window_2");
         Projects.Add_Visualisation_Window
           (Test_Project_1, A_Vis_Window);
         Assert 
           (False,
            "Check Add_Visualisation_Window "
            & "Visualisation_Window_Is_Already_Part_Of_Project_Exception");
      exception
         when Visualisation_Window_Is_Already_Part_Of_Project_Exception =>
           null;                                
      end;  
      
      -- New_Subgraph_Name_Does_Already_Exist_Exception
      begin 
         Projects.Change_Subgraph_Name
           (Test_Project_1,
            "Sub_Graph_2",
            "Sub_Graph_3");     
         Assert 
           (False,
            "Check Change_Subgraph_Name "
            & "New_Subgraph_Name_Does_Already_Exist_Exception");
      exception
         when New_Subgraph_Name_Does_Already_Exist_Exception =>
           null;                                
      end;
      
      -- Subgraph_Is_Already_Part_Of_Project_Exception
      begin       
         A_Subgraph := Giant.Graph_Lib.Subgraphs.Create ("Sub_Graph_2");
         Giant.Projects.Add_Subgraph (Test_Project_1, A_Subgraph);       
         Assert 
           (False,
            "Check A_Subgraph "
            & "Subgraph_Is_Already_Part_Of_Project_Exception");
      exception
         when Subgraph_Is_Already_Part_Of_Project_Exception =>
           null;                                
      end;
                        
      Projects.Remove_Visualisation_Window 
        (Test_Project_1, "Vis_Window_2");
        
      Projects.Remove_Subgraph
        (Test_Project_1, "Sub_Graph_4");
      
      -- check status after changes
      -----------------------------
      Assert (not Projects.Does_Vis_Window_Exist
        (Test_Project_1, "Vis_Window_1"),
         "Check Vis_Window_1 renamed correctly - 1");
         
      Assert (Projects.Does_Vis_Window_Exist
        (Test_Project_1, "Vis_Window_1_New_Name"),
         "Check Vis_Window_1 renamed correctly - 2");

      A_Vis_Window := Projects.Get_Visualisation_Window
        (Test_Project_1, "Vis_Window_1_New_Name");

      Assert 
        (Vis_Windows.Get_Name (A_Vis_Window) = "Vis_Window_1_New_Name",
         "Check Vis_Window_1 renamed correctly - 3");
         
      -- Visualisation_Window_Is_Not_Part_Of_Project_Exception
      begin
         A_Vis_Window := Projects.Get_Visualisation_Window
           (Test_Project_1, "Vis_Window_1");
         Assert 
           (False,
            "Check Get_Visualisation_Window "
            & "Visualisation_Window_Is_Not_Part_Of_Project_Exception - 1");
      exception
         when Visualisation_Window_Is_Not_Part_Of_Project_Exception =>
           null;                                
      end;
      
      -- Visualisation_Window_Is_Not_Part_Of_Project_Exception
      begin
         A_Vis_Window := Projects.Get_Visualisation_Window
           (Test_Project_1, "Vis_Window_2");
         Assert 
           (False,
            "Check Get_Visualisation_Window "
            & "Visualisation_Window_Is_Not_Part_Of_Project_Exception - 2");
      exception
         when Visualisation_Window_Is_Not_Part_Of_Project_Exception =>
           null;                                
      end;
      
      Assert (not Projects.Does_Vis_Window_Exist
        (Test_Project_1, "Vis_Window_2"),
         "Check Vis_Window_2 removed correctly");
      
         
      Assert (not Projects.Does_Subgraph_Exist
        (Test_Project_1, "Sub_Graph_1"),
         "Check whether Sub_Graph_1 renamed correctly - 1");  
         
      Assert (Projects.Does_Subgraph_Exist
        (Test_Project_1, "Sub_Graph_1_New_Name"),
         "Check whether Sub_Graph_1 renamed correctly - 2");           
         
      A_Subgraph := Get_Subgraph
        (Test_Project_1, "Sub_Graph_1_New_Name");        
      Assert 
        (Graph_Lib.Subgraphs.Get_Name (A_Subgraph) = "Sub_Graph_1_New_Name",
         "Check whether Sub_Graph_1 renamed correctly - 3"); 
         
      Assert (not Projects.Does_Subgraph_Exist
        (Test_Project_1, "Sub_Graph_4"),
         "Check whether Sub_Graph_4 removed correctly");          
                  
      -- Subgraph_Is_Not_Part_Of_Project_Exception
      begin
         A_Subgraph := Projects.Get_Subgraph
           (Test_Project_1, "Sub_Graph_1");
         Assert 
           (False,
            "Check Get_Subgraph "
            & "Subgraph_Is_Not_Part_Of_Project_Exception - 1");
      exception
         when Subgraph_Is_Not_Part_Of_Project_Exception =>
           null;                                
      end;
      
      -- Subgraph_Is_Not_Part_Of_Project_Exception
      begin
         A_Subgraph := Projects.Get_Subgraph
           (Test_Project_1, "Sub_Graph_4");
         Assert 
           (False,
            "Check Get_Subgraph "
            & "Subgraph_Is_Not_Part_Of_Project_Exception - 2");
      exception
         when Subgraph_Is_Not_Part_Of_Project_Exception =>
           null;                                
      end;   

      -- redo all changes
      -------------------
      Projects.Change_Vis_Window_Name
        (Test_Project_1,
         "Vis_Window_1_New_Name",
         "Vis_Window_1");
                  
      A_Vis_Window := Giant.Vis_Windows.Create_New ("Vis_Window_2");
        Giant.Projects.Add_Visualisation_Window
          (Test_Project_1, A_Vis_Window);         
                  
      Projects.Change_Subgraph_Name
        (Test_Project_1,
         "Sub_Graph_1_New_Name",
         "Sub_Graph_1");
              
      A_Subgraph := Giant.Graph_Lib.Subgraphs.Create ("Sub_Graph_4");
      Giant.Projects.Add_Subgraph (Test_Project_1, A_Subgraph);        
         

      -- check dir
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 6,
         "Test correct ammount of files in dir_1 - after changing");
      String_Lists.Destroy (Test_List); 
      
      -- test emergency files
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_2.viswin~"),
         "Emergency file on deleting Vis_Window_2 created correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_4.subgraph~"),
         "Emergency file on deleting Sub_Graph_4 created correctly"); 
           
      -- status checks
      Check_Test_Project_1 (Test_Project_1); 
      
      Projects.Store_Whole_Project (Test_Project_1);     
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_1/");
                
      Kill_Files_In_Dir ("resources/test_project_directory/dir_2");      
      Projects.Store_Whole_Project_As_For_File
      (Test_Project_1,
       "resources/test_project_directory/dir_2/Test_Project_1.xml");      
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_2/");
        
      -- check removal of emergency files 
      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_2.viswin~"),
         "Emergency file for Vis_Window_2 removed correctly");
      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_4.subgraph~"),
         "Emergency file for Sub_Graph_4 removed correctly");   
                    
      Projects.Deallocate_Project_Deep (Test_Project_1);            
   end Changing_Test_Only_Memory;   

   ---------------------------------------------------------------------------
   procedure Vis_Window_Persistence_Status_Test
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Project_1 : Giant.Projects.Project_Access;
      Test_List      : String_Lists.List; 
      
      A_Vis_Window   : Giant.Vis_Windows.Visual_Window_Access;
   begin
   
      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");
      Test_Project_1 := Create_Test_Project_1;            
      Check_Test_Project_1 (Test_Project_1);
      
       -- check dir
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 4,
         "Test correct ammount of files in dir_1 - before changing");
      String_Lists.Destroy (Test_List); 
                  
      Assert 
        (Projects.Is_Vis_Window_Memory_Loaded
          (Test_Project_1, "Vis_Window_1"),
         "Check Memory Status of Vis_Window_1 at initialisation");
                      
      Projects.Store_Single_Visualisation_Window
        (Test_Project_1, "Vis_Window_1");
      Projects.Store_Single_Visualisation_Window
        (Test_Project_1, "Vis_Window_2");      
           
      Projects.Free_Memory_For_Vis_Window
        (Test_Project_1, "Vis_Window_1");     
      Projects.Free_Memory_For_Vis_Window
        (Test_Project_1, "Vis_Window_2");        
      Projects.Free_Memory_For_Vis_Window
        (Test_Project_1, "Vis_Window_3");  
      
      Assert
        (Projects.Does_Vis_Window_Exist 
          (Test_Project_1, "Vis_Window_1"),
         "Check Vis_Window_1 still exist status file linked - 1");
      Assert
        (not Projects.Is_Vis_Window_Memory_Loaded 
          (Test_Project_1, "Vis_Window_1"),
         "Check Vis_Window_1 still exist status file linked - 2");        
        
      Assert
        (Projects.Does_Vis_Window_Exist 
          (Test_Project_1, "Vis_Window_2"),
         "Check Vis_Window_2 still exist status file linked - 1");
      Assert
        (not Projects.Is_Vis_Window_Memory_Loaded 
          (Test_Project_1, "Vis_Window_2"),
         "Check Vis_Window_2 still exist status file linked - 2");     

      Assert
        (not Projects.Does_Vis_Window_Exist 
          (Test_Project_1, "Vis_Window_3"),
         "Check Vis_Window_3 removed completely");
 
      Assert
        (Projects.Does_Vis_Window_Exist 
          (Test_Project_1, "Vis_Window_4"),
         "Check Vis_Window_4 still exist status memory loaded - 1");
      Assert
        (Projects.Is_Vis_Window_Memory_Loaded 
          (Test_Project_1, "Vis_Window_4"),
         "Check Vis_Window_4 still exist status memory loaded - 2");     
            
      -- check windows
      Test_List := 
        Projects.Get_All_Visualisation_Window_Names (Test_Project_1);
      Assert (String_Lists.Length (Test_List) = 3,
              "Test Lenght of Vis Window Names list after first changes");
      String_Lists.Destroy (Test_List); 
            
      -- check dir
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 9,
         "Test correct ammount of files in dir_1 - after first changes");
      String_Lists.Destroy (Test_List); 

      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_1.viswin"),
         "Check Management file for Vis_Window_1");            
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_1.viswin~"),
         "Check Emergency file for Vis_Window_1"); 
         
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_2.viswin"),
         "Check Management file for Vis_Window_2");            
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_2.viswin~"),
         "Check Emergency file for Vis_Window_2");          
         
      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_3.viswin"),
         "Check Management file for Vis_Window_3 should not exist");            
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_3.viswin~"),
         "Check Emergency file for Vis_Window_3"); 
                   
      A_Vis_Window := Projects.Get_Visualisation_Window
        (Test_Project_1,
         "Vis_Window_1");
                         
      -- check renaming of file linked vis windows
      Projects.Change_Vis_Window_Name 
        (Test_Project_1,
         "Vis_Window_1",
         "Vis_Window_1_New"); 
         
      Projects.Free_Memory_For_Vis_Window
        (Test_Project_1, "Vis_Window_1_New"); 
         
      -- check correct renaming of file linked window Vis_Window_1
      Assert
        (not Projects.Does_Vis_Window_Exist 
          (Test_Project_1, "Vis_Window_1"),
         "Check Vis_Window_1 file linked renaming - 1");
      Assert
        (not Projects.Is_Vis_Window_Memory_Loaded 
          (Test_Project_1, "Vis_Window_1"),
         "Check Vis_Window_1 file linked renaming - 2");     
      Assert
        (Projects.Does_Vis_Window_Exist 
          (Test_Project_1, "Vis_Window_1_New"),
         "Check Vis_Window_1 file linked renaming - 3");
      Assert
        (not Projects.Is_Vis_Window_Memory_Loaded 
          (Test_Project_1, "Vis_Window_1_New"),
         "Check Vis_Window_1 file linked renaming - 4"); 
                  
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_1.viswin~"),
         "Check Vis_Window_1 file linked renaming - 5");           
      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_1.viswin"),
         "Check Vis_Window_1 file linked renaming - 6");         
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_1_New.viswin"),
         "Check Vis_Window_1 file linked renaming - 7");           
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_1_New.viswin~"),
         "Check Vis_Window_1 file linked renaming - 8");    
                  
      Projects.Remove_Visualisation_Window
        (Test_Project_1, "Vis_Window_1_New"); 
        
      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Vis_Window_1_New.viswin"),
         "Check Vis_Window_1_New file linked - removed - 1"); 
         
      Test_List := 
        Projects.Get_All_Visualisation_Window_Names (Test_Project_1);
      Assert (String_Lists.Length (Test_List) = 2,
              "Check Vis_Window_1_New file linked - removed - 2");                                                       
      String_Lists.Destroy (Test_List);                      
      
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 9,
         "Test correct ammount of files in dir_1 - after changes");
      String_Lists.Destroy (Test_List);
               
      ---
      Projects.Store_Whole_Project (Test_Project_1);
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 10,
         "Test correct ammount of files in dir_1 - after changes and save");
      String_Lists.Destroy (Test_List);
      
      ---
      Kill_Files_In_Dir ("resources/test_project_directory/dir_3");  
      Projects.Store_Whole_Project_As_For_File        
        (Test_Project_1,
         "resources/test_project_directory/dir_3/Test_Project_1.xml");    
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_3",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 10,
         "Test correct ammount of files in dir_3 - after save new");
      String_Lists.Destroy (Test_List); 
                                                      
   end Vis_Window_Persistence_Status_Test;  
         
   ---------------------------------------------------------------------------
   procedure Subgraph_Persistence_Status_Test
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Project_1 : Giant.Projects.Project_Access;
      Test_List      : String_Lists.List; 
      
      A_Subgraph     : Giant.Graph_Lib.Subgraphs.Subgraph;  
      A_Vis_Window   : Giant.Vis_Windows.Visual_Window_Access;
   begin
   
      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");
      Test_Project_1 := Create_Test_Project_1;            
      Check_Test_Project_1 (Test_Project_1);
      
       -- check dir
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 4,
         "Test correct ammount of files in dir_1 - before changing");
      String_Lists.Destroy (Test_List);
      
      Projects.Remove_Subgraph 
        (Test_Project_1, "Sub_Graph_3");     
        
      Projects.Change_Subgraph_Name
        (Test_Project_1, "Sub_Graph_4", "Sub_Graph_4_New"); 
        
      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_4_New.subgraph"),
         "Check Sub_Graph_4_New not yet written"); 
      
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 5,
         "Test correct ammount of files in dir_1 - after first changes");
      String_Lists.Destroy (Test_List);      
      
      -- save
      Projects.Store_Whole_Project (Test_Project_1);
      
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 11,
         "Test correct ammount of files in dir_1 - after first save");
      String_Lists.Destroy (Test_List); 
                                                      
      -- do changes
      Projects.Change_Subgraph_Name 
        (Test_Project_1, "Sub_Graph_1", "Sub_Graph_1_New");
      
      Projects.Remove_Subgraph 
        (Test_Project_1, "Sub_Graph_1_New");      
      
      Projects.Change_Subgraph_Name 
        (Test_Project_1, "Sub_Graph_2", "Sub_Graph_2_New");  
        
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 11,
         "Test correct ammount of files in dir_1 - after first changes");
      String_Lists.Destroy (Test_List);      
            
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_1_New.subgraph~"),
         "Check status after doing changes on saved data - 1");       
      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_1_New.subgraph"),
         "Check status after doing changes on saved data - 2");       
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_2_New.subgraph"),
         "Check status after doing changes on saved data - 3");       
      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_3.subgraph"),
         "Check status after doing changes on saved data - 4"); 
      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_3.subgraph~"),
         "Check status after doing changes on saved data - 5");        
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_4_New.subgraph"),
         "Check status after doing changes on saved data - 6");          
              
      -- second save
      Projects.Store_Whole_Project (Test_Project_1);        
        
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 10,
         "Test correct ammount of files in dir_3 - after second save");
      String_Lists.Destroy (Test_List);   
      
      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_1_New.subgraph~"),
         "Check status after second change - 1");     

      Assert
        (not Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "Sub_Graph_3.subgraph~"),
         "Check status after second change - 1");                
          
   end Subgraph_Persistence_Status_Test;     
   
   ---------------------------------------------------------------------------
   procedure Test_Highlight_Status
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Project_1 : Giant.Projects.Project_Access;
      Test_List      : String_Lists.List; 
      Test_HS        : Projects.Subgraph_Highlight_Status;
   begin
   
      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");
      Test_Project_1 := Create_Test_Project_1;            
      Check_Test_Project_1 (Test_Project_1);
       
      Projects.Change_Highlight_Status
        (Test_Project_1, "Sub_Graph_1", Color_1);         
      Projects.Change_Highlight_Status
        (Test_Project_1, "Sub_Graph_2", Color_1);  

      Assert (Get_Highlight_Status 
         (Test_Project_1, "Sub_Graph_1") = Color_1,
         "Check Highlight Status changed Sub_Graph_1 - 1");
      Assert (Get_Highlight_Status 
         (Test_Project_1, "Sub_Graph_2") = Color_1,
         "Check Highlight Status changed Sub_Graph_2 - 1");  
  
      Projects.Change_Highlight_Status
        (Test_Project_1, "Sub_Graph_1", None);         
         
      Assert (Get_Highlight_Status 
         (Test_Project_1, "Sub_Graph_1") = None,
         "Check Highlight Status changed Sub_Graph_1 - 2");
         
      -- save and store
      Projects.Store_Whole_Project (Test_Project_1);
      
      
      Projects.Store_Whole_Project (Test_Project_1); 
      Projects.Deallocate_Project_Deep (Test_Project_1);  
      Test_Project_1 := Projects.Load_Project_File
        ("resources/test_project_directory/dir_1/Test_Project_1.xml");
    
      Check_Test_Project_1 (Test_Project_1);

      Assert (Get_Highlight_Status 
         (Test_Project_1, "Sub_Graph_1") = None,
         "Check Highlight Status changed Sub_Graph_1 - after reloading");

      Assert (Get_Highlight_Status 
         (Test_Project_1, "Sub_Graph_2") = Color_1,
         "Check Highlight Status changed Sub_Graph_2 - after reloading"); 
      
      
      Projects.Remove_Subgraph (Test_Project_1, "Sub_Graph_1");
      
      -- Subgraph_Is_Not_Part_Of_Project_Exception
      begin
        Projects.Change_Highlight_Status
          (Test_Project_1, "Sub_Graph_1", Color_1);  
        Assert (False,
          "Change_Highlight_Status - " 
          & "Subgraph_Is_Not_Part_Of_Project_Exception");
      exception
         when Subgraph_Is_Not_Part_Of_Project_Exception =>
            null;
      end;
      
      -- Subgraph_Is_Not_Part_Of_Project_Exception
      begin
        Test_HS := Projects.Get_Highlight_Status
          (Test_Project_1, "Sub_Graph_1");  
        Assert (False,
          "Change_Highlight_Status - " 
          & "Subgraph_Is_Not_Part_Of_Project_Exception");
      exception
         when Subgraph_Is_Not_Part_Of_Project_Exception =>
            null;
      end;
      
      Projects.Deallocate_Project_Deep (Test_Project_1);            
   end Test_Highlight_Status;  

   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Giant.Projects - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine 
       (T, Test_Initialize'Access, 
        "Inititialisation");
      Register_Routine 
       (T, Basic_File_Mangement_Test'Access, 
         "Basic_File_Mangement_Test");      
      Register_Routine 
       (T, Changing_Test_Only_Memory'Access, 
         "Changing_Test_Only_Memory"); 
      Register_Routine 
        (T, Vis_Window_Persistence_Status_Test'Access, 
         "Vis_Window_Persistence_Status_Test");         
      Register_Routine 
        (T, Subgraph_Persistence_Status_Test'Access, 
         "Subgraph_Persistence_Status_Test");                     
      Register_Routine 
        (T, Test_Highlight_Status'Access, 
         "Test_Highlight_Status");                       
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin

      Gtk.Main.Init;
      
      Giant.Graph_Lib.Initialize;

      Giant.Graph_Lib.Load
        ("resources/rfg_examp.iml");

      Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
        ("",
         "resources/vis_styles/vis_styles_test_set_1",
         "",
         "resources/vis_styles/vis_styles_test_set_1/"
         & "test_vis_style_1_default.xml");
         
      -- create directories necessary for test (if they do not already exist)
      File_Management.Create_Dir_Path 
        ("resources/test_project_directory/dir_1");
      File_Management.Create_Dir_Path 
        ("resources/test_project_directory/dir_2");                 
      File_Management.Create_Dir_Path 
        ("resources/test_project_directory/dir_3");                      
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
   
      Giant.Graph_Lib.Destroy;
      Giant.Config.Vis_Styles.Clear_Config_Vis_Styles;
   end Tear_Down;

end Giant.Projects.Test;
