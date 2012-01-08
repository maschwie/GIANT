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
--  $RCSfile: giant-main_window.adb,v $, $Revision: 1.72 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with System;

with Gdk.Color;
with Gdk.Event;
with Gdk.Types;
with Gdk.Window;
with Glib; use type Glib.Gint;
with Glib.Object;
with Gtk.Box;
with Gtk.Clist;
pragma Elaborate_All (Gtk.Clist);
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Main;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Object;
with Gtk.Paned;
with Gtk.Status_Bar;
with Gtk.Stock;
with Gtk.Style;
with Gtk.Tooltips;
with Gtk.Widget;
with Gtk.Window;
with Gtk.Tearoff_Menu_Item;
with Gtkada.Types;

with Giant.About_Dialog;
with Giant.Clists;
with Giant.Config;
with Giant.Config.Global_Data;
with Giant.Config_Settings;
with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Default_Logger;
with Giant.Dialogs;
with Giant.File_Management;
with Giant.File_Selection;
with Giant.Generate_Subgraph_Dialog;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Subgraphs;
with Giant.Gsl_Dialog;
with Giant.Gsl;
with Giant.Gsl.Interpreters;
with Giant.Gui_Manager;
with Giant.Gui_Manager.Actions;
with Giant.Gui_Utils;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);
with Giant.Main_Window.Actions;
with Giant.Menu_Factory;
with Giant.Node_Info_Dialog;
with Giant.Projects;
with Giant.Subgraph_Operation_Dialog;

package body Giant.Main_Window is

   package Logger is new Giant.Logger("giant.main_window");

   package Highlight_Menu_Callback is
      new Gtk.Handlers.User_Callback
     (Gtk.Menu_Item.Gtk_Menu_Item_Record,
      Projects.Subgraph_Highlight_Status);

   procedure Update_Subgraph
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String);

   procedure Update_Window
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String);

   --  signal stuff

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => Interfaces.C.Strings.New_String ("can_close_project"),
      2 => Interfaces.C.Strings.New_String ("close_project"));

   --  main window instance
   Window : Main_Window_Access;

   Pane : Gtk.Paned.Gtk_Vpaned;

   Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
   Project_Menu : Gtk.Menu.Gtk_Menu;
   Project_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   Project_New_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   Project_Open_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   Project_Quit_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;

   Help_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;

   Window_List_Menu : Gtk.Menu.Gtk_Menu;
   Window_List : Gui_Utils.String_Clists.Giant_Data_Clist;

   Subgraph_List_Menu : Gtk.Menu.Gtk_Menu;
   Subgraph_List : Gui_Utils.String_Clists.Giant_Data_Clist;

   Status_Bar : Gtk.Status_Bar.Gtk_Status_Bar;
   Graph_Filename_Bar : Gtk.Label.Gtk_Label;

   --  holds multiple status bars
   Status_Box : Gtk.Box.Gtk_Hbox;

   Styles : array (Projects.Subgraph_Highlight_Status) of Gtk.Style.Gtk_Style
     := (others => null);

   Tooltips : Gtk.Tooltips.Gtk_Tooltips;

   --  ugly workaround, see Close_Project
   Can_Close_Project : Boolean;

   ---------------------------------------------------------------------------
   --  Helper Methods
   ---------------------------------------------------------------------------

   function Get_Selected_Subgraph
     return String
   is
   begin
      return Gui_Utils.String_Clists.Get_Selected_Item (Subgraph_List);
   end Get_Selected_Subgraph;

   function Get_Selected_Window
     return String
   is
   begin
      return Gui_Utils.String_Clists.Get_Selected_Item (Window_List);
   end Get_Selected_Window;

   Loaded: Boolean;

   procedure Update_Children
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Gtk.Widget.Set_Sensitive (Widget, Loaded);
   end;

   procedure Set_Project_Loaded
     (Loaded : in Boolean)
   is
--        package Menu_Bar_Forall is new Gtk.Container.Forall_Pkg
--          (Boolean);

   begin
      --  set menu bar sensitive
      Main_Window.Loaded := Loaded;
      Gtk.Menu_Bar.Forall (Menu_Bar, Update_Children'Access);
      Gtk.Menu.Forall (Project_Menu, Update_Children'Access);

      if (Loaded) then
         Set_Title (Window, "GIANT - "
                    & Projects.Get_Project_Name (Controller.Get_Project));
         Set_Graph_Filename
           (File_Management.Get_File (Projects.Get_Graph_Filename
                                      (Controller.Get_Project)));
      else
         Set_Title (Window, "GIANT");
         Set_Graph_Filename ("");

         --  clear lists
         Gui_Utils.String_Clists.Clear (Subgraph_List);
         Gui_Utils.String_Clists.Clear (Window_List);

         --  active a few items that can be selected when no project
         --  is loaded
         Gtk.Menu_Item.Set_Sensitive (Project_Menu_Item, True);
         Gtk.Menu_Item.Set_Sensitive (Project_New_Menu_Item, True);
         Gtk.Menu_Item.Set_Sensitive (Project_Open_Menu_Item, True);
         Gtk.Menu_Item.Set_Sensitive (Project_Quit_Menu_Item, True);

         Gtk.Menu_Item.Set_Sensitive (Help_Menu_Item, True);
      end if;

      Gui_Utils.String_Clists.Set_Sensitive (Subgraph_List, Loaded);
      Gui_Utils.String_Clists.Set_Sensitive (Window_List, Loaded);
   end Set_Project_Loaded;


   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   function On_Delete
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Boolean
   is
      Closed : Boolean;
   begin
      Closed := Controller.Hide_Gui;
      return True;
   end On_Delete;

   ---------------------------------------------------------------------------
   --  Project Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Project_New
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      declare
         Project_Filename : String := Giant.File_Selection.File_Selection_Dialog
           (Title       => -"Enter New Project File",
            Default_Dir => "",
            Dir_Only    => False,
            Must_Exist  => False);
      begin
         if (Project_Filename /= "") then
            if (Projects.Does_Project_Exist_File (Project_Filename)) then
               --  check this not to avoid graph file selection
               Dialogs.Show_Error_Dialog (-"The project could not be created. The directory already contains a project.");
               return;
            end if;

            declare
               Graph_Filename : String
                 := Giant.File_Selection.File_Selection_Dialog
                 (Title       => -"Select IML File",
                  Default_Dir => Project_Filename & ".iml",
                  Dir_Only    => False,
                  Must_Exist  => False);
            begin
               if (Graph_Filename /= "") then
                  begin
                     Controller.Create_Project
                       (Project_Filename, Graph_Filename);
                  exception
                    when E: others =>
                       Controller.Handle_Project_Exception (E, Graph_Filename);
                  end;
               end if;
            end;
         end if;
      end;
   end On_Project_New;

   procedure On_Project_Open
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      declare
         Filename : String := Giant.File_Selection.File_Selection_Dialog
           (Title       => -"Open Project",
            Default_Dir => "",
            Dir_Only    => False,
            Must_Exist  => True);
      begin
         if (Filename /= "") then
            begin
               Controller.Open_Project (Filename);
            exception
              when E: others =>
                 Controller.Handle_Project_Exception (E, Filename);
            end;
         end if;
      end;
   end On_Project_Open;

   procedure On_Project_Close
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Closed : Boolean;
   begin
      Closed := Controller.Close_Project;
   end On_Project_Close;

   procedure On_Project_Save
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Save_Project;
   end On_Project_Save;

   procedure On_Project_Save_As
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      declare
         Filename : String := Giant.File_Selection.File_Selection_Dialog
           (-"Save Project",
            Projects.Get_Project_File_Name (Controller.Get_Project),
            Dir_Only => False, Must_Exist => False);
      begin
         if (Filename /= "") then
            begin
               Controller.Save_Project (Filename);
            exception
              when E: others =>
                 Controller.Handle_Project_Exception (E, Filename);
            end;
         end if;
      end;
   end On_Project_Save_As;

   procedure On_Project_Info
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Dialog: Node_Info_Dialog.Node_Info_Dialog_Access;
   begin
      Logger.Info ("Nodes : " & Natural'Image (Graph_Lib.Get_Node_Count)
                   & ", Edges : " & Natural'Image (Graph_Lib.Get_Edge_Count));
      Node_Info_Dialog.Create (Dialog);
      Node_Info_Dialog.Set_Node (Dialog, Graph_Lib.Get_Root_Node);
      Node_Info_Dialog.Show_All (Dialog);
   end On_Project_Info;

   procedure On_Project_Quit
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Closed : Boolean;
   begin
      Closed := Controller.Hide_Gui;
   end On_Project_Quit;

   ---------------------------------------------------------------------------
   --  Tools Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Tools_Execute_GSL_Script
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Dialog : Gsl_Dialog.Gsl_Dialog_Access;
   begin
      Gsl_Dialog.Create (Dialog);
      Gsl_Dialog.Show_All (Dialog);
   end On_Tools_Execute_GSL_Script;

   ---------------------------------------------------------------------------
   --  Window Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Window_New
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Create_Window (Controller.Get_Unique_Name);
   end On_Window_New;

   ---------------------------------------------------------------------------
   --  Scripts Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event)
   is
      Params : Gsl.Interpreters.Gsl_Params
        := Gsl.Interpreters.Create_Parameter_List;
   begin
      Controller.Execute_GSL
        (Script_Name => Event.Label,
         Context     => "",
         Parameter   => Params);
   end On_Script;

   ---------------------------------------------------------------------------
   --  Help Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Help_About
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      About_Dialog.Show;
   end On_Help_About;

   ---------------------------------------------------------------------------
   --  Window Context Menu Callbacks
   ---------------------------------------------------------------------------
   function Validate_Window_Name
     (Name   : in String;
      Widget : in Gtk.Object.Gtk_Object)
      return Boolean
   is
   begin
      if (Projects.Does_Vis_Window_Exist (Controller.Get_Project, Name)) then
         Dialogs.Show_Error_Dialog
           (-"A window with this name already exists.");
         return False;
      end if;
      return True;
   end Validate_Window_Name;


   procedure On_Window_List_Open
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Open_Window (Get_Selected_Window);
   end On_Window_List_Open;

   procedure On_Window_List_Close
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Closed : Boolean;
   begin
      Closed := Controller.Close_Window (Get_Selected_Window);
   end On_Window_List_Close;

   procedure On_Window_List_Save
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Save_Window (Get_Selected_Window);
   end On_Window_List_Save;

   procedure On_Window_List_Rename
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is

      Old_Name : String := Get_Selected_Window;
   begin
      declare
         New_Name : constant String
           := Dialogs.Show_Input_Dialog
           (-"New name", -"Rename Window",
            Old_Name, Validate_Window_Name'Access);
      begin

         if (New_Name /= "" and then New_Name /= Old_Name) then
            Controller.Rename_Window (Old_Name, New_Name);
         end if;
      end;
   end On_Window_List_Rename;

   procedure On_Window_List_Delete
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Removed : Boolean;
   begin
      Removed := Controller.Remove_Window (Get_Selected_Window);
   end On_Window_List_Delete;

   ---------------------------------------------------------------------------
   --  Subgraph Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Subgraph_New
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Create_Subgraph (Controller.Get_Unique_Name);
   end On_Subgraph_New;

   procedure On_Subgraph_Set_Operation
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Dialog : Subgraph_Operation_Dialog.Subgraph_Operation_Dialog_Access;
   begin
      Subgraph_Operation_Dialog.Create (Dialog);
      Subgraph_Operation_Dialog.Show_All (Dialog);
   end On_Subgraph_Set_Operation;

   ---------------------------------------------------------------------------
   --  Subgraph Context Menu Callbacks
   ---------------------------------------------------------------------------

   function Validate_Subgraph_Name
     (Name   : in String;
      Widget : in Gtk.Object.Gtk_Object)
      return Boolean
   is
   begin
      if (Projects.Does_Subgraph_Exist (Controller.Get_Project, Name)) then
         Dialogs.Show_Error_Dialog
           (-"A subgraph with this name already exists.");
         return False;
      end if;
      return True;
   end Validate_Subgraph_Name;

   procedure On_Subgraph_Generate_New_Subgraph
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Generate_Subgraph_Dialog.Show
        (Controller.Get_Subgraph (Get_Selected_Subgraph));
   end On_Subgraph_Generate_New_Subgraph;

   procedure On_Subgraph_List_Highlight
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Status : in Projects.Subgraph_Highlight_Status)
   is
   begin
      Controller.Highlight_Subgraph (Get_Selected_Subgraph, Status);
   end On_Subgraph_List_Highlight;

   procedure On_Subgraph_List_Create_Selection
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Action : Actions.Create_Selection_Action_Access;
   begin
      Action := Actions.Create (Get_Selected_Subgraph);
      Gui_Manager.Actions.Set_Global_Action (Action);
   end On_Subgraph_List_Create_Selection;

   procedure On_Subgraph_List_Duplicate
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
     Source_Name : String := Get_Selected_Subgraph;
   begin
      declare
         Target_Name : constant String
           := Dialogs.Show_Input_Dialog
           (-"Target name", -"Duplicate Subgraph",
            Source_Name, Validate_Subgraph_Name'Access);
      begin
         if (Target_Name /= "" and then Target_Name /= Source_Name) then
            Controller.Duplicate_Subgraph (Source_Name, Target_Name);
         end if;
      end;
   end On_Subgraph_List_Duplicate;

   procedure On_Subgraph_List_Delete
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Removed : Boolean;
   begin
      Removed := Controller.Remove_Subgraph (Get_Selected_Subgraph);
   end On_Subgraph_List_Delete;

   procedure On_Subgraph_List_Rename
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Old_Name : String := Get_Selected_Subgraph;
   begin
      declare
         New_Name : constant String
           := Dialogs.Show_Input_Dialog
           (-"New name", -"Rename Subgraph",
            Old_Name, Validate_Subgraph_Name'Access);
      begin

         if (New_Name /= "" and then New_Name /= Old_Name) then
            Controller.Rename_Subgraph (Old_Name, New_Name);
         end if;
      end;
   end On_Subgraph_List_Rename;

   procedure On_Subgraph_List_Execute_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event)
   is
      Params : Gsl.Interpreters.Gsl_Params
        := Gsl.Interpreters.Create_Parameter_List;
   begin
      Gsl.Interpreters.Add_Parameter (Params, Get_Selected_Subgraph);

      Controller.Execute_GSL
        (Script_Name => Event.Label,
         Context     => "",
         Parameter   => Params);
   end On_Subgraph_List_Execute_Script;

   ---------------------------------------------------------------------------
   --  Status Bar Callbacks
   ---------------------------------------------------------------------------

   procedure On_Log_Message
     (Level   : in Default_Logger.Level_Type;
      Name    : in String;
      Message : in String)
   is
      use type Default_Logger.Level_Type;
   begin
      if (Level = Default_Logger.Level_Info) then
         Set_Status (Message);
      end if;
   end;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Initialize_Styles
   is

      function Initialize_Style
        (Config_Id : in Config.Global_Data.Subgraph_High_Light_ID)
         return Gtk.Style.Gtk_Style
      is
         Style : Gtk.Style.Gtk_Style;
         Color_Access : Config.Color_Access;
         Color : Gdk.Color.Gdk_Color;
      begin
         Style := Gtk.Style.Copy
           (Gui_Utils.String_Clists.Get_Style (Subgraph_List));

         Color_Access
           := Config.Global_Data.Get_Subgraph_Highlight_Color (Config_Id);
         Color := Gdk.Color.Parse (Config.Get_Color_Value (Color_Access));

         Gtk.Style.Set_Foreground (Style, State_Normal, Color);
         Gtk.Style.Set_Foreground (Style, State_Selected, Color);
         return Style;
      exception
         when Gdk.Color.Wrong_Color =>
            Logger.Error ("Could not be parse color. Using default instead.");
            return Style;
      end;
   begin
      Styles (Projects.None) := null;
      Styles (Projects.Color_1)
        := Initialize_Style (Config.Global_Data.Color_1);
      Styles (Projects.Color_2)
        := Initialize_Style (Config.Global_Data.Color_2);
      Styles (Projects.Color_3)
        := Initialize_Style (Config.Global_Data.Color_3);
   end Initialize_Styles;

   function Initialize_Menu
     return Gtk.Menu_Bar.Gtk_Menu_Bar is
      use Giant.Gui_Utils;

      Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Menu : Gtk.Menu.Gtk_Menu;
      Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Gtk.Menu_Bar.Gtk_New (Menu_Bar);

      --  project menu
      Gtk.Menu_item.Gtk_New (Item, -"Project");
      Gtk.Menu_Bar.Add (Menu_Bar, Item);
      Project_Menu_Item := Item;

      Gtk.Menu.Gtk_New (Menu);
      Gtk.Menu_Item.Set_Submenu (Item, Menu);
      Project_Menu := Menu;

      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Project_New_Menu_Item
        := New_Stock_Menu_Item (Gtk.Stock.Stock_New, On_Project_New'Access);
      Gtk.Menu.Add (Menu, Project_New_Menu_Item);
      Project_Open_Menu_Item
        := New_Stock_Menu_Item (Gtk.Stock.Stock_Open, On_Project_Open'Access);
      Gtk.Menu.Add (Menu, Project_Open_Menu_Item);
      Gtk.Menu.Add (Menu, New_Stock_Menu_Item (Gtk.Stock.Stock_Close,
                                               On_Project_Close'Access));
      Gtk.Menu.Add (Menu, New_Menu_Separator);
      Gtk.Menu.Add (Menu, New_Stock_Menu_Item (Gtk.Stock.Stock_Save,
                                               On_Project_Save'Access));
      Gtk.Menu.Add (Menu, New_Stock_Menu_Item (Gtk.Stock.Stock_Save_As,
                                               On_Project_Save_As'Access));
      Gtk.Menu.Add (Menu, New_Menu_Separator);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Info...",
                                         On_Project_Info'Access));
      Gtk.Menu.Add (Menu, New_Menu_Separator);
      Project_Quit_Menu_Item
        := New_Stock_Menu_Item (Gtk.Stock.Stock_Quit, On_Project_Quit'Access);
      Gtk.Menu.Add (Menu, Project_Quit_Menu_Item);

      --  tools menu
      Menu := New_Sub_Menu (Menu_Bar, -"Tools");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"GSL Editor...",
                                         On_Tools_Execute_GSL_Script'Access));

      --  window menu
      Menu := New_Sub_Menu (Menu_Bar, -"Window");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"New", On_Window_New'Access));

      --  subgraph menu
--        Menu := New_Sub_Menu (Menu_Bar, -"Subgraph");
--        Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
--        Gtk.Menu.Add (Menu, New_Menu_Item (-"New", On_Subgraph_New'Access));
--        Gtk.Menu.Add (Menu, New_Menu_Separator);
--        Gtk.Menu.Add (Menu, New_Menu_Item (-"Set Operation...",
--                                           On_Subgraph_Set_Operation'Access));

      --  scripts menu
      Menu := New_Sub_Menu (Menu_Bar, -"Scripts");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Giant.Menu_Factory.Generate
        (Labels    => Config_Settings.Get_Setting_As_String ("GSL.No_Param"),
         Separator => File_Management.Path_Separator,
         Menu      => Menu,
         Callback  => On_Script'Access,
         Widget    => Window);

      --  help menu
      Gtk.Menu_item.Gtk_New (Item, -"Help");
      Gtk.Menu_Bar.Add (Menu_Bar, Item);
      Help_Menu_Item := Item;

      Gtk.Menu.Gtk_New (Menu);
      Gtk.Menu_Item.Set_Submenu (Item, Menu);

      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item ("About", On_Help_About'Access));

      return Menu_Bar;
   end Initialize_Menu;

   function New_Highlight_Menu_Item
     (Label    : in String;
      Status   : in Projects.Subgraph_Highlight_Status)
      return Gtk.Menu_Item.Gtk_Menu_Item
   is
      Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Gtk.Menu_Item.Gtk_New (Item, Label);
      Highlight_Menu_Callback.Connect
        (Item, "activate",
         Highlight_Menu_Callback.To_Marshaller
         (On_Subgraph_List_Highlight'Access),
         Status);
      return Item;
   end New_Highlight_Menu_Item;

   procedure Initialize is
      use Giant.Gui_Utils;

      Box : Gtk.Box.Gtk_Vbox;
      Submenu : Gtk.Menu.Gtk_Menu;
   begin
      Gtk.Window.Initialize (Window, Window_Toplevel);
      Set_Title (Window, -"GIANT");

      --  provide signals
      Glib.Object.Initialize_Class_Record
        (Object       => Window,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "Main_Window");

      --  tooltips
      Gtk.Tooltips.Gtk_New (Tooltips);
      Gtk.Tooltips.Enable (Tooltips);

      --  center box
      Gtk.Box.Gtk_New_Vbox (Box);
      Add (Window, Box);

      --  menu bar
      Menu_Bar := Initialize_Menu;
      Gtk.Box.Pack_Start (Box, Menu_Bar, Expand => False, Fill => True,
                          Padding => 0);

      --  split pane
      Pane := New_Vpaned;
      Gtk.Box.Add (Box, Pane);

      --  window list popup menu
      Gtk.Menu.Gtk_New (Window_List_Menu);
      Gtk.Menu.Append (Window_List_Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Open", On_Window_List_Open'Access));
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Close", On_Window_List_Close'Access));
      Gtk.Menu.Append (Window_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Save", On_Window_List_Save'Access));
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Rename...",
                                      On_Window_List_Rename'Access));
      Gtk.Menu.Append (Window_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Delete",
                                      On_Window_List_Delete'Access));

      --  window list
      String_Clists.Create (Window_List, 2, Update_Window'Access);
      String_Clists.Connect_Popup_Menu (Window_List, Window_List_Menu);

      String_Clists.Set_Column_Title (Window_List, 0, -"Window");
      String_Clists.Set_Column_Title (Window_List, 1, -"Status");

      Gtk.Paned.Add (Pane, Add_Scrollbars (Window_List));

      --  sub graph list popup menu
      Gtk.Menu.Gtk_New (Subgraph_List_Menu);
      Gtk.Menu.Append (Subgraph_List_Menu, New_TearOff_Menu_Item);
      Submenu := New_Sub_Menu (Subgraph_List_Menu, -"Highlight");
      Gtk.Menu.Append (Submenu,
                       New_Highlight_Menu_Item
                       (Gui_Utils.To_Display_Name (Projects.Color_1),
                        Projects.Color_1));
      Gtk.Menu.Append (Submenu,
                       New_Highlight_Menu_Item
                       (Gui_Utils.To_Display_Name (Projects.Color_2),
                        Projects.Color_2));
      Gtk.Menu.Append (Submenu,
                       New_Highlight_Menu_Item
                       (Gui_Utils.To_Display_Name (Projects.Color_3),
                        Projects.Color_3));
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Highlight_Menu_Item
                       (-"Unhighlight In All Windows", Projects.None));
      Gtk.Menu.Append (Subgraph_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Set Operation...",
                                      On_Subgraph_Set_Operation'Access));
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Generate new subgraph...",
                                      On_Subgraph_Generate_New_Subgraph'Access));
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Insert As Selection...",
                                      On_Subgraph_List_Create_Selection'Access));
      Gtk.Menu.Append (Subgraph_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Rename...",
                                      On_Subgraph_List_Rename'Access));
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Duplicate...",
                                      On_Subgraph_List_Duplicate'Access));
      Gtk.Menu.Append (Subgraph_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Delete",
                                      On_Subgraph_List_Delete'Access));
      Gtk.Menu.Append (Subgraph_List_Menu, New_Menu_Separator);
      Submenu := New_Sub_Menu (Subgraph_List_Menu, -"Scripts");
      Giant.Menu_Factory.Generate
        (Labels
         => Config_Settings.Get_Setting_As_String ("GSL.Subgraph_Param"),
         Separator => File_Management.Path_Separator,
         Menu      => Submenu,
         Callback  => On_Subgraph_List_Execute_Script'Access,
         Widget    => Window);

      --  sub graph list
      String_Clists.Create (Subgraph_List, 4, Update_Subgraph'Access);
      String_Clists.Set_Show_Titles (Subgraph_List, True);
      String_Clists.Connect_Popup_Menu (Subgraph_List, Subgraph_List_Menu);

      String_Clists.Set_Column_Title (Subgraph_List, 0, -"Subgraph");
      String_Clists.Set_Column_Title (Subgraph_List, 1, -"Nodes");
      String_Clists.Set_Column_Title (Subgraph_List, 2, -"Edges");
      String_Clists.Set_Column_Title (Subgraph_List, 3, -"Highlight Color");

      Gtk.Paned.Add (Pane, Add_Scrollbars (Subgraph_List));

      --  status bar
      Gtk.Box.Gtk_New_Hbox (Status_Box);
      Gtk.Box.Set_Homogeneous (Status_Box, False);
      Gtk.Box.Pack_End (Box, Status_Box, Expand => False, Fill => True,
                        Padding => 0);

      Gtk.Status_Bar.Gtk_New (Status_Bar);
      Gtk.Status_Bar.Set_Has_Resize_Grip (Status_Bar, False);
      Gtk.Box.Pack_Start (Status_Box, Status_Bar, Expand => True,
                          Fill => True, Padding => 0);
      Default_Logger.Set_Listener (On_Log_Message'Access);

      Gtk.Label.Gtk_New (Graph_Filename_Bar);
      Gtk.Box.Pack_Start (Status_Box, Graph_Filename_Bar,
                          Expand => False, Fill => True, Padding => 0);

      --  connect close button
      Widget_Boolean_Callback.Connect
        (Window, "delete_event",
         Widget_Boolean_Callback.To_Marshaller (On_Delete'Access));
   end Initialize;

   ---------------------------------------------------------------------------
   --  Public Methods
   ---------------------------------------------------------------------------

   procedure Update_Window
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
   begin
      Gui_Utils.String_Clists.Set_Text (List, Row, 0, Name);
      if (Gui_Manager.Is_Window_Open (Name)) then
         Gui_Utils.String_Clists.Set_Text (List, Row, 1, -"Open");
      elsif (Projects.Is_Vis_Window_Memory_Loaded
             (Controller.Get_Project, Name)) then
         Gui_Utils.String_Clists.Set_Text (List, Row, 1, -"Loaded");
      else
         Gui_Utils.String_Clists.Set_Text (List, Row, 1, -"");
      end if;
   end Update_Window;

   procedure Add_Window
     (Name : in String)
   is
   begin
      Gui_Utils.String_Clists.Add (Window_List, Name);
   end Add_Window;

   procedure Update_Window
     (Name : in String)
   is
   begin
      Gui_Utils.String_Clists.Update (Window_List, Name);
   end Update_Window;

   procedure Remove_Window
     (Name : in String)
   is
   begin
      Gui_Utils.String_Clists.Remove (Window_List, Name);
   end Remove_Window;

   ---------------------------------------------------------------------------
   --  Subgraph Methods
   ---------------------------------------------------------------------------

   procedure Update_Subgraph
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
      use type Gtk.Style.Gtk_Style;
      use type Projects.Subgraph_Highlight_Status;

      Subgraph : Graph_Lib.Subgraphs.Subgraph := Controller.Get_Subgraph (Name);
      Highlight_Status : Projects.Subgraph_Highlight_Status
        := Projects.Get_Highlight_Status (Controller.Get_Project, Name);
   begin
      if (Styles (Projects.Color_1) = null) then
         Initialize_Styles;
      end if;

      Gui_Utils.String_Clists.Set_Text (List, Row, 0, Name);
      Gui_Utils.String_Clists.Set_Text
        (List, Row, 1,
         Natural'Image (Graph_Lib.Subgraphs.Get_Node_Count (Subgraph)));
      Gui_Utils.String_Clists.Set_Text
        (List, Row, 2,
         Natural'Image (Graph_Lib.Subgraphs.Get_Edge_Count (Subgraph)));

      Gui_Utils.String_Clists.Set_Cell_Style
        (List, Row, 3, Styles (Highlight_Status));
      Gui_Utils.String_Clists.Set_Text
        (List, Row, 3, Gui_Utils.To_Display_Name (Highlight_Status));
   end Update_Subgraph;

   procedure Add_Subgraph
     (Name : in String)
   is
   begin
      Gui_Utils.String_Clists.Add (Subgraph_List, Name);
   end Add_Subgraph;

   procedure Update_Subgraph
     (Name : in String)
   is
   begin
      Gui_Utils.String_Clists.Update (Subgraph_List, Name);
   end Update_Subgraph;

   procedure Remove_Subgraph
     (Name : in String)
   is
   begin
      Gui_Utils.String_Clists.Remove (Subgraph_List, Name);
   end Remove_Subgraph;

   ---------------------------------------------------------------------------
   --  Public Methods
   ---------------------------------------------------------------------------

   procedure Cancel_Close_Project
   is
   begin
      Can_Close_Project := False;
   end Cancel_Close_Project;

   function Close_Project
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
      use type Default_Dialog.Response_Type;

      Response : Default_Dialog.Response_Type;
   begin
      if (Ask_For_Confirmation) then
         Can_Close_Project := True;
         --  notify all open dialogs
         Gui_Utils.Widget_Callback.Emit_By_Name
           (Window, "can_close_project");
         if (not Can_Close_Project) then
            --  a dialog or window could not be closed
            return False;
         end if;

         Response := Dialogs.Show_Confirmation_Dialog
           (-"The project has changed. Save changes?",
            Default_Dialog.Button_Yes_No_Cancel);
         if (Response = Default_Dialog.Response_Yes) then
            Controller.Save_Project;
         elsif (Response = Default_Dialog.Response_Cancel) then
            return False;
         end if;
      end if;

      --  notify all open dialogs
      Gui_Utils.Widget_Callback.Emit_By_Name (Window, "close_project");

      --  disable widgets
      Set_Project_Loaded (False);

      return True;
   end Close_Project;

   procedure Connect_Can_Close_Project
     (Callback : in     Gui_Utils.Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget   : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Gui_Utils.Widget_Callback.Object_Connect
        (Window, "can_close_project",
         Gui_Utils.Widget_Callback.To_Marshaller (Callback),
         Widget);
   end Connect_Can_Close_Project;

   procedure Connect_Close_Project
     (Callback : in     Gui_Utils.Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget   : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Gui_Utils.Widget_Callback.Object_Connect
        (Window, "close_project",
         Gui_Utils.Widget_Callback.To_Marshaller (Callback),
         Widget);
   end Connect_Close_Project;

   function Hide
     (Ask_For_Confirmation: Boolean)
     return Boolean
   is
   begin
      if (Ask_For_Confirmation
          and then Controller.Is_Project_Loaded
          and then Controller.Has_Project_Changed) then
         if (not Close_Project (Ask_For_Confirmation)) then
            --  the user has cancelled
            return False;
         end if;
      end if;

      --  save settings
      Config_Settings.Set_Setting
        ("Main_Window.Width",
         Integer (Get_Allocation_Width (Window)));
      Config_Settings.Set_Setting
        ("Main_Window.Height",
         Integer (Get_Allocation_Height (Window)));
      Config_Settings.Set_Setting
        ("Main_Window.Separator",
         Integer (Gui_Utils.String_Clists.Get_Allocation_Height (Window_List)));

      return True;
   end Hide;

   procedure Initialize_Project
   is
   begin
      Set_Project_Loaded (True);
   end Initialize_Project;

   procedure Show
   is
   begin
      Window := new Main_Window_Record;
      Initialize;

      --  restore size
      Set_Default_Size
        (Window,
         Glib.Gint (Config_Settings.Get_Setting_As_Integer
                    ("Main_Window.Width")),
         Glib.Gint (Config_Settings.Get_Setting_As_Integer
                    ("Main_Window.Height")));
      Gtk.Paned.Set_Position
        (Pane,
         Glib.Gint (Config_Settings.Get_Setting_As_Integer
                    ("Main_Window.Separator")));

      --  disable all widgets initially
      Set_Project_Loaded (False);

      Show_All (Window);
      Gui_Utils.Set_Icon (Window, "giant.xpm");
   end Show;

   procedure Set_Status
     (Text : in String)
   is
      Id : Gtk.Status_Bar.Message_Id;
   begin
      Gtk.Status_Bar.Pop (Status_Bar, 1);
      Id := Gtk.Status_Bar.Push (Status_Bar, 1, " " & Text & " ");
   end Set_Status;

   procedure Set_Graph_Filename
     (Text : in String)
   is
   begin
      Gtk.Label.Set_Text (Graph_Filename_Bar, " " & Text & " ");
   end Set_Graph_Filename;

   procedure Update_Column_Sizes
   is
   begin
      Gui_Utils.String_Clists.Columns_Autosize (Window_List);
      Gui_Utils.String_Clists.Columns_Autosize (Subgraph_List);
   end Update_Column_Sizes;

end Giant.Main_Window;
