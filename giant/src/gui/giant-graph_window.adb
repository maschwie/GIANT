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
--  $RCSfile: giant-graph_window.adb,v $, $Revision: 1.67 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Gdk.Color;
with Glib;
with Gtk.Arguments;
with Gtk.Box;
with Gtk.Button;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.List;
with Gtk.List_Item;
with Gtk.Menu_Item;
with Gtk.Stock;
with Gtk.Widget;

with Giant.Clists;
with Giant.Config;
with Giant.Config_Settings;
with Giant.Config.Global_Data;
with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Dialogs;
with Giant.File_Management;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Widgets.Handlers;
with Giant.Gui_Manager;
with Giant.Gui_Manager.Actions;
with Giant.Gui_Utils;
with Giant.Input_Dialog;
pragma Elaborate (Giant.Input_Dialog);
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);
with Giant.Main_Window;
with Giant.Menu_Factory;

with Giant.Graph_Window.Callbacks; use Giant.Graph_Window.Callbacks;

package body Giant.Graph_Window is

   package Logger is new Giant.Logger("giant.graph_window");

   package Graph_Window_Input_Dialog is
     new Input_Dialog (Graph_Window_Access);

   procedure Update_Pin
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String);

   procedure Update_Selection
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String);

   ---------------------------------------------------------------------------
   --  Marshaller Packages
   ---------------------------------------------------------------------------

   package Action_Mode_Marshallers is new
     Gui_Utils.Widget_Callback.Marshallers.Generic_Marshaller
     (Base_Type   => Graph_Widgets.Handlers.Button_Press_Action,
      Conversion  => Graph_Widgets.Handlers.To_Button_Press_Action);

   package Selection_Change_Marshallers is new
     Gui_Utils.Widget_Callback.Marshallers.Generic_Marshaller
     (Base_Type   => Graph_Widgets.Handlers.Selection_Change_Action,
      Conversion  => Graph_Widgets.Handlers.To_Selection_Change_Action);


   ---------------------------------------------------------------------------
   --  Package: Actions
   ---------------------------------------------------------------------------

   package body Actions is

      procedure Free is new Ada.Unchecked_Deallocation
        (Graph_Window_Action_Type'Class, Graph_Window_Action_Access);

      procedure Destroy
        (Action : access Graph_Window_Action_Type)
      is
         P : Graph_Window_Action_Access := Graph_Window_Action_Access (Action);
      begin
         Free (P);
      end Destroy;

   end Actions;

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   function Get_Selected_Selection
     (Window : access Graph_Window_Record)
     return String
   is
   begin
      return Gui_Utils.String_Clists.Get_Selected_Item (Window.Selection_List);
   end Get_Selected_Selection;

   function Get_Selected_Pin
     (Window : access Graph_Window_Record)
     return String
   is
   begin
      return Gui_Utils.String_Clists.Get_Selected_Item (Window.Pin_List);
   end Get_Selected_Pin;

   function Get_Window_Name
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
     return String
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      return Vis_Windows.Get_Name (Window.Visual_Window);
   end Get_Window_Name;

   ---------------------------------------------------------------------------
   --  Returns:
   --    False, if user cancelled
   function Save_Changes
     (Window : access Graph_Window_Record'Class)
     return Boolean
   is
      use type Default_Dialog.Response_Type;

      Response : Default_Dialog.Response_Type;
   begin
      if (Window.Is_Modified) then
         Response := Dialogs.Show_Confirmation_Dialog
           (-"The content has changed. Save changes?",
            Default_Dialog.Button_Yes_No_Cancel);
         if (Response = Default_Dialog.Response_Yes) then
            Controller.Save_Window (Get_Window_Name (Window));
         elsif (Response = Default_Dialog.Response_Cancel) then
            return False;
         end if;
     end if;
     return True;
   end Save_Changes;

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   procedure On_Can_Close_Project
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      if (not Save_Changes (Window)) then
         Main_Window.Cancel_Close_Project;
      end if;
   end On_Can_Close_Project;

   procedure On_Close_Project
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Closed : Boolean;
   begin
      Closed := Controller.Close_Window
        (Get_Window_Name (Gtk.Widget.Get_Toplevel (Source)),
         Ask_For_Confirmation => False);
   end On_Close_Project;

   function On_Close
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Boolean
   is
      Closed : Boolean;
   begin
      Closed := Controller.Close_Window
        (Get_Window_Name (Gtk.Widget.Get_Toplevel (Source)),
         Ask_For_Confirmation => True);
      return not Closed;
   end On_Close;

   procedure On_Pick_Edge_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Pick_Edge_Clicked;

   ---------------------------------------------------------------------------
   --  Pin Menu Callbacks
   ---------------------------------------------------------------------------

   function Validate_Pin_Name
     (Name   : in String;
      Window : in Graph_Window_Access)
      return Boolean
   is
   begin
      if (Vis_Windows.Does_Pin_Exist (Window.Visual_Window, Name)) then
         Dialogs.Show_Error_Dialog (-"A pin with this name already exists.");
         return False;
      end if;
      return True;
   end Validate_Pin_Name;

   procedure On_Pin_List_Delete
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : constant Graph_Window_Access := Graph_Window_Access (Source);
      Removed : Boolean;
   begin
      Removed := Controller.Remove_Pin
        (Get_Window_Name (Source), Get_Selected_Pin (Window));
   end On_Pin_List_Delete;

   procedure On_Pin_List_Rename
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : constant Graph_Window_Access := Graph_Window_Access (Source);
   begin
      declare
         Old_Name : constant String := Get_Selected_Pin (Window);
         New_Name : constant String
           := Graph_Window_Input_Dialog.Show
           (-"New name", -"Rename Pin",
            Old_Name, Validate_Pin_Name'Access, Window);
      begin

         if (New_Name /= "" and then New_Name /= Old_Name) then
            Controller.Rename_Pin (Get_Window_Name (Source),
                                   Old_Name, New_Name);
         end if;
      end;
   end On_Pin_List_Rename;

   procedure On_Pin_List_Show
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : constant Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Controller.Show_Pin (Get_Window_Name (Window), Get_Selected_Pin (Window));
   end On_Pin_List_Show;

   ---------------------------------------------------------------------------
   --  Selection Menu Callbacks
   ---------------------------------------------------------------------------

   function Validate_Selection_Name
     (Name   : in String;
      Window : in Graph_Window_Access)
      return Boolean
   is
   begin
      if (Vis_Windows.Does_Selection_Exist
          (Window.Visual_Window, Name)) then
         Dialogs.Show_Error_Dialog
           (-"A selection with this name already exists.");
         return False;
      end if;
      return True;
   end Validate_Selection_Name;

   procedure On_Selection_List_Create_Subgraph
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Selection_Name : String := Get_Selected_Selection (Window);
      Subgraph_Name : String
        := Controller.Get_Unique_Name
        (Get_Window_Name (Window) & "_" & Get_Selected_Selection (Window));
   begin
      Controller.Create_Subgraph_From_Selection
        (Window_Name    => Get_Window_Name (Window),
         Selection_Name => Get_Selected_Selection (Window),
         Subgraph_Name  => Subgraph_Name);
   end On_Selection_List_Create_Subgraph;

   procedure On_Selection_List_Delete
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : constant Graph_Window_Access := Graph_Window_Access (Source);
      Removed : Boolean;
   begin
      Removed := Controller.Remove_Selection
        (Get_Window_Name (Source), Get_Selected_Selection (Window),
         Remove_Content => False, Ask_For_Confirmation => True);
   exception
      when Vis_Windows.Standard_Selection_May_Not_Be_Removed_Exception =>
         Dialogs.Show_Error_Dialog ("The default selection may not be removed.");
   end On_Selection_List_Delete;

   procedure On_Selection_List_Delete_With_Content
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : constant Graph_Window_Access := Graph_Window_Access (Source);
      Removed : Boolean;
   begin
      Removed := Controller.Remove_Selection
        (Get_Window_Name (Source), Get_Selected_Selection (Window),
         Remove_Content => True, Ask_For_Confirmation => True);
   exception
      when Vis_Windows.Standard_Selection_May_Not_Be_Removed_Exception =>
         Dialogs.Show_Error_Dialog ("The default or active selection can not be removed.");
   end On_Selection_List_Delete_With_Content;

   procedure On_Selection_List_Duplicate
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Source_Name : String := Get_Selected_Selection (Window);
   begin
      declare
         Target_Name : constant String
           := Graph_Window_Input_Dialog.Show
           (-"Target name", -"Duplicate Selection",
            Source_Name, Validate_Selection_Name'Access, Window);
      begin
         if (Target_Name /= "" and then Target_Name /= Source_Name) then
            Controller.Duplicate_Selection
              (Get_Window_Name (Source), Source_Name, Target_Name);
         end if;
      end;
   end On_Selection_List_Duplicate;

   procedure On_Selection_List_Hide
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Controller.Hide_Selection (Get_Window_Name (Source),
                                 Get_Selected_Selection (Window));
   exception
      when Vis_Windows.Selection_May_Not_Be_Faded_Out_Exception =>
         Dialogs.Show_Error_Dialog ("The active and default selections can not be hidden.");
   end On_Selection_List_Hide;

   generic
      Highlight_Status : Vis_Windows.Changable_Highlight_Status;
   package Highlight_Menu_Callback is

        procedure On_Highlight
          (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   end Highlight_Menu_Callback;

   package body Highlight_Menu_Callback is

        procedure On_Highlight
          (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
        is
           Window : Graph_Window_Access := Graph_Window_Access (Source);
        begin
           Controller.Highlight_Selection
             (Get_Window_Name (Window), Get_Selected_Selection (Window),
              Highlight_Status);
        exception
           when Vis_Windows.Highlight_Status_Of_Selection_May_Not_Be_Changed_Exception =>
              Dialogs.Show_Error_Dialog (-"The active selection can not be highlighted.");
        end On_Highlight;

   end Highlight_Menu_Callback;

   package Highlight_Status_None is
     new Highlight_Menu_Callback (Vis_Windows.None);

   package Highlight_Status_Color_1 is
     new Highlight_Menu_Callback (Vis_Windows.Color_1);

   package Highlight_Status_Color_2 is
     new Highlight_Menu_Callback (Vis_Windows.Color_2);

   package Highlight_Status_Color_3 is
     new Highlight_Menu_Callback (Vis_Windows.Color_3);

   procedure On_Selection_List_Rename
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : constant Graph_Window_Access := Graph_Window_Access (Source);
   begin
      declare
         Old_Name : constant String := Get_Selected_Selection (Window);
         New_Name : constant String
           := Graph_Window_Input_Dialog.Show
           (-"New name", -"Rename Selection",
            Old_Name, Validate_Selection_Name'Access, Window);
      begin

         if (New_Name /= "" and then New_Name /= Old_Name) then
            Controller.Rename_Selection (Get_Window_Name (Source),
                                         Old_Name, New_Name);
         end if;
      end;
   exception
      when Vis_Windows.Standard_Selection_Name_May_Not_Be_Changed_Exception =>
         Dialogs.Show_Error_Dialog ("The default selection can not be renamed.");
   end On_Selection_List_Rename;

   procedure On_Selection_List_Set_Active
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : constant Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Controller.Set_Current_Selection
        (Get_Window_Name (Source),
         Get_Selected_Selection (Window));
   exception
      when Vis_Windows.Illegal_Current_Selection_Exception =>
         Dialogs.Show_Error_Dialog (-"The selection is hidden, please select Show first.");
   end On_Selection_List_Set_Active;

   procedure On_Selection_List_Show
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Controller.Show_Selection (Get_Window_Name (Source),
                                 Get_Selected_Selection (Window));
   end On_Selection_List_Show;

   procedure On_Selection_List_Show_All
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Controller.Show_All_Selections
        (Vis_Windows.Get_Name (Window.Visual_Window));
   end On_Selection_List_Show_All;

   ---------------------------------------------------------------------------
   --  Vis Style Callbacks
   ---------------------------------------------------------------------------

   procedure On_Vis_Style_Selected
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Vis_Style_Name : String
        := Gtk.Gentry.Get_Text (Gtk.Combo.Get_Entry (Window.Vis_Style_Combo));
   begin
      if (not Controller.Set_Vis_Style
          (Get_Window_Name (Window), Vis_Style_Name)) then

         Controller.Show_Error (-"Please select a valid style.");
      end if;
   end On_Vis_Style_Selected;

   ---------------------------------------------------------------------------
   --  Zoom Callbacks
   ---------------------------------------------------------------------------

   procedure On_Zoom_In_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Zoom_Level : Vis.Zoom_Level
        := Graph_Widgets.Get_Zoom_Level (Window.Graph) + DEFAULT_ZOOM_STEP;
   begin
      Controller.Set_Zoom_Level (Get_Window_Name (Window), Zoom_Level);
   end On_Zoom_In_Clicked;

   procedure On_Zoom_Level_Selected
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Zoom_String : String
        := Gtk.Gentry.Get_Text (Gtk.Combo.Get_Entry (Window.Zoom_Combo));
      Zoom_Level : Vis.Zoom_Level;
   begin
      if (Zoom_String = "") then
         --  nothing to do, reset text
         Update_Zoom_Level (Window);
      elsif (Zoom_String = -"Whole Graph") then
         Controller.Zoom_To_All (Get_Window_Name (Window));
      else
         if (Zoom_String (Zoom_String'Last) = '%') then
            Zoom_Level := Vis.Zoom_Level'Value
              (Zoom_String (Zoom_String'First .. Zoom_String'Last - 1));
         else
            Zoom_Level := Vis.Zoom_Level'Value (Zoom_String);
         end if;
         Zoom_Level := Zoom_Level / 100.0;
         Controller.Set_Zoom_Level (Get_Window_Name (Window), Zoom_Level);
      end if;
   exception
      when Constraint_Error =>
         Logger.Debug ("invalid zoom level entered: " & Zoom_String);
         Dialogs.Show_Error_Dialog (-"The entered zoom level is invalid.");
   end On_Zoom_Level_Selected;

   procedure On_Zoom_Out_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Zoom_Level : Vis.Zoom_Level
        := Graph_Widgets.Get_Zoom_Level (Window.Graph) - DEFAULT_ZOOM_STEP;
   begin
      Controller.Set_Zoom_Level (Get_Window_Name (Window), Zoom_Level);
   end On_Zoom_Out_Clicked;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Window        :    out Graph_Window_Access;
      Visual_Window : in     Vis_Windows.Visual_Window_Access)
   is
   begin
      Window := new Graph_Window_Record;
      Window.Visual_Window := Visual_Window;
      Initialize (Window);
   end Create;

   procedure Initialize_Styles
     (Window : access Graph_Window_Record'Class)
   is

      function Initialize_Style
        (Config_Id : in Config.Global_Data.Selection_High_Light_ID)
         return Gtk.Style.Gtk_Style
      is
         Style : Gtk.Style.Gtk_Style;
         Color_Access : Config.Color_Access;
         Color : Gdk.Color.Gdk_Color;
      begin
         Style := Gtk.Style.Copy
           (Gui_Utils.String_Clists.Get_Style (Window.Selection_List));

         Color_Access
           := Config.Global_Data.Get_Selection_Highlight_Color (Config_Id);
         Color := Gdk.Color.Parse (Config.Get_Color_Value (Color_Access));

         Gtk.Style.Set_Foreground (Style, Gtk.Enums.State_Normal, Color);
         Gtk.Style.Set_Foreground (Style, Gtk.Enums.State_Selected, Color);
         return Style;
      exception
         when Gdk.Color.Wrong_Color =>
            Logger.Error ("Could not be parse color. Using default instead.");
            return Style;
      end;

   begin
      Window.Styles (Vis_Windows.None) := null;
      Window.Styles (Vis_Windows.Color_1)
        := Initialize_Style (Config.Global_Data.Color_1);
      Window.Styles (Vis_Windows.Color_2)
        := Initialize_Style (Config.Global_Data.Color_2);
      Window.Styles (Vis_Windows.Color_3)
        := Initialize_Style (Config.Global_Data.Color_3);
      Window.Styles (Vis_Windows.Current_Selection)
        := Initialize_Style (Config.Global_Data.Current_Selection);
   end Initialize_Styles;

   procedure Initialize_Pin_List_Menu
     (Window : access Graph_Window_Record'Class)
   is
      use Giant.Gui_Utils;
   begin
      Gtk.Menu.Gtk_New (Window.Pin_List_Menu);
      Gtk.Menu.Append (Window.Pin_List_Menu,
                       New_Menu_Item (-"Show",
                                      On_Pin_List_Show'Access,
                                      Window));
      Gtk.Menu.Append (Window.Pin_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Pin_List_Menu,
                       New_Menu_Item (-"Rename...",
                                      On_Pin_List_Rename'Access,
                                      Window));
      Gtk.Menu.Append (Window.Pin_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Pin_List_Menu,
                       New_Menu_Item (-"Delete",
                                      On_Pin_List_Delete'Access,
                                      Window));
   end Initialize_Pin_List_Menu;

   procedure Initialize_Selection_List_Menu
     (Window : access Graph_Window_Record'Class)
   is
      use Giant.Gui_Utils;
      Submenu : Gtk.Menu.Gtk_Menu;
   begin
      Gtk.Menu.Gtk_New (Window.Selection_List_Menu);
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Set Active",
                                      On_Selection_List_Set_Active'Access,
                                     Window));
      Gtk.Menu.Append (Window.Selection_List_Menu, New_Menu_Separator);
      Submenu := New_Sub_Menu (Window.Selection_List_Menu, -"Highlight");
      Gtk.Menu.Append (Submenu, New_Menu_Item
                       (Gui_Utils.To_Display_Name (Vis_Windows.Color_1),
                        Highlight_Status_Color_1.On_Highlight'Access,
                        Window));
      Gtk.Menu.Append (Submenu, New_Menu_Item
                       (Gui_Utils.To_Display_Name (Vis_Windows.Color_2),
                        Highlight_Status_Color_2.On_Highlight'Access,
                        Window));
      Gtk.Menu.Append (Submenu, New_Menu_Item
                       (Gui_Utils.To_Display_Name (Vis_Windows.Color_3),
                        Highlight_Status_Color_3.On_Highlight'Access,
                        Window));
      Gtk.Menu.Append (Window.Selection_List_Menu, New_Menu_Item
                       (-"Unhighlight",
                        Highlight_Status_None.On_Highlight'Access,
                        Window));
      Gtk.Menu.Append (Window.Selection_List_Menu, New_Menu_Separator);
--        Gtk.Menu.Append (Window.Selection_List_Menu,
--                         New_Menu_Item (-"Show", On_Selection_List_Show'Access,
--                                        Window));
--        Gtk.Menu.Append (Window.Selection_List_Menu,
--                         New_Menu_Item (-"Hide", On_Selection_List_Hide'Access,
--                                        Window));
--        Gtk.Menu.Append (Window.Selection_List_Menu,
--                         New_Menu_Item (-"Show All",
--                                        On_Selection_List_Show_All'Access,
--                                        Window));
--        Gtk.Menu.Append (Window.Selection_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Apply Layout...",
                                      On_Apply_Layout'Access, Window));
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Zoom To Selection",
                                      On_Selection_Zoom_To'Access, Window));
      Gtk.Menu.Append (Window.Selection_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Set Operation...",
                                      On_Selection_List_Set_Operation'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Insert As Subgraph",
                                      On_Selection_List_Create_Subgraph'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Rename...",
                                      On_Selection_List_Rename'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Duplicate...",
                                      On_Selection_List_Duplicate'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Delete",
                                      On_Selection_List_Delete'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Delete With Content",
                                      On_Selection_List_Delete_With_Content'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu, New_Menu_Separator);
      Submenu := New_Sub_Menu (Window.Selection_List_Menu, -"Scripts");
      Giant.Menu_Factory.Generate
        (Labels    => Config_Settings.Get_Setting_As_String
          ("GSL.Selection_Param"),
         Separator => File_Management.Path_Separator,
         Menu      => Submenu,
         Callback  => On_Selection_Script'Access,
         Widget    => Window);

   end Initialize_Selection_List_Menu;

   procedure Initialize_Background_Menu
     (Window : access Graph_Window_Record'Class)
   is
      use Giant.Gui_Utils;
      Submenu : Gtk.Menu.Gtk_Menu;
   begin
      Gtk.Menu.Gtk_New (Window.Background_Menu);
      Gtk.Menu.Append (Window.Background_Menu,
                       New_Menu_Item (-"New Pin...",
                                      On_Background_Create_Pin'Access, Window));
      Gtk.Menu.Append (Window.Background_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Background_Menu,
                       New_Menu_Item (-"Make Room...",
                                      On_Background_Make_Room'Access, Window));
      Gtk.Menu.Append (Window.Background_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Background_Menu,
                       New_Menu_Item (-"Select All",
                                      On_Background_Select_All'Access, Window));
      Gtk.Menu.Append (Window.Background_Menu,
                       New_Menu_Item (-"Select Nothing",
                                      On_Background_Select_Nothing'Access, Window));
      Gtk.Menu.Append (Window.Background_Menu, New_Menu_Separator);
      Submenu := New_Sub_Menu (Window.Background_Menu, -"Scripts");
      Giant.Menu_Factory.Generate
        (Labels    => Config_Settings.Get_Setting_As_String
          ("GSL.No_Param_Context"),
         Separator => File_Management.Path_Separator,
         Menu      => Submenu,
         Callback  => On_Background_Script'Access,
         Widget    => Window);
   end Initialize_Background_Menu;

   procedure Initialize_Edge_Menu
     (Window : access Graph_Window_Record'Class)
   is
      use Giant.Gui_Utils;
      Submenu : Gtk.Menu.Gtk_Menu;
   begin
      Gtk.Menu.Gtk_New (Window.Edge_Menu);
      Gtk.Menu.Append (Window.Edge_Menu,
                       New_Menu_Item (-"Zoom To Edge",
                                      On_Edge_Zoom'Access, Window));
      Gtk.Menu.Append (Window.Edge_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Edge_Menu,
                       New_Menu_Item (-"Center On Source Node",
                                      On_Edge_Show_Source'Access, Window));
      Gtk.Menu.Append (Window.Edge_Menu,
                       New_Menu_Item (-"Center On Target Node",
                                      On_Edge_Show_Target'Access, Window));
      Gtk.Menu.Append (Window.Edge_Menu, New_Menu_Separator);
      Submenu := New_Sub_Menu (Window.Edge_Menu, -"Scripts");
      Giant.Menu_Factory.Generate
        (Labels    => Config_Settings.Get_Setting_As_String
          ("GSL.Edge_Id_Param"),
         Separator => File_Management.Path_Separator,
         Menu      => Submenu,
         Callback  => On_Edge_Script'Access,
         Widget    => Window);
   end Initialize_Edge_Menu;

   procedure Initialize_Node_Menu
     (Window : access Graph_Window_Record'Class)
   is
      use Giant.Gui_Utils;

      Submenu : Gtk.Menu.Gtk_Menu;
   begin
      Gtk.Menu.Gtk_New (Window.Node_Menu);
      Gtk.Menu.Append (Window.Node_Menu,
                       New_Menu_Item (-"Show Info...",
                                      On_Node_Show_Info'Access, Window));
      Gtk.Menu.Append (Window.Node_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Node_Menu,
                       New_Menu_Item (-"Show Source",
                                      On_Node_Show_Source'Access, Window));
      Gtk.Menu.Append (Window.Node_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Node_Menu,
                       New_Menu_Item (-"Annotate...",
                                      On_Node_Annotate'Access, Window));
      Gtk.Menu.Append (Window.Node_Menu, New_Menu_Separator);
      Submenu := New_Sub_Menu (Window.Node_Menu, -"Scripts");
      Giant.Menu_Factory.Generate
        (Labels    => Config_Settings.Get_Setting_As_String
          ("GSL.Node_Id_Param"),
         Separator => File_Management.Path_Separator,
         Menu      => Submenu,
         Callback  => On_Node_Script'Access,
         Widget    => Window);
   end Initialize_Node_Menu;

   procedure Initialize
     (Window : access Graph_Window_Record'Class)
   is
      use Giant.Gui_Utils;

      Left_Box : Gtk.Box.Gtk_Vbox;
      Left_Paned : Gtk.Paned.Gtk_Paned;
      Hbox : Gtk.Box.Gtk_Hbox;
      Vis_Styles : Gtk.Enums.String_List.Glist;
      Zoom_Levels : Gtk.Enums.String_List.Glist;
      Button : Gtk.Button.Gtk_Button;
   begin
      Gtk.Window.Initialize (Window, Gtk.Enums.Window_Toplevel);
      Update_Title (Window);
      --  set default window size
      Set_Usize (Window, 600, 600);

      --  horizontal split pane
      Gtk.Paned.Gtk_New_Hpaned (Window.Split_Pane);
      Gtk.Paned.Set_Handle_Size (Window.Split_Pane, 8);
      Gtk.Paned.Set_Gutter_Size (Window.Split_Pane, 12);
      Add (Window, Window.Split_Pane);

      --  right box: graph widget (needs to be created prior to the minimap)
      Window.Graph := Vis_Windows.Get_Graph_Widget (Window.Visual_Window);
      Window.Graph_Scrolled_Window := Add_Scrollbars (Window.Graph);
      Gtk.Paned.Pack2 (Window.Split_Pane, Window.Graph_Scrolled_Window,
                       Resize => True, Shrink => False);

      Widget_Callback.Object_Connect
        (Window.Graph, "action_mode_button_press_event",
         Action_Mode_Marshallers.To_Marshaller
         (On_Action_Mode_Button_Pressed'Access), Window);
      Widget_Callback.Object_Connect
        (Window.Graph, "background_popup_event",
         Action_Mode_Marshallers.To_Marshaller
         (On_Background_Popup'Access), Window);
      Widget_Callback.Object_Connect
        (Window.Graph, "edge_popup_event",
         Action_Mode_Marshallers.To_Marshaller
         (On_Edge_Popup'Access), Window);
      Widget_Callback.Object_Connect
        (Window.Graph, "node_popup_event",
         Action_Mode_Marshallers.To_Marshaller
         (On_Node_Popup'Access), Window);
      Widget_Callback.Object_Connect
        (Window.Graph, "selection_change_signal",
         Selection_Change_Marshallers.To_Marshaller
         (On_Selection_Changed'Access), Window);

      Initialize_Background_Menu (Window);
      Initialize_Edge_Menu (Window);
      Initialize_Node_Menu (Window);

      --  left box
      Gtk.Box.Gtk_New_Vbox (Left_Box, Homogeneous => False,
                            Spacing => DEFAULT_SPACING);
      Gtk.Box.Set_Border_Width (Left_Box, DEFAULT_SPACING);
      Gtk.Paned.Pack1 (Window.Split_Pane, Left_Box,
                       Resize => False, Shrink => False);

      --  minimap
      Mini_Maps.Create (Window.Mini_Map, Window.Graph);
      Gtk.Box.Pack_Start (Left_Box, Add_Frame (Window.Mini_Map, -"MiniMap"),
                          Expand => False, Fill => True, Padding => 0);

      --  vertical split pane
      Gtk.Paned.Gtk_New_Vpaned (Left_Paned);
      Gtk.Paned.Set_Handle_Size (Left_Paned, 8);
      Gtk.Paned.Set_Gutter_Size (Left_Paned, 12);
      Gtk.Box.Pack_Start (Left_Box, Left_Paned,
                          Expand => True, Fill => True, Padding => 0);

      --  pins list menu
      Initialize_Pin_List_Menu (Window);

      --  pins
      Gui_Utils.String_Clists.Create (Window.Pin_List, 1, Update_Pin'Access);
      Gui_Utils.String_Clists.Connect_Popup_Menu
        (Window.Pin_List, Window.Pin_List_Menu);

      Gui_Utils.String_Clists.Set_Column_Title (Window.Pin_List, 0, -"Pin");

      Gtk.Paned.Add1 (Left_Paned, Add_Scrollbars (Window.Pin_List));

      --  selections list menu
      Initialize_Selection_List_Menu (Window);

      --  selections
      Gui_Utils.String_Clists.Create (Window.Selection_List, 5,
                                      Update_Selection'Access);
      Gui_Utils.String_Clists.Set_Show_Titles (Window.Selection_List, True);
      Gui_Utils.String_Clists.Connect_Popup_Menu
        (Window.Selection_List, Window.Selection_List_Menu);

      Gui_Utils.String_Clists.Set_Column_Title
        (Window.Selection_List, 0, -"Selection");
      Gui_Utils.String_Clists.Set_Column_Title
        (Window.Selection_List, 1, -"Nodes");
      Gui_Utils.String_Clists.Set_Column_Title
        (Window.Selection_List, 2, -"Edges");
      Gui_Utils.String_Clists.Set_Column_Title
        (Window.Selection_List, 3, -"Active");
      Gui_Utils.String_Clists.Set_Column_Title
        (Window.Selection_List, 4, -"Color");

      Gtk.Paned.Add2 (Left_Paned, Add_Scrollbars (Window.Selection_List));

      --  visualization style
      Gtk.Box.Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 0);
      Gtk.Box.Set_Border_Width (Hbox, DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Left_Box, Add_Frame (Hbox, -"Style"),
                          Expand => False, Fill => True, Padding => 0);

      Gtk.Combo.Gtk_New (Window.Vis_Style_Combo);
      Gtk.Combo.Disable_Activate (Window.Vis_Style_Combo);
      Gtk.Box.Pack_Start (Hbox, Window.Vis_Style_Combo,
                          Expand => True, Fill => True, Padding => 0);
      Widget_Callback.Object_Connect
        (Gtk.Combo.Get_List (Window.Vis_Style_Combo), "select_child",
         Widget_Callback.To_Marshaller (On_Vis_Style_Selected'Access),
         Window);
      Widget_Callback.Object_Connect
           (Gtk.Combo.Get_Entry (Window.Vis_Style_Combo), "activate",
            Widget_Callback.To_Marshaller (On_Vis_Style_Selected'Access),
            Window);

      --  zoom
      Gtk.Box.Gtk_New_Hbox (Hbox, Homogeneous => False,
                            Spacing => DEFAULT_SPACING);
      Gtk.Box.Set_Border_Width (Hbox, DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Left_Box, Add_Frame (Hbox, -"Zoom"),
                          Expand => False, Fill => True, Padding => 0);

      --  zoom selection
      Button := New_Button (" - ", On_Zoom_Out_Clicked'Access, Window);
      Gtk.Box.Pack_Start (Hbox, Button, Expand => False, Fill => False,
                          Padding => 0);

      Gtk.Enums.String_List.Append (Zoom_Levels, -"100%");
      Gtk.Enums.String_List.Append (Zoom_Levels, -"50%");
      Gtk.Enums.String_List.Append (Zoom_Levels, -"Whole Graph");

      Gtk.Combo.Gtk_New (Window.Zoom_Combo);
      Gtk.Combo.Disable_Activate (Window.Zoom_Combo);
      Gtk.Combo.Set_Popdown_Strings (Window.Zoom_Combo,
                                     Zoom_Levels);
      Gtk.Box.Pack_Start (Hbox, Window.Zoom_Combo,
                          Expand => True, Fill => True, Padding => 0);
      Gtk.Combo.Set_Usize (Window.Zoom_Combo, 100, Glib.Gint (-1));

      --  connect zoom callbacks
      Widget_Callback.Object_Connect
        (Gtk.Combo.Get_List (Window.Zoom_Combo), "select_child",
         Widget_Callback.To_Marshaller (On_Zoom_Level_Selected'Access),
         Window);
      Widget_Callback.Object_Connect
           (Gtk.Combo.Get_Entry (Window.Zoom_Combo), "activate",
            Widget_Callback.To_Marshaller (On_Zoom_Level_Selected'Access),
            Window);

      Button := New_Button (" + ", On_Zoom_In_Clicked'Access, Window);
      Gtk.Box.Pack_Start (Hbox, Button, Expand => False, Fill => False,
                          Padding => 0);

      --  listen for the close button
      Widget_Boolean_Callback.Connect
        (Window, "delete_event",
         Widget_Boolean_Callback.To_Marshaller (On_Close'Access));

      --  connect close project
      Main_Window.Connect_Can_Close_Project
        (On_Can_Close_Project'Access, Window);
      Main_Window.Connect_Close_Project (On_Close_Project'Access, Window);
   end;

   procedure Post_Initialize
     (Window : access Graph_Window_Record)
   is
   begin
      Update_Vis_Style (Window);
      Update_Zoom_Level (Window);
   end Post_Initialize;

   ---------------------------------------------------------------------------
   --  Public Methods
   ---------------------------------------------------------------------------

   function Close
     (Window               : access Graph_Window_Record;
      Ask_For_Confirmation : in     Boolean)
     return Boolean
   is
   begin
      if (Ask_For_Confirmation and then not Save_Changes (Window)) then
         return False;
      end if;

      Hide (Window);
      return True;
   end;

   function Get_Current_Edge
     (Window : access Graph_Window_Record)
     return Graph_Lib.Edge_Id
   is
   begin
      return Window.Current_Edge;
   end Get_Current_Edge;

   function Get_Current_Node
     (Window : access Graph_Window_Record)
     return Graph_Lib.Node_Id
   is
   begin
      return Window.Current_Node;
   end Get_Current_Node;

   function Get_Vis_Window
     (Window : access Graph_Window_Record)
     return Vis_Windows.Visual_Window_Access
   is
   begin
      return Window.Visual_Window;
   end Get_Vis_Window;

   procedure Remove_Graph_Widget
     (Window : access Graph_Window_Record)
   is
   begin
      Gtk.Scrolled_Window.Remove (Window.Graph_Scrolled_Window, Window.Graph);
   end Remove_Graph_Widget;

   procedure Update_Title
     (Window : access Graph_Window_Record)
   is
   begin
      Set_Title (Window, Vis_Windows.Get_Name (Window.Visual_Window));
   end Update_Title;

   procedure Set_Global_Action_Mode
     (Window : access Graph_Window_Record;
      Enable : in     Boolean)
   is
   begin
      if (Enable) then
         if (not Graph_Widgets.Is_Action_Mode_Active (Window.Graph)) then
            Graph_Widgets.Start_Action_Mode (Window.Graph);
         end if;
      else
         if (Graph_Widgets.Is_Action_Mode_Active (Window.Graph)
             and not Is_Local_Action_Pending (Window)) then
            Graph_Widgets.Cancel_Action_Mode (Window.Graph);
         end if;
      end if;
   end Set_Global_Action_Mode;

   ---------------------------------------------------------------------------
   --  Local Action Methods
   ---------------------------------------------------------------------------

   function Is_Local_Action_Pending
     (Window : access Graph_Window_Record)
     return Boolean
   is
      use type Actions.Graph_Window_Action_Access;
   begin
      return (Window.Local_Action /= null);
   end Is_Local_Action_Pending;

   procedure Cancel_Local_Action
     (Window : access Graph_Window_Record)
   is
      use type Actions.Graph_Window_Action_Access;
   begin
      if (Window.Local_Action /= null) then
         Gui_Manager.Set_Status ("");
         Actions.Cancel (Window.Local_Action);
         Actions.Destroy (Window.Local_Action);
         Window.Local_Action := null;
         Graph_Widgets.Cancel_Action_Mode (Window.Graph);
      end if;
   end Cancel_Local_Action;

   procedure Set_Local_Action
     (Window : access Graph_Window.Graph_Window_Record;
      Action : access Graph_Window.Actions.Graph_Window_Action_Type'Class)
   is
      use type Actions.Graph_Window_Action_Access;
   begin
      --  cancel currently pending action
      Cancel_Local_Action (Window);

      Gui_Manager.Set_Status (-"Please select the target position");
      Window.Local_Action
        := Graph_Window.Actions.Graph_Window_Action_Access (Action);
      if (not Graph_Widgets.Is_Action_Mode_Active (Window.Graph)) then
         Graph_Widgets.Start_Action_Mode (Window.Graph);
      end if;
   end Set_Local_Action;

   procedure Trigger_Local_Action
     (Window   : access Graph_Window.Graph_Window_Record;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
   is
      use type Actions.Graph_Window_Action_Access;
   begin
      if (Window.Local_Action /= null) then
         if (Actions.Execute (Window.Local_Action, Window, Event)) then
            Actions.Destroy (Window.Local_Action);
            Window.Local_Action := null;
            Graph_Widgets.Cancel_Action_Mode (Window.Graph);
         end if;
      end if;
   end Trigger_Local_Action;

   ---------------------------------------------------------------------------
   --  Node Annotations
   ---------------------------------------------------------------------------

   procedure Update_Node_Annotation
     (Window : access Graph_Window_Record;
      Node   : in     Graph_Lib.Node_Id)
   is
   begin
      Graph_Widgets.Change_Annotation_State (Window.Graph, Node);
   end Update_Node_Annotation;

   ---------------------------------------------------------------------------
   --  Pin Methods
   ---------------------------------------------------------------------------

   procedure Update_Pin
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
   begin
      Gui_Utils.String_Clists.Set_Text (List, Row, 0, Name);
   end Update_Pin;

   procedure Add_Pin
     (Window : access Graph_Window_Record;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Add (Window.Pin_List, Name);
   end Add_Pin;

   procedure Remove_Pin
     (Window : access Graph_Window_Record;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Remove (Window.Pin_List, Name);
   end Remove_Pin;

   procedure Update_Pin
     (Window : access Graph_Window_Record;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Update (Window.Pin_List, Name);
   end Update_Pin;

   ---------------------------------------------------------------------------
   --  Selection Methods
   ---------------------------------------------------------------------------

   procedure Update_Selection
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
      use type Gtk.Style.Gtk_Style;
      use type Vis_Windows.Selection_Highlight_Status;

      Window : Graph_Window_Access
        := Graph_Window_Access (Gui_Utils.String_Clists.Get_Toplevel
                                (List));
      Selection : Graph_Lib.Selections.Selection
        := Controller.Get_Selection (Get_Window_Name (Window), Name);
      Highlighted : Vis_Windows.Selection_Highlight_Status
        := Vis_Windows.Get_Highlight_Status (Window.Visual_Window, Name);
      Is_Active : Boolean
        := (Vis_Windows.Get_Current_Selection (Window.Visual_Window) = Name);
   begin
      if (Window.Styles (Vis_Windows.Color_1) = null) then
         Initialize_Styles (Window);
      end if;

      Gui_Utils.String_Clists.Set_Text (List, Row, 0, Name);

      Gui_Utils.String_Clists.Set_Text
        (List, Row, 1,
         Natural'Image (Graph_Lib.Selections.Get_Node_Count (Selection)));
      Gui_Utils.String_Clists.Set_Text
        (List, Row, 2,
         Natural'Image (Graph_Lib.Selections.Get_Edge_Count (Selection)));

      if (Is_Active) then
         Gui_Utils.String_Clists.Set_Text (List, Row, 3, -"Yes");
      else
         Gui_Utils.String_Clists.Set_Text (List, Row, 3, "");
      end if;

      Gui_Utils.String_Clists.Set_Cell_Style
        (List, Row, 4, Window.Styles (Highlighted));
      Gui_Utils.String_Clists.Set_Text
        (List, Row, 4, Gui_Utils.To_Display_Name (Highlighted));
   end Update_Selection;

   procedure Add_Selection
     (Window : access Graph_Window_Record;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Add (Window.Selection_List, Name);
   end Add_Selection;

   procedure Remove_Selection
     (Window : access Graph_Window_Record;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Remove (Window.Selection_List, Name);
   end Remove_Selection;

   procedure Update_Selection
     (Window : access Graph_Window_Record;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Update (Window.Selection_List, Name);
   end Update_Selection;

   ---------------------------------------------------------------------------
   --  Vis Style Methods
   ---------------------------------------------------------------------------

   procedure Add_Vis_Style
     (Window : access Graph_Window_Record;
      Name   : in     String)
   is
      Item : Gtk.List_Item.Gtk_List_Item;
   begin
      Gtk.List_Item.Gtk_New (Item, Name);
      Gtk.List_Item.Show (Item);
      Gtk.List.Add (Gtk.Combo.Get_List (Window.Vis_Style_Combo), Item);
   end Add_Vis_Style;

   procedure Update_Vis_Style
     (Window     : access Graph_Window_Record)
   is
   begin
      Gtk.Gentry.Set_Text (Gtk.Combo.Get_Entry (Window.Vis_Style_Combo),
                           Vis_Windows.Get_Vis_Style (Window.Visual_Window));
   end Update_Vis_Style;

   ---------------------------------------------------------------------------
   --  Zoom
   ---------------------------------------------------------------------------

   procedure Update_Zoom_Level
     (Window : access Graph_Window_Record)
   is
      package Zoom_Level_IO is new Ada.Text_IO.Float_IO(Vis.Zoom_Level);

      Zoom_Level : Vis.Zoom_Level
        := Graph_Widgets.Get_Zoom_Level (Window.Graph);
      Zoom_String : String(1 .. 12);
   begin
      Zoom_Level_IO.Put
        (Zoom_String, Zoom_Level * 100.0, Aft => 2, Exp => 0);
      --Vis.Zoom_Level'Image (Zoom_Level * 100.0)
      Gtk.Gentry.Set_Text (Gtk.Combo.Get_Entry (Window.Zoom_Combo),
                           Ada.Strings.Fixed.Trim
                           (Zoom_String, Ada.Strings.Both) & "%");
   end Update_Zoom_Level;

end Giant.Graph_Window;
