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
--  $RCSfile: giant-main.adb,v $, $Revision: 1.48 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
--
------------------------------------------------------------------------------
--
--  The GIANT main program.
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;

with Gdk.Threads;
with Gtk.Main;
with Gtkada.Intl;

with Giant.Config;
with Giant.Config_Settings;
with Giant.Config.Class_Sets;
with Giant.Config.Global_Data;
with Giant.Config.Vis_Styles;
with Giant.Constants;
with Giant.Controller;
with Giant.Default_Logger;
with Giant.File_Management;
with Giant.Graph_Lib;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

procedure Giant.Main
is
   package Logger is new Giant.Logger("giant.main");

   Start_Gui : Boolean := True;

   --  can be overriden by parameter
   Global_Config_Filename : Ada.Strings.Unbounded.String_Access
     := new String' ("etc" & GNAT.OS_Lib.Directory_Separator
                     & "global_config.xml");

   User_Config_Filename : constant String
     := File_Management.Get_User_Config_Path & "settings.xml";


   procedure Put_Help
   is
   begin
      Ada.Text_IO.Put_Line
        ("usage: giant [-c config-file] [[-g graph-file] [-e script-file] project-file] | -h | -v");
   end;

   procedure Evaluate_Arguments
     (Project_Filename : in String;
      Graph_Filename   : in String;
      Script_Filename  : in String)
   is
   begin
      begin
         Controller.Open_Project (Project_Filename);
      exception
        when E: others =>
           Controller.Handle_Project_Exception (E, Project_Filename);

           if (Graph_Filename /= "") then
              begin
                 Controller.Create_Project (Project_Filename, Graph_Filename);
              exception
                when E: others =>
                   Controller.Handle_Project_Exception (E, Project_Filename);
                   return;
              end;
           end if;
      end;

      if (Script_Filename /= "") then
         begin
            Controller.Execute_GSL
              (Filename => Script_Filename);
         exception
           when E : others =>
              Controller.Handle_IO_Exception (E, Script_Filename);
         end;
      end if;
   end Evaluate_Arguments;

   procedure Parse_Arguments
   is
      use Ada.Strings.Unbounded;

      Project_Filename : String_Access;
      Graph_Filename   : String_Access := new String' ("");
      Script_Filename  : String_Access := new String' ("");

      procedure Handle_Switch
        (Switch : Character)
      is
      begin
         case Switch is
           when 'c' =>
              --  "config-file"
              Free (Global_Config_Filename);
              Global_Config_Filename := new String'(GNAT.Command_Line.Parameter);
           when 'e' =>
              --  "execute"
              Free (Script_Filename);
              Script_Filename := new String'(GNAT.Command_Line.Parameter);
           when 'g' =>
              --  "graph-file"
              Free (Graph_Filename);
              Graph_Filename := new String'(GNAT.Command_Line.Parameter);
           when 'h' =>
              --  "help"
                   Put_Help;
                   GNAT.OS_Lib.OS_Exit (0);
           when 'n' =>
              --  "nogui"
              Start_Gui := False;
           when 'v' =>
              --  "version"
              Ada.Text_IO.Put_Line ("version: " & Constants.Version);
              GNAT.OS_Lib.OS_Exit (0);
           when others =>
              --  should not happen
              Logger.Warn ("Invalid command line switch: "
                           & GNAT.Command_Line.Full_Switch);
         end case;
      end Handle_Switch;

   begin
      GNAT.Command_Line.Initialize_Option_Scan;

      loop
         case GNAT.Command_Line.Getopt
           ("c: e: g: h n v -config: -execute: -graph: -help -nogui -version")
         is
           --  long option names
           when '-' =>
              Handle_Switch (GNAT.Command_Line.Full_Switch
                             (GNAT.Command_Line.Full_Switch'First + 1));
           when ASCII.Nul =>
              exit;
           when others =>
              Handle_Switch (GNAT.Command_Line.Full_Switch
                             (GNAT.Command_Line.Full_Switch'First));
         end case;
      end loop;

      --  non-switch argument
      Project_Filename := new String' (GNAT.Command_Line.Get_Argument);

      if (Project_Filename.all /= "") then
         Evaluate_Arguments
           (Project_Filename => Project_Filename.all,
            Graph_Filename   => Graph_Filename.all,
            Script_Filename  => Script_Filename.all);
      end if;

      Free (Project_Filename);
      Free (Graph_Filename);
      Free (Script_Filename);

   exception
     when Gnat.Command_Line.Invalid_Switch =>
        Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Invalid argument.");
        Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
        Put_Help;
        GNAT.OS_Lib.OS_Exit (1);
   end Parse_Arguments;

begin
   Default_Logger.Init ("debug.log");

   begin
      --  create user config path if not existant
      File_Management.Create_Dir_Path (File_Management.Get_User_Config_Path);

      --  load config settings
      Config_Settings.Initialize_Config_Settings
        (Global_Config_Filename.all, User_Config_Filename);

      Config.Global_Data.Initialize_Config_Data;

      Giant.Graph_Lib.Initialize;

      Logger.Debug ("reading configuration");
      Config.Vis_Styles.Initialize_Config_Vis_Styles
        (Resources_Root_Dir
           => File_Management.Get_Shared_Path,
         GIANT_VIS_Directory
           => File_Management.Get_Shared_Path ("styles"),
         User_Vis_Directory
           => File_Management.Get_User_Config_Path & "styles",
         Default_Vis_Style_File
           => File_Management.Get_Shared_Path ("styles", "Default.xml"));

      Logger.Debug ("intializing class sets");
      Config.Class_Sets.Initialize_Class_Sets
        (GIANT_Class_Sets_Directory
           => File_Management.Get_Shared_Path ("class_sets"));

   exception
      when Config.Vis_Styles.Illegal_Default_Vis_Style_Exception =>
         Ada.Text_IO.Put_Line
           ("Default stlye not found: "
            & File_Management.Get_Shared_Path ("styles", "Default.xml"));
         GNAT.OS_Lib.OS_Exit (1);
      when E: others =>
         Logger.Warn ("Error during intialization");
         Logger.Error (E);
   end;

   Logger.Debug ("Initializing GTK");

   Gtkada.Intl.Bind_Text_Domain
     ("giant", File_Management.Get_Shared_Path ("locale"));

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gdk.Threads.G_Init;
   Gdk.Threads.Init;

   Gdk.Threads.Enter;

   Logger.Debug ("parsing command line arguments");
   Parse_Arguments;

   Logger.Debug ("starting giant");

   if (Start_Gui) then
      Logger.Debug ("initializing gui");
      Controller.Show_Gui;
   end if;

   Gdk.Threads.Leave;

   Logger.Debug ("closing giant");

   Controller.Exit_Application;
   Giant.Graph_Lib.Destroy;

   --  store config settings
   Logger.Debug ("storing config settings: " & User_Config_Filename);
   Config_Settings.Store_User_Config_File (User_Config_Filename);

   Giant.Default_Logger.Close;
end Giant.Main;
