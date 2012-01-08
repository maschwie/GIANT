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
--  $RCSfile: giant-dialogs.ads,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
------------------------------------------------------------------------------
--
-- Provides common dialogs.
--

with Gtk.Object;

with Giant.Default_Dialog;
with Giant.Input_Dialog;
pragma Elaborate (Giant.Input_Dialog);

package Giant.Dialogs is

   package Gtk_Object_Input_Dialog is new Giant.Input_Dialog
     (Gtk.Object.Gtk_Object);

   function Show_Confirmation_Dialog
     (Message : in String;
      Buttons : in Default_Dialog.Button_Type := Default_Dialog.Button_Yes_No)
      return Default_Dialog.Response_Type;

   function Show_Delete_Confirmation_Dialog
     (Message : in String := -"Do you really want to delete?")
     return Boolean;

   procedure Show_Error_Dialog
     (Message : in String;
      Title   : in String := -"Giant Error");

   function Show_Input_Dialog
     (Message         : in String;
      Title           : in String                                   := -"Giant Input";
      Default_Input   : in String                                   := "";
      Input_Validator : in Gtk_Object_Input_Dialog.Input_Validator_Type := null)

     return String;

end Giant.Dialogs;
