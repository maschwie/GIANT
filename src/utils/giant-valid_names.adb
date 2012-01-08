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
-- $RCSfile: giant-valid_names.adb,v $, $Revision: 1.5 $
-- $Author: squig $
-- $Date: 2003-06-17 15:05:37 $
--
with GNAT.OS_Lib;

package body Giant.Valid_Names is

   procedure Verify_Standard_Name
     (String_Value : in String)
   is
   begin
      if (not Is_Standard_Name (String_Value)) then
         raise No_Correct_Standard_Name_Exception;
      end if;
   end Verify_Standard_Name;

   ---------------------------------------------------------------------------
   function Is_Standard_Name
     (String_Value : in String) return Boolean is

   begin

      if ((String_Value'Length < Min_Name_Length) or
          (String_Value'Length > Max_Name_Length)) then

        return False;
      end if;


      for I in String_Value'Range loop

         if (Character'Pos(String_Value(I))
                 -- '0'(ASCII 48) .. '9'(ASCII 57)
                 not in Character'Pos('0') .. Character'Pos('9'))
           and  (Character'Pos(String_Value(I))
                 -- 'A'(ASCII 65) .. 'Z'(ASCII 90)
                 not in Character'Pos('A') .. Character'Pos('Z'))
           and  (Character'Pos(String_Value(I))
                 -- 'a'(ASCII 97) .. 'z'(ASCII 122)
                 not in Character'Pos('a') .. Character'Pos('z'))
                 -- '_'(ASCII 95)
           and  (Character'Pos(String_Value(I)) /= Character'Pos('_'))
         then

            return False;
         end if;
      end loop;

      return True;
   end Is_Standard_Name;

   ---------------------------------------------------------------------------
   function To_Standard_Name
     (String_Value : in String) return Standard_Name is

   begin
      if (Is_Standard_Name (String_Value) = False) then
         raise No_Correct_Standard_Name_Exception;
      end if;

      return Standard_Name
        (Ada.Strings.Unbounded.To_Unbounded_String(String_Value));
   end To_Standard_Name;

   ---------------------------------------------------------------------------
   function To_String
     (Name : Standard_Name) return String is


   begin

      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String(Name));
   end To_String;

end Giant.Valid_Names;
