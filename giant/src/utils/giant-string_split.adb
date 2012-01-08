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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-string_split.adb,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

package body Giant.String_Split is

   function Split_String
     (Source  : in String;
      Pattern : in String;
      Trim    : in Boolean := false)
     return String_Lists.List
   is

      procedure Split_String
        (Source  : in     String;
         Pattern : in     String;
         Trim    : in     Boolean;
         List    : in out String_Lists.List)
      is

         function Generate_String_To_Attach
           (The_String : in String)
           return Ada.Strings.Unbounded.Unbounded_String
         is
         begin
            if Trim then
               return Ada.Strings.Unbounded.To_Unbounded_String
                 (Ada.Strings.Fixed.Trim (The_String, Ada.Strings.Both));
            else
               return Ada.Strings.Unbounded.To_Unbounded_String
                 (The_String);
            end if;
         end Generate_String_To_Attach;

         I : Natural;
      begin
         I := Ada.Strings.Fixed.Index (Source, Pattern);

         if I < Source'First then
            --  string doesn't hold any separator pattern;
            --    (i.e. Source looks like "X")
            String_Lists.Attach
              (List,
               Generate_String_To_Attach (Source));
         else
            --  I >= Source'First
            --  Index has found the pattern

            if I = Source'First then
               --  if there is no string before pattern -> attach a null string
               --    (i.e. Source looks like ",X")
               String_Lists.Attach
                 (List,
                  Ada.Strings.Unbounded.Null_Unbounded_String);
            else
               --  attach the full found string
               String_Lists.Attach
                 (List,
                  Generate_String_To_Attach
                  (Source (Source'First .. I-1)));
            end if;

            if I = Source'Last then
               --  if there is no string at the end,
               --    (i.e. Source looks like "X,")
               --     attach a null string and finish
               String_Lists.Attach
                 (List,
                  Ada.Strings.Unbounded.Null_Unbounded_String);
            else
               --  there is more to split
               --    (i.e. Source looks like "X,Y"
               --  recursively split
               Split_String
                 (Source (I + Pattern'Length .. Source'Last),
                  Pattern,
                  Trim,
                  List);
            end if;
         end if;

      end Split_String;

      Res : String_Lists.List;
      I   : Integer;

   begin
      Res := String_Lists.Create;

      --  Check for empty string
      --  a bit more complicated, since we support triming
      I := Ada.Strings.Fixed.Index (Source, Pattern);
      if I < Source'First then
         if Trim then
            declare
               Trimmed : String := Ada.Strings.Fixed.Trim
                 (Source, Ada.Strings.Both);
            begin
               if Trimmed'Length = 0 then
                  -- exit returning empty list
                  return Res;
               end if;
            end;
         else
            if Source'Length = 0 then
               -- exit returning empty list
               return Res;
            end if;
         end if;
      end if;

      Split_String (Source, Pattern, Trim, Res);
      return Res;
   end Split_String;

end Giant.String_Split;
