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
--  $RCSfile: giant-string_split.ads,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  Contains routine to split springs according to a defined pattern
--

with String_Lists;

package Giant.String_Split is

   ---------------------------------------------------------------------------
   --  Splits given string
   --
   --  Example:
   --      "a,b,,c", ","
   --    gets
   --      ("a", "b", "", "c")
   --
   --  Parameters:
   --    Source:  String to split
   --    Pattern: Separation character
   --    Trim:    If blanks should be trimmed at both sides
   --
   --  Returns:
   --    * List of substrings
   --    * Empty list, if source is empty
   function Split_String
     (Source  : in String;
      Pattern : in String;
      Trim    : in Boolean := false)
     return String_Lists.List;

end Giant.String_Split;
