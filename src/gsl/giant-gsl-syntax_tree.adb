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
-- First Author: Gerrit Schulz
--
-- $RCSfile: giant-gsl-syntax_tree.adb,v $, $Revision: 1.5 $
-- $Author: schulzgt $
-- $Date: 2003-08-14 14:36:20 $
--
with Unchecked_Deallocation;
with Giant.Gsl.Types;

package body Giant.Gsl.Syntax_Tree is

   ---------------------------------------------------------------------------
   -- creates a new Syntax_Node
   function Create_Node
     (N_Type : Node_Type;
      Child1 : Syntax_Node;
      Child2 : Syntax_Node)
      return Syntax_Node is

      Node : Syntax_Node;
   begin
      Node := new Syntax_Node_Record;
      Node.N_Type  := N_Type;
      Node.Child1  := Child1;
      Node.Child2  := Child2;
      Node.Literal := Gsl_Null;
      return Node;
   end Create_Node;

   ---------------------------------------------------------------------------
   --
   function Copy_Node
     (Node : Syntax_Node)
      return Syntax_Node is

      N   : Syntax_Node;
   begin      
      N := new Syntax_Node_Record;
      N.all := Node.all;
      return N;
   end Copy_Node;

   ---------------------------------------------------------------------------
   -- destroys a Syntax_Node and frees the memory
   procedure Destroy_Node
     (Node : in out Syntax_Node) is

      procedure Free is new Unchecked_Deallocation
        (Syntax_Node_Record,
         Syntax_Node);

   begin
      if Node /= Null_Node then
         Free (Node);
      end if;
   end Destroy_Node;

   ---------------------------------------------------------------------------
   -- destroys a Syntax_Node with all children and frees the memory
   procedure Destroy_Syntax_Tree
     (Node : in out Syntax_Node) is

      procedure Free is new Unchecked_Deallocation
        (Syntax_Node_Record,
         Syntax_Node);

   begin
      if Node /= Null_Node then
         if Node.Child1 /= Null_Node then
            Destroy_Syntax_Tree (Node.Child1);
         end if;
         if Node.Child2 /= Null_Node then
            Destroy_Syntax_Tree (Node.Child2);
         end if;
         Gsl.Types.Destroy_Gsl_Type (Node.Literal);
         Free (Node);
      end if;
   end Destroy_Syntax_Tree;

   ---------------------------------------------------------------------------
   -- get the type of a Syntax_Node
   function Get_Node_Type
     (Node : Syntax_Node)
      return Node_Type is
   begin
      return Node.N_Type;
   end Get_Node_Type;
      
   ---------------------------------------------------------------------------
   -- get the 1st child of a Syntax_Node
   function Get_Child1
     (Node : Syntax_Node)
      return Syntax_Node is
   begin
      return Node.Child1;
   end Get_Child1;

   ---------------------------------------------------------------------------
   -- get the 2nd child of a Syntax_Node
   function Get_Child2
     (Node : Syntax_Node)
      return Syntax_Node is
   begin
      return Node.Child2;
   end Get_Child2;

   ---------------------------------------------------------------------------
   -- get the literal of a Syntax_Node
   function Get_Literal
     (Node : Syntax_Node)
      return Gsl_Type is
   begin
      return Node.Literal;
   end Get_Literal;

   ---------------------------------------------------------------------------
   -- set the literal of a Syntax_Node
   procedure Set_Literal
     (Node : Syntax_Node;
      Literal : Gsl_Type) is
   begin
      Node.Literal := Literal;
   end Set_Literal;
	
   ---------------------------------------------------------------------------
   -- get the size of a Syntax_Node (Sequence or List)
   function Get_Size
     (Node : Syntax_Node)
      return Natural is
   begin
      return Node.Size;
   end Get_Size;

   ---------------------------------------------------------------------------
   -- set the size of a Syntax_Node (Sequence or List)
   procedure Set_Size
     (Node : Syntax_Node;
      Size : Natural) is
   begin
      Node.Size := Size;
   end Set_Size;
	
end Giant.Gsl.Syntax_Tree;
