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
-- $RCSfile: giant-gsl-syntax_tree.ads,v $
-- $Author: schulzgt $
-- $Date: 2003-06-30 16:00:51 $
--
-- This package implements functions needed to work with the Gsl syntax tree.
-- The datastructure for the nodes is located in the parent package Giant.Gsl.
--

package Giant.Gsl.Syntax_Tree is

   ---------------------------------------------------------------------------
   --  Creates a new Syntax_Node.
   --
   --  Parameters:
   --    N_Type - type of the node
   --    Child1 -
   --    Child2 -
   --  Returns:
   --    the new Syntay_Node
   function Create_Node
     (N_Type : Node_Type;
      Child1 : Syntax_Node;
      Child2 : Syntax_Node)
      return Syntax_Node;

   ---------------------------------------------------------------------------
   --
   function Copy_Node
     (Node : Syntax_Node)
      return Syntax_Node;

   ---------------------------------------------------------------------------
   -- destroys a Syntax_Node with all children and frees the memory
   procedure Destroy_Node
     (Node : in out Syntax_Node);

   ---------------------------------------------------------------------------
   -- destroys a Syntax_Node with all children and frees the memory
   procedure Destroy_Syntax_Tree
     (Node : in out Syntax_Node);

   ---------------------------------------------------------------------------
   -- get the type of a Syntax_Node
   function Get_Node_Type
     (Node : Syntax_Node)
      return Node_Type;
      
   ---------------------------------------------------------------------------
   -- get the 1st child of a Syntax_Node
   function Get_Child1
     (Node : Syntax_Node)
      return Syntax_Node;

   ---------------------------------------------------------------------------
   -- get the 2nd child of a Syntax_Node
   function Get_Child2
     (Node : Syntax_Node)
      return Syntax_Node;
    
   ---------------------------------------------------------------------------
   -- get the literal of a Syntax_Node
   function Get_Literal
     (Node : Syntax_Node)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   -- set the literal of a Syntax_Node
   procedure Set_Literal
     (Node : Syntax_Node;
      Literal : Gsl_Type);

   ---------------------------------------------------------------------------
   -- get the size of a Syntax_Node (Sequence or List)
   function Get_Size
     (Node : Syntax_Node)
      return Natural;

   ---------------------------------------------------------------------------
   -- set the size of a Syntax_Node (Sequennce or List)
   procedure Set_Size
     (Node : Syntax_Node;
      Size : Natural);

end Giant.Gsl.Syntax_Tree;
