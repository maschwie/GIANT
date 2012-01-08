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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-simple_priority_queues.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  Generic heap implementation of a priority queue with a fixed maximum size.
--
--  The type of items is a generic parameter. A strict total order must
--  be provided on that type ("priority order").
--
--  It is not possible to determine if a given Item is contained in a
--  queue or not. The priority of any item contained in any queue except
--  the head item must not change. If this is needed, then the package
--  Giant.Fixed_Priority_Queues should be used.
--


generic

   ----------------------------------------------------------------------------
   --  Type for items
   type Item_Type is private;

   ----------------------------------------------------------------------------
   --  Priority order. This function must either always return the
   --  same value for the same arguments, or the data structure must
   --  be updated by calls 'Update_Head'
   --
   --  Returns:
   --    True if 'Left' has a higher priority than 'Right', False else
   with function Has_Higher_Priority
     (Left  : in Item_Type;
      Right : in Item_Type)
     return Boolean;

package Giant.Simple_Priority_Queues is

   ----------------------------------------------------------------------------
   --  Type for fixed size priority queues of size at most 'Max_Size' elements
   type Queue_Type (Max_Size : Natural) is private;

   ----------------------------------------------------------------------------
   --  Raised whenever the size of a Q : 'Queue_Type' exceeds
   --  'Get_Max_Size (Q)' items
   Too_Many_Items   : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever a 'Queue_Type' is empty, but an item is needed
   Not_Enough_Items : exception;

   ----------------------------------------------------------------------------
   --  Inserts an item into a queue. Note that each item must be
   --  contained at most in one queue at any point of time.
   --
   --  Precondition:
   --    Get_Size (Queue) < Get_Max_Size (Queue) and
   --  Postcondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    * Too_Many_Items if Get_Size (Queue) >= Get_Max_Size (Queue)
   procedure Insert
     (Queue : in out Queue_Type;
      Item  : in     Item_Type);

   ----------------------------------------------------------------------------
   --  Must be called whenever the priority of the head item has changed.
   --
   --  Precondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   procedure Update_Head
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Removes the head item from 'Queue'
   --
   --  Precondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   procedure Remove_Head
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Postcondition:
   --    Is_Empty (Queue)
   procedure Clear
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Returns an item contained in 'Queue' that has the maximum priority.
   --  If there are more than one items in 'Queue' that have the maximum
   --  priority then it is unspecified which one will be returned. However
   --  two calls to 'Get_Head' always yield the same item unless 'Queue'
   --  was modified.
   --
   --  Returns:
   --    Head item in 'Queue':
   --    for all I : Item_Type and I is contained 'Queue':
   --      not Has_Higher_Priority (I, Head)
   --  Precondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   function Get_Head
     (Queue : in     Queue_Type)
     return Item_Type;

   ----------------------------------------------------------------------------
   --  Returns:
   --    True if there is no Item in 'Queue', False else
   function Is_Empty
     (Queue : in     Queue_Type)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Returns:
   --    Number of Items inserted in, but not removed from 'Queue'
   function Get_Size
     (Queue : in     Queue_Type)
     return Natural;

   ----------------------------------------------------------------------------
   --  Returns:
   --    The number of Items that can be inserted at most into 'Queue'
   function Get_Max_Size
     (Queue : in     Queue_Type)
     return Natural;

private

   subtype Array_Bounds is Positive;

   type Heap_Array is array (Array_Bounds range <>) of Item_Type;

   type Queue_Type (Max_Size : Natural) is
      record
         Size  : Natural := 0;
         Field : Heap_Array (1 .. Max_Size);
      end record;

end Giant.Simple_Priority_Queues;
