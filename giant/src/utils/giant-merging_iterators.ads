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
--  $RCSfile: giant-merging_iterators.ads,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  Provides the type 'Merger_Type'. An instance of 'Merger_Type' is a
--  special iterator that combines a number of 'Sets.Iterator's by merging
--  their items.
--
--  'Sets' is an instance of the generic package 'Ordered_Sets' and provides
--  the type of the iterators to be merged.
--
--  A 'Merger_Type' returns all elements contained in any of the iterators
--  it is based on, in strictly ascending order.
--
--  Instances of 'Merger_Type' can be created within the default storage
--  pool (heap) or can be declared as variables. If declared as variables
--  the maximum number of iterators to merge must be known to the user
--  at declaration time. If created on the heap, then the subprogram
--  'Create' will figure out the correct size itself.
--
--  Complexity:
--
--  :: Time
--  * Let n be the number of iterators a merging iterator is based on
--  * Let sum be the number of items all those n iterators iterate over
--  * By Sets.Destroy think of the overloaded
--    procedure Destroy (An_Iterator : in out Iterator);
--
--  Iteration over all sum items takes time
--    O (sum * (log n + Complexity of Sets.Current + Complexity of Sets.Next))
--
--  Creation and destruction of a merging iterator takes time
--    O (n * (Log n
--             + Complexity of Sets.Next
--             + Complexity of Sets.Current
--             + Complexity of Sets.Destroy))
--
--  :: Space
--  O (n)
--
--  --> Copying in an array and sorting that array gives
--  time in O (sum * log sum),
--  space in O (sum)
--


with Lists;
with Ordered_Sets;

with Giant.Simple_Priority_Queues;

generic

   ---------------------------------------------------------------------------
   --  Items of 'Merger_Type's are of this type
   type Item_Type is private;

   ---------------------------------------------------------------------------
   --  Strict linear order on 'Item_Type'
   with function "<"
     (Left : in Item_Type; Right : in Item_Type)
     return Boolean;

   ---------------------------------------------------------------------------
   --  The package providing 'Iterator' this is based upon.
   with package Sets is new Ordered_Sets
     (Item_Type => Item_Type,
      "<"       => "<");

package Giant.Merging_Iterators is

   ---------------------------------------------------------------------------
   --  List of Iterators
   package Iterator_Lists is new Lists
     (ItemType => Sets.Iterator);

   ---------------------------------------------------------------------------
   --  The merging iterator type. Can be based on up to 'Number_Of_Iterators'
   --  different 'Sets.Iterator' instances.
   type Merger_Type (Number_Of_Iterators : Natural) is private;

   type Merger_Access is access Merger_Type;


   ------------------------------
   -- Access via access values --
   ------------------------------

   ---------------------------------------------------------------------------
   --  Creates a new merging iterator in the default storage pool and returns
   --  an access value to that merging iterator.
   --
   --  The merging iterator is initialized with a list of iterators.
   --  Iteration can be started immediately. As soon as the merging iterator
   --  is not needed any more, it must be destroyed using 'Destroy'.
   --
   --  The list 'Iterators' and all iterators contained in that list must
   --  be destroyed by the user. The merging iterators stores only copies
   --  of the iterators.
   --
   --  Parameters:
   --    Iterators - List of iterators to merge
   --  Returs:
   --    Access value to a new merging iterator that merges all iterators
   --    contained in 'Iterators'
   --  Complexity:
   --    O (n * (Log n
   --            + Complexity of Sets.Current
   --            + Complexity of Sets.Next)),
   --    n = Length (Iterators)
   function Create
     (Iterators : in Iterator_Lists.List)
     return Merger_Access;

   ---------------------------------------------------------------------------
   --  Returns:
   --    True if a call to 'Get_Current' is legal.
   --  Precondition:
   --    'Merger' must have been created using 'Create' and 'Destroy (Merger)'
   --    must not habe been called.
   --  Complexity:
   --    O (1)
   function Has_More
     (Merger    : access Merger_Type)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Gets the next item from the merging iterator
   --
   --  Parameters:
   --    Merger - the merging iterator
   --  Returns:
   --    An item I. If a call to 'Get_Current (Merger)' for the same 'Merger'
   --    returned an item I_before and if 'Forward (Merger)' was called after
   --    that call but before this call, then I_before < I.
   --  Precondition:
   --    Has_More (Merger)
   --  Complexity:
   --    O (1)
   function Get_Current
     (Merger    : access Merger_Type)
     return Item_Type;

   ---------------------------------------------------------------------------
   --  Sets the merging iterator to the next item
   --
   --  Parameters:
   --    Merger - the merging iterator
   --  Precondition:
   --    Has_More (Merger)
   --  Complexity:
   --    O (log n + Complexity of Sets.Current + Complexity of Sets.Next)
   --    if n = number of iterators 'Merger' is based on
   procedure Forward
     (Merger    : access Merger_Type);

   ---------------------------------------------------------------------------
   --  Destroys a merging iterator. Must be called on every merging iterator
   --  as soon as it is no longer needed.
   --
   --  Parameters:
   --    Merger - Access value to a merging iterator or null
   --  Postcondition:
   --    Merger = null
   --  Complexity:
   --    O (n * Complexity of Sets.Destroy)
   --    if n = number of iterators 'Merger' is based on
   procedure Destroy
     (Merger    : in out Merger_Access);


   -------------------------------
   -- Local Merger_Type-objects --
   -------------------------------

   ---------------------------------------------------------------------------
   --  Adds another iterator this merging iterator is based on. Adds
   --  that iterator only if it can return one (or more) items.
   --
   --  Parameters:
   --    Merger   - the merging iterator
   --    Iterator - An iterator to be merged
   --  Precondition:
   --    'Start_Iteration' must not have been called on 'Merger' and
   --    'Merger' must not have been created by the subprogram 'Create' and
   --    the number of iterators 'Merger' is currently based on must be
   --    smaller than the 'Number_Of_Iterators' given in 'Merger's
   --    declaration.
   --  Postcondition:
   --    'Merger' is based on all iterators it was previously based on and
   --    is based on 'Iterator' as well if 'Sets.More (Iterator)'.
   --  Complexity:
   --    O (log n + Complexity of Sets.Current + Complexity of Sets.Next)
   --    if n = number of iterators 'Merger' is based on
   procedure Add_Iterator
     (Merger   : in out Merger_Type;
      Iterator : in     Sets.Iterator);

   ---------------------------------------------------------------------------
   --  Prepares 'Merger' for the iteration. Finishes the phase in wich
   --  more iterators may be added. 'Add_Iterator (Merger)' must not be
   --  called after a call to this procedure.
   --
   --  Parameters:
   --    Merger - The merging iterator
   --  Complexity:
   --    Same as 'Forward'
   procedure Start_Iteration
     (Merger   : in out Merger_Type);

   ---------------------------------------------------------------------------
   --  Tests if there are more items in an merging iterator to be iterated
   --  over.
   --
   --  Returns:
   --    True if a call to 'Get_Current (Merger)' is allowed, False otherwise
   --  Precondition:
   --    'Start_Iteration (Merger)' must have been called, but not
   --    'Destroy (Merger)'
   --  Complexity:
   --    O (1)
   function Has_More
     (Merger   : in     Merger_Type)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Gets the next item from the merging iterator
   --
   --  Parameters:
   --    Merger - the merging iterator
   --  Returns:
   --    An item I. If a call to 'Get_Current (Merger)' for the same 'Merger'
   --    returned an item I_before and if 'Forward (Merger)' was called after
   --    that call but before this call, then I_before < I.
   --  Precondition:
   --    Has_More (Merger)
   --  Complexity:
   --    O (1)
   function Get_Current
     (Merger   : in     Merger_Type)
     return Item_Type;

   ---------------------------------------------------------------------------
   --  Sets the merging iterator to the next item
   --
   --  Parameters:
   --    Merger - the merging iterator
   --  Precondition:
   --    Has_More (Merger)
   --  Complexity:
   --    O (log n + Complexity of Sets.Current + Complexity of Sets.Next)
   --    if n = number of iterators 'Merger' is based on
   procedure Forward
     (Merger   : in out Merger_Type);

   ---------------------------------------------------------------------------
   --  Destroys a merging iterator. Must be called on every merging iterator
   --  as soon as it is no longer needed.
   --
   --  Parameters:
   --    Merger - Access value to a merging iterator or null
   --  Postcondition:
   --    Merger = null
   --  Complexity:
   --    O (n * Complexity of Sets.Destroy)
   --    if n = number of iterators 'Merger' is based on
   procedure Destroy
     (Merger   : in out Merger_Type);


private

   type Pool_Array is array (Positive range <>) of Sets.Iterator;

   type Single_Iterator is
      record
         Pool_Index : Positive;
         Head       : Item_Type;
      end record;

   function Has_Higher_Priority
     (Left  : in Single_Iterator;
      Right : in Single_Iterator)
     return Boolean;

   package Single_Iterator_Queues is new Simple_Priority_Queues
     (Single_Iterator,
      Has_Higher_Priority);

   type Merger_Type (Number_Of_Iterators : Natural) is
      record
         Current              : Item_Type;
         Current_Is_Available : Boolean := False;
         Next                 : Item_Type;
         Next_Is_Available    : Boolean := False;
         Pool                 : Pool_Array (1 .. Number_Of_Iterators);
         Queue                : Single_Iterator_Queues.Queue_Type
                                  (Number_Of_Iterators);
      end record;

end Giant.Merging_Iterators;
