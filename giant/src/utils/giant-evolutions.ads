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
--  $RCSfile: giant-evolutions.ads,v $, $Revision: 1.14 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  This package provides a framework for user-cancelable calculations.
--
--  Each such calculation is considered an Individual that is created in
--  an initial state, then undergoes a certain evolution process until
--  the calculation is completed. During the evolution process, a dialog
--  box is shown to the user. In that box the progress of the Evolution
--  is reported to the user. Inside the box the user can hit the
--  cancel-button in order to stop the evolution before it has finished.
--
--  Every Individual is represented as an instance of any subclass of the
--  tagged type 'Evolution'. The evolution process is modeled by subsequent
--  calls to the primitive subprogram 'Step' or 'Synchronized_Step'.
--
--  Two different types of evolutions are distinguished:
--  * 'Concurrent_Evolution'
--    Individuals of that class can run in a task parallel to the GtkAda
--    main task. During normal running they must not execute any action
--    that might affect other tasks. From time to time these Individuals
--    can perform an action that is synchronized within the GtkAda main
--    task.
--  * 'Iterative_Evolution'
--    Individuals of that class are run within the GtkAda main task. They
--    need not care about synchronization.
--    The dialog box is modal so no other function of the system should be
--    available. Functions accessible by other means than GtkAda must be
--    locked explicitely. The iterative evolution is interrupted after each
--    step and the control resource is given back to GtkAda. Thus GtkAda
--    is still able to receive the cancel hit-event.
--


with Ada.Real_Time;

with Glib;
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Giant.Progress_Dialog;


package Giant.Evolutions is

   ---------------------------------------------------------------------------
   --  Maximum Number of slots. Each concurrent evolution process uses one
   --  slot. Thus the number of concurrently running evolutions is limited
   --  by this number.
   Number_Of_Slots          : constant :=  10;

   ---------------------------------------------------------------------------
   --  Delay time between two updates of a progress dialog. At each update
   --  only one window will be updated, so each process has a delay time
   --  of this constant multiplyed by the number of running processes.
   Poll_Delay_Milli_Seconds : constant := 1000;

   ---------------------------------------------------------------------------
   --  There seem to be some problems with idle signals emitted by GtkAda.
   --  This exception is raised if the disconnection of a handler for such
   --  a signal failed.
   Gtk_Idle_Disconnect_Failed : exception;

   ---------------------------------------------------------------------------
   --  All types of atomic operations of an Evolution
   --  * 'Run':         Perform the calculation. 'Step' will be called.
   --  * 'Synchronize': Perform an action within a synchronized environment
   --                   (only applies to instances of 'Concurrent_Evolution'
   --  * 'Cancel':      Stop the Calculation, 'Finish' will be called.
   --  * 'Finish':      Calculation complete, 'Finish' will be called.
   type Evolution_Action is (Run, Synchronize, Cancel, Finish);


   --------------------------------------
   -- Tagged type Evolution (abstract) --
   --------------------------------------

   ---------------------------------------------------------------------------
   --  Evolution base type. Do not subclass this type directly! Use
   --  'Concurrent_Evolution' or 'Iterative_Evolution'.
   type Evolution is abstract tagged limited private;

   type Evolution_Class_Access is access all Evolution'Class;

   ---------------------------------------------------------------------------
   --  Initializes 'Individual'. Must be called by children of this tagged
   --  type.
   --
   --  Parameters:
   --    Individual - Individual to be initialized
   --    Complexity - Estimate of the complexity of this calculation or 0
   --                 if a sensible estimate is not available.
   procedure Initialize
     (Individual : access Evolution;
      Complexity : in     Natural := 0);

   ---------------------------------------------------------------------------
   --  Performs one atomic step in 'Individual's calculation. Before and
   --  after this step 'Individual' is in a state in which it can be
   --  finished by a call to 'Finish'.
   --
   --  Depending on the actual type of Individual more constraints are
   --  specified. See description of type 'Concurrent_Evolution' and
   --  'Iterative_Evolution' for details.
   --
   --  'Step' must set 'Next_Action' to the desired value. See type
   --  'Evolution_Action' for reference.
   --
   --  !This procedure should not be called from outside this package!
   --
   --  Parameters:
   --    Individual  - Subject of the evolution process
   --    Next_Action - Determines the action to be performed next
   procedure Step
     (Individual  : access Evolution;
      Next_Action :    out Evolution_Action) is abstract;

   ---------------------------------------------------------------------------
   --  Performs one atomic step in 'Individual's calculation. Before and
   --  after this step 'Individual' is in a state in which it can be
   --  finished by a call to 'Finish'. In difference to the procedure 'Step'
   --  this procedure is always executed inside a task synchronized with
   --  GtkAda.
   --
   --  An Individual must apply for a call to this procedure by setting
   --  the value of the out-Parameter 'Next_Action' of the procedure
   --  'Step' to 'Synchronize'. It might take a lot of time until the call
   --  is actually made (up to several seconds). So this should be used
   --  only when really necessary.
   --
   --  'Synchronized_Step' must set 'Next_Action' to the desired value.
   --  See type 'Evolution_Action' for reference.
   --
   --  The default implementation of this procedure does nothing but set
   --  'Next_Action' to 'Run'.
   --
   --  !This procedure should not be called from outside this package!
   --
   --  Parameters:
   --    Individual  - Subject of the evolution process
   --    Next_Action - Determines the action to be performed next
   procedure Synchronized_Step
     (Individual  : access Evolution;
      Next_Action :    out Evolution_Action);

   ---------------------------------------------------------------------------
   --  Will be called after the last call to 'Step' or 'Synchronized_Step'
   --  by this package.
   --
   --  Is run synchronized with Gtk even if is multi-tasking is possible
   --  for 'Individual'. Should free System resources and perform any action
   --  that is necessary to make the calculation's result useful to the
   --  application (eg. release locks, show windows, ...)
   --
   --  May destroy 'Individual'. The access value 'Individual' will not be
   --  used anymore within this package.
   --
   --  Parameters:
   --    Individual - Subject of the evolution process
   --    Canceled   - True if the Calculation was canceled by user action
   --                 or by 'Individuals's own request.
   --                 False if 'Individual' requested 'Finish' (by setting
   --                 'Next_Action').
   procedure Finish
     (Individual : access Evolution;
      Canceled   : in     Boolean) is abstract;

   ---------------------------------------------------------------------------
   --  Starts the evolution of 'Individual' if enough slots are available
   --  (see 'Number_Of_Slots' above). If not then does nothing. In this
   --  case another call to this function will only be successful after a
   --  different currently running evolution has been finished and its
   --  execution environment is cleared from memory. It may take some time
   --  (up to several seconds) after the call to 'Finish' in one Individual
   --  and the removal of its execution environment from memory.
   --
   --  If the evolution starts then the 'Dialog' will be shown and updated
   --  from time to time if one is provided.
   --
   --  If 'Dialog' is provided then 'Progress_Dialog.Destroy (Dialog)' will
   --  be called after the evolution has been canceled or has finished.
   --
   --  Parameters:
   --    Individual      - Subject of the evolution process
   --    Dialog          - Dialog to monitor the progress of the evolution
   --                      or null.
   --    Started         - Set to True if the evolution of 'Individual'
   --                      started, set to False if there are not enough
   --                      resources to start the evolution.
   procedure Start_Calculation
     (Individual : access Evolution;
      Dialog     : in     Progress_Dialog.Progress_Dialog_Access := null;
      Started    :    out Boolean) is abstract;

   ---------------------------------------------------------------------------
   --  Sets the complexity of this 'Individuals's evolution to 'Total_Number'
   --  items. If the complexity is unknown, this procedure should either
   --  never be called or 0 should be set.
   --
   --  If the complexity is set to a value different to 0 then the complexity
   --  should at any given point of time be greater or equal to the progress
   --  counter accessed through 'Advance_Progress'.
   --
   --  The complexity is interrogated from a multi-tasking environment without
   --  synchronization. Each Individual must through the order of calls to
   --  'Set_Complexity' and 'Advance_Progress' ensure that the combination of
   --  progress counter and complexity is meaningful to the user at any
   --  point of time.
   --
   --  Parameters
   --    Individual   - Subject of the evolution process
   --    Total_Number - Number of items processed when this evolution has
   --                   finished, or 0 if unknown
   procedure Set_Complexity
     (Individual   : access Evolution'Class;
      Total_Number : in     Natural);

   ---------------------------------------------------------------------------
   --  Should only be called inside 'Step' or 'Synchronized_Step'. Increases
   --  the progress counter of this 'Individual' by 'Progress'.
   --
   --  Parameters:
   --    Individual - Subject of the evolution progress
   --    Progress   - Number of items recently processed
   procedure Advance_Progress
     (Individual : access Evolution'Class;
      Progress   : in     Natural);

   ---------------------------------------------------------------------------
   --  Returns the number of processed items stored in the progress counter.
   --  Initially the progress counter is 0 and will be increased during
   --  calculation through calls to 'Advance_Progress'.
   function Get_Progress_Count
     (Individual   : access Evolution'Class)
     return Natural;

   ---------------------------------------------------------------------------
   --  Returns the total complexity of the calculation measured in number
   --  of items to be processed. If the complexity is unknown, than this
   --  function returns 0.
   --  If the complexity is non-zero then the calculation might finish
   --  when 'Get_Progress_Count = Get_Complexity' although this is NOT assured.
   function Get_Complexity
     (Individual   : access Evolution'Class)
     return Natural;

   ----------------------------------------------------------------------------
   --  Returns a format string to be displayed in a Progress_Dialog if the
   --  complexity of 'Individual' is known. Should be overwritten by
   --  descendants to show a more meaningful text.
   --
   --  MUST be able to be reentrant from different tasks. Usually should
   --  return a constant String
   --
   --  The following Texts can be replaced inside the String:
   --    %v - Natural'Image (Get_Progress_Count (Individual))
   --    %u - Natural'Image (Get_Complexity (Individual))
   --    %p - current progress percentage
   --
   --  Precondition
   --    Get_Complexity (Individual) > 0
   --  Returns:
   --    "%v " & (-"of") & " %u"
   function Get_Progress_Text_Showing_Complexity
     (Individual : access Evolution)
     return String;

   ----------------------------------------------------------------------------
   --  Returns a format string to be displayed in a Progress_Dialog if the
   --  complexity of 'Individual' is unknown. Should be overwritten by
   --  descendants to show a more meaningful text.
   --
   --  MUST be able to be reentrant from different tasks. Usually should
   --  return a constant String.
   --
   --  The following Texts can be replaced inside the String:
   --    %v - Natural'Image (Get_Progress_Count (Individual))
   --
   --  Precondition
   --    Get_Complexity (Individual) = 0
   --  Returns:
   --    "%v"
   function Get_Progress_Text_Unknown_Complexity
     (Individual : access Evolution)
     return String;


   -------------------------------------------------
   -- Tagged type Concurrent_Evolution (abstract) --
   -- derived from Evolution                      --
   -------------------------------------------------

   ---------------------------------------------------------------------------
   --  Individuals of this class can be run in a multi-tasking environment.
   --  They affect other concurrently executed tasks only in defined and
   --  desired ways.
   type Concurrent_Evolution is abstract new Evolution with private;

   type Concurrent_Evolution_Class_Access is
     access all Concurrent_Evolution'Class;

   ---------------------------------------------------------------------------
   --  procedure Step
   --    (Individual  : access Evolution;
   --     Next_Action : in     Evolution_Action) is abstract;
   --
   --  See description of type 'Evolution' for details. Only the constraints
   --  specific to the type 'Concurrent_Evolution' are stated here:
   --
   --  Since 'Individual' can be used in a multi-tasking environment,
   --  the execution of this procedure must not affect other tasks.
   --  This procedure is NOT synchronized with GtkAda.

   ---------------------------------------------------------------------------
   --  Derived from 'Evolution'. Starts an 'Concurrent_Evolution' in
   --  multi-tasking mode.
   --
   --  This procedure returns immediately (does not wait until the evolution
   --  has finished).
   --
   --  Parameters:
   --    Individual      - Subject of the evolution process
   --    Dialog          - Dialog to monitor the progress of 'Individual'
   --                      will be shown before the beginning of the evolution
   --                      and destroyed after the finish of the evolution.
   --                      May be null.
   --    Started         - Set to True if the evolution of 'Individual'
   --                      started, set to False if there are not enough
   --                      resources to start the evolution.
   procedure Start_Calculation
     (Individual : access Concurrent_Evolution;
      Dialog     : in     Progress_Dialog.Progress_Dialog_Access := null;
      Started    :    out Boolean);

   ---------------------------------------------------------------------------
   --  Freezes 'Master's evolution process and starts evolution of
   --  'Sub_Calculation'. Uses the same progress dialog used for 'Master'
   --  If 'Sub_Calculation' gets canceled, then the Master will be canceled,
   --  too. If 'Sub_Calculation' finishes, then the Master will be woken up
   --  and continue its evolution process.
   --
   --  Since Sub_Calculation can reuse the 'Master's slot, this procedure
   --  never fails in contrast to 'Start_Calculation'. This procedure
   --  returns immediately, without waiting for the evolution of
   --  'Sub_Calculation' to finish.
   --
   --  'Sub_Calculation' must be derived from 'Concurrent_Evolution' and is
   --  thus able to be run in multi-tasking mode.
   --  There is another 'Start_Sub_Calculation' for 'Iterative_Evolution'
   --  as well.
   --
   --  Parameters:
   --    Master          - An Individual that is currently evolving.
   --    Sub_Calculation - An Individual that should be evolved before
   --                      'Master' can continue its evolution
   --  Precondition:
   --    'Master's evolution has been started by a call to
   --    'Start_Calculation' and 'Start_Calculation' has set the 'Started'
   --    Argument to True
   --  Postcondition:
   --    * For 'Master' no call to 'Step' or 'Synchronized_Step' will be made
   --      until 'Finish (Sub_Calculation, False)' has been called.
   --    * If 'Finish (Sub_Calculation, True)' is called then
   --      'Finish (Master, True)' will be called as well.
   procedure Start_Sub_Calculation
     (Master          : access Concurrent_Evolution'Class;
      Sub_Calculation : access Concurrent_Evolution'Class);


   ------------------------------------------------
   -- Tagged type Iterative_Evolution (abstract) --
   -- derived from Evolution                     --
   ------------------------------------------------

   ---------------------------------------------------------------------------
   --  Individuals of this class are run inside an idle-event of GtkAda.
   type Iterative_Evolution is abstract new Evolution with private;

   type Iterative_Evolution_Class_Access is
     access all Iterative_Evolution'Class;


   ---------------------------------------------------------------------------
   --  Derived from 'Evolution'. Starts an 'Iterative_Evolution' in
   --  single-tasking mode.
   --
   --  This procedure returns immediately (does not wait until the evolution
   --  has finished).
   --
   --  Parameters:
   --    Individual      - Subject of the evolution process
   --    Dialog          - Dialog to monitor the progress of 'Individual'
   --                      will be set to modal and shown before the beginning
   --                      of the evolution and destroyed after the finish
   --                      of the evolution. May be null.
   --    Started         - Set to True if the evolution of 'Individual'
   --                      started, set to False if an 'Iterative_Evolution'
   --                      is already being processed.
   procedure Start_Calculation
     (Individual : access Iterative_Evolution;
      Dialog     : in     Progress_Dialog.Progress_Dialog_Access := null;
      Started    :    out Boolean);

   ---------------------------------------------------------------------------
   --  Freezes 'Master's evolution process and starts evolution of
   --  'Sub_Calculation'. Uses the same progress dialog used for 'Master'
   --  If 'Sub_Calculation' gets canceled, then the Master will be canceled,
   --  too. If 'Sub_Calculation' finishes, then the Master will be woken up
   --  and continue its evolution process.
   --
   --  Since 'Sub_Calculation' can reuse the 'Master's slot, this procedure
   --  never fails in contrast to 'Start_Calculation'. This procedure
   --  returns immediately, without waiting for the evolution of
   --  'Sub_Calculation' to finish.
   --
   --  'Sub_Calculation' is always run in synchronized mode, even if it was
   --  able to handle multi-tasking.
   --  There is a 'Start_Sub_Calculation' that deals only with
   --  'Concurrent_Evolution's.
   --
   --  Parameters:
   --    Master          - An Individual that is currently evolving.
   --    Sub_Calculation - An Individual that should be evolved before
   --                      'Master' can continue its evolution
   --  Precondition:
   --    'Master's evolution has been started by a call to
   --    'Start_Calculation' and 'Start_Calculation' has set the 'Started'
   --    Argument to True
   --  Postcondition:
   --    * For 'Master' no call to 'Step' or 'Synchronized_Step' will be made
   --      until 'Finish (Sub_Calculation, False)' has been called.
   --    * If 'Finish (Sub_Calculation, True)' is called then
   --      'Finish (Master, True)' will be called as well.
   procedure Start_Sub_Calculation
     (Master          : access Iterative_Evolution'Class;
      Sub_Calculation : access Evolution'Class);

   ---------------------------------------------------------------------------
   --  Can be used to test evolutions without tasking.
   procedure Start_Calculation_Blocked
     (Individual : access Evolution'Class);

                              ------------------
private                       -- Private Part --
                              ------------------

   --  Initial value for Handler_Ids
   Null_Handler : constant Gtk.Handlers.Handler_Id :=
     (Signal  => Glib.Null_Signal_Id,
      Closure => null);

   -------------------------
   -- Private subprograms --
   -- for Evolution       --
   -------------------------

   function Logging_Name
     (Individual : access Evolution'Class)
     return String;


   function Validate
     (Individual : access Evolution'Class)
     return Boolean;


   procedure Update_Visuals
     (Individual : access Evolution'Class;
      Dialog     : in     Progress_Dialog.Progress_Dialog_Access := null);


   procedure Add_Child_Progress
     (Individual    : access Evolution'Class;
      Progress      : in     Natural);


   procedure Done
     (Individual    : access Evolution'Class;
      Canceled      : in     Boolean);


   procedure Set_Next_Action
     (Individual  : access Evolution'Class;
      Next_Action : in     Evolution_Action);

   function Get_Next_Action
     (Individual  : access Evolution'Class)
     return Evolution_Action;


   procedure Set_Parent
     (Individual : access Evolution'Class;
      Parent     : Evolution_Class_Access);

   function Get_Parent
     (Individual : access Evolution'Class)
     return Evolution_Class_Access;

   function Has_Parent
     (Individual : access Evolution'Class)
     return Boolean;


   procedure Set_Child
     (Individual : access Evolution'Class;
      Child      : in     Evolution_Class_Access);

   function Get_Child
     (Individual : access Evolution'Class)
     return Evolution_Class_Access;

   function Has_Child
     (Individual : access Evolution'Class)
     return Boolean;


   ------------------------------
   -- Private subprograms      --
   -- for Concurrent_Evolution --
   ------------------------------

   procedure Set_Concurrent_Child
     (Individual : access Concurrent_Evolution'Class;
      Child      : in     Concurrent_Evolution_Class_Access);

   function Get_Concurrent_Child
     (Individual : access Concurrent_Evolution'Class)
     return Concurrent_Evolution_Class_Access;

   procedure Set_Concurrent_Parent
     (Individual : access Concurrent_Evolution'Class;
      Parent     : in     Concurrent_Evolution_Class_Access);

   function Get_Concurrent_Parent
     (Individual : access Concurrent_Evolution'Class)
     return Concurrent_Evolution_Class_Access;


   ---------------
   -- Framework --
   ---------------

   subtype Driver_Position is Positive range 1 .. Number_Of_Slots;
   type Driver_Id_Type is new Driver_Position;

   --  States a driver can be in. MUST be ordered to reflect a priority of
   --  drivers. Enumeration_Literals that appear first have a higher
   --  priority.
   type Driver_Action_Type is
     (Dead, Waiting_On_Sync, Running);


   -----------------------
   -- Concurrent_Driver --
   -----------------------

   task type Concurrent_Driver
     (Id                 : Driver_Id_Type;
      Driven_Calculation : access Concurrent_Evolution'Class) is

      entry Synchronize;
   end Concurrent_Driver;

   type Concurrent_Driver_Access is access Concurrent_Driver;

   type Driver_State_Type;
   type Driver_State_Access is access Driver_State_Type;
   type Driver_State_Type is
      record
         Driver          : Concurrent_Driver_Access;
         --  Evolution instance, set to null after 'Driver' has died
         Individual      : Concurrent_Evolution_Class_Access;
         --  Position in 'Priority_Queue'
         Position        : Natural;
         --  State 'Driver' is currently in
         Current_State   : Driver_Action_Type;
         --  Point of time when 'Driver' last changed its state
         State_Change    : Ada.Real_Time.Time;
         --  Set to True if and only if the user has requested the evolution
         --  to be cancelled
         Cancel_Request  : Boolean;
         --  Progress-monitoring dialog or null
         Dialog          : Progress_Dialog.Progress_Dialog_Access;
         --  Callback-binding for 'Progress_Dialog'
         Cancel_Handler  : Gtk.Handlers.Handler_Id;
      end record;

   No_Driver_State : constant Driver_State_Type :=
     (Driver          => null,
      Individual      => null,
      Position        => 0,
      Current_State   => Running,
      State_Change    => Ada.Real_Time.Time_Last,
      Cancel_Request  => False,
      Dialog          => null,
      Cancel_Handler  => Null_Handler);

   --  Priority ordering of 'Driver_State_Type's. Orders according to
   --  'Current_State', if equal then earlier 'State_Change' will decide.
   function Has_Higher_Priority
     (Left  : in Driver_State_Type;
      Right : in Driver_State_Type)
     return Boolean;


   ----------------------
   -- Iterative_Driver --
   ----------------------

   type Iterative_Driver_State_Type is
      record
         --  Evolution instance, set to null after 'Driver' has died
         Individual      : Evolution_Class_Access;
         --  Point of time when visuals were last updated
         Update_Time     : Ada.Real_Time.Time;
         --  Set to True if and only if the user has requested the evolution
         --  to be cancelled
         Cancel_Request  : Boolean;
         --  A monitoring dialog or null
         Dialog          : Progress_Dialog.Progress_Dialog_Access;
         --  Callback-binding for 'Progress_Dialog'
         Cancel_Handler  : Gtk.Handlers.Handler_Id;
         --  Handler id for the idle event
--         Idle_Handler    : Gtk.Main.Idle_Handler_Id;
      end record;

   No_Iterative_Driver_State : constant Iterative_Driver_State_Type :=
     (Individual      => null,
      Update_Time     => Ada.Real_Time.Time_Last,
      Cancel_Request  => True,
      Dialog          => null,
      Cancel_Handler  => Null_Handler);
--,
--      Idle_Handler    => 0);


   ----------------------------------
   -- Type declaration completions --
   ----------------------------------

   type Evolution is abstract tagged limited
      record
         --  Number of items processed
         Progress_Count   : Natural                           := 0;
         --  Estimate of the maximum value 'Progress_Count' will ever reach.
         --  Does not need to be precise, 0 if unknown
         Complexity       : Natural                           := 0;
         --  Number of items processed in sub-calculations
         Child_Progress   : Natural                           := 0;
         --  next action this evolution needs to perform. Must be set
         --  via 'Set_Next_Action'.
         Next_Action      : Evolution_Action                  := Cancel;
         --  Parent evolution to be continued after this has finished or null
         Parent           : Evolution_Class_Access            := null;
         --  Child evolution to be evolved before this can continue or null
         Child            : Evolution_Class_Access            := null;
      end record;

   type Concurrent_Evolution is abstract new Evolution with null record;

   type Iterative_Evolution is abstract new Evolution with null record;


end Giant.Evolutions;
