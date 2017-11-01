with Lobby;
with Boards;
with Actions;

package VL is
    type Player_Status is (EMPTY, ACTIVE, COMMITTED, QUIT);
    type Player is
        record
            Status : Player_Status;
            Team : Natural;
        end record;
    type Player_Array is array (Positive range <>) of Player;

    type VL_State (
        Width, Height, Num_Players : Positive;
        Num_Actions : Natural) is record
            Board : Boards.Board (Width, Height);
            Players : Player_Array (1 .. Num_Players);
            Last_Actions : Actions.Action_Array (1 .. Num_Actions);
        end record;
    type VL_State_Access is access VL_State;
    procedure Free_State (State : in out VL_State_Access);

    protected type VL_Notifier is
        entry Query (Changed : out Boolean);
        entry Get_Dimensions (
            Width : out Positive;
            Height : out Positive;
            Num_Players : out Positive;
            Num_Actions : out Natural);
        entry Get_State (State : out VL_State);

        procedure Notify (State : in VL_State);
    private
        Has_Changed : Boolean := False;
        Last_State : VL_State_Access := null;
    end VL_Notifier;
    type VL_Notifier_Access is access all VL_Notifier;

    type VL_Callback is access procedure (State : in VL_State);

    type Listener_Node;
    type Listener_Node_Ptr is access Listener_Node;
    type Listener_Node is
        record
            Callback : VL_Callback;
            Next : Listener_Node_Ptr;
        end record;

    type Notifier_Node;
    type Notifier_Node_Ptr is access Notifier_Node;
    type Notifier_Node is
        record
            Notifier : VL_Notifier_Access;
            Next : Notifier_Node_Ptr;
        end record;
    type Action_List_Ptr is access Actions.Action_Array;
    type Uncommitted_Actions is array (Positive range <>) of
        Action_List_Ptr;
    type Uncommitted_Actions_Ptr is access Uncommitted_Actions;

    protected type VL_Game is
        -- Lifespan Entries
        entry Create (Target : in Boards.Board);
        entry Destroy;

        -- Observer Entries
        entry Attach_Listener (What : in VL_Callback);
        entry Attach_Notifier (Notifier : in VL_Notifier_Access);

        -- Controller Entries
        entry Join (Team : out Natural);
        entry Leave (Team : in Natural);
        entry Commit (Which : in Actions.Action_Array; Team : in Natural);
        entry Uncommit (Team : in Natural);
        entry Set (New_State : in VL_State);
    private
        Listeners : Listener_Node_Ptr := null;
        Notifiers : Notifier_Node_Ptr := null;

        Current_Actions : Uncommitted_Actions_Ptr;

        State : VL_State_Access := null;
        Created : Boolean := False;
        Destroyed : Boolean := False;
    end VL_Game;

    task VL_Manager is
        -- Lifespan Entries
        entry Detach;
        entry Kill; -- Used to end task entirely

        -- Initializers
        entry Host (Which : in Lobby.Lobby_Element);
        entry Join (Which : in Lobby.Lobby_Element);

        -- Interactors
        entry Commit (Which : in Actions.Action_Array);
        entry Uncommit;

        entry Set_Notifier (Notifier : in VL_Notifier_Access);
    end VL_Manager;
end VL;
