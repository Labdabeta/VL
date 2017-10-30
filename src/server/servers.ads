with Lobby;
with Boards;
with Actions;

package Servers is
    type Player_Activity is (
        EMPTY, ACTIVE, COMMITTED, DISCONNECTED, QUIT, SELF);
    type Player_Status is
        record
            Team : Natural;
            Activity : Player_Activity;
        end record;
    type Player_Status_Array is array (Positive range <>) of Player_Status;

    --  Used as a central datastore for servers
    task type Server is
        entry Create (Board : in Boards.Board);
        entry Connect (Who : out Natural);
        entry Disconnect (Who : in Natural);
        entry Get_Dimensions (
            Width : out Positive;
            Height : out Positive;
            Player_Count : out Positive);
        entry Commit (Which : in Actions.Action_Array; Who : in Natural);
        entry Uncommit (Who : in Natural);

        entry Check (Changed : out Boolean);
        --  Only returns when something changes. Stop querying if you have quit!
        entry Wait_For_Change;
        entry Query (Who : in Natural;
            Board : out Boards.Board;
            Statuses : out Player_Status_Array);
        entry Quit (Who : in Natural);
        entry Kill; -- Only call when you are the only one holding the task!
    end Server;
end Servers;
