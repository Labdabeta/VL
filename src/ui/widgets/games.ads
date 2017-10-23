with Actions;
with Boards;
with SDL;

package Games is
    Max_Players : constant := 255;

    procedure Initialize;
    procedure Finalize;

    procedure Draw_Board (
        Which : in Boards.Board;
        Where : in SDL.Rectangle);

    procedure Draw_Actions (
        Reference : in Boards.Board;
        Which : in Actions.Action_Array;
        Where : in SDL.Rectangle);
end Games;
