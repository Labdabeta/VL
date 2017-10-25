with Boards;

package Maps is
    type Map;
    type Map_Ptr is access Map;
    type Map is
        record
            Contents : Boards.Board_Ptr;
            Name : String (1 .. 120);
            Name_Length : Natural;
            Prev, Next : Map_Ptr;
        end record;

    function Load_Maps return Map;
    procedure Free_Maps (Which : in out Map);

    function Create_Map (
        Name : String;
        Width : Positive;
        Height : Positive) return Map;

    procedure Next (This : in out Map);
    procedure Prev (This : in out Map);
    procedure Save (This : in Map);
end Maps;
