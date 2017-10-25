with SDL;

package Text_Boxes is
    type Text_Box is
        record
            Contents : String (1 .. 120);
            Length : Natural;
            Area : SDL.Rectangle;
            Is_Active : Boolean;
            Hint : String (1 .. 120);
            Hint_Length : Natural;
            Current : SDL.Image;
        end record;

    function Create_Text_Box (
        Hint : String;
        Enabled : Boolean) return Text_Box;

    procedure Process_Event (
        This : in out Text_Box;
        What : in SDL.Event);

    procedure Draw (This : Text_Box);

    procedure Reset (This : in out Text_Box);
end Text_Boxes;
