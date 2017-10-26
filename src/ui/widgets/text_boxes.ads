with SDL;

package Text_Boxes is
    type Text_Box is private;
    function Create (
        Hint : String;
        Max_Length : Positive;
        Enabled : Boolean) return Text_Box;

    procedure Free (This : in out Text_Box);

    function Get_Content (This : Text_Box) return String;

    procedure Set_Area (
        This : in out Text_Box;
        Area : in SDL.Rectangle);

    procedure Process_Event (
        This : in out Text_Box;
        What : in SDL.Event);

    procedure Draw (This : Text_Box);

    procedure Reset (This : in out Text_Box);
private
    type Text_Box is
        record
            Contents : String (1 .. 120);
            Length : Natural;
            Max_Length : Natural;
            Area : SDL.Rectangle;
            Is_Active : Boolean;
            Hint : String (1 .. 120);
            Hint_Length : Natural;
            Current : SDL.Image;
        end record;

end Text_Boxes;
