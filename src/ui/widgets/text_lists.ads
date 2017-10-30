with SDL;

package Text_Lists is
    --  The dimensions are characters and lines respectively, not pixels
    type Text_List (Width, Height : Positive) is private;

    function Create (
        Width : Positive;
        Height : Positive) return Text_List;

    procedure Set_Area (
        This : in out Text_List;
        Area : in SDL.Rectangle);

    procedure Free (This : in out Text_List);

    --  Returns 0, or the vertical index of the pressed item
    function Process_Event (
        This : in out Text_List;
        What : in SDL.Event)
        return Natural;

    procedure Draw (This : Text_List);

    procedure Set_Line (
        This : in out Text_List;
        Which : in Positive;
        Data : in String);
private
    type Image_Array is array (Positive range <>) of SDL.Image;
    type Character_Display is array (Positive range <>, Positive range <>) of
        Character;
    type Boolean_Array is array (Positive range <>) of Boolean;
    type Text_List (Width, Height : Positive) is
        record
            Area : SDL.Rectangle;
            Overlays : Image_Array (1 .. Height);
            Was_Pressed : Boolean_Array (1 .. Height);
        end record;
end Text_Lists;
