with Actions;
with SDL;
with Tiles;
with Units;

package Sprites is
    procedure Initialize;
    procedure Finalize;

    Unit_Sprites : array (Units.Unit) of SDL.Image;
    Glow_Sprites : SDL.Image;
    Base_Sprites : SDL.Image;
    function Unit_Sprite_Clips (Team : in Natural) return SDL.Rectangle;

    Tile_Sprites : SDL.Image;
    function Tile_Sprite_Clips (Which : in Tiles.Tile_Kind)
        return SDL.Rectangle;

    Arrow_Sprites : SDL.Image;
    function Arrow_Sprite_Clips (Which : in Actions.Action_Kind)
        return SDL.Rectangle;

    Spawn_Sprites : SDL.Image;
    function Spawn_Sprite_Clips (Which : in Units.Unit) return SDL.Rectangle;

    Button_Sprites : SDL.Image;
    Button_Normal_Clip, Button_Hover_Clip, Button_Pressed_Clip : SDL.Rectangle;
end Sprites;
