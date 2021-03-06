with Actions;
with Units;
with Tiles;
with SDL;

package Paths is
    Unit_Sprites : constant array (Units.Unit) of String (1 .. 12) := (
        Units.VAMPIRE => "../res/v.png",
        Units.LEPRECHAUN => "../res/l.png",
        Units.HUMAN => "../res/h.png",
        Units.ZOMBIE => "../res/z.png",
        Units.FAIRY => "../res/f.png");
    Glow_Sprites : constant String := "../res/glow.png";
    Base_Sprites : constant String := "../res/bases.png";
    Tile_Sprites : constant String := "../res/tiles.png";
    Arrow_Sprites : constant String := "../res/arrows.png";
    Spawn_Sprites : constant String := "../res/spawn.png";
    Font_TTF : constant String := "../res/font.ttf";
    Button_Sprites : constant String := "../res/button.png";
    List_File : constant String := "../maps/list.txt";
    Map_Prefix : constant String := "../maps/";
    Text_Box_Sprites : constant String := "../res/boxes.png";
    Text_List_Sprites : constant String := "../res/lists.png";
    Background_Sprite : constant String := "../res/background.png";
end Paths;
