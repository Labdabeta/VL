with SDL;

package Colours is
    Grey : constant SDL.Colour := (
        A => SDL.Colour_Component'Last,
        R => 128,
        G => 128,
        B => 128);
end Colours;
