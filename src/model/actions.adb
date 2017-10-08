with Units; use Units;

package body Actions is

    function Is_Valid (
        This : in Action)
        return Boolean is

        Distance : Natural := Coordinates.Distance (
            Source => This.Source,
            Destination => This.Target);

    begin

        case This.Kind is
            when MOVE =>
                if Distance > 1 then
                    if This.Unit /= VAMPIRE then
                        return False;
                    else
                        if (This.Source.X = This.Target.X or
                            This.Source.Y = This.Target.Y) and
                            Distance = 2
                        then
                            return True;
                        else
                            return False;
                        end if;
                    end if;
                else
                    return True;
                end if;
            when CREATE =>
                if This.Unit /= LEPRECHAUN or Distance /= 1 then
                    return False;
                else
                    return True;
                end if;
            when DESTROY =>
                if This.Unit /= HUMAN or Distance /= 1 then
                    return False;
                else
                    return True;
                end if;
            when INFECT =>
                if This.Unit /= ZOMBIE or Distance /= 1 then
                    return False;
                else
                    return True;
                end if;
            when SPAWN =>
                if Distance > 0 then
                    return False;
                else
                    return True;
                end if;
        end case;
    end Is_Valid;

end Actions;
