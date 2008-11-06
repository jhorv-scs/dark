#light

namespace dark
    type Tile = {
            x : int;
            y : int;
            CanSee : bool;
            CanWalk : bool;
            Display : char;}

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]            
    module Tile =        
        let get_display_char tile =
            tile.Display            
            
        let get_x tile =
            tile.x
            
        let get_y tile = 
            tile.y

        let get_tile x y tiles =
            List.first (function | t when (get_x t) = x && (get_y t) = y -> Some(t) | _ -> None) tiles

        let get_door x y =
            {x = x; y = y; CanSee = false; CanWalk = false; Display = '+'}
            
        let get_door_secret x y = 
            {x = x; y = y; CanSee = false; CanWalk = false; Display = 'S'}
            
        let get_floor x y =
            {x = x; y = y; CanSee = true; CanWalk = true; Display = '.'}
            
        let get_rock x y = 
            {x = x; y = y; CanSee = false; CanWalk = false; Display = ' '}
            
        let get_wall x y =
            {x = x; y = y; CanSee = false; CanWalk = false; Display = '#'}

        let is_floor tile = 
            (get_display_char tile) = '.'
            
        let is_seeable tile = 
            tile.CanSee
            
        let is_rock tile = 
            (get_display_char tile) = ' '
            
        let is_walkable tile = 
            tile.CanWalk
            
        let is_wall tile = 
            (get_display_char tile) = '#'