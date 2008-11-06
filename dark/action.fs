#light

namespace dark
    open libtcodWrapper    
    open dark   
    
    module Action =  
        let private generic_move session movef =
            let player = Session.get_player session
            let map = Session.get_current_map session
            let x,y = movef player
            
            match (Map.get_tile x y map) with
            | Some(t) when (Tile.is_walkable t) = true -> {session with Player = {player with x = (Tile.get_x t); y = (Tile.get_y t)}}
            | _ -> session
           
        let terminate session =
            {session with Status = Terminate}
            
        let move_down session = 
            generic_move session (fun p -> (Character.get_x p, (Character.get_y p) + 1))
            
        let move_down_left session = 
            generic_move session (fun p -> ((Character.get_x p) - 1), (Character.get_y p) + 1)
            
        let move_down_right session = 
            generic_move session (fun p -> ((Character.get_x p) + 1), (Character.get_y p) + 1)
            
        let move_left session =
            generic_move session (fun p -> ((Character.get_x p) - 1, Character.get_y p))
            
        let move_right session = 
            generic_move session (fun p -> ((Character.get_x p) + 1, Character.get_y p))
            
        let move_up session = 
            generic_move session (fun p -> (Character.get_x p, (Character.get_y p) - 1))
            
        let move_up_left session = 
            generic_move session (fun p -> ((Character.get_x p) - 1), (Character.get_y p) - 1)
            
        let move_up_right session =
            generic_move session (fun p -> ((Character.get_x p) + 1), (Character.get_y p) - 1)
        
        let get_predefined_actions () =
            Microsoft.FSharp.Collections.Map.of_list [((KeyCode.TCODK_CHAR, (byte 'q'), false, false, false), terminate);
                                                      ((KeyCode.TCODK_CHAR, (byte 'w'), false, false, false), move_up);
                                                      ((KeyCode.TCODK_KP8, (byte '8'), false, false, false), move_up);
                                                      ((KeyCode.TCODK_KP8, (byte 0), false, false, false), move_up);
                                                      ((KeyCode.TCODK_UP, (byte 0), false, false, false), move_up);
                                                      ((KeyCode.TCODK_CHAR, (byte 's'), false, false, false), move_down);
                                                      ((KeyCode.TCODK_KP2, (byte '2'), false, false, false), move_down);
                                                      ((KeyCode.TCODK_KP2, (byte 0), false, false, false), move_down);
                                                      ((KeyCode.TCODK_DOWN, (byte 0), false, false, false), move_down);
                                                      ((KeyCode.TCODK_CHAR, (byte 'a'), false, false, false), move_left);
                                                      ((KeyCode.TCODK_KP4, (byte '4'), false, false, false), move_left);
                                                      ((KeyCode.TCODK_KP4, (byte 0), false, false, false), move_left);
                                                      ((KeyCode.TCODK_LEFT, (byte 0), false, false, false), move_left);
                                                      ((KeyCode.TCODK_CHAR, (byte 'd'), false, false, false), move_right);
                                                      ((KeyCode.TCODK_KP6, (byte '6'), false, false, false), move_right);
                                                      ((KeyCode.TCODK_KP6, (byte 0), false, false, false), move_right); 
                                                      ((KeyCode.TCODK_RIGHT, (byte 0), false, false, false), move_right);
                                                      ((KeyCode.TCODK_KP7, (byte '7'), false, false, false), move_up_left);
                                                      ((KeyCode.TCODK_KP7, (byte 0), false, false, false), move_up_left);
                                                      ((KeyCode.TCODK_KP9, (byte '9'), false, false, false), move_up_right);
                                                      ((KeyCode.TCODK_KP9, (byte 0), false, false, false), move_up_right); 
                                                      ((KeyCode.TCODK_KP1, (byte '1'), false, false, false), move_down_left);
                                                      ((KeyCode.TCODK_KP1, (byte 0), false, false, false), move_down_left); 
                                                      ((KeyCode.TCODK_KP3, (byte '3'), false, false, false), move_down_right);
                                                      ((KeyCode.TCODK_KP3, (byte 0), false, false, false), move_down_right); ]
            