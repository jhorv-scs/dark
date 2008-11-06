#light

namespace dark
    
    type Character = {
        x : int;
        y : int;
        Display: char}
        
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]   
    module Character =
        let get_display_char character = 
            character.Display
    
        let get_x character = 
            character.x
            
        let get_y character = 
            character.y