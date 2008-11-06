#light

namespace dark
    open dark
    
    type World = { 
        Map : Map;}

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]   
    module World =
        let get_map world = 
            world.Map
