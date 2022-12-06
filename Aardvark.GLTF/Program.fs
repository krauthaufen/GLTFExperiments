open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.GLTF
open Aardvark.Application


module Shader =
    open FShade
    let tcColor (v : Effects.Vertex) =
        fragment {
            return V4d(v.tc, 1.0, 1.0)
        }
 
[<EntryPoint>]
let main _args = 
    Aardvark.Init()
         
    let model =
        GLTF.loadScene "/Users/schorsch/Downloads/x-wing_-_starwars_starship.glb"
        
    let sg =
        Scene.toSimpleSg model
        |> Sg.scale 20.0
        |> Sg.trafo' (Trafo3d.RotationX Constant.PiHalf)
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.diffuseTexture
            do! DefaultSurfaces.simpleLighting
        }
        |> Sg.diffuseTexture DefaultTextures.checkerboard
        
    show {
        scene sg
    }
    0