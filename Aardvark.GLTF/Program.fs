open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.GLTF
open Aardvark.Application
open FSharp.Data.Adaptive


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
        GLTF.loadScene "/Users/schorsch/Downloads/millennium_falcon.glb"
        
    let white =
        let img = PixImage<byte>(Col.Format.RGBA, V2i.II)
        img.GetMatrix<C4b>().Set(C4b.White) |> ignore
        PixTexture2d(PixImageMipMap [| img :> PixImage  |], TextureParams.empty) :> ITexture |> AVal.constant
        
        
    let sg =
        Scene.toSimpleSg model
        |> Sg.scale 0.05
        |> Sg.trafo' (Trafo3d.RotationX Constant.PiHalf)
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.diffuseTexture
            do! DefaultSurfaces.simpleLighting
        }
        |> Sg.diffuseTexture white
        
    show {
        scene sg
    }
    0