open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.GLTF
open Aardvark.Application
open FSharp.Data.Adaptive

[<EntryPoint>]
let main _args =
    
    let rand = RandomSystem()
    
    // printfn "let samples = "
    // printfn "    [|"
    // for i in 0 .. 23 do
    //     let u = rand.UniformDouble()
    //     let v = rand.UniformDouble()
    //     
    //     printfn "        V2d(%f, %f)" u v
    //
    // printfn "    |]"
    // exit 0
    Aardvark.Init()
         
    let model =
        GLTF.loadScene "/Users/schorsch/Downloads/millennium_falcon.glb"
        
    let centerTrafo =
        Trafo3d.Translation(-model.BoundingBox.Center) *
        Trafo3d.Scale(5.0 / model.BoundingBox.Size.NormMax)
        
    let white =
        let img = PixImage<byte>(Col.Format.RGBA, V2i.II)
        img.GetMatrix<C4b>().Set(C4b.White) |> ignore
        PixTexture2d(PixImageMipMap [| img :> PixImage  |], TextureParams.empty) :> ITexture |> AVal.constant
        
        
    let win =
        window {
            samples 4
        }
        
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let rot = win.Time |> AVal.map (fun _ -> Trafo3d.RotationZ (0.2 * sw.Elapsed.TotalSeconds))
        
        
    let sg =
        Scene.toSimpleSg win.Runtime model
        |> Sg.transform centerTrafo
        |> Sg.trafo' (Trafo3d.RotationX Constant.PiHalf)
        |> Sg.shader {
            do! Shader.trafo
            do! Shader.shade
        }
        |> Sg.diffuseTexture white
        |> Sg.trafo rot
        
    win.Scene <- sg
    win.Run()
    0