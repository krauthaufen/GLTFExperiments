open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.GLTF
open Aardvark.Application
open FSharp.Data.Adaptive

[<EntryPoint>]
let main _args =
    
    let rand = RandomSystem()
    //
    // printfn "let samples = "
    // printfn "    [|"
    // for i in 0 .. 40 do
    //     let u = rand.UniformDouble()
    //     let v = rand.UniformDouble()
    //     
    //     printfn "        V2d(%f, %f)" u v
    //
    // printfn "    |]"
    // exit 0
    Aardvark.Init()
   
    let app = new Aardvark.Application.Slim.OpenGlApplication(DebugLevel.None)
    let win = app.CreateGameWindow(4)
    
    let view =
        CameraView.lookAt (V3d(4,5,6)) V3d.Zero V3d.OOI
        |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
        |> AVal.map CameraView.viewTrafo
        
    let proj =
        win.Sizes
        |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))
        |> AVal.map Frustum.projTrafo
        
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let rot = win.Time |> AVal.map (fun _ -> Trafo3d.RotationZ (0.2 * sw.Elapsed.TotalSeconds))
        
    let enableTask =
        RenderTask.custom (fun _ ->
            OpenTK.Graphics.OpenGL4.GL.Enable(OpenTK.Graphics.OpenGL4.EnableCap.TextureCubeMapSeamless)
        )
        
              
    let model1 =
        GLTF.loadScene "/Users/schorsch/Downloads/millennium_falcon.glb"
        
    let centerTrafo1 =
        Trafo3d.Translation(-model1.BoundingBox.Center) *
        Trafo3d.Scale(5.0 / model1.BoundingBox.Size.NormMax)
              
    let model2 =
        GLTF.loadScene "/Users/schorsch/Downloads/x-wing_-_starwars_starship.glb"
        
    let centerTrafo2 =
        Trafo3d.Translation(-model2.BoundingBox.Center) *
        Trafo3d.Scale(2.0 / model2.BoundingBox.Size.NormMax)
        


    let renderTask =
        Sg.ofList [
            Scene.toSimpleSg win.Runtime model1
            |> Sg.transform centerTrafo1
            |> Sg.trafo' (Trafo3d.RotationX Constant.PiHalf)
            
            Scene.toSimpleSg win.Runtime model2
            |> Sg.transform centerTrafo2
            |> Sg.trafo' (Trafo3d.RotationX Constant.PiHalf)
            |> Sg.transform (Trafo3d.RotationZ -Constant.PiHalf)
            |> Sg.translate 0.0 3.0 1.0
        ]
        |> Sg.uniform' "LightLocation" (V3d(10,20,30))
        |> Sg.shader {
            do! Shader.trafo
            do! Shader.shade
        }
        //|> Sg.trafo rot
        |> Sg.viewTrafo view
        |> Sg.projTrafo proj
        |> Sg.compile win.Runtime win.FramebufferSignature
    win.RenderTask <- RenderTask.ofList [enableTask; renderTask]
    win.Run()
    
    0