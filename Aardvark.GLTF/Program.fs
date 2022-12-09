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
        GLTF.loadScene "/Users/schorsch/Downloads/untitled.glb"
        
    let mutable materials = HashMap.empty
    let mutable geometries = HashMap.empty
    let mutable nodes = []
        
    let geometry =
        let prim = IndexedGeometryPrimitives.solidPhiThetaSphere (Sphere3d(V3d.Zero, 0.3)) 36 C4b.White
        let pos = prim.IndexedAttributes.[DefaultSemantic.Positions] :?> V3f[]
        
        {
            BoundingBox     = Box3d.FromCenterAndSize(V3d.Zero, V3d.III * 0.6)
            Mode            = IndexedGeometryMode.TriangleList
            Index           = Some (prim.IndexArray :?> int[])
            Positions       = pos
            Normals         = Some (pos |> Array.map Vec.normalize)
            Tangents        = None
            TexCoords       = []
            Colors          = None
        }
        
    let gid = MeshId.New()
    geometries <- HashMap.add gid geometry geometries
    
    let testScene =
        let steps = 8
        for ri in 0 .. steps - 1 do
            let mutable roughness = float ri / float (steps - 1)
            for mi in 0 .. steps - 1 do
                let mutable metalness = float mi / float (steps - 1)
                let offset = Trafo3d.Translation(float ri, float mi, 0.0)
                
                let mid = MaterialId.New()
                
                let material =  
                    {
                        Name                = sprintf "%.3f_%.3f" roughness metalness
                        
                        DoubleSided         = true
                        Opaque              = true
                            
                        AlbedoTexutre       = None
                        AlbedoColor         = C4f.White
                            
                        Roughness           = roughness
                        RoughnessTexture    = None
                        
                        Metallicness        = metalness
                        MetallicnessTexture = None
                        
                        EmissiveColor       = C4f.Black
                        EmissiveTexture     = None
                        
                        NormalTexture       = None
                        NormalTextureScale  = 1.0
                    }
                
                materials <- HashMap.add mid material materials
                nodes <- { Trafo = Some offset; Geometry = [gid]; Children = []; Material = Some mid } :: nodes
        
        Scene.withBounds {
            Materials = materials
            Meshes = geometries
            BoundingBox = Box3d.Invalid
            Images = HashMap.empty
            RootNode = { Trafo = None; Geometry = []; Children = nodes; Material = None}
        }
        
    let model1 = testScene
        
    let centerTrafo1 =
        Trafo3d.Translation(-model1.BoundingBox.Center) *
        Trafo3d.Scale(5.0 / model1.BoundingBox.Size.NormMax)
 

    let renderTask =
        Sg.ofList [
            SceneSg.toSimpleSg win.Runtime model1
            |> Sg.transform centerTrafo1
            |> Sg.trafo' (Trafo3d.RotationX Constant.PiHalf)
            
        ]
        //|> Sg.uniform' "LightLocation" (V3d(10,20,30))
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