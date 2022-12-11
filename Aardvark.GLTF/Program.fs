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
        
              
    let loadedModel =
        GLTF.loadScene "/Users/schorsch/Downloads/x-wing_-_starwars_starship.glb"
        
    let print (scene : Scene) =
        let rec print (n : Node) =
            Log.start "%A" n.Name
            match n.Trafo with
            | Some t ->
                let (u, s, vt) = SVD.Decompose (t.Forward.UpperLeftM33()) |> Option.get
                let rot = Rot3d.FromM33d u
                let trans = t.Forward.TransformPos V3d.Zero
                Log.line "rot: %A" rot
                Log.line "trans: %A" trans
                Log.line "scale: %A" s.Diagonal
                ()
            | None ->
                ()
            if not (List.isEmpty n.Meshes) then
                Log.start "meshes"
                for m in n.Meshes do
                    let mesh = scene.Meshes.[m.Mesh]
                    let mat = m.Material |> Option.bind (fun mid -> HashMap.tryFind mid scene.Materials)
                    Log.line "%A -> %A" mesh.Name (mat |> Option.bind (fun m -> m.Name))
                Log.stop()
                
            if not (List.isEmpty n.Children) then
                for m in n.Children do
                    print m
                    
            Log.stop()
            
        print scene.RootNode
       
    print loadedModel
        
    let testScene =
        let mutable materials = HashMap.empty
        let mutable geometries = HashMap.empty
        let mutable nodes = []
            
        let geometry =
            let prim = IndexedGeometryPrimitives.solidPhiThetaSphere (Sphere3d(V3d.Zero, 0.3)) 36 C4b.White
            let pos = prim.IndexedAttributes.[DefaultSemantic.Positions] :?> V3f[]
            
            {
                Name            = Some "Sphere"
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
        
        
        let steps = 8
        for ri in 0 .. steps - 1 do
            let mutable roughness = float ri / float (steps - 1)
            for mi in 0 .. steps - 1 do
                let mutable metalness = float mi / float (steps - 1)
                let offset = Trafo3d.Translation(float ri, float mi, 0.0)
                
                let mid = MaterialId.New()
                
                let material =  
                    {
                        Name                = Some (sprintf "%.3f_%.3f" roughness metalness)
                        
                        DoubleSided         = true
                        Opaque              = true
                            
                        AlbedoTexutre       = None
                        AlbedoColor         = C4f.Beige
                            
                        Roughness           = roughness
                        RoughnessTexture    = None
                        RoughnessTextureComponent = 0
                        
                        Metallicness        = metalness
                        MetallicnessTexture = None
                        MetallicnessTextureComponent = 0
                        
                        EmissiveColor       = C4f.Black
                        EmissiveTexture     = None
                        
                        NormalTexture       = None
                        NormalTextureScale  = 1.0
                    }
                
                materials <- HashMap.add mid material materials
                nodes <- { Name = None; Trafo = Some offset; Meshes = [ { Mesh = gid; Material = Some mid } ]; Children = [] } :: nodes
        
        {
            Materials = materials
            Meshes = geometries
            ImageData = HashMap.empty
            RootNode = { Name = None; Trafo = None; Meshes = []; Children = nodes }
        }
        
    GLTF.save "/Users/schorsch/Desktop/bla.glb" loadedModel
               
    let model1 =
        GLTF.loadScene "/Users/schorsch/Desktop/bla.glb"
        
    //let model1 = loadedModel
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