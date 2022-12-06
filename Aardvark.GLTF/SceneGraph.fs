namespace Aardvark.GLTF

open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Scene =
    
    let private meshSg (scene : Scene) (textures : HashMap<ImageId, aval<ITexture>>) (m : Mesh) =
        let tc =
            m.TexCoords |> Seq.tryFind (fun (_, set) -> HashSet.contains TexCoordSemantic.BaseColor set)
        
        let mat =
            match m.Material with
            | Some matId ->
                HashMap.tryFind matId scene.Materials
            | None ->
                None
                
        let diffuseTexture =
            match mat with
            | Some mat ->
                match mat.AlbedoTexutre with
                | Some id -> HashMap.tryFind id textures
                | None -> None
            | None -> None
                
        let fvc =
            match m.Index with
            | Some i -> i.Length
            | None -> m.Positions.Length
                
        let mutable sg = 
            Sg.render m.Mode (DrawCallInfo(FaceVertexCount = fvc, InstanceCount = 1))
            |> Sg.vertexAttribute' DefaultSemantic.Positions m.Positions
            |> (match m.Index with | Some n -> Sg.indexArray n | None -> id)
            |> (match m.Normals with | Some n -> Sg.vertexAttribute' DefaultSemantic.Normals n | None -> Sg.vertexBufferValue' DefaultSemantic.Normals V3f.Zero)
            |> (match m.Colors with | Some n -> Sg.vertexAttribute' DefaultSemantic.Colors n | None -> Sg.vertexBufferValue' DefaultSemantic.Colors V4f.IIII)
            |> (match tc with | Some(att,_) -> Sg.vertexAttribute' DefaultSemantic.DiffuseColorCoordinates att | None -> Sg.vertexBufferValue' DefaultSemantic.DiffuseColorCoordinates V2f.Zero)
            |> (match diffuseTexture with | Some t -> Sg.diffuseTexture t| None -> Sg.diffuseTexture DefaultTextures.checkerboard)
            
        sg
        
    let toSimpleSg (scene : Scene) =
        let texture = scene.Images |> HashMap.map (fun _ i -> PixTexture2d(PixImageMipMap [|i|], TextureParams.mipmapped) :> ITexture |> AVal.constant)

        let meshes = scene.Meshes |> HashMap.map (fun _ m -> meshSg scene texture m)
        let rec traverse (node : Node) =
            let cs = node.Children |> Seq.map traverse |> Sg.ofSeq
            let ms = node.Meshes |> List.choose (fun id -> HashMap.tryFind id meshes) |> Sg.ofList
            
            Sg.ofList [cs; ms]
            |> Sg.trafo' node.Trafo
            
        traverse scene.RootNode


