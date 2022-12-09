namespace Aardvark.GLTF

open System.Threading
open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive

[<Struct; StructuredFormatDisplay("{AsString}")>]
type MaterialId private (value : int) =
    static let mutable currentId = -1
    static member New() = MaterialId(Interlocked.Increment(&currentId))
    override x.ToString() = string value
    member private x.AsString = x.ToString()
   
[<Struct; StructuredFormatDisplay("{AsString}")>]
type ImageId private (value : int) =
    static let mutable currentId = -1
    static member New() = ImageId(Interlocked.Increment(&currentId))
    override x.ToString() = string value
    member private x.AsString = x.ToString()

[<Struct; StructuredFormatDisplay("{AsString}")>]
type MeshId private (value : int) =
    static let mutable currentId = -1
    static member New() = MeshId(Interlocked.Increment(&currentId))
    override x.ToString() = string value
    member private x.AsString = x.ToString()
        
type TexCoordSemantic =
    | BaseColor
    | Roughness
    | Metallicness
    | Normal
    | Emissive

type Material =
    {
        Name            : string
        
        DoubleSided         : bool
        Opaque              : bool  
            
        AlbedoTexutre       : option<ImageId>
        AlbedoColor         : C4f
            
        Roughness           : float
        RoughnessTexture    : option<ImageId>
        
        Metallicness        : float
        MetallicnessTexture : option<ImageId>
        
        EmissiveColor       : C4f
        EmissiveTexture     : option<ImageId>
        
        NormalTexture       : option<ImageId>
        NormalTextureScale  : float
    }

type Mesh =
    {
        BoundingBox     : Box3d
        Mode            : IndexedGeometryMode
        Index           : option<int[]>
        Positions       : V3f[]
        Normals         : option<V3f[]>
        Tangents        : option<V4f[]>
        TexCoords       : list<V2f[] * HashSet<TexCoordSemantic>>
        Colors          : option<C4b[]>
    }
 
type Node =
    {
        Trafo           : option<Trafo3d>
        Material        : option<MaterialId>
        Geometry        : list<MeshId>
        Children        : list<Node>
    }
    
type Scene =
    {
        BoundingBox : Box3d
        Materials   : HashMap<MaterialId, Material>
        Meshes      : HashMap<MeshId, Mesh>
        Images      : HashMap<ImageId, PixImage>
        RootNode    : Node
    }


module Scene =
    let computeBounds (scene : Scene) =
        
        let rec traverse (n : Node) : Box3d =
            let mutable box = n.Children |> List.map traverse |> Box3d
            for mid in n.Geometry do
                match HashMap.tryFind mid scene.Meshes with
                | Some m -> box.ExtendBy m.BoundingBox
                | None -> ()
            
            match n.Trafo with
            | Some t -> box.Transformed t
            | None -> box

        traverse scene.RootNode
        
    let withBounds (scene : Scene) =
        { scene with BoundingBox = computeBounds scene }