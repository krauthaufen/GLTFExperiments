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
        Name            : option<string>
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
        Name            : option<string>
        Trafo           : option<Trafo3d>
        Material        : option<MaterialId>
        Meshes          : list<MeshId>
        Children        : list<Node>
    }

type Scene =
    {
        Materials   : HashMap<MaterialId, Material>
        Meshes      : HashMap<MeshId, Mesh>
        ImageData   : HashMap<ImageId, byte[]>
        RootNode    : Node
    }
    member x.BoundingBox =
        let rec traverse (n : Node) : Box3d =
            let mutable box = n.Children |> List.map traverse |> Box3d
            for mid in n.Meshes do
                match HashMap.tryFind mid x.Meshes with
                | Some m -> box.ExtendBy m.BoundingBox
                | None -> ()
            
            match n.Trafo with
            | Some t -> box.Transformed t
            | None -> box

        traverse x.RootNode

    
module Node =
    
    let empty =
        {
            Name = None
            Trafo = None
            Material = None
            Meshes = []
            Children = []
        }

    let ofList (children : list<Node>) =
        {
            Name = None
            Trafo = None
            Material = None
            Meshes = []
            Children = children
        }
        
    let ofSeq (children : seq<Node>) =
        children |> Seq.toList |> ofList
    
    let ofArray (children : Node[]) =
        children |> Array.toList |> ofList
    
    let ofMeshes (meshes : list<MeshId>) =
        {
            Name = None
            Trafo = None
            Material = None
            Meshes = meshes
            Children = []
        }
    
    let trafo (t : Trafo3d) (n : Node) =
        match n.Trafo with
        | Some o ->
            { n with Trafo = Some (t * o) }
        | None ->
            { n with Trafo = Some t }
    