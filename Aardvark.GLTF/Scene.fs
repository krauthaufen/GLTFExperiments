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
        Material        : option<MaterialId>
        Mode            : IndexedGeometryMode
        Index           : option<int[]>
        Positions       : V3f[]
        Normals         : option<V3f[]>
        Tangents        : option<V4f[]>
        TexCoords       : list<V2f[] * HashSet<TexCoordSemantic>>
        Colors          : option<C4b[]>
    }
    
type Geometry =
    {
        Name : option<string>
        Meshes : list<MeshId>
    }
    
type Node =
    {
        Trafo           : Trafo3d
        Geometry        : option<Geometry>
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



