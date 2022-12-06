open System.IO
open glTFLoader
open glTFLoader.Schema
open Aardvark.Base
open Aardvark.Geometry
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Microsoft.FSharp.NativeInterop
open Aardvark.SceneGraph


Aardvark.Init()



[<Struct; CustomEquality; CustomComparison>]
type MaterialId private (value : System.Guid) =
    member private x.Value = value
    override x.GetHashCode() = Unchecked.hash value
    override x.Equals(o) =
        match o with
        | :? MaterialId as other -> value = other.Value
        | _ -> false
        
        
    static member New() = MaterialId(System.Guid.NewGuid())
        
    interface System.IComparable with
        member x.CompareTo(o) =
            value.CompareTo((o :?> MaterialId).Value)
  
        
[<Struct; CustomEquality; CustomComparison>]
type ImageId private (value : System.Guid) =
    member private x.Value = value
    override x.GetHashCode() = Unchecked.hash value
    override x.Equals(o) =
        match o with
        | :? ImageId as other -> value = other.Value
        | _ -> false
        
        
    static member New() = ImageId(System.Guid.NewGuid())
        
    interface System.IComparable with
        member x.CompareTo(o) =
            value.CompareTo((o :?> ImageId).Value)
        
[<Struct; CustomEquality; CustomComparison>]
type MeshId private (value : System.Guid) =
    member private x.Value = value
    override x.GetHashCode() = Unchecked.hash value
    override x.Equals(o) =
        match o with
        | :? MeshId as other -> value = other.Value
        | _ -> false
        
        
    static member New() = MeshId(System.Guid.NewGuid())
        
    interface System.IComparable with
        member x.CompareTo(o) =
            value.CompareTo((o :?> MeshId).Value)
        
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
        Material        : option<MaterialId>
        Mode            : IndexedGeometryMode
        Index           : option<int[]>
        Positions       : V3f[]
        Normals         : option<V3f[]>
        TexCoords       : list<V2f[] * HashSet<TexCoordSemantic>>
        Colors          : option<C4b[]>
    }
    
type Node =
    {
        Trafo           : Trafo3d
        Meshes          : list<MeshId> 
        Children        : list<Node>
    }
    

type Scene =
    {
        Materials   : HashMap<MaterialId, Material>
        Meshes      : HashMap<MeshId, Mesh>
        Images      : HashMap<ImageId, PixImage>
        RootNode    : Node
    }


module GLTF =
    
    let private getArray<'a when 'a : unmanaged> (byteOffset : int) (byteStride : int) (cnt : int) (buffer : byte[]) =
        let size = sizeof<'a>
        
        if byteStride = 0 || byteStride = size then
            let arr = Array.zeroCreate<'a> cnt
            use ptr = fixed arr
            let src = System.Span(buffer, byteOffset, buffer.Length - byteOffset)
            let dst = System.Span<byte>(NativePtr.toVoidPtr ptr, arr.Length * size)
            src.Slice(0, arr.Length * size).CopyTo(dst)
            arr
        else
            let res = Array.zeroCreate<'a> cnt
            use pSrc = fixed buffer
            use pDst = fixed res
            
            let mutable pSrc = NativePtr.ofNativeInt<'a> (NativePtr.toNativeInt pSrc + nativeint byteOffset)
            let mutable pDst = pDst
            for i in 0 .. cnt - 1 do
                NativePtr.write pDst (NativePtr.read pSrc)
                pSrc <- NativePtr.add pSrc 1
                pDst <- NativePtr.add pDst 1
            res
    
    let private getAttributeArray (readBuffer : int -> byte[]) (model : Gltf) (accId : int) =
        let acc = model.Accessors.[accId]
        if acc.BufferView.HasValue then
            let view = model.BufferViews.[acc.BufferView.Value]
            let bufferData = readBuffer view.Buffer
            let stride = if view.ByteStride.HasValue then view.ByteStride.Value else 0
            
            let byteOffset = view.ByteOffset + acc.ByteOffset
            
            match acc.ComponentType with
            | Accessor.ComponentTypeEnum.UNSIGNED_BYTE ->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<byte> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC3 -> getArray<C3b> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC4 -> getArray<C4b> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | Accessor.ComponentTypeEnum.UNSIGNED_SHORT ->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<uint16> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC3 -> getArray<C3us> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC4 -> getArray<C4us> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | Accessor.ComponentTypeEnum.UNSIGNED_INT->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<uint32> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC2 -> getArray<V2ui> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC3 -> getArray<V3ui> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC4 -> getArray<V4ui> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | Accessor.ComponentTypeEnum.FLOAT ->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<float32> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC2 -> getArray<V2f> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC3 -> getArray<V3f> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC4 -> getArray<V4f> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | Accessor.ComponentTypeEnum.SHORT ->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<int16> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | Accessor.ComponentTypeEnum.BYTE ->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<int8> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | _ ->
                failwith ""
        else
            failwith ""
    
    let private flipY (arr : System.Array) =
        match arr with
        | :? array<V2f> as arr -> arr |> Array.map (fun v -> V2f(v.X, 1.0f - v.Y)) :> System.Array
        | :? array<V2d> as arr -> arr |> Array.map (fun v -> V2d(v.X, 1.0 - v.Y)) :> System.Array
        | _ -> failwithf "bad TC type: %A" arr
    
    let private getTrafo (node : glTFLoader.Schema.Node) =

        let mutable trafo = Trafo3d.Identity
        
        if not (isNull node.Matrix) then
            let m = node.Matrix
            let fw =
                M44d(
                    float m.[0], float m.[4], float m.[8], float m.[12],
                    float m.[1], float m.[5], float m.[9], float m.[13],
                    float m.[2], float m.[6], float m.[10], float m.[14],
                    float m.[3], float m.[7], float m.[11], float m.[15]
                )
            trafo <- trafo * Trafo3d(fw, fw.Inverse)
            
        if not (isNull node.Scale) then
            let s = node.Scale
            trafo <- trafo * Trafo3d.Scale(float s.[0], float s.[1], float s.[2])
        
        if not (isNull node.Rotation) then
            let q = node.Rotation
            let q = QuaternionD(float q.[3], float q.[0], float q.[1], float q.[2])
            let q = Rot3d(q.Normalized)
            
            let q : Trafo3d = q |> Rot3d.op_Explicit
            trafo <- trafo * q
            
        if not (isNull node.Translation) then
            let t = node.Translation
            trafo <- trafo * Trafo3d.Translation(V3d(float t.[0], float t.[1], float t.[2]))
            
        trafo
            
    
    let private getSemantic (name : string) =
        match name.ToLower().Trim() with
        | "position" | "positions" -> DefaultSemantic.Positions
        | "normal" | "normals" -> DefaultSemantic.Normals
        | "texcoord_0" -> DefaultSemantic.DiffuseColorCoordinates
        | "color_0" -> DefaultSemantic.Colors
        | _ -> Symbol.Create name
    
    let private toScene (file : string) (model : Gltf) =
        
        let bufferCache = Dict<int, byte[]>()
        
        let readBuffer (i : int) =
            bufferCache.GetOrCreate(i, fun i ->
                model.LoadBinaryBuffer(i, file)    
            )
        
        let images =
            if isNull model.Images || model.Images.Length = 0 then
                [||]
            else
                model.Images |> Array.map (fun img ->
                    let id = ImageId.New()
                    if img.BufferView.HasValue then 
                        let view = model.BufferViews.[img.BufferView.Value]
                        
                        let buffer = readBuffer view.Buffer
                        
                        let pimg = 
                            use ms = new MemoryStream(buffer, view.ByteOffset, view.ByteLength)
                            PixImage.Load(ms)
                            
                        Some (id, pimg)
                    else
                        None
                )
        
        let mutable tcMapping =
            HashMap.empty<MaterialId, HashMap<int, HashSet<TexCoordSemantic>>>
        
        let materials =
            if isNull model.Materials then
                [||]
            else
                model.Materials |> Array.map (fun mat ->
                    let id = MaterialId.New()
                    let opaque = mat.AlphaMode <> Material.AlphaModeEnum.BLEND
                    let doubleSided = mat.DoubleSided
                    
                    let tryGetTextureWithIndex (sem : TexCoordSemantic) (index : int) (tcIndex : int) =
                        let tex = model.Textures.[index]
                        if tex.Source.HasValue then
                            match images.[tex.Source.Value] with
                            | Some (imageId, _) ->
                                let tcIdx = tcIndex
                                tcMapping <-
                                    tcMapping |> HashMap.alter id (function
                                        | Some o ->
                                            o |> HashMap.alter tcIdx (function
                                                | Some o -> Some (HashSet.add sem o)
                                                | None -> Some (HashSet.single sem)
                                            )
                                            |> Some
                                        | None ->
                                            Some (HashMap.single tcIdx (HashSet.single sem))
                                    ) 
                                Some imageId
                            | None ->
                                None
                        else
                            None
                
                    let tryGetTexture (sem : TexCoordSemantic) (info : TextureInfo) =
                        if isNull info then
                            None
                        else
                            tryGetTextureWithIndex sem info.Index info.TexCoord
                    
                    let albedoColor =
                        if isNull mat.PbrMetallicRoughness then C4f(1.0f, 1.0f, 1.0f, 1.0f)
                        else C4f mat.PbrMetallicRoughness.BaseColorFactor
                    
                    let albedoTexture =
                        if isNull mat.PbrMetallicRoughness then None
                        else tryGetTexture TexCoordSemantic.BaseColor mat.PbrMetallicRoughness.BaseColorTexture
                        
                    let roughness =
                        if isNull mat.PbrMetallicRoughness then 1.0
                        else float mat.PbrMetallicRoughness.RoughnessFactor
                        
                    let roughnessTexture =
                        if isNull mat.PbrMetallicRoughness then None
                        else tryGetTexture TexCoordSemantic.Roughness mat.PbrMetallicRoughness.MetallicRoughnessTexture
                        
                    let metallicness =
                        if isNull mat.PbrMetallicRoughness then 0.0
                        else float mat.PbrMetallicRoughness.MetallicFactor
                        
                    let emissive =
                        if isNull mat.EmissiveFactor then C4f(0.0f, 0.0f, 0.0f, 0.0f)
                        else C3f(mat.EmissiveFactor).ToC4f()
                        
                    let emissiveTexture =
                        tryGetTexture TexCoordSemantic.Emissive mat.EmissiveTexture
                        
                    let normalTexture =
                        if isNull mat.NormalTexture then None
                        else tryGetTextureWithIndex TexCoordSemantic.Normal mat.NormalTexture.Index mat.NormalTexture.TexCoord
                        
                    let normalTextureScale =
                        if isNull mat.NormalTexture then 1.0
                        else float mat.NormalTexture.Scale
                        
                    id, {
                        Name                = mat.Name
                            
                        DoubleSided         = doubleSided
                        Opaque              = opaque
                            
                        AlbedoTexutre       = albedoTexture
                        AlbedoColor         = albedoColor
                        Roughness           = roughness
                        RoughnessTexture    = roughnessTexture
                        
                        Metallicness        = metallicness
                        MetallicnessTexture = None
                        
                        EmissiveColor       = emissive
                        EmissiveTexture     = emissiveTexture
                        
                        NormalTexture       = normalTexture
                        NormalTextureScale  = normalTextureScale
                    }
                )
        
        let meshes =
            if isNull model.Meshes then
                [||]
            else
                model.Meshes |> Array.map (fun m ->
                    m.Primitives |> Array.choose (fun p ->
                        let index, indexRange =
                            if p.Indices.HasValue then
                                let acc = model.Accessors.[p.Indices.Value]
                                let minIndex = if isNull acc.Min then 0 else int acc.Min.[0]
                                let maxIndex = if isNull acc.Max then System.Int32.MaxValue else int acc.Max.[0]
                                getAttributeArray readBuffer model p.Indices.Value, Some (Range1i(minIndex, maxIndex))
                            else
                                null, None
                                
                        let attributes =
                            p.Attributes |> Seq.toArray |> Array.map (fun (KeyValue(name, att)) ->
                                let arr = getAttributeArray readBuffer model att
                                let arr =
                                    if name.StartsWith "TEXCOORD" then flipY arr
                                    else arr
                                
                                name, arr
                            )
                            
                        let attributeMap =
                            HashMap.ofArray attributes
                          
                        match HashMap.tryFind "POSITION" attributeMap with
                        | Some (:? array<V3f> as position) ->  
                      
                            let mode =
                                match p.Mode with
                                | MeshPrimitive.ModeEnum.POINTS -> IndexedGeometryMode.PointList
                                | MeshPrimitive.ModeEnum.LINES -> IndexedGeometryMode.LineList
                                | MeshPrimitive.ModeEnum.LINE_STRIP -> IndexedGeometryMode.LineStrip
                                | MeshPrimitive.ModeEnum.TRIANGLES -> IndexedGeometryMode.TriangleList
                                | MeshPrimitive.ModeEnum.TRIANGLE_STRIP -> IndexedGeometryMode.TriangleStrip
                                | m -> failwithf "bad mode: %A" m // TODO: convert to TriangleList indices??
                                
                            let material =
                                if p.Material.HasValue then
                                    let mid, _mat = materials.[p.Material.Value]
                                    Some mid
                                else
                                    None
                            let tcMapping =
                                if p.Material.HasValue then
                                    let mid, _mat = materials.[p.Material.Value]
                                    match HashMap.tryFind mid tcMapping with
                                    | Some map -> map
                                    | None -> HashMap.empty
                                else
                                    HashMap.empty
                                
                            let texCoords =
                                attributes |> Array.choose (fun (name, arr) ->
                                    if name.StartsWith "TEXCOORD_" then
                                        match System.Int32.TryParse (name.Substring 9) with
                                        | (true, id) ->
                                            match arr with
                                            | :? array<V2f> as arr ->
                                                match HashMap.tryFind id tcMapping with
                                                | Some tcId -> Some (arr, tcId)
                                                | None -> None
                                            | arr ->
                                                Log.warn "bad TexCoord type: %A" arr
                                                None
                                        | _ -> None
                                    else
                                        None
                                )
                                
                            let index =
                                match index with
                                | null -> null
                                | :? array<uint8> as arr -> Array.map int arr
                                | :? array<uint16> as arr -> Array.map int arr
                                | :? array<uint32> as arr -> Array.map int arr
                                | _ -> failwithf "unexpected index-type: %A" index
                                
                            let normals =
                                match HashMap.tryFind "NORMAL" attributeMap with
                                | Some (:? array<V3f> as normals) -> Some normals
                                | _ -> None
                                
                            let colors =
                                match HashMap.tryFind "COLOR_0" attributeMap with
                                | Some (:? array<C4b> as cs) -> Some cs
                                | _ -> None
                                
                            let mesh =
                                {
                                    Material        = material
                                    Mode            = mode
                                    Index           = if isNull index then None else Some index
                                    Positions       = position
                                    Normals         = normals
                                    TexCoords       = Array.toList texCoords
                                    Colors          = colors
                                }
                            Some (MeshId.New(), mesh)
                        | m ->
                            Log.warn "mesh has incompatible positions: %A" m
                            None
                         
                    )
                )
            
        let roots =
            
            let rec traverse (nid : int) =
                let node = model.Nodes.[nid]
                let trafo = getTrafo node
                
                let cs =
                    if isNull node.Children then []
                    else node.Children |> Array.toList |> List.map traverse
                    
                let meshes =
                    if node.Mesh.HasValue then
                        let arr = meshes.[node.Mesh.Value]
                        arr |> Array.map fst |> Array.toList
                    else
                        []
                    
                {
                    Trafo = trafo
                    Children = cs
                    Meshes = meshes
                }
                
            if model.Scene.HasValue then
                let scene = model.Scenes.[model.Scene.Value]
                scene.Nodes |> Array.map (fun r ->
                    traverse r
                )
            else
                [||]
            
        let root =
            match roots with
            | [||] -> { Trafo = Trafo3d.Identity; Meshes = []; Children = [] }
            | [|r|] -> r
            | rs -> { Trafo = Trafo3d.Identity; Meshes = []; Children = Array.toList rs }
            
            
        {
            Meshes      = meshes |> Array.concat |> HashMap.ofArray
            Materials   = materials |> HashMap.ofArray
            Images      = images |> Array.choose id |> HashMap.ofArray
            RootNode    = root
        }
    
    let loadScene (file : string) =
        let model = Interface.LoadModel file
        toScene file model
    
    let tryLoad (file : string) =
        use stream = File.OpenRead file
        let model = Interface.LoadModel stream
       
        exit 0
        
        let bufferCache = Dict<int, byte[]>()
        
        let readBuffer (i : int) =
            bufferCache.GetOrCreate(i, fun i ->
                model.LoadBinaryBuffer(i, file)    
            )
        
        if model.Scene.HasValue then
            let scene = model.Scenes.[model.Scene.Value]
            
            let map (mapping : 'a -> 'b) (arr : 'a[]) =
                let res = Array.zeroCreate arr.Length
                for i in 0 .. arr.Length - 1 do
                    res.[i] <- mapping arr.[i]
                res
            
            let mutable dasdasd = false
            
            let imageTextures =
                model.Images |> Array.map (fun img ->
                    let view = model.BufferViews.[img.BufferView.Value]
                    
                    let buffer = readBuffer view.Buffer
                    let mem = System.Memory<byte>(buffer, view.ByteOffset, view.ByteLength)
                    
                    let pimg = 
                        use ms = new MemoryStream(buffer, view.ByteOffset, view.ByteLength)
                        PixImage.Load(ms)
                           
                    let pimg =
                        if pimg.Format = Col.Format.Gray then
                            pimg.Visit {
                                new IPixImageVisitor<PixImage> with
                                    member x.Visit(a : PixImage<'a>) =
                                        a.ToFormat Col.Format.RGBA :> PixImage
                            }
                        else
                            pimg
                           
                    PixTexture2d (PixImageMipMap [| pimg |], TextureParams.mipmapped) :> ITexture
                )
            
            let geometries = 
                model.Meshes |> map (fun m ->
                    if not (isNull m.Weights) && m.Weights.Length > 0 then failwith "NO MORPHING"
                    
                    m.Primitives |> map (fun p ->
                        let index, indexRange =
                            if p.Indices.HasValue then
                                let acc = model.Accessors.[p.Indices.Value]
                                let minIndex = if isNull acc.Min then 0 else int acc.Min.[0]
                                let maxIndex = if isNull acc.Max then System.Int32.MaxValue else int acc.Max.[0]
                                getAttributeArray readBuffer model p.Indices.Value, Some (Range1i(minIndex, maxIndex))
                            else
                                null, None
                                
                        let attributes =
                            p.Attributes |> Seq.map (fun (KeyValue(name, att)) ->
                                let arr = getAttributeArray readBuffer model att
                                // match indexRange with
                                // | Some r ->
                                //     if r.Min <> 0 || arr.Length <> r.Max + 1 then Log.warn "asdasdasd"
                                // | _ ->
                                //     ()
                                
                                let arr =
                                    if name.StartsWith "TEXCOORD" then flipY arr
                                    else arr
                                
                                getSemantic name, arr
                            )
                            |> SymDict.ofSeq
                            
                        let mode =
                            match p.Mode with
                            | MeshPrimitive.ModeEnum.POINTS -> IndexedGeometryMode.PointList
                            | MeshPrimitive.ModeEnum.LINES -> IndexedGeometryMode.LineList
                            | MeshPrimitive.ModeEnum.LINE_STRIP -> IndexedGeometryMode.LineStrip
                            | MeshPrimitive.ModeEnum.TRIANGLES -> IndexedGeometryMode.TriangleList
                            | MeshPrimitive.ModeEnum.TRIANGLE_STRIP -> IndexedGeometryMode.TriangleStrip
                            | m -> failwithf "bad mode: %A" m // TODO: convert to TriangleList indices??
                         
                        let geometry =
                            IndexedGeometry(
                                Mode = mode,
                                IndexArray = index,
                                IndexedAttributes = attributes
                            )
                            
                        let mutable sg = Sg.ofIndexedGeometry geometry
                        
                        if p.Material.HasValue then
                            let mat = model.Materials.[p.Material.Value]
                            
                            let bct = mat.PbrMetallicRoughness.BaseColorTexture
                            if not (isNull bct) then
                                let tex = model.Textures.[bct.Index]
                                let img = imageTextures.[tex.Source.Value]
                                sg <- sg |> Sg.diffuseTexture' img
                        sg
                        
                    )
                )
            
            
            scene.Nodes |> Array.map (fun rootId ->
                let rec visit (nodeId : int) =
                    let node = model.Nodes.[nodeId]
                    
                    let trafo = 
                        let mutable trafo = Trafo3d.Identity
                        
                        if not (isNull node.Matrix) then
                            let m = node.Matrix
                            let fw =
                                M44d(
                                    float m.[0], float m.[4], float m.[8], float m.[12],
                                    float m.[1], float m.[5], float m.[9], float m.[13],
                                    float m.[2], float m.[6], float m.[10], float m.[14],
                                    float m.[3], float m.[7], float m.[11], float m.[15]
                                )
                            trafo <- trafo * Trafo3d(fw, fw.Inverse)
                            
                        if not (isNull node.Scale) then
                            let s = node.Scale
                            trafo <- trafo * Trafo3d.Scale(float s.[0], float s.[1], float s.[2])
                        
                        if not (isNull node.Rotation) then
                            let q = node.Rotation
                            let q = QuaternionD(float q.[3], float q.[0], float q.[1], float q.[2])
                            let q = Rot3d(q.Normalized)
                            
                            let q : Trafo3d = q |> Rot3d.op_Explicit
                            trafo <- trafo * q
                            
                        if not (isNull node.Translation) then
                            let t = node.Translation
                            trafo <- trafo * Trafo3d.Translation(V3d(float t.[0], float t.[1], float t.[2]))
                            
                        trafo
                        
                        
                        
                    let sg =
                        if node.Mesh.HasValue then
                            let meshes = geometries.[node.Mesh.Value]
                            meshes |> Sg.ofArray
                        else
                            Sg.empty
                            
                    let childSg = 
                        if not (isNull node.Children) then
                            node.Children |> Seq.map (fun ci ->
                                visit ci
                            )
                            |> Sg.ofSeq
                        else
                            Sg.empty
                       
                    Sg.ofList [sg; childSg]
                    |> Sg.trafo' trafo
                    
                visit rootId
            )
            |> Sg.ofArray
        else
            failwith "bad model"
   
   
module Mesh =
    let toSg (scene : Scene) (textures : HashMap<ImageId, aval<ITexture>>) (m : Mesh) =
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
   
module Scene =
    let toSg (scene : Scene) =
        let texture = scene.Images |> HashMap.map (fun _ i -> PixTexture2d(PixImageMipMap [|i|], TextureParams.mipmapped) :> ITexture |> AVal.constant)

        let meshes = scene.Meshes |> HashMap.map (fun _ m -> Mesh.toSg scene texture m)
        let rec traverse (node : Node) =
            let cs = node.Children |> Seq.map traverse |> Sg.ofSeq
            let ms = node.Meshes |> List.choose (fun id -> HashMap.tryFind id meshes) |> Sg.ofList
            
            Sg.ofList [cs; ms]
            |> Sg.trafo' node.Trafo
            
        traverse scene.RootNode
            
let model =
    GLTF.loadScene "/Users/schorsch/Downloads/x-wing_-_starwars_starship.glb"
    
    
open Aardvark.Application

module Shader =
    open FShade
    let tcColor (v : Effects.Vertex) =
        fragment {
            return V4d(v.tc, 1.0, 1.0)
        }

let sg =
    Scene.toSg model
    |> Sg.scale 20.0
    |> Sg.trafo' (Trafo3d.RotationX Constant.PiHalf)
    |> Sg.shader {
        do! DefaultSurfaces.trafo
        do! DefaultSurfaces.diffuseTexture
        do! DefaultSurfaces.simpleLighting
    }
    |> Sg.diffuseTexture DefaultTextures.checkerboard
    // Sg.fullScreenQuad
    // |> Sg.shader {
    //     do! Shader.tcColor
    // }
show {
    scene sg
}