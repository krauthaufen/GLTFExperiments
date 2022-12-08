namespace Aardvark.GLTF

open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph


module Skybox =
    open SixLabors.ImageSharp
    open SixLabors.ImageSharp.Processing
    type Marker = Marker
    let getImage =
        let names = typeof<Marker>.Assembly.GetManifestResourceNames()
        let load (name : string) =
            let name = names |> Array.find (fun str -> str.EndsWith name)
            use s = typeof<Marker>.Assembly.GetManifestResourceStream(name)
            PixImage.Load(s)

        load

    let get (name : string) =
        
        AVal.custom (fun _ ->
            let env =
                let names = typeof<Marker>.Assembly.GetManifestResourceNames()
                let getMipMaps (img : PixImage) =
                    use ii = PixImageSharp.ToImage img
                    let res = Array.zeroCreate (1 + int (floor (log2 (float img.Size.X))))
                    res.[0] <- img
                    for l in 1 .. res.Length - 1 do
                        ii.Mutate (fun ctx ->
                            ctx.Resize(ii.Width/2, ii.Height/2)
                            |> ignore
                        )
                        res.[l] <- PixImageSharp.ToPixImage ii
                    res
                let trafo t (img : PixImage) =
                    img.Transformed t
                    //|> getMipMaps

                    
                PixImageCube [|
                    PixImageMipMap(
                        getImage (name.Replace("$", "rt"))
                        |> trafo ImageTrafo.Rot90
                    )
                    PixImageMipMap(
                        getImage (name.Replace("$", "lf"))
                        |> trafo ImageTrafo.Rot270
                    )
                
                    PixImageMipMap(
                        getImage (name.Replace("$", "bk"))
                    )
                    PixImageMipMap(
                        getImage (name.Replace("$", "ft"))
                        |> trafo ImageTrafo.Rot180
                    )
                
                    PixImageMipMap(
                        getImage (name.Replace("$", "up"))
                        |> trafo ImageTrafo.Rot90
                    )
                    PixImageMipMap(
                        getImage (name.Replace("$", "dn"))
                        |> trafo ImageTrafo.Rot90
                    )
                |]

            PixTextureCube(env, TextureParams.mipmapped) :> ITexture
        )


module Semantic =
    let AlbedoCoordinate = Symbol.Create "AlbedoCoordinate"
    let RoughnessCoordinate = Symbol.Create "RoughnessCoordinate"
    let MetallicnessCoordinate = Symbol.Create "MetallicnessCoordinate"
    let EmissiveCoordinate = Symbol.Create "EmissiveCoordinate"
    let NormalCoordinate = Symbol.Create "NormalCoordinate"
    let Tangent = Symbol.Create "Tangent"

[<ReflectedDefinition>]
module Shader =
    open FShade
    
    type ViewPositionAttribute() = inherit SemanticAttribute("ViewPosition")
    type AlbedoCoordinateAttribute() = inherit SemanticAttribute("AlbedoCoordinate")
    type RoughnessCoordinateAttribute() = inherit SemanticAttribute("RoughnessCoordinate")
    type MetallicnessCoordinateAttribute() = inherit SemanticAttribute("MetallicnessCoordinate")
    type EmissiveCoordinateAttribute() = inherit SemanticAttribute("EmissiveCoordinate")
    type NormalCoordinateAttribute() = inherit SemanticAttribute("NormalCoordinate")
    type TangentAttribute() = inherit SemanticAttribute("Tangent")
    type ViewTangentAttribute() = inherit SemanticAttribute("ViewTangent")
    type ViewBiTangentAttribute() = inherit SemanticAttribute("ViewBiTangent")
    type ViewLightDirectionAttribute() = inherit SemanticAttribute("ViewLightDirection")
    
    let skybox =
        samplerCube {
            texture uniform?Skybox
            filter Filter.Anisotropic
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            addressW WrapMode.Wrap
        }
    
    let albedoTexture =
        sampler2d {
            texture uniform?AlbedoTexture
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinMagMipLinear
        }
    
    let roughnessTexture =
        sampler2d {
            texture uniform?RoughnessTexture
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinMagMipLinear
        }
    
    let metallicnessTexture =
        sampler2d {
            texture uniform?MetallicnessTexture
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinMagMipLinear
        }
        
    let emissiveTexture =
        sampler2d {
            texture uniform?EmissiveTexture
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinMagMipLinear
        }
        
    let normalTexture =
        sampler2d {
            texture uniform?NormalTexture
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinMagMipLinear
        }
        
    type UniformScope with
        member x.AlbedoColor : V4d = uniform?Material?AlbedoColor
        member x.Roughness : float = uniform?Material?Roughness
        member x.Metallicness : float = uniform?Material?Metallicness
        member x.EmissiveColor : V4d = uniform?Material?EmissiveColor
        member x.NormalTextureScale : float = uniform?Material?NormalTextureScale
        
        member x.HasAlbedoTexture : bool = uniform?Material?HasAlbedoTexture
        member x.HasRoughnessTexture : bool = uniform?Material?HasRoughnessTexture
        member x.HasMetallicnessTexture : bool = uniform?Material?HasMetallicnessTexture
        member x.HasEmissiveTexture : bool = uniform?Material?HasEmissiveTexture
        member x.HasNormalTexture : bool = uniform?Material?HasNormalTexture
        
        member x.HasNormals : bool = uniform?Mesh?HasNormals
        member x.HasTangents : bool = uniform?Mesh?HasTangents
        member x.HasColors : bool = uniform?Mesh?HasColors
        
    type Vertex =
        {
            [<Position>]                pos             : V4d
            [<ViewPosition>]            viewPos         : V4d
            [<Normal>]                  normal          : V3d
            [<AlbedoCoordinate>]        albedoCoord     : V2d
            [<RoughnessCoordinate>]     roughCoord      : V2d
            [<MetallicnessCoordinate>]  metalCoord      : V2d
            [<EmissiveCoordinate>]      emissCoord      : V2d
            [<NormalCoordinate>]        normCoord       : V2d
            [<Tangent>]                 tangent         : V4d
            [<ViewTangent>]             viewTangent     : V3d
            [<ViewBiTangent>]           viewBiTangent   : V3d
            [<ViewLightDirection>]      viewLightDir    : V3d
        }
        
    let trafo (v : Vertex) =
        vertex {
            
            let vp = uniform.ModelViewTrafo * v.pos
            let vld = (uniform.ViewTrafo * V4d(uniform.LightLocation, 1.0) - vp).XYZ |> Vec.normalize
            let vn = uniform.ModelViewTrafoInv.Transposed.TransformDir v.normal |> Vec.normalize
            let vt = uniform.ModelViewTrafoInv.Transposed.TransformDir v.tangent.XYZ |> Vec.normalize
            let vb = v.tangent.W * Vec.cross vn vt
            
            return
                { v with
                    pos = uniform.ProjTrafo * vp
                    viewPos = vp
                    normal = uniform.ModelViewTrafoInv.Transposed.TransformDir v.normal |> Vec.normalize
                    viewTangent = vt
                    viewBiTangent = vb
                    viewLightDir = vld
                }
        }
    let random24 = 
        [|
            V2d(0.183014, 0.057349)
            V2d(0.066681, 0.719551)
            V2d(0.826578, 0.636280)
            V2d(0.207799, 0.240253)
            V2d(0.541970, 0.090749)
            V2d(0.014185, 0.866101)
            V2d(0.929036, 0.417594)
            V2d(0.569475, 0.583288)
            V2d(0.810305, 0.290636)
            V2d(0.621660, 0.031124)
            V2d(0.442131, 0.832180)
            V2d(0.404755, 0.239412)
            V2d(0.039530, 0.738925)
            V2d(0.293360, 0.543963)
            V2d(0.630658, 0.075838)
            V2d(0.060362, 0.139359)
            V2d(0.269777, 0.819915)
            V2d(0.748755, 0.432552)
            V2d(0.756616, 0.750890)
            V2d(0.222299, 0.989969)
            V2d(0.830065, 0.813399)
            V2d(0.449001, 0.862132)
            V2d(0.688157, 0.222665)
            V2d(0.880603, 0.981080)
        |]


    let samples24 =
        [|
            V2d( -0.4612850228120782, -0.8824263018037591 )
            V2d( 0.2033539719528926, 0.9766070232577696 )
            V2d( 0.8622755945065503, -0.4990552917715807 )
            V2d( -0.8458406529500018, 0.4340626564690164 )
            V2d( 0.9145341241356336, 0.40187426079092753 )
            V2d( -0.8095919285224212, -0.2476471278659192 )
            V2d( 0.2443597793708885, -0.8210571365841042 )
            V2d( -0.29522102954593127, 0.6411496844366571 )
            V2d( 0.4013698454531175, 0.47134750051312063 )
            V2d( -0.1573158341083741, -0.48548502348882533 )
            V2d( 0.5674301785250454, -0.1052346781436156 )
            V2d( -0.4929375319230899, 0.09422383038685558 )
            V2d( 0.967785465127825, -0.06868225365333279 )
            V2d( 0.2267967507441493, -0.40237871966279687 )
            V2d( -0.7200979001122771, -0.6248240905561527 )
            V2d( -0.015195608523765971, 0.35623701723070667 )
            V2d( -0.11428925675805125, -0.963723441683084 )
            V2d( 0.5482105069441386, 0.781847612911249 )
            V2d( -0.6515264455787967, 0.7473765703131305 )
            V2d( 0.5826875031269089, -0.6956573112908789 )
            V2d( -0.8496230198638387, 0.09209564840857346 )
            V2d( 0.38289808661249414, 0.15269522898022844 )
            V2d( -0.4951171173546325, -0.2654758742352245 )
        |]
        
    let linearToSrgb (v : V4d) =
        let e = 1.0 / 2.2
        V4d(v.X ** e, v.Y ** e, v.Z ** e, v.W)
        
    [<ReflectedDefinition>]
    let srgbToLinear (v : V4d) =
        let e = 2.2
        V4d(v.X ** e, v.Y ** e, v.Z ** e, v.W)
        
    let trowbridgeReitzNDF (roughness : float) (nDotH : float) =
        let a = roughness * roughness
        let a2 = a * a
        let nDotH2 = nDotH * nDotH
        let denom = nDotH2 * (a2 - 1.0) + 1.0
        a2 / (Constant.Pi * denom * denom)
        
    let fresnel (f0 : V3d) (nv : float) (roughness : float) =
        let a = V3d.III * (1.0 - roughness)
        f0 + (max f0 a - f0) * nv ** 5.0
        
    let schlickBeckmannGAF (d : float) (roughness : float) =
        let a = roughness * roughness
        let k = a * 0.797884560803
        d / (d * (1.0 - k) + k)
        
    let sampleEnv (viewDir : V3d) (roughness : float) =
        let worldDir = uniform.ViewTrafoInv.TransformDir viewDir |> Vec.normalize
 
        //skybox.SampleLevel(worldDir, 12.0) |> srgbToLinear |> Vec.xyz
        
        
        let range = Constant.PiHalf * roughness / 10.0
        
        let size = 2048
        let anglePerPixel = Constant.PiHalf / float size
        let pixelsPerSample = range / (5.0 * anglePerPixel)
        let level = clamp 0.0 12.0 (log2 pixelsPerSample)
        
        let z = worldDir
        let x =
            if abs z.X > abs z.Y then Vec.cross z V3d.OIO |> Vec.normalize
            else Vec.cross z V3d.IOO |> Vec.normalize
        let y = Vec.cross z x
        
        
        let mutable sum = V4d.Zero
        let dt = range / 24.0
        for o in random24 do
            let phi = o.X * Constant.PiTimesTwo
            let theta = o.Y * range
            let dir = V3d(cos phi * sin theta, sin phi * sin theta, cos theta)
            let realDir = x * dir.X + y * dir.Y + z * dir.Z
            
            let c = skybox.SampleLevel(realDir, level) |> srgbToLinear |> Vec.xyz
            let w = exp (-Vec.dot o o * 1.96)
            sum <- sum + V4d(c * w, w)
        sum.XYZ / sum.W
        
    let shade (v : Vertex) =
        fragment {
            let eps = 0.00001
            let fresnelColor = V4d.IIII
            
            let albedo =
                if uniform.HasAlbedoTexture then
                    let tex = albedoTexture.Sample(v.albedoCoord) |> srgbToLinear
                    tex * uniform.AlbedoColor
                else
                    uniform.AlbedoColor
            
            let roughness =
                if uniform.HasRoughnessTexture then
                    let tv = roughnessTexture.Sample(v.roughCoord).X
                    let uv = eps + uniform.Roughness
                    tv * uv |> saturate
                else
                    uniform.Roughness + eps |> saturate
            
            let metalness =
                if uniform.HasMetallicnessTexture then
                    let tv = metallicnessTexture.Sample(v.roughCoord).X
                    let uv = eps + uniform.Metallicness
                    tv * uv |> saturate
                else
                    uniform.Metallicness + eps |> saturate
            
            let occlusion = 1.0
            
            if albedo.W < 0.01 then discard()
            
            let vn = v.normal |> Vec.normalize
            let vld = v.viewLightDir |> Vec.normalize
            let vcd = -v.viewPos.XYZ |> Vec.normalize
            
            let half =
                let v = vld + vcd
                let l = Vec.Length v
                if l > eps then v / l
                else V3d.Zero
            
            let vn =
                if uniform.HasNormalTexture then
                    let vt = Vec.normalize v.viewTangent
                    let vb = Vec.normalize v.viewBiTangent
                    
                    let v = normalTexture.Sample(v.normCoord).XYZ
                    let nn = (v * 2.0 - 1.0) * V3d(V2d.II * uniform.NormalTextureScale * 0.5, 1.0)
                    
                    let newNormal = vn * nn.Z + vt * nn.X + vb * nn.Y |> Vec.normalize
                    if newNormal.Z < 0.0 then vn
                    else newNormal
                else
                    vn
                    
                    
            let refl = -Vec.reflect vn vcd

            let nl = Vec.dot vn vld |> max 0.0
            let nh = Vec.dot vn half |> max 0.0
            //let hv = Vec.dot half vcd |> max 0.0
            let nv = Vec.dot vn vcd |> max 0.0
            
            let f0 = lerp (V3d(0.04, 0.04, 0.04)) (fresnelColor.XYZ * albedo.XYZ) metalness
            let d = trowbridgeReitzNDF nh roughness
            
            let f = fresnel f0 nv roughness
            let g = schlickBeckmannGAF nv roughness * schlickBeckmannGAF nl roughness
            
            let lambert = nl
            let dr = V3d.III * occlusion
            
            let diffuseIrradiance = sampleEnv vn 1.0 * occlusion
            let specularIrradiance = sampleEnv refl roughness * occlusion
            
            let diffuseDirectTerm = (albedo.XYZ / Constant.Pi) * (V3d.III - f) * (1.0 - metalness)
            
            let specularDirectTerm =
                (f * g * d) / (4.0 * nl * nv + eps)
            
            let brdfDirectOutput = (diffuseDirectTerm + specularDirectTerm) * lambert * dr
            let ambientDiffuse = diffuseIrradiance * (albedo.XYZ / Constant.Pi) * (1.0 - f) * (1.0 - metalness)
            
            let ambientSpecular = specularIrradiance * f
            
            let color = brdfDirectOutput + ambientDiffuse + ambientSpecular
            
            return V4d(saturate color, 1.0) |> linearToSrgb
        }
    
    let environment (v : Effects.Vertex) =
        fragment {
            let ndc = v.pos.XY / v.pos.W
            
            let p04 = uniform.ProjTrafoInv * V4d(ndc, -1.0, 1.0)
            let p14 = uniform.ProjTrafoInv * V4d(ndc, -0.8, 1.0)
            
            let dir = p14.XYZ / p14.W - p04.XYZ / p04.W
            
            let res = V4d(sampleEnv dir 0.0, 1.0)
            return linearToSrgb res
            //return skybox.Sample(dir)
        }
    
module private DownsampleCube =
        
    [<ReflectedDefinition>]
    module Shader =
        open FShade
        
        // r = 1
        // l = -1
        // n = 1
        // M44d(
        // 1.0,                     0.0,                       0.0,        0.0,
        // 0.0,                     1.0,                       0.0,        0.0,
        // 0.0,                     0.0,                       0.0,       -1.0,
        // 0.0,                     0.0,                      a,  b
        // )

      
        let skybox =
            samplerCube {
                texture uniform?Skybox
                filter Filter.MinMagMipPoint
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
                addressW WrapMode.Wrap
            }
        
        type UniformScope with
            member x.SourceLevel : int = uniform?SourceLevel
            
        let sampleFace (v : Effects.Vertex) =
            fragment {
                let dx = V2d(1.0 / (float uniform.ViewportSize.X), 0.0)
                let dy = V2d(0.0, 1.0 / (float uniform.ViewportSize.Y))
                
                let p00 = v.pos.XY / v.pos.W
                let p01 = p00 + dy
                let p10 = p00 + dx
                let p11 = p00 + dx + dy
                
                let s00 = uniform.ViewTrafoInv.TransformDir (V3d(p00, -1.0)) |> Vec.normalize
                let s01 = uniform.ViewTrafoInv.TransformDir (V3d(p01, -1.0)) |> Vec.normalize
                let s10 = uniform.ViewTrafoInv.TransformDir (V3d(p10, -1.0)) |> Vec.normalize
                let s11 = uniform.ViewTrafoInv.TransformDir (V3d(p11, -1.0)) |> Vec.normalize
                
                let c00 = skybox.SampleLevel(s00, float uniform.SourceLevel)
                let c01 = skybox.SampleLevel(s01, float uniform.SourceLevel)
                let c10 = skybox.SampleLevel(s10, float uniform.SourceLevel)
                let c11 = skybox.SampleLevel(s11, float uniform.SourceLevel)
                return (c00 + c01 + c10 + c11) * 0.25
            }
    
    let private faces =
        [|
            0, CameraView.lookAt V3d.Zero V3d.IOO V3d.ONO
            1, CameraView.lookAt V3d.Zero V3d.NOO V3d.ONO
            2, CameraView.lookAt V3d.Zero V3d.OIO V3d.OOI
            3, CameraView.lookAt V3d.Zero V3d.ONO V3d.OON
            4, CameraView.lookAt V3d.Zero V3d.OOI V3d.ONO
            5, CameraView.lookAt V3d.Zero V3d.OON V3d.ONO
        |]
    

    let downsampleCubeMap (runtime : IRuntime) (src : ITexture) =
        let src = runtime.PrepareTexture src
        
        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, src.Format
            ]
            
        let view = cval (CameraView.lookAt V3d.Zero V3d.IOO V3d.OOI)
        let viewportSize = cval V2i.II
        let sourceLevel = cval 0
        let task =
            Sg.fullScreenQuad
            |> Sg.shader {
                do! Shader.sampleFace
            }
            |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
            |> Sg.texture' "Skybox" src
            |> Sg.uniform "ViewportSize" viewportSize
            |> Sg.uniform "SourceLevel" sourceLevel
            |> Sg.depthTest' DepthTest.None
            |> Sg.compile runtime signature
            
        let mutable dstSize = max V2i.II (src.Size.XY / 2)
        let mutable dstLevel = 1
        while dstLevel < src.MipMapLevels do
            let srcLevel = dstLevel - 1
            transact (fun () ->
                viewportSize.Value <- dstSize
                sourceLevel.Value <- srcLevel
            )
            
            for (index, cam) in faces do
                use fbo = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, src.[TextureAspect.Color, dstLevel, index] :> IFramebufferOutput])
                transact (fun () -> view.Value <- cam)
                task.Run(fbo)
                
            
            dstSize <- max V2i.II (dstSize / 2)
            dstLevel <- dstLevel + 1
        
        for level in 0 .. src.MipMapLevels - 1 do
            for face in 0 .. 5 do
            
        
            runtime.Download(src, level, face).Save (sprintf "/Users/schorsch/Desktop/textures/f%d_%d.jpg" face level)
        src

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Scene =
    
    let private white =
        let img = PixImage<byte>(Col.Format.RGBA, V2i.II)
        img.GetMatrix<C4b>().Set(C4b.White) |> ignore
        PixTexture2d(PixImageMipMap [| img :> PixImage  |], TextureParams.empty) :> ITexture |> AVal.constant
        
        
    let private meshSg (scene : Scene) (textures : HashMap<ImageId, aval<ITexture>>) (m : Mesh) =
        let fvc =
            match m.Index with
            | Some i -> i.Length
            | None -> m.Positions.Length
                
        let mutable sg = 
            Sg.render m.Mode (DrawCallInfo(FaceVertexCount = fvc, InstanceCount = 1))
            |> Sg.vertexAttribute' DefaultSemantic.Positions m.Positions
                
        match m.Index with
        | Some idx -> sg <- sg |> Sg.indexArray idx
        | None -> ()
        
        match m.Normals with
        | Some ns -> sg <- sg |> Sg.vertexAttribute' DefaultSemantic.Normals ns
        | None -> ()
        
        match m.Tangents with
        | Some ns -> sg <- sg |> Sg.vertexAttribute' Semantic.Tangent ns
        | None -> ()
        
        match m.Colors with
        | Some cs -> sg <- sg |> Sg.vertexAttribute' DefaultSemantic.Colors cs
        | None -> ()
        
        for (data, sems) in m.TexCoords do
            let view = BufferView.ofArray data
            for sem in sems do
                let semantic =
                    match sem with
                    | TexCoordSemantic.BaseColor -> Semantic.AlbedoCoordinate
                    | TexCoordSemantic.Roughness -> Semantic.RoughnessCoordinate
                    | TexCoordSemantic.Emissive -> Semantic.EmissiveCoordinate
                    | TexCoordSemantic.Metallicness -> Semantic.MetallicnessCoordinate
                    | TexCoordSemantic.Normal -> Semantic.NormalCoordinate
                    
                sg <- sg |> Sg.vertexBuffer semantic view
                
        match m.Material |> Option.bind (fun id -> HashMap.tryFind id scene.Materials) with
        | Some mat ->
            let albedoTexture =
                match mat.AlbedoTexutre |> Option.bind (fun id -> HashMap.tryFind id textures) with
                | Some t -> t
                | None -> white
                
            let roughnessTexture =
                match mat.RoughnessTexture |> Option.bind (fun id -> HashMap.tryFind id textures) with
                | Some t -> t
                | None -> white
                
            let metallicnessTexture =
                match mat.MetallicnessTexture |> Option.bind (fun id -> HashMap.tryFind id textures) with
                | Some t -> t
                | None -> white
            
            let normalTexture =
                match mat.NormalTexture |> Option.bind (fun id -> HashMap.tryFind id textures) with
                | Some t -> t
                | None -> white
            
            let emissiveTexture =
                match mat.EmissiveTexture |> Option.bind (fun id -> HashMap.tryFind id textures) with
                | Some t -> t
                | None -> white
                
            let uniforms =
                UniformProvider.ofList [
                    "AlbedoColor", AVal.constant mat.AlbedoColor :> IAdaptiveValue
                    "Roughness", AVal.constant mat.Roughness
                    "Metallicness", AVal.constant mat.Metallicness
                    "EmissiveColor", AVal.constant mat.EmissiveColor
                    "NormalTextureScale", AVal.constant mat.NormalTextureScale
                    
                    "HasNormals", AVal.constant (Option.isSome m.Normals)
                    "HasTangents", AVal.constant (Option.isSome m.Tangents)
                    "HasColors", AVal.constant (Option.isSome m.Colors)
                    
                    "HasAlbedoTexture", AVal.constant (Option.isSome mat.AlbedoTexutre) 
                    "HasRoughnessTexture", AVal.constant (Option.isSome mat.RoughnessTexture) 
                    "HasMetallicnessTexture", AVal.constant (Option.isSome mat.MetallicnessTexture) 
                    "HasEmissiveTexture", AVal.constant (Option.isSome mat.EmissiveTexture) 
                    "HasNormalTexture", AVal.constant (Option.isSome mat.NormalTexture) 
                    
                    "AlbedoTexture", albedoTexture
                    "RoughnessTexture", roughnessTexture
                    "MetallicnessTexture", metallicnessTexture
                    "NormalTexture", normalTexture
                    "EmissiveTexture", emissiveTexture
                ]
            
            
            sg <- Sg.UniformApplicator(uniforms, sg)
                
                
        | None ->
            
            let uniforms =
                UniformProvider.ofList [
                    "AlbedoColor", AVal.constant C4f.White :> IAdaptiveValue
                    "Roughness", AVal.constant 0.5
                    "Metallicness", AVal.constant 0.0
                    "EmissiveColor", AVal.constant C4f.Black
                    "NormalTextureScale", AVal.constant 1.0
                    "HasAlbedoTexture", AVal.constant false
                    "HasRoughnessTexture", AVal.constant false
                    "HasMetallicnessTexture", AVal.constant false
                    "HasEmissiveTexture", AVal.constant false
                    "HasNormalTexture", AVal.constant false
                    
                    "HasNormals", AVal.constant (Option.isSome m.Normals)
                    "HasTangents", AVal.constant (Option.isSome m.Tangents)
                    "HasColors", AVal.constant (Option.isSome m.Colors)
                    
                    "AlbedoTexture", white
                    "RoughnessTexture", white
                    "MetallicnessTexture", white
                    "NormalTexture", white
                    "EmissiveTexture", white
                ]
            sg <- Sg.UniformApplicator(uniforms, sg)
            
        //         
        // let mutable sg = 
        //     Sg.render m.Mode (DrawCallInfo(FaceVertexCount = fvc, InstanceCount = 1))
        //     |> Sg.vertexAttribute' DefaultSemantic.Positions m.Positions
        //     |> (match m.Index with | Some n -> Sg.indexArray n | None -> id)
        //     |> (match m.Normals with | Some n -> Sg.vertexAttribute' DefaultSemantic.Normals n | None -> Sg.vertexBufferValue' DefaultSemantic.Normals V3f.Zero)
        //     |> (match m.Colors with | Some n -> Sg.vertexAttribute' DefaultSemantic.Colors n | None -> Sg.vertexBufferValue' DefaultSemantic.Colors V4f.IIII)
        //     |> (match tc with | Some(att,_) -> Sg.vertexAttribute' DefaultSemantic.DiffuseColorCoordinates att | None -> Sg.vertexBufferValue' DefaultSemantic.DiffuseColorCoordinates V2f.Zero)
        //     |> (match diffuseTexture with | Some t -> Sg.diffuseTexture t| None -> Sg.diffuseTexture white)
        //     
        sg
        
    let toSimpleSg (runtime : IRuntime) (scene : Scene) =
        let texture =
            scene.Images |> HashMap.map (fun _ i -> PixTexture2d(PixImageMipMap [|i|], TextureParams.mipmapped) :> ITexture |> AVal.constant)
 
        let meshes =
            scene.Meshes |> HashMap.map (fun _ m -> meshSg scene texture m)
             
        let rec traverse (node : Node) =
            let cs = node.Children |> Seq.map traverse |> Sg.ofSeq
            let ms =
                match node.Geometry with
                | Some g -> g.Meshes |> List.choose (fun id -> HashMap.tryFind id meshes) |> Sg.ofList
                | None -> Sg.empty
                
            Sg.ofList [cs; ms]
            |> Sg.trafo' node.Trafo
            
        let env = Skybox.get "chapel_$.png" |> AVal.force |> DownsampleCube.downsampleCubeMap runtime :> ITexture |> AVal.constant
            
        Sg.ofList [
            traverse scene.RootNode
            |> Sg.vertexBufferValue' DefaultSemantic.Normals V3f.OOI
            |> Sg.vertexBufferValue' Semantic.Tangent V4f.IOOI
            |> Sg.vertexBufferValue' Semantic.AlbedoCoordinate V2f.Zero
            |> Sg.vertexBufferValue' Semantic.RoughnessCoordinate V2f.Zero
            |> Sg.vertexBufferValue' Semantic.EmissiveCoordinate V2f.Zero
            |> Sg.vertexBufferValue' Semantic.MetallicnessCoordinate V2f.Zero
            |> Sg.vertexBufferValue' Semantic.NormalCoordinate V2f.Zero
            |> Sg.texture "Skybox" env
            
            Sg.farPlaneQuad
            |> Sg.texture "Skybox" env
            |> Sg.shader {
                do! Shader.environment
            }
            
               
        ]
            
            

