module Cube where

import Prelude

import Data.Tuple
import Data.Array (mapWithIndex, (!!), range)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (snoc, fromFoldable, take, length, drop)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (cos, sin)
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE

-- Core Types
type Distance = Number

type Angle = Number

type Point2D =
  { x :: Distance
  , y :: Distance
  }

type Point3D =
  { x :: Distance
  , y :: Distance
  , z :: Distance
  }

type Edge = Tuple Int Int

type Shape =
  { vertices :: Array Point3D
  , edges :: Array Edge
  }

type Angle3D =
  { xa :: Angle
  , ya :: Angle
  , za :: Angle
  }

type AngVelocity3D = Angle3D -- velocity = angle/sec

type Cube =
  { shape :: Shape
  , angVel :: AngVelocity3D
  , forward :: Boolean
  , accelerateBy :: Number -- Add this field
  , cubeCount :: Int
  }

data Axis = X | Y | Z

-- Model / State
type State = Array Cube

-- Values

viewBoxSize :: Number
viewBoxSize = 600.0

viewCenter :: Point2D
viewCenter =
  { x: viewBoxSize / 2.0
  , y: viewBoxSize / 2.0
  }

frameRate :: Number
frameRate = 200.0

oneDegInRad :: Angle
oneDegInRad = 0.01745329255

tenDegInRad :: Angle
tenDegInRad = oneDegInRad * 10.0

accelerateBy :: Number
accelerateBy = oneDegInRad * 50.0

dampenPercent :: Number
dampenPercent = 1.0 - (0.9 / frameRate) -- 10% per second

initCube :: Cube
initCube =
  { shape:
      { vertices:
          [ { x:  100.0, y:  100.0, z:  100.0 }
          , { x: -100.0, y:  100.0, z:  100.0 }
          , { x:  100.0, y: -100.0, z:  100.0 }
          , { x: -100.0, y: -100.0, z:  100.0 }
          , { x:  100.0, y:  100.0, z: -100.0 }
          , { x: -100.0, y:  100.0, z: -100.0 }
          , { x:  100.0, y: -100.0, z: -100.0 }
          , { x: -100.0, y: -100.0, z: -100.0 }
          ]
      , edges:
          [ Tuple 0 1
          , Tuple 0 2
          , Tuple 0 4
          , Tuple 1 5
          , Tuple 1 3
          , Tuple 2 3
          , Tuple 2 6
          , Tuple 4 5
          , Tuple 4 6
          , Tuple 3 7
          , Tuple 6 7
          , Tuple 5 7
          ]
      }
  , angVel: 
      { xa: tenDegInRad
      , ya: tenDegInRad
      , za: tenDegInRad
      }
  , forward: true
  , accelerateBy: oneDegInRad * 50.0 -- Default accelerateBy value
  , cubeCount: 1
  }

data Query a = Tick a | Other a

-- Events
data Action
  = DecAngVelocity Axis
  | IncAngVelocity Axis
  | IncAngVelocityLocal Int Axis
  | ReverseCube Int
  | DecMoreAngVelocity Int
  | IncMoreAngVelocity Int
  | AddCube 
  | RemoveCube Int

cubes :: forall query input output m. H.Component Query input output m
cubes =
    H.mkComponent
        { initialState: const [initCube]
        , render
        , eval: H.mkEval $ H.defaultEval 
              { 
                handleAction = handleAction 
              , handleQuery = handleQuery
              }
        }
    where
        render :: forall m. State -> H.ComponentHTML Action () m
        render = renderView

        runFunction :: (State -> State) -> H.HalogenM State Action () output m Unit
        runFunction fn = do
          _ <- H.modify fn
          pure unit

        handleAction :: Action -> H.HalogenM State Action () output m Unit
        handleAction action = case action of
            DecAngVelocity axis -> runFunction (map (decAngVelocity axis))
            IncAngVelocity axis -> runFunction (map (incAngVelocity axis))
            IncAngVelocityLocal idx axis -> runFunction (updateCube idx (incAngVelocityLocal axis))
            ReverseCube idx -> runFunction (updateCube idx reverseCube)
            DecMoreAngVelocity idx -> runFunction (updateCube idx decMoreAngVelocity)
            IncMoreAngVelocity idx -> runFunction (updateCube idx incMoreAngVelocity)
            AddCube -> runFunction addCube
            RemoveCube idx -> runFunction (removeCube idx)
          
        handleQuery :: forall m a message. Query a -> H.HalogenM State Action () message m (Maybe a)
        handleQuery = case _ of
          Tick a -> do
            _ <- H.modify (map tick)
            pure (Just a)
          Other a -> 
            pure (Just a)

updateCube :: Int -> (Cube -> Cube) -> State -> State
updateCube idx fn state = mapWithIndex (\i cube -> if i == idx then fn cube else cube) state

-- Increase `accelerateBy` by 3 times
incMoreAngVelocity :: Cube -> Cube
incMoreAngVelocity c =
  let newAccelerateBy = c.accelerateBy * 4.0 -- 3.0 times + the original value
  in c { accelerateBy = newAccelerateBy }

-- Decrease `accelerateBy` by 3 times
decMoreAngVelocity :: Cube -> Cube
decMoreAngVelocity c =
  let newAccelerateBy = c.accelerateBy * 0.25 -- Decrease by 3.0 times
  in c { accelerateBy = newAccelerateBy }

-- Reverse the rotation direction and update forward state
reverseCube :: Cube -> Cube
reverseCube c = c { forward = not c.forward }

-- Increase angular velocity by `accelerateBy`
incAngVelocity :: Axis -> Cube -> Cube
incAngVelocity axis c =
  let {xa, ya, za} = c.angVel
      increase ang = ang + c.accelerateBy
  in case axis of
    X -> c { angVel = c.angVel { xa = increase xa } }
    Y -> c { angVel = c.angVel { ya = increase ya } }
    Z -> c { angVel = c.angVel { za = increase za } }

-- Increase angular velocity by `accelerateBy` Local considering forward state
incAngVelocityLocal :: Axis -> Cube -> Cube
incAngVelocityLocal axis c =
  let {xa, ya, za} = c.angVel
      adjust ang = if c.forward then ang + c.accelerateBy else ang - c.accelerateBy
  in case axis of
    X -> c { angVel = c.angVel { xa = adjust xa } }
    Y -> c { angVel = c.angVel { ya = adjust ya } }
    Z -> c { angVel = c.angVel { za = adjust za } }

-- Decrease angular velocity by `accelerateBy`
decAngVelocity :: Axis -> Cube -> Cube
decAngVelocity axis c =
  let {xa, ya, za} = c.angVel
      decrease ang = ang - c.accelerateBy
  in case axis of
    X -> c { angVel = c.angVel { xa = decrease xa } }
    Y -> c { angVel = c.angVel { ya = decrease ya } }
    Z -> c { angVel = c.angVel { za = decrease za } }

-- Assuming `initCube` is the default new cube
addCube :: State -> State
addCube state = snoc state initCube

-- Remove the cube at the specified index, ensuring there is at least one cube
removeCube :: Int -> State -> State
removeCube idx state = 
  if length state > 1 then
    fromMaybe state (deleteAt idx state)
  else
    state

deleteAt :: Int -> State -> Maybe (State)
deleteAt idx xs = 
  if length xs > idx then 
    Just $ take idx xs <> drop (idx + 1) xs 
  else 
    Nothing

tick :: Cube -> Cube
tick c = 
  let angVel = c.angVel
      {vertices, edges} = c.shape
      newShape =
        { edges: edges
        , vertices: rotateShape vertices (anglePerFrame angVel)
        }
      newCube = c
        { angVel = dampenAngVelocity angVel
        , shape = newShape
        }
  in newCube

rotateShape :: Array Point3D -> AngVelocity3D -> Array Point3D
rotateShape vertices ang =
  map (rotate ang) vertices

rotate :: AngVelocity3D -> Point3D -> Point3D
rotate { xa, ya, za } = rotateX xa >>> rotateY ya >>> rotateZ za
  where
    rotateX ang {x,y,z} = let Tuple ny nz = rotateInPlane y z ang in { x, y:ny, z:nz }
    rotateY ang {x,y,z} = let Tuple nx nz = rotateInPlane x z ang in { x:nx, y, z:nz }
    rotateZ ang {x,y,z} = let Tuple nx ny = rotateInPlane x y ang in { x:nx, y:ny, z }

    rotateInPlane :: Number -> Number -> Number -> Tuple Number Number
    rotateInPlane axis1 axis2 ang =
      Tuple (axis1 * cos(ang) - axis2 * sin(ang)) (axis2 * cos(ang) + axis1 * sin(ang))

anglePerFrame :: AngVelocity3D -> Angle3D
anglePerFrame {xa, ya, za} =
  { xa: xa / frameRate
  , ya: ya / frameRate
  , za: za / frameRate
  }

dampenAngVelocity :: AngVelocity3D -> AngVelocity3D
dampenAngVelocity {xa, ya, za} =
    { xa: dampen xa
    , ya: dampen ya
    , za: dampen za }
  where
    dampen :: Number -> Number
    dampen ang = ang * dampenPercent -- Basics.max 0 (ang-drpf)

renderView :: forall m. Array Cube -> H.ComponentHTML Action () m
renderView state =
    HH.div [] $ 
    [ HH.div [] 
      (renderGlobalControls <> renderCubes state)
    ]
  where
    renderGlobalControls = 
      [ renderButton "Global rotX++" (IncAngVelocity X)
      , renderButton "Global rotY++" (IncAngVelocity Y)
      , renderButton "Global rotZ++" (IncAngVelocity Z)
      ]

    renderCubes :: Array Cube -> Array (H.ComponentHTML Action () m)
    renderCubes cubes = mapWithIndex renderCube cubes

    renderCube :: Int -> Cube -> H.ComponentHTML Action () m
    renderCube idx cube = 
      let
        { vertices, edges } = cube.shape
        vert2Ds = map project vertices
      in
        HH.div [] 
          [ renderButton "rotX++" (IncAngVelocityLocal idx X)
          , renderButton "rotY++" (IncAngVelocityLocal idx Y)
          , renderButton "rotZ++" (IncAngVelocityLocal idx Z)
          , renderButton "Reverse" (ReverseCube idx)
          , renderButton "Vel++" (IncMoreAngVelocity idx)
          , renderButton "Vel--" (DecMoreAngVelocity idx)
          , renderButton "Add" AddCube
          , renderButton "Remove" (RemoveCube idx)
          , SE.svg
              [ SA.viewBox 0.0 0.0 viewBoxSize viewBoxSize ]
              [ SE.g [] (drawCube edges vert2Ds) ]
          ]

    renderButton :: String -> Action -> H.ComponentHTML Action () m
    renderButton label action =
        HH.button
        [ HP.title label
        , HE.onClick \_ -> action
        ]
        [ HH.text label ]

    -- parallel projection
    project :: Point3D -> Point2D
    project p =
        { x: p.x + viewCenter.x
        , y: p.y + viewCenter.y
        }

    drawCube :: forall m. Array Edge -> Array Point2D -> Array (H.ComponentHTML Action () m)
    drawCube edges vert2Ds =
        drawEdges edges vert2Ds <> drawVertices vert2Ds

    drawEdges :: forall m. Array Edge -> Array Point2D -> Array (H.ComponentHTML Action () m)
    drawEdges edges verts = 
        let connectedVerts = map (\(Tuple v1 v2) -> Tuple (verts !! v1) (verts !! v2)) edges
        in map (\(Tuple v1 v2) -> drawLine (getPoint v1) (getPoint v2)) connectedVerts

    getPoint :: Maybe Point2D -> Point2D
    getPoint maybePoint = fromMaybe { x: 100.0, y: 100.0 } maybePoint

    drawVertices :: forall m. Array Point2D -> Array (H.ComponentHTML Action () m)
    drawVertices vert2Ds =
        mapWithIndex drawVertex vert2Ds

    drawLine :: forall m. Point2D -> Point2D -> H.ComponentHTML Action () m
    drawLine a b =
        SE.line 
        [ SA.x1 a.x
        , SA.x2 b.x
        , SA.y1 a.y
        , SA.y2 b.y
        , SA.stroke $ Just (SA.RGB 50 50 50)
        ]

    drawVertex :: forall m. Int -> Point2D -> H.ComponentHTML Action () m
    drawVertex idx {x, y} = SE.g []
        [ SE.text
            [ SA.x $ x + 5.0
            , SA.y $ y - 5.0
            , SA.fill $ Just (SA.RGB 150 150 150)
            ]
            [ HH.text $ show idx ]
        , SE.circle
            [ SA.r 3.0
            , SA.cx x
            , SA.cy y
            , SA.fill $ Just (SA.RGB 100 100 100)
            ]
        ]
