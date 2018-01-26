{-|
This module is responsible for supplying general configuration for
the application. For example: camera position, window size etc.
-}
module Config
  ( ratio
  , windowSize
  , fov
  , dataHeight
  , dataWidth
  , dataDepth
  , cameraPosition
  , cameraCenter
  , upVector
  , samplesPerTime
  , samplesPerFreq
  , samplesShift
  ) where

import           Graphics.UI.GLUT (GLdouble, GLfloat, Size (..), Vector3 (..),
                                   Vertex3 (..))

f2d :: GLfloat -> GLdouble
f2d x = realToFrac x

-- | Window size.
windowSize :: Size
windowSize = Size 800 450

-- | Window ratio.
ratio :: GLdouble
ratio = (fromIntegral x) / (fromIntegral y)
  where Size x y = windowSize

-- | Field of view of the camera.
fov :: GLdouble
fov = 40

-- | Width of the generated data.
dataWidth :: GLfloat
dataWidth = 100

-- | Height of the generated data.
dataHeight :: GLfloat
dataHeight = 50

-- | Depth of the generated data.
dataDepth :: GLfloat
dataDepth = 50

-- | Number of samples for time domain.
samplesPerTime :: Int
samplesPerTime = 1024

-- | Number of samples for frequency domain.
samplesPerFreq :: Int
samplesPerFreq = quot samplesPerTime 4

-- | Number of samples to ignore from the start of
-- frequency domain.
samplesShift :: Int
samplesShift = quot samplesPerFreq 20

-- | Camera position.
cameraPosition :: Vertex3 GLdouble
cameraPosition = Vertex3 (f2d dataWidth) (f2d $ 2*dataHeight/3) (f2d $ dataWidth)

-- | A point, the camera will be looking at.
cameraCenter :: Vertex3 GLdouble
cameraCenter = Vertex3 (f2d $ 5*dataWidth/9) (f2d $ dataHeight/2) 0

-- | A vector indicating the vertical.
upVector :: Vector3 GLdouble
upVector = Vector3 0 1 0
