module GUI (runApplication) where

import qualified Config                               as Conf
import           Data.IORef
import           Data.WAVE
import           Graphics.Rendering.OpenGL.GLU.Matrix
import           Graphics.UI.GLUT
import qualified Queue                                as Q
import           Sound
import           System.Posix.Unistd


type Spectra = Q.Queue [GLfloat];
type Spectrum = [GLfloat];


runApplication :: IO ()
runApplication = do
  (_progName, _args) <- getArgsAndInitialize

  setupEnvironment

  -- set up spectra
  spectra <- newIORef (Q.newQueue :: Spectra)
  modifyIORef spectra (Q.push [])

  -- set up a wave file
  wave <- getWAVEFile (head _args)
  waves <- newIORef (waveSamples wave)

  let samplesPerSecond = (waveFrameRate $ waveHeader wave)

  -- set up callbacks
  displayCallback $= display spectra waves samplesPerSecond
  reshapeCallback $= Just reshape

  mainLoop


-- | Set up environment, i.e. create window, perspective.
setupEnvironment :: IO ()
setupEnvironment = do
  -- set up a window
  _window <- createWindow "Haskell FFT"
  -- set up window size
  windowSize $= Conf.windowSize
  -- set up perspective
  perspective Conf.fov Conf.ratio 0.01 1000
  lookAt Conf.cameraPosition Conf.cameraCenter Conf.upVector


-- | The display callback used for rendering.
display :: IORef Spectra -> IORef WAVESamples -> Int -> DisplayCallback
display spectraRef waveRef samplesPerSecond = do
  updateSpectra spectraRef waveRef

  render spectraRef

  -- wait in order to sync the audio
  nanosleep $ fromIntegral (quot (10^9) samplesPerSecond)
  postRedisplay Nothing


-- | Function responsible for updating spectra.
updateSpectra :: IORef Spectra -> IORef WAVESamples -> IO ()
updateSpectra spectraRef waveRef = do
  wave <- get waveRef
  let timeDomain = (take Conf.samplesPerTime (wavesToTime wave))
  let freqDomain = soundFFT timeDomain
  let freqDomain2 = map (\f -> realToFrac $ (8 * log(f+1))) freqDomain :: [GLfloat]
  modifyIORef waveRef (\w -> drop (Conf.samplesPerTime) w)

  modifyIORef spectraRef (\s ->
    Q.push freqDomain2 $
    snd (Q.pop s))


-- | The whole process of rendering.
render :: IORef Spectra -> IO ()
render spectraRef = do
  clear [ColorBuffer]
  spectra <- get spectraRef
  renderSpectra spectra 0 (Conf.dataWidth/fromIntegral Conf.samplesPerFreq) 40
  flush


renderSpectra :: Spectra -> GLfloat -> GLfloat -> GLfloat -> IO ()
renderSpectra spectra from dx dz = renderPrimitive Quads $ do
  z <- newIORef (from :: GLfloat)
  mapM_ (\spec -> do
    zv <- get z
    renderSpectrum spec (zv) dx
    modifyIORef z (+(dz))) spectra


renderSpectrum :: Spectrum -> GLfloat -> GLfloat -> IO ()
renderSpectrum spec z dd = do
  x <- newIORef (0 :: GLfloat)
  mapM_ (\val -> do
    xv <- get x
    -- colorize
    color $ Color3 (xv/fromIntegral Conf.samplesPerFreq) 0 (1-xv/fromIntegral Conf.samplesPerFreq)
    vertex $ Vertex3 (xv*dd) 0 z
    vertex $ Vertex3 (xv*dd+dd) 0 z
    vertex $ Vertex3 (xv*dd+dd) val z
    vertex $ Vertex3 (xv*dd) val z
    modifyIORef x (+1)) spec


reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing
