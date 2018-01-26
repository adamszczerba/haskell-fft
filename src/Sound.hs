module Sound (soundFFT, wavesToTime) where

import qualified Config       as Conf
import           Data.Complex
import           Data.WAVE
import           Numeric.FFT

-- | Perform Fast Fourier Transform on the input data, called the
-- "time" domain, in order to get its representation in the "frequency"
-- domain.
soundFFT :: [Double] -> [Double]
soundFFT timeDomain = take Conf.samplesPerFreq (drop Conf.samplesShift $ mfft $ map (\t -> t :+ 0) timeDomain)


mfft :: [Complex Double] -> [Double]
mfft p = map (\l -> realPart $ abs l) (fft p)


-- | Converts WAVESamples to [Double] using sampleToDouble
-- function. For each channel it takes the average.
wavesToTime :: WAVESamples -> [Double]
wavesToTime []     = []
wavesToTime (h:tl) = (average $ map sampleToDouble h) : wavesToTime tl


-- | Calculates the average value of a given list.
average :: [Double] -> Double
average list = (sum list) / realToFrac (length list)
