package io.continuum.pdiff
package tests

import org.specs2.mutable._

import java.awt.Color
import java.awt.image.BufferedImage

import PDiff.{compare,Identical,Indistinguishable,WrongDimensions,Different}

class PDiffSpec extends Specification {
    sequential

    def mkImage(width: Int, height: Int)(fn: (Int, Int) => Color): BufferedImage = {
        val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
        for (y <- 0 until height) {
            for (x <- 0 until width) {
                image.setRGB(x, y, fn(x, y).getRGB)
            }
        }
        image
    }

    def mkImage(fn: (Int, Int) => Color): BufferedImage = mkImage(100, 100)(fn)

    "PDiff" should {
        "support succeed on identical images" in {
            val white0 = mkImage((_, _) => Color.white)
            val white1 = mkImage((_, _) => Color.white)
            compare(white0, white1) === Identical
        }

        "support fail on different images" in {
            val white0 = mkImage((_, _) => Color.white)
            val black0 = mkImage((_, _) => Color.black)
            compare(white0, black0) === Different(10000)
        }
    }
}
