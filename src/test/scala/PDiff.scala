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
        "succeed on identical images" in {
            val imgA = mkImage((_, _) => Color.white)
            val imgB = mkImage((_, _) => Color.white)
            compare(imgA, imgB) === Identical
        }

        "succeed on indistinguishable images" in {
            val imgA = mkImage((_, _) => Color.white)
            val imgB = mkImage { case (0, y) if y < 99 => Color.black; case _ => Color.white }
            compare(imgA, imgB) === Indistinguishable(99)
        }

        "fail on different images" in {
            val imgA = mkImage((_, _) => Color.white)
            val imgB = mkImage((_, _) => Color.black)
            compare(imgA, imgB) === Different(10000)
        }

        "fail on images with wrong dimensions" in {
            val imgA = mkImage(100, 100)((_, _) => Color.white)
            val imgB = mkImage(100, 200)((_, _) => Color.white)
            compare(imgA, imgB) === WrongDimensions
        }
    }
}
