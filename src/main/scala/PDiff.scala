package io.continuum.pdiff

/**
 * PerceptualDiff - a program that compares two images using a perceptual metric based on the paper:
 *
 * A perceptual metric for production testing. Journal of graphics tools, 9(4):33-40, 2004, Hector Yee
 *
 * Copyright (C) 2006 Yangli Hector Yee
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the
 * GNU General Public License as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

object PDiff {
    val MAX_PYR_LEVELS = 8

    class LPyramid(image: Array[Float], width: Int, height: Int) {
        // Succesively blurred versions of the original image
        val levels: Array[Array[Float]] = new Array(MAX_PYR_LEVELS)

        for (i <- 0 until MAX_PYR_LEVELS) {
            i match {
                case 0 => levels(i) = image.clone()
                case _ => levels(i) = convolve(levels(i - 1))
            }
        }

        def getValue(x: Int, y: Int, level: Int): Float = {
            val index = x + y*width
            //int l = level
            //if (l > MAX_PYR_LEVELS) l = MAX_PYR_LEVELS
            levels(level)(index)
        }

        // convolves image b with the filter kernel and stores it in a
        protected def convolve(input: Array[Float]): Array[Float] = {
            val kernel: Array[Float] = Array(0.05f, 0.25f, 0.40f, 0.25f, 0.05f)
            val output: Array[Float] = new Array(width*height)

            for (y <- 0 until height) {
                for (x <- 0 until width) {
                    val index = y*width + x
                    output(index) = 0.0f
                    for (i <- -2 to 2) {
                        for (j <- -2 to 2) {
                            var nx = x + i
                            var ny = y + j
                            if (nx < 0) nx = -nx
                            if (ny < 0) ny = -ny
                            if (nx >= width)  nx = 2*width  - nx - 1
                            if (ny >= height) ny = 2*height - ny - 1
                            output(index) += kernel(i+2)*kernel(j+2)*input(ny*width + nx)
                        }
                    }
                }
            }

            output
        }
    }

    /** Class encapsulating an image containing R,G,B,A channels.
     *
     * Internal representation assumes data is in the ABGR format, with the RGB
     * color channels premultiplied by the alpha value.  Premultiplied alpha is
     * often also called "associated alpha" - see the tiff 6 specification for some
     * discussion - http://partners.adobe.com/asn/developer/PDFS/TN/TIFF6.pdf
     *
     */
    object RGBAImage {
        def apply(file: java.io.File): RGBAImage = {
            import javax.imageio.ImageIO

            val input = new java.io.FileInputStream(file)
            val image = ImageIO.read(input)
            val width = image.getWidth
            val height = image.getHeight
            val data = new Array[Long](width*height)

            for (y <- 0 until height) {
                for (x <- 0 until width) {
                    data(y*width + x) = image.getRGB(x, y)
                }
            }

            new RGBAImage(width, height, data)
        }
    }
    class RGBAImage(val width: Int, val height: Int, val data: Array[Long]) {
        def this(width: Int, height: Int) = this(width, height, new Array(width*height))

        /*unsigned char*/ def getRed(i: Int): Long = data(i) & 0xFF
        /*unsigned char*/ def getGreen(i: Int): Long = (data(i) >> 8) & 0xFF
        /*unsigned char*/ def getBlue(i: Int): Long = (data(i) >> 16) & 0xFF
        /*unsigned char*/ def getAlpha(i: Int): Long = (data(i) >> 24) & 0xFF

        def set(/*unsigned char*/ r: Long, /*unsigned char*/ g: Long, /*unsigned char */ b: Long, /*unsigned char*/ a: Long, i: Int) {
            data(i) = r | (g << 8) | (b << 16) | (a << 24)
        }

        def set(x: Int, y: Int, d: Long) {
            data(x + y*width) = d
        }

        def get(x: Int, y: Int): Long = data(x + y*width)
        def get(i: Int): Long = data(i)

        def downSample(): RGBAImage = {
           // if (Width <=1 || Height <=1) return NULL;
           val nw = width / 2
           val nh = height / 2
           val img = new RGBAImage(nw, nh)
           for (y <- 0 until nh) {
              for (x <- 0 until nw) {
                 // Sample a 2x2 patch from the parent image.
                 val d0 = get(2*x + 0, 2*y + 0)
                 val d1 = get(2*x + 1, 2*y + 0)
                 val d2 = get(2*x + 0, 2*y + 1)
                 val d3 = get(2*x + 1, 2*y + 1)
                 var rgba: Long = 0
                 // Find the average color.
                 for (i <- 0 until 4) {
                    var c = (d0 >> (8*i)) & 0xFF
                    c += (d1 >> (8*i)) & 0xFF
                    c += (d2 >> (8*i)) & 0xFF
                    c += (d3 >> (8*i)) & 0xFF
                    c /= 4
                    rgba |= (c & 0xFF) << (8*i)
                 }
                 img.set(x, y, rgba)
              }
           }
           img
        }
    }

    val M_PI = 3.14159265f

    /*
    * Given the adaptation luminance, this function returns the
    * threshold of visibility in cd per m^2
    * TVI means Threshold vs Intensity function
    * This version comes from Ward Larson Siggraph 1997
    */

    def tvi(adaptation_luminance: Float): Float = {
        // returns the threshold luminance given the adaptation luminance
        // units are candelas per meter squared
        val log_a: Float = math.log10(adaptation_luminance).toFloat

        val r: Float =
                 if (log_a < -3.94f)   -2.86f
            else if (log_a < -1.44f)   math.pow(0.405f*log_a + 1.6f, 2.18f).toFloat - 2.86f
            else if (log_a < -0.0184f) log_a - 0.395f
            else if (log_a < 1.9f)     math.pow(0.249f*log_a + 0.65f, 2.7f).toFloat - 0.72f
            else                      log_a - 1.255f

        math.pow(10.0f, r).toFloat
    }

    // computes the contrast sensitivity function (Barten SPIE 1989)
    // given the cycles per degree (cpd) and luminance (lum)
    def csf(cpd: Float, lum: Float): Float = {
        val a: Float = 440.0f*math.pow((1.0f + 0.7f / lum), -0.2f).toFloat
        val b: Float = 0.3f*math.pow((1.0f + 100.0f / lum), 0.15f).toFloat
        a*cpd*math.exp(-b*cpd).toFloat*math.sqrt(1.0f + 0.06f*math.exp(b*cpd).toFloat).toFloat
    }

    /*
    * Visual Masking Function
    * from Daly 1993
    */
    def mask(contrast: Float): Float = {
        val a: Float = math.pow(392.498f*contrast, 0.7f).toFloat
        val b: Float = math.pow(0.0153f*a, 4.0f).toFloat
        math.pow(1.0f + b, 0.25f).toFloat
    }

    // convert Adobe RGB (1998) with reference white D65 to XYZ
    def AdobeRGBToXYZ(r: Float, g: Float, b: Float): (Float, Float, Float) = {
        // matrix is from http://www.brucelindbloom.com/
        val x: Float = r*0.576700f + g*0.185556f + b*0.188212f
        val y: Float = r*0.297361f + g*0.627355f + b*0.0752847f
        val z: Float = r*0.0270328f + g*0.0706879f + b*0.991248f
        (x, y, z)
    }

    object XYZToLAB {
        private val (xw, yw, zw) = AdobeRGBToXYZ(1, 1, 1) // reference white

        def apply(x: Float, y: Float, z: Float): (Float, Float, Float) = {
            val epsilon = 216.0f / 24389.0f
            val kappa = 24389.0f / 27.0f
            val f: Array[Float] = new Array(3)
            val r: Array[Float] = new Array(3)
            r(0) = x / xw
            r(1) = y / yw
            r(2) = z / zw
            for (i <- 0 until 3) {
                if (r(i) > epsilon) {
                    f(i) = math.pow(r(i), 1.0f / 3.0f).toFloat
                } else {
                    f(i) = (kappa*r(i) + 16.0f) / 116.0f
                }
            }
            val L: Float = 116.0f*f(1) - 16.0f
            val A: Float = 500.0f*(f(0) - f(1))
            val B: Float = 200.0f*(f(1) - f(2))
            (L, A, B)
        }
    }

    case class CompareArgs(
        imgA: RGBAImage,                   // Image A
        imgB: RGBAImage,                   // Image B
        imgDiff: Option[RGBAImage] = None, // Diff image
        verbose: Boolean = false,          // Print lots of text or not
        luminanceOnly: Boolean = false,    // Only consider luminance; ignore chroma channels in the comparison.
        fieldOfView: Float = 45.0f,        // Field of view in degrees
        gamma: Float = 2.2f,               // The gamma to convert to linear color space
        luminance: Float = 100.0f,         // the display's luminance
        thresholdPixels: Int = 100,        // How many pixels different to ignore

        // How much color to use in the metric.
        // 0.0 is the same as LuminanceOnly = true,
        // 1.0 means full strength.
        colorFactor: Float = 1.0f,
        // How much to down sample image before comparing, in powers of 2.
        downSample: Int = 0)

    sealed trait CompareResult
    sealed trait CompareSuccess extends CompareResult
    sealed trait CompareFailure extends CompareResult

    case object Identical extends CompareSuccess
    case class Indistinguishable(pixels_failed: Int) extends CompareSuccess

    case object WrongDimensions extends CompareFailure
    case class Different(pixels_failed: Int) extends CompareFailure

    def compare(args: CompareArgs): CompareResult = {
        if ((args.imgA.width != args.imgB.width) || (args.imgA.height != args.imgB.height)) {
            return WrongDimensions
        }

        if (args.imgA.data sameElements args.imgB.data) {
            return Identical
        }

        val dim = args.imgA.width*args.imgA.height

        // assuming colorspaces are in Adobe RGB (1998) convert to XYZ
        val aX: Array[Float] = new Array(dim)
        val aY: Array[Float] = new Array(dim)
        val aZ: Array[Float] = new Array(dim)
        val bX: Array[Float] = new Array(dim)
        val bY: Array[Float] = new Array(dim)
        val bZ: Array[Float] = new Array(dim)
        val aLum: Array[Float] = new Array(dim)
        val bLum: Array[Float] = new Array(dim)

        val aA: Array[Float] = new Array(dim)
        val bA: Array[Float] = new Array(dim)
        val aB: Array[Float] = new Array(dim)
        val bB: Array[Float] = new Array(dim)

        if (args.verbose) println("Converting RGB to XYZ")

        val w = args.imgA.width
        val h = args.imgA.height

        for (y <- 0 until h) {
            for (x <- 0 until w) {
                val i = x + y*w

                {
                    val r: Float = math.pow(args.imgA.getRed(i)   / 255.0f, args.gamma).toFloat
                    val g: Float = math.pow(args.imgA.getGreen(i) / 255.0f, args.gamma).toFloat
                    val b: Float = math.pow(args.imgA.getBlue(i)  / 255.0f, args.gamma).toFloat
                    val (aXi, aYi, aZi) = AdobeRGBToXYZ(r, g, b)
                    aX(i) = aXi; aY(i) = aYi; aZ(i) = aZi
                    val (_, aAi, aBi) = XYZToLAB(aX(i), aY(i), aZ(i))
                    aA(i) = aAi; aB(i) = aBi
                }

                {
                    val r: Float = math.pow(args.imgB.getRed(i)   / 255.0f, args.gamma).toFloat
                    val g: Float = math.pow(args.imgB.getGreen(i) / 255.0f, args.gamma).toFloat
                    val b: Float = math.pow(args.imgB.getBlue(i)  / 255.0f, args.gamma).toFloat
                    val (bXi, bYi, bZi) = AdobeRGBToXYZ(r, g, b)
                    bX(i) = bXi; bY(i) = bYi; bZ(i) = bZi
                    val (_, bAi, bBi) = XYZToLAB(bX(i), bY(i), bZ(i))
                    bA(i) = bAi; bB(i) = bBi
                }

                aLum(i) = aY(i)*args.luminance
                bLum(i) = bY(i)*args.luminance
            }
        }

        if (args.verbose) println("Constructing Laplacian Pyramids")

        val la = new LPyramid(aLum, w, h)
        val lb = new LPyramid(bLum, w, h)

        val num_one_degree_pixels: Float = 2*math.tan(args.fieldOfView*0.5*M_PI / 180).toFloat*180 / M_PI
        val pixels_per_degree: Float = w / num_one_degree_pixels

        if (args.verbose) println("Performing test")

        def compute_adaptation_level(): Int = {
            var num_pixels: Float = 1
            var adaptation_level = 0;
            for (i <- 0 until MAX_PYR_LEVELS) {
                adaptation_level = i
                if (num_pixels > num_one_degree_pixels) return adaptation_level
                num_pixels *= 2
            }

            return adaptation_level
        }

        val adaptation_level = compute_adaptation_level()

        val cpd: Array[Float] = new Array(MAX_PYR_LEVELS)
        cpd(0) = 0.5f*pixels_per_degree
        for (i <- 1 until MAX_PYR_LEVELS) {
            cpd(i) = 0.5f*cpd(i - 1)
        }

        val csf_max = csf(3.248f, 100.0f)

        val F_freq: Array[Float] = new Array(MAX_PYR_LEVELS - 2)
        for (i <- 0 until MAX_PYR_LEVELS - 2) {
            F_freq(i) = csf_max / csf(cpd(i), 100.0f)
        }

        var pixels_failed = 0

        for (y <- 0 until h) {
          for (x <- 0 until w) {
            val index = x + y*w
            val contrast: Array[Float] = new Array(MAX_PYR_LEVELS - 2)
            var sum_contrast: Float = 0
            for (i <- 0 until MAX_PYR_LEVELS - 2) {
                val n1: Float = math.abs(la.getValue(x, y, i) - la.getValue(x, y, i + 1)).toFloat
                val n2: Float = math.abs(lb.getValue(x, y, i) - lb.getValue(x, y, i + 1)).toFloat
                val numerator: Float = if (n1 > n2) n1 else n2
                val d1: Float = math.abs(la.getValue(x, y, i + 2)).toFloat
                val d2: Float = math.abs(lb.getValue(x, y, i + 2)).toFloat
                var denominator: Float = if (d1 > d2) d1 else d2
                if (denominator < 1e-5f) denominator = 1e-5f
                contrast(i) = numerator / denominator
                sum_contrast += contrast(i)
            }
            if (sum_contrast < 1e-5) sum_contrast = 1e-5f
            val F_mask: Array[Float] = new Array(MAX_PYR_LEVELS - 2)
            var adapt: Float = la.getValue(x, y, adaptation_level) + lb.getValue(x, y, adaptation_level)
            adapt *= 0.5f
            if (adapt < 1e-5) adapt = 1e-5f

            for (i <- 0 until MAX_PYR_LEVELS - 2) {
                F_mask(i) = mask(contrast(i)*csf(cpd(i), adapt))
            }
            var factor: Float = 0
            for (i <- 0 until MAX_PYR_LEVELS - 2) {
                factor += contrast(i)*F_freq(i)*F_mask(i) / sum_contrast
            }
            if (factor < 1) factor = 1
            if (factor > 10) factor = 10
            val delta: Float = math.abs(la.getValue(x, y, 0) - lb.getValue(x, y, 0)).toFloat
            var pass: Boolean = true
            // pure luminance test
            if (delta > factor*tvi(adapt)) {
                pass = false
            } else if (!args.luminanceOnly) {
                // CIE delta E test with modifications
                var color_scale: Float = args.colorFactor
                // ramp down the color test in scotopic regions
                if (adapt < 10.0f) {
                    // Don't do color test at all.
                    color_scale = 0.0f
                }
                var da: Float = aA(index) - bA(index)
                var db: Float = aB(index) - bB(index)
                da = da*da
                db = db*db
                val delta_e: Float = (da + db)*color_scale
                if (delta_e > factor) {
                    pass = false
                }
            }

            if (!pass) pixels_failed += 1
            args.imgDiff.map(_.set(if (!pass) 255 else 0, 0, 0, 255, index))
          }
        }

        if (pixels_failed < args.thresholdPixels) {
            return Indistinguishable(pixels_failed)
        } else {
            return Different(pixels_failed)
        }
    }
}
