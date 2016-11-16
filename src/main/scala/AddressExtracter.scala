import java.io.ByteArrayInputStream
import java.util.Base64
import java.util.zip.GZIPInputStream

/**
  * Simple class to extract an address from a challenge string
  */
object AddressExtracter {
  def extractAddress(address: String): String = {
    val reversed = address.reverse
    val base64Decoded = Base64.getDecoder.decode(reversed)
    val gzipInput = new GZIPInputStream(new ByteArrayInputStream(base64Decoded))
    val readBuffer = new Array[Byte](8)

    def gunzip(outputBuilder: StringBuilder): StringBuilder = {
      if (gzipInput.available() == 0)
        outputBuilder
      else {
        val bytesRead = gzipInput.read(readBuffer, 0, readBuffer.length)
        if (bytesRead <= 0) outputBuilder
        else {
          outputBuilder.append(new String(readBuffer, 0, bytesRead))
          gunzip(outputBuilder)
        }
      }
    }

    gunzip(new StringBuilder(reversed.length)).toString()
  }

  def extractMessage(message: String): String = {
    val grouped = Base64.getDecoder.decode(message).map(x => if (x == 48) 0 else 1).grouped(8)
    grouped.map(arr => arr.reduceLeft((z, a) => (z << 1) + a).toChar)
      .foldLeft(new StringBuilder)((b, a) => b.append(a))
      .toString
  }
}