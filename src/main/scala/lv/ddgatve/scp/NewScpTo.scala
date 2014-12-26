package lv.ddgatve.scp

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.UserInfo;

object NewScpTo {

  def checkAck(in: InputStream): Int = {
    val b = in.read();
    // b may be 0 for success,
    // 1 for error,
    // 2 for fatal error,
    // -1
    if (b == 0)
      return b;
    if (b == -1)
      return b;

    if (b == 1 || b == 2) {
      val sb = new StringBuffer();
      var c: Int = 0;
      do {
        c = in.read();
        sb.append(c.toChar);
      } while (c != '\n');
      if (b == 1) { // error
        System.out.print(sb.toString());
      }
      if (b == 2) { // fatal error
        System.out.print(sb.toString());
      }
    }
    return b;
  }

  def copyToRemote(lfile:List[String], rfile:List[String]): Unit = {

    var fis: FileInputStream = null;
    try {

//      val lfile = List("src/test/resources/README00.txt",
//        "src/test/resources/README10.txt")
      val user = "kalvis"
      val host = "85.254.250.28"
//      val rfile = List("/home/lighttpd/demografija.lv/http/README01.txt",
//        "/home/lighttpd/demografija.lv/http/README11.txt")
      val jsch = new JSch()
      val session = jsch.getSession(user, host, 22)

      // username and password will be given via UserInfo interface.
      val ui = new CliUserInfo()
      session.setUserInfo(ui)
      session.connect()
      val ptimestamp = true

      for (ii <- 0 until lfile.length) {
        // exec 'scp -t rfile' remotely
        var command = if (ptimestamp) { "scp -p -t " + rfile(ii) }
        else { "scp -t " + rfile(ii) }
        val channel = session.openChannel("exec")
        (channel.asInstanceOf[ChannelExec]).setCommand(command)

        // get I/O streams for remote scp
        val out = channel.getOutputStream()
        val in = channel.getInputStream()

        channel.connect()

        if (checkAck(in) != 0) {
          System.exit(0)
        }

        val _lfile = new File(lfile(ii));

        if (ptimestamp) {
          command = "T " + (_lfile.lastModified() / 1000) + " 0"
          // The access time should be sent here,
          // but it is not accessible with JavaAPI ;-<
          command += (" " + (_lfile.lastModified() / 1000) + " 0\n")
          out.write(command.getBytes())
          out.flush();
          if (checkAck(in) != 0) {
            System.exit(0)
          }
        }

        // send "C0644 filesize filename", where filename should not
        // include
        // '/'
        val filesize = _lfile.length();
        command = "C0644 " + filesize + " "
        if (lfile(ii).lastIndexOf('/') > 0) {
          command += lfile(ii).substring(lfile(ii).lastIndexOf('/') + 1)
        } else {
          command += lfile;
        }
        command += "\n";
        out.write(command.getBytes());
        out.flush();
        if (checkAck(in) != 0) {
          System.exit(0);
        }

        // send a content of lfile
        fis = new FileInputStream(lfile(ii));
        val buf = Array.fill[Byte](1024)(0)

        //val itera = Iterator.continually({fis.read(buf, 0, buf.length);buf}).takeWhile(_ != null)

        var len = 1;
        while (len > 0) {
          len = fis.read(buf, 0, buf.length);
          if (len > 0) {
            out.write(buf, 0, len); // out.flush();
          }
        }
        fis.close();
        fis = null;
        // send '\0'
        buf(0) = 0;
        out.write(buf, 0, 1);
        out.flush();
        if (checkAck(in) != 0) {
          System.exit(0);
        }
        out.close();
        channel.disconnect();
      }
      session.disconnect();
      System.exit(0);
    } catch {
      case e: Exception =>
        println(e);
        try {
          if (fis != null)
            fis.close();
        } catch {
          case ee: Exception => Unit
        }
    }
  }
}