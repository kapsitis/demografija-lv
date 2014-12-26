package lv.ddgatve.scp

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader

import com.jcraft.jsch.UserInfo

class CliUserInfo extends UserInfo {

  // this method is never called
  override def getPassphrase(): String = {
    return null;
  }

  // this method should return the password, which it reads from the console
  override def getPassword(): String = {
    var result = "qwerty";
    val br = new BufferedReader(new InputStreamReader(
      System.in));
    print("Enter Password:");
    try {
      result = br.readLine();
    } catch {
      case nfe: IOException =>
        System.err.println("Invalid Format!");
    }
    return result;
  }

  // this method is never called
  override def promptPassphrase(arg0: String): Boolean = {
    return false;
  }

  // this method means that we'll authenticate - return true;
  override def promptPassword(arg0: String): Boolean = {
    return true;
  }

  // this method means that we'll accept the certificate - return true;
  override def promptYesNo(arg0: String): Boolean = {
    return true;
  }

  // this method is never called
  override def showMessage(arg0: String): Unit = {
    println("Displaying message: " + arg0);
  }
}