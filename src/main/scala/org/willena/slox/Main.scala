package org.willena.slox

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object Main:

  @main def run(path: String) =
    val bytes = Files.readAllBytes(Paths.get(path))
    exec(String(bytes, Charset.defaultCharset()))

  private def exec(source: String) =
    val tokens = Scanner(source).scanTokens
