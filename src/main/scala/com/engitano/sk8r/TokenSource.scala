package com.engitano.sk8r

trait TokenSource[F[_]] {
    def getBearerToken: F[String]
}