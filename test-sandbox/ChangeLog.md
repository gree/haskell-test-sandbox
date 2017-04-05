## 0.1.6

* Support for HUnit>=1.5

## 0.1.5

* Add the function of changing environment-variables and working-directory with register-function-arguments.

## 0.1.4

* Allow '.' and '-' for valid procsss name

## 0.1.3.1

* Replace nc with runhaskell

## 0.1.3

* Fix build failure with directory-1.2.2.0 which exposes findExecutables

## 0.1.2

* Add MIN_VERSION_mtl(2,2,1) for Control.Monad.Except

## 0.1.1

* Fix bugs of both isBindable and sendToPort : I was mistaken about PortNum.
* Support for ghc 7.8.4

## 0.1.0

* Change "return value" of both getPort and setPort from PortNumber-type to Port(Int)-type 
* Add Capture*WithFile to data Capture

## 0.0.1.13

* Fix bug that throw error even if exit code is ExitSuccess
