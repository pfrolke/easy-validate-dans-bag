MANUAL
======
[![Build Status](https://travis-ci.org/DANS-KNAW/easy-validate-dans-bag.png?branch=master)](https://travis-ci.org/DANS-KNAW/easy-validate-dans-bag)

Determines whether a DANS bag is valid according to the DANS BagIt Profile.

SYNOPSIS
--------

    easy-validate-dans-bag [--aip] [--bag-store <uri>] [--response-format,-f json|text] <bag>
    easy-validate-dans-bag run-service


DESCRIPTION
-----------

Determines whether a DANS bag is valid according to the DANS BagIt Profile v0 or v1. If the bag
does not specify what version of the profile it claims to comply with, v0 is assumed. This module has
both a command line and an HTTP interface. The command line interface is documented in the
[ARGUMENTS](#arguments) section below. The HTTP interface is documented in the <a href="../api.html" target="__blank">Swagger UI</a>.

ARGUMENTS
---------

    Options:

          --aip                      Validate as AIP (instead of as SIP)
          --bag-store  <arg>         The bag store to use for deep validation
      -f, --response-format  <arg>   Format for the result report (default = text)
      -h, --help                     Show help message
      -v, --version                  Show version of this program
    
     trailing arguments:
      bag (not required)   The bag to validate
    
    Subcommand: run-service - Starts EASY Validate Dans Bag as a daemon that services HTTP requests
      -h, --help   Show help message
    ---

INSTALLATION AND CONFIGURATION
------------------------------

Currently this project is built as an RPM package for RHEL7/CentOS7 and later. The RPM will install the binaries to
`/opt/dans.knaw.nl/easy-validate-dans-bag` and the configuration files to `/etc/opt/dans.knaw.nl/easy-validate-dans-bag`. 

To install the module on systems that do not support RPM, you can copy and unarchive the tarball to the target host.
You will have to take care of placing the files in the correct locations for your system yourself. For instructions
on building the tarball, see next section.


BUILDING FROM SOURCE
--------------------

Prerequisites:

* Java 8 or higher
* Maven 3.3.3 or higher
* RPM

Steps:
    
    git clone https://github.com/DANS-KNAW/easy-validate-dans-bag.git
    cd easy-validate-dans-bag 
    mvn clean install

If the `rpm` executable is found at `/usr/local/bin/rpm`, the build profile that includes the RPM 
packaging will be activated. If `rpm` is available, but at a different path, then activate it by using
Maven's `-P` switch: `mvn -Pprm install`.

Alternatively, to build the tarball execute:

    mvn clean install assembly:single

[local-file-uri]: https://dans-knaw.github.io/easy-bag-store/03_definitions.html#local-file-uri
