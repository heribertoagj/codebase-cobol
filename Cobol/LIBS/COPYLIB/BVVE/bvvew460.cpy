      *----------------------------------------------------------------*
      *    IMPORTACAO DE CERTIFICADOS                                  *
      *----------------------------------------------------------------*
       01  LK-7460-IMPORTA-CERTF.
           03 LK-7460-RETURN-CODE     PIC  9(0008) COMP.
           03 LK-7460-MESSAGE         PIC  X(0080).
           03 LK-7460-CERTF-64-LEN    PIC  9(0008) COMP.
           03 LK-7460-CERTF-64        PIC  X(3000).
           03 LK-CA-AREA.
              05 LK-7460-TP-CERT         PIC  X(0001).
              05 LK-7460-IN-CA-CODE      PIC  9(0001).
              05 LK-7460-IN-DOMAIN       PIC  9(0002).
              05 LK-7460-IN-ISPB         PIC  9(0008).
              05 LK-7460-CA-CODE         PIC  9(0001).
              05 LK-7460-CA-SER-NUMBER   PIC  X(0040).
              05 LK-7460-CA-COMMON-NAME  PIC  X(0100).
              05 LK-7460-CA-ORG          PIC  X(0100).
              05 LK-7460-CA-COUNTRY      PIC  X(0002).
              05 LK-7460-CA-ORG-UNIT     PIC  X(0100).
              05 LK-7460-SERIAL-NUMBER   PIC  X(0040).
              05 LK-7460-VALDD-INICIO    PIC  X(0014).
              05 LK-7460-VALDD-FINAL     PIC  X(0014).
           03 LK-SB-AREA.
              05 LK-7460-SB-COUNTRY      PIC  X(0002).
              05 LK-7460-SB-ORG          PIC  X(0100).
              05 LK-7460-SB-ORG-UNIT-1   PIC  X(0100).
              05 LK-7460-SB-ORG-UNIT-2   PIC  X(0100).
              05 LK-7460-SB-ORG-UNIT-3   PIC  X(0100).
              05 LK-7460-SB-ORG-UNIT-4   PIC  X(0100).
              05 LK-7460-SB-ORG-UNIT-5   PIC  X(0100).
              05 LK-7460-SB-ORG-UNIT-6   PIC  X(0100).
              05 LK-7460-SB-ORG-UNIT-7   PIC  X(0100).
              05 LK-7460-SB-ORG-UNIT-8   PIC  X(0100).
              05 LK-7460-SB-ORG-UNIT-9   PIC  X(0100).
              05 LK-7460-SB-ORG-UNIT-10  PIC  X(0100).
              05 LK-7460-SB-COMMON-NAME  PIC  X(0100).
              05 LK-7460-SB-LOCALITY     PIC  X(0020).
              05 LK-7460-SB-STATE        PIC  X(0002).
           03 LK-CERTF-AREA.
              05 LK-7460-AMBIENT         PIC  X(0001).
              05 LK-7460-PUBLIC-KEY-LEN  PIC  9(0008) COMP.
              05 LK-7460-MODULUS         PIC  X(1024).
              05 LK-7460-EXPOENT         PIC  X(0005).
              05 LK-7460-LABEL           PIC  X(0064).
              05 LK-7460-HASH-DN-EMISSOR PIC  X(0020).
              05 LK-7460-HASH-DN-CA      PIC  X(0020).
