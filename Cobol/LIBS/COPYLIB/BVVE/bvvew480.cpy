      *----------------------------------------------------------------
      * AREA DE TRANSFERENCIA
      * ACESSO AO ARQUIVO VSAM TRANSFERENCIA CERTIFICADO
      *----------------------------------------------------------------
       01 LK-7480-TRANSFERENCIA.
          05 LK-7480-CD-RETORNO            PIC S9(004) COMP.
          05 LK-7480-DS-RETORNO            PIC  X(100).
          05 LK-7480-CD-PROGRAMA           PIC  X(008).
          05 LK-7480-TP-ACESSO             PIC  X(003).
          05 LK-7480-NR-ACESSO             PIC  9(002).
          05 LK-7480-AREA-DB2.
             10 LK-7480-CHAVE.
                15 LK-7480-CAUTDD-CERTD    PIC S9(0001) COMP-3.
                15 LK-7480-CID-CERTF-DGTAL PIC  X(0040).
             10 LK-7480-HATIVO-CERTF-DGTAL PIC  X(0026).
             10 LK-7480-HINIC-VALDD-CERTF  PIC  X(0026).
             10 LK-7480-HFNAL-VALDD-CERTF  PIC  X(0026).
             10 LK-7480-CPAIS-CERTF-DGTAL  PIC  X(0002).
             10 LK-7480-RORGNZ-CERTF-DGTAL PIC  X(0020).
             10 LK-7480-IINSTC-CERTF-DGTAL PIC  X(0100).
             10 LK-7480-RLOC-CERTF-DGTAL   PIC  X(0015).
             10 LK-7480-RUF-CERTF-DGTAL    PIC  X(0015).
             10 LK-7480-NTMNHO-CHAVE-CERTF PIC S9(0004) COMP-3.
             10 LK-7480-ICHAVE-CRIPT-CERTF PIC  X(0064).
             10 LK-7480-NEXPNT-CHAVE-CERTF PIC S9(0005) COMP-3.
             10 LK-7480-IMDULO-CERTF-LEN   PIC S9(0004) COMP.
             10 LK-7480-IMDULO-CERTF-TEXT  PIC  X(1024).
             10 LK-7480-WHASH-CERTF-DGTAL  PIC  X(0020).
             10 LK-7480-WHASH-CERTF-ORIGE  PIC  X(0020).
             10 LK-7480-CAUTDD-CERTD-ORIGE PIC S9(0001) COMP-3.
             10 LK-7480-CID-CERTF-ORIGE    PIC  X(0040).
             10 LK-7480-CISPB-PARTC-MSGEM  PIC S9(0009) COMP-3.
             10 LK-7480-CSIT-CERTF         PIC  X(0001).
             10 LK-7480-CMOTVO-CERTF       PIC S9(0001) COMP-3.
             10 LK-7480-CUSUAR-INCL        PIC  X(0009).
             10 LK-7480-HINCL-REG          PIC  X(0026).
             10 LK-7480-HMANUT-REG         PIC  X(0026).
             10 LK-7480-CUSUAR-MANUT       PIC  X(0009).
             10 LK-7480-CTPO-CERTF-DGTAL   PIC S9(0001) COMP-3.
          05 LK-7480-AREA-SB-ORG.
             10 LK-7480-SB-ORG-UNIT-1      PIC  X(0100).
             10 LK-7480-SB-ORG-UNIT-2      PIC  X(0100).
             10 LK-7480-SB-ORG-UNIT-3      PIC  X(0100).
             10 LK-7480-SB-ORG-UNIT-4      PIC  X(0100).
             10 LK-7480-SB-ORG-UNIT-5      PIC  X(0100).
             10 LK-7480-SB-ORG-UNIT-6      PIC  X(0100).
             10 LK-7480-SB-ORG-UNIT-7      PIC  X(0100).
             10 LK-7480-SB-ORG-UNIT-8      PIC  X(0100).
             10 LK-7480-SB-ORG-UNIT-9      PIC  X(0100).
             10 LK-7480-SB-ORG-UNIT-10     PIC  X(0100).
          05 FILLER REDEFINES LK-7480-AREA-SB-ORG.
             10 LK-7480-SB-ORG-UNIT        OCCURS 10 TIMES
                                           PIC  X(0100).
