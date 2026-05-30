      *----------------------------------------------------------------
      * AREA DE TRANSFERENCIA
      * ACESSO A BASE DB2 THIST_CERTF_CRIPT
      *----------------------------------------------------------------
       01 LK-7482-TRANSFERENCIA.
          05 LK-7482-CD-RETORNO             PIC S9(0004) COMP.
          05 LK-7482-DS-RETORNO             PIC  X(0100).
          05 LK-7482-CD-PROGRAMA            PIC  X(0008).
          05 LK-7482-TP-ACESSO              PIC  X(0003).
          05 LK-7482-NR-ACESSO              PIC  9(0002).
          05 LK-7482-TIMESTAMP-INICIO       PIC  X(0026).
          05 LK-7482-TIMESTAMP-FINAL        PIC  X(0026).
          05 LK-7482-ORG-UNIT               OCCURS 3 TIMES.
             10 LK-7482-OU                  PIC  X(0100).
          05 LK-7482-AREA-DB2.
             10 LK-7482-CHAVE-DB2.
                15 LK-7482-CAUTDD-CERTD     PIC S9(0001) COMP-3.
                15 LK-7482-CID-CERTF-DGTAL  PIC  X(0040).
                15 LK-7482-HINCL-REG-HIST   PIC  X(0026).
             10 LK-7482-HATIVO-CERTF-DGTAL  PIC  X(0026).
             10 LK-7482-HINIC-VALDD-CERTF   PIC  X(0026).
             10 LK-7482-HFNAL-VALDD-CERTF   PIC  X(0026).
             10 LK-7482-CPAIS-CERTF-DGTAL   PIC  X(0002).
             10 LK-7482-RORGNZ-CERTF-DGTAL  PIC  X(0020).
             10 LK-7482-IINSTC-CERTF-DGTAL  PIC  X(0100).
             10 LK-7482-RLOC-CERTF-DGTAL    PIC  X(0015).
             10 LK-7482-RUF-CERTF-DGTAL     PIC  X(0015).
             10 LK-7482-NTMNHO-CHAVE-CERTF  PIC S9(0004) COMP-3.
             10 LK-7482-ICHAVE-CRIPT-CERTF  PIC  X(0064).
             10 LK-7482-NEXPNT-CHAVE-CERTF  PIC S9(0005) COMP-3.
             10 LK-7482-IMDULO-CERTF-DGTAL.
                15 LK-7482-IMDULO-CERTF-DGTAL-LEN PIC S9(0004) COMP.
                15 LK-7482-IMDULO-CERTF-DGTAL-TXT PIC  X(1024).
             10 LK-7482-WHASH-CERTF-DGTAL   PIC  X(0020).
             10 LK-7482-WHASH-CERTF-ORIGE   PIC  X(0020).
             10 LK-7482-CAUTDD-CERTD-ORIGE  PIC S9(0001) COMP-3.
             10 LK-7482-CID-CERTF-ORIGE     PIC  X(0040).
             10 LK-7482-CISPB-PARTC-MSGEM   PIC S9(0009) COMP-3.
             10 LK-7482-CSIT-CERTF          PIC  X(0001).
             10 LK-7482-CMOTVO-CERTF        PIC S9(0001) COMP-3.
             10 LK-7482-CUSUAR-INCL         PIC  X(0009).
             10 LK-7482-HINCL-REG           PIC  X(0026).
             10 LK-7482-CUSUAR-MANUT        PIC  X(0009).
             10 LK-7482-HMANUT-REG          PIC  X(0026).
