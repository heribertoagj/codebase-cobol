      *---------------------------------------------------------------*
      *         EMPF - SISTEMA DE EMPRESTIMOS E FINANCIAMENTOS        *
      *                                                               *
      *                A N A L I T I C O                              *
      *                =================                              *
      *                                                               *
      *   INCLUDE          : I#DCOMAP                                 *
      *   TAMANHO          : 130                                      *
      *   RESPONSAVEL      : FRUTEIRO/LEANDRO                         *
      *   DATA DE CRIACAO  : 04.05.2005                               *
      *---------------------------------------------------------------*

       01  DCOMAP-REGTO.
           03  DCOMAP-DATA-BASE            PIC  X(10).
           03  DCOMAP-CPRODT-BDSCO         PIC  9(03)       COMP-3.
           03  DCOMAP-CFAML-CONTR          PIC  9(01).
           03  DCOMAP-CCONTR               PIC  9(09)       COMP-3.
           03  DCOMAP-SEQ-ATULZ-CONTR      PIC  9(03)       COMP-3.
           03  DCOMAP-CTPO-SIT-CONTR       PIC  9(03)       COMP-3.
           03  DCOMAP-DCELEB-CONTR         PIC  X(10).
           03  DCOMAP-CJUNC-DEPDC          PIC  9(05)       COMP-3.
           03  DCOMAP-CPSSOA-DCFI          PIC  9(07)       COMP-3.
           03  DCOMAP-CJUNC-DEPDC-NEGOC    PIC  9(05)       COMP-3.
           03  DCOMAP-CSPROD               PIC  9(03)       COMP-3.
           03  DCOMAP-CMODLD-ATULZ         PIC  9(01).
           03  DCOMAP-CIDTFD-ATULZ         PIC  9(05)       COMP-3.
           03  DCOMAP-CTPO-GARNT           PIC  9(03)       COMP-3.
           03  DCOMAP-CPOSTO-SERVC         PIC  9(03)       COMP-3.
           03  DCOMAP-SEGMENTO             PIC  9(03)       COMP-3.
           03  DCOMAP-CCART                PIC  X(03).
           03  DCOMAP-QCONTR-DIA           PIC  9(15)       COMP-3.
           03  DCOMAP-VALOR-DIA            PIC S9(15)V9(2)  COMP-3.
           03  DCOMAP-QCONTR-ESTOQUE       PIC  9(15)       COMP-3.
           03  DCOMAP-VALOR-ESTOQUE        PIC S9(15)V9(2)  COMP-3.
           03  DCOMAP-QTIT-DIA             PIC  9(15)       COMP-3.
           03  DCOMAP-QTIT-ESTOQUE         PIC  9(15)       COMP-3.
           03  DCOMAP-VALOR-VENCD-ESTOQUE  PIC S9(15)V9(2)  COMP-3.
           03  FILLER                      PIC  X(14).

      *---------------------------------------------------------------*
      *     DCOMAP-REGTO                         1   130  A           *
      *     DCOMAP-DMOVTO-BASE                   1    10  A           *
      *     DCOMAP-CPRODT-BDSCO                 11     2  P           *
      *     DCOMAP-CFAML-CONTR                  13     1  N           *
      *     DCOMAP-CCONTR                       14     5  P           *
      *     DCOMAP-SEQ-ATULZ-CONTR              19     2  P           *
      *     DCOMAP-CTPO-SIT-CONTR               21     2  P           *
      *     DCOMAP-DCELEB-CONTR                 23    10  A           *
      *     DCOMAP-CJUNC-DEPDC                  33     3  P           *
      *     DCOMAP-CPSSOA-DCFI                  36     4  P           *
      *     DCOMAP-CJUNC-DEPDC-NEGOC            40     3  P           *
      *     DCOMAP-CSPROD                       43     2  P           *
      *     DCOMAP-CMODLD-ATULZ                 45     1  N           *
      *     DCOMAP-CIDTFD-ATULZ                 46     3  P           *
      *     DCOMAP-CTPO-GARNT                   49     2  P           *
      *     DCOMAP-CPOSTO-SERVC                 51     2  P           *
      *     DCOMAP-SEGMENTO                     53     2  P           *
      *     DCOMAP-CCART                        55     3  A           *
      *     DCOMAP-QCONTR-DIA                   58     8  P           *
      *     DCOMAP-VALOR                        66     9  P           *
      *     DCOMAP-QCONTR-ESTOQUE               75     8  P           *
      *     DCOMAP-VALOR-ESTOQUE                83     9  P           *
      *     DCOMAP-QTIT-DIA                     92     8  P           *
      *     DCOMAP-QTIT-ESTOQUE                100     8  P           *
      *     DCOMAP-VALOR-VENCD-ESTOQUE         108     9  P           *
      *     FILLER-001                         117    14  A           *
      *---------------------------------------------------------------*
