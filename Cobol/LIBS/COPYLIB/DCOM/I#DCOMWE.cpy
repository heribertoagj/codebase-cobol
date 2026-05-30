      *---------------------------------------------------------------*
      *           INTERFACE - EMPI - PAGOS NO VENCIMENTO              *
      *---------------------------------------------------------------*
      *   DDNAME           : SARQPAGO                                 *
      *   INCLUDE          : I#DCOMWE                                 *
      *   TAMANHO          : 118                                      *
      *---------------------------------------------------------------*

       01  DCOMWE-REGTO.
           03  DCOMWE-DATA-BASE            PIC  X(10).
           03  DCOMWE-CJUNC-DEPDC          PIC  9(05)       COMP-3.
           03  DCOMWE-CPSSOA-DCFI          PIC  9(07)       COMP-3.
           03  DCOMWE-CCART                PIC  X(05).
           03  DCOMWE-CPRODT-BDSCO         PIC  9(03)       COMP-3.
           03  DCOMWE-CFAML-CONTR          PIC  9(01).
           03  DCOMWE-CCONTR               PIC  9(09)       COMP-3.
           03  DCOMWE-SEQ-ATULZ-CONTR      PIC  9(03)       COMP-3.
           03  DCOMWE-CTPO-SIT-CONTR       PIC  9(03)       COMP-3.
           03  DCOMWE-DCELEB-CONTR         PIC  X(10).
           03  DCOMWE-CJUNC-DEPDC-NEGOC    PIC  9(05)       COMP-3.
           03  DCOMWE-CSPROD               PIC  9(03)       COMP-3.
           03  DCOMWE-CMODLD-ATULZ         PIC  9(01).
           03  DCOMWE-CIDTFD-ATULZ         PIC  9(05)       COMP-3.
           03  DCOMWE-CTPO-GARNT           PIC  9(03)       COMP-3.
           03  DCOMWE-CPOSTO-SERVC         PIC  9(03)       COMP-3.
           03  DCOMWE-SEGMENTO             PIC  9(03)       COMP-3.
           03  DCOMWE-QCONTR-DIA           PIC  9(15)       COMP-3.
           03  DCOMWE-VALOR-DIA            PIC S9(15)V9(2)  COMP-3.
           03  DCOMWE-QCONTR-ESTOQUE       PIC  9(15)       COMP-3.
           03  DCOMWE-VALOR-ESTOQUE        PIC S9(15)V9(2)  COMP-3.
           03  DCOMWE-QTIT-DIA             PIC  9(15)       COMP-3.
           03  DCOMWE-QTIT-ESTOQUE         PIC  9(15)       COMP-3.
           03  DCOMWE-VALOR-VENCD-ESTOQUE  PIC S9(15)V9(2)  COMP-3.
