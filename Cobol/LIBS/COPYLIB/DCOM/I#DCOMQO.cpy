      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQO                                      *
      * TAMANHO : 0130 BYTES                                           *
      * ARQUIVO : OPERACAO DESCONTO COMERCIAL (INTERFACE DEF VIA ISD)  *
      ******************************************************************
       01  QO-REGISTRO.
 01 03     03 QO-DANO-OPER-DESC        PIC S9(04)V     USAGE COMP-3.
 04 05     03 QO-NSEQ-OPER-DESC        PIC S9(09)V     USAGE COMP-3.
 09 02     03 QO-CBCO                  PIC S9(03)V     USAGE COMP-3.
 11 03     03 QO-CAG-BCRIA             PIC S9(05)V     USAGE COMP-3.
 14 07     03 QO-CCTA-BCRIA-CLI        PIC S9(13)V     USAGE COMP-3.
 21 03     03 QO-COD-POSTO-PAB         PIC S9(05)V     USAGE COMP-3.
 24 02     03 QO-TIPO-POSTO-PAB        PIC  X(02).
 26 02     03 QO-COD-SEGMTO-CLIE       PIC S9(03)V     USAGE COMP-3.
 28 05     03 QO-CCNPJ-CPF             PIC S9(09)V     USAGE COMP-3.
 33 03     03 QO-CFLIAL-CNPJ           PIC S9(05)V     USAGE COMP-3.
 36 02     03 QO-CCTRL-CNPJ-CPF        PIC S9(02)V     USAGE COMP-3.
 38 02     03 QO-CSIT-DESC-COML        PIC S9(03)V     USAGE COMP-3.
 40 02     03 QO-CPRODT                PIC S9(03)V     USAGE COMP-3.
 42 02     03 QO-CSPROD-DESC-COML      PIC S9(03)V     USAGE COMP-3.
 44 05     03 QO-CCONTR-CONVE-DESC     PIC S9(09)V     USAGE COMP-3.
 49 02     03 QO-CVRSAO-CONTR-CONVE    PIC S9(03)V     USAGE COMP-3.
 51 02     03 QO-CELMTO-DESC-COML      PIC S9(03)V     USAGE COMP-3.
 53 03     03 QO-TOPER-DESC-COML       PIC S9(05)V     USAGE COMP-3.
 56 02     03 QO-TMED-PONDE-OPER       PIC S9(03)V     USAGE COMP-3.
 58 02     03 QO-CTPO-GARNT            PIC S9(03)V     USAGE COMP-3.
 60 02     03 QO-CTPO-GARNT-COMPLE     PIC S9(03)V     USAGE COMP-3.
 62 10     03 QO-DINIC-OPER-DESC       PIC  X(10).
 72 10     03 QO-DVCTO-FNAL-OPER       PIC  X(10).
 82 09     03 QO-VOPER-DESC-COML       PIC S9(15)V9(2) USAGE COMP-3.
 91 05     03 QO-PTX-JURO-MES          PIC S9(03)V9(5) USAGE COMP-3.
 96 05     03 QO-PTX-JURO-ANO          PIC S9(03)V9(5) USAGE COMP-3.
101 09     03 QO-VTARIF-REG-TITLO      PIC S9(15)V9(2) USAGE COMP-3.
110 09     03 QO-VTAC-OPER-DESC        PIC S9(15)V9(2) USAGE COMP-3.
119 09     03 QO-VIOF-OPER-DESC        PIC S9(15)V9(2) USAGE COMP-3.
128 03     03 QO-COMPLEMENTO           PIC  X(03).
   *********************************************************************
