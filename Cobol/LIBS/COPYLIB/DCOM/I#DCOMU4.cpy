      ******************************************************************
      * SISTEMA : DCOM -  ESTATISTICA NET EMPRESAS           I#DCOMU4  *
      * TAMANHO : 0100 BYTES                                           *
      * ARQUIVO : PARCELA COMPLEMENTADA COM AG/CONTA/PROD/SUBPRODUTO   *
      * INCLUDE DA TABELA DCOMB0C9 - DB2PRD.TPCELA_OPER_DESC           *
      ******************************************************************
 01    01 U4-REGISTRO.
 01       05 U4-CHAVE.
 01          10 U4-CCORR.
 01 02          15 U4-CBCO            PIC S9(3)V USAGE COMP-3.
 03 03          15 U4-CAG-BCRIA       PIC S9(5)V USAGE COMP-3.
 06 07          15 U4-CCTA-BCRIA-CLI  PIC S9(13)V USAGE COMP-3.
 13 02       10 U4-CPRODT             PIC S9(3)V USAGE COMP-3.
 15 02       10 U4-CSPROD-DESC-COML   PIC S9(3)V USAGE COMP-3.
 17 08       10 U4-DPOSIC-CONSL-DESC  PIC  9(08).
 17          10 FILLER REDEFINES      U4-DPOSIC-CONSL-DESC.
 17 04          15 U4-DANODPOSIC      PIC 9(04).
 21 02          15 U4-DMESDPOSIC      PIC 9(02).
 23 02          15 U4-DDIADPOSIC      PIC 9(02).
 25 09    05 U4-VPRINC-PCELA-DESC     PIC S9(15)V9(2) USAGE COMP-3.
 34 08    05 U4-DVCTO-PCELA-DESC      PIC  9(08).
 34       05 FILLER REDEFINES         U4-DVCTO-PCELA-DESC.
 34 04       10 U4-DANODVCTO          PIC 9(04).
 38 02       10 U4-DMESDVCTO          PIC 9(02).
 40 02       10 U4-DDIADVCTO          PIC 9(02).
 42 02    05 U4-CSIT-DESC-COML        PIC S9(3)V USAGE COMP-3.
 44 57    05 U4-FILLER                PIC X(57).
