      ******************************************************************
      *  NOME DA INC:     I#BVVEAC                                     *
      *  ROTINA:          BVVE                                         *
      *  DATA DA CRIACAO: 19/11/1997                                   *
      *  LRECL:           120                                          *
      *  OBJETIVO:        ARQUIVO COM CAMPOS COMPACTADOS  ( ENTRADA )  *
      *                   ARQUIVO CADASTRO DE RO'S PARCELADOS          *
      *  PGM QUE UTILIZA: BVVE4012                                     *
      ******************************************************************
      **
       01  CAD-HEADER.
      **
           05  HEA-AGENCIA                PIC S9(05)    COMP-3.
           05  HEA-DIG-AGEN               PIC  X(01).
           05  HEA-CONTA-CORR             PIC S9(09)    COMP-3.
           05  HEA-DIG-CCORR              PIC  X(01).
           05  HEA-TIPO                   PIC  9(02).
           05  FILLER                     PIC  X(08).
           05  HEA-DATA-MOVTO             PIC S9(09)    COMP-3.
           05  HEA-DATA-PROC              PIC S9(09)    COMP-3.
           05  HEA-DATA-INI-PER           PIC S9(09)    COMP-3.
           05  HEA-DATA-FIN-PER           PIC S9(09)    COMP-3.
           05  FILLER                     PIC  X(69).
           05  HEA-REMESSA-BANCO          PIC S9(05)    COMP-3.
           05  HEA-REMESSA-VISANET        PIC S9(05)    COMP-3.
           05  HEA-SEQ-HEADER             PIC S9(09)    COMP-3.
      **
       01  CAD-DETALHE REDEFINES CAD-HEADER.
      **
           05  DET-AGENCIA                PIC S9(05)    COMP-3.
           05  DET-DIG-AGEN               PIC  X(01).
           05  DET-CONTA-CORR             PIC S9(09)    COMP-3.
           05  DET-DIG-CCORR              PIC  X(01).
           05  DET-NRO-ESTAB              PIC S9(11)    COMP-3.
           05  DET-DIG-ESTAB              PIC  X(01).
           05  DET-NOME-ESTAB             PIC  X(30).
           05  DET-NRO-RESUMO             PIC S9(07)    COMP-3.
           05  DET-DIG-RESUMO             PIC  X(01).
           05  DET-DATA-SUBM              PIC S9(09)    COMP-3.
           05  DET-DATA-ULT-LIB           PIC S9(09)    COMP-3.
           05  DET-COD-TRANSAC            PIC  9(01).
           05  DET-VALOR-TRANSAC          PIC S9(15)V99 COMP-3.
           05  FILLER                     PIC  X(10).
           05  DET-INDCR-MOEDA            PIC  9(01).
           05  DET-INDCR-SITUACAO         PIC  9(01).
           05  DET-COD-OCORR              PIC S9(03)    COMP-3.
           05  DET-SALDO-ABERTO           PIC S9(15)V99 COMP-3.
           05  DET-QTD-PARC               PIC  9(02).
           05  DET-QTD-PARC-PEND          PIC  9(02).
           05  FILLER                     PIC  X(05).
           05  DET-CTRL-VISANET           PIC  X(11).
           05  DET-SEQ-DETALHE            PIC S9(09)    COMP-3.
      **
       01  CAD-TRAILER REDEFINES CAD-HEADER.
      **
           05  TRA-AGENCIA                PIC S9(05)    COMP-3.
           05  TRA-DIG-AGEN               PIC  X(01).
           05  TRA-CONTA-CORR             PIC S9(09)    COMP-3.
           05  TRA-DIG-CCORR              PIC  X(01).
           05  TRA-VLR-TOT-CRED           PIC S9(15)V99 COMP-3.
           05  TRA-VLR-TOT-DEB            PIC S9(15)V99 COMP-3.
           05  TRA-SOMA-VLRS-DC           PIC S9(15)V99 COMP-3.
           05  FILLER                     PIC  X(78).
           05  TRA-SEQ-TRAILER            PIC S9(09)    COMP-3.

