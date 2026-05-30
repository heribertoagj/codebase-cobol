      ******************************************************************
      *  NOME DA INC:     I#BVVEAC                                     *
      *  ROTINA:          BVVE                                         *
      *  DATA DA CRIACAO: 19/11/1997                                   *
      *  LRECL:           160                                          *
      *  OBJETIVO:        ARQUIVO COM CAMPOS ZONADOS      ( SAIDA )    *
      *                   ARQUIVO CADASTRO DE RO'S PARCELADOS          *
      *  PGM QUE UTILIZA: BVVE4012                                     *
      ******************************************************************
      **
       01  CAD-HEADER-S.
      **
           05  HEA-AGENCIA-S              PIC S9(05).
           05  HEA-DIG-AGEN-S             PIC  X(01).
           05  HEA-CONTA-CORR-S           PIC S9(09).
           05  HEA-DIG-CCORR-S            PIC  X(01).
           05  HEA-TIPO-S                 PIC  9(02).
           05  FILLER                     PIC  X(08).
           05  HEA-DATA-MOVTO-S           PIC S9(08).
           05  HEA-DATA-PROC-S            PIC S9(08).
           05  HEA-DATA-INI-PER-S         PIC S9(08).
           05  HEA-FINAL-PER-S            PIC S9(08).
           05  FILLER                     PIC  X(69).
           05  HEA-REMESSA-BANCO-S        PIC S9(05).
           05  HEA-REMESSA-VISANET-S      PIC S9(05).
           05  HEA-SEQ-HEADER-S           PIC S9(09).
           05  FILLER                     PIC  X(14).
      **
       01  CAD-DETALHE-S REDEFINES CAD-HEADER-S.
      **
           05  DET-AGENCIA-S              PIC S9(05).
           05  DET-DIG-AGEN-S             PIC  X(01).
           05  DET-CONTA-CORR-S           PIC S9(09).
           05  DET-DIG-CCORR-S            PIC  X(01).
           05  DET-NRO-ESTAB-S            PIC S9(11).
           05  DET-DIG-ESTAB-S            PIC  X(01).
           05  DET-NOME-ESTAB-S           PIC  X(30).
           05  DET-NRO-RESUMO-S           PIC S9(07).
           05  DET-DIG-RESUMO-S           PIC  X(01).
           05  DET-DATA-SUBM-S            PIC S9(08).
           05  DET-DATA-ULT-LIB-S         PIC S9(08).
           05  DET-COD-TRANSAC-S          PIC  9(01).
           05  DET-VALOR-TRANSAC-S        PIC S9(15)V99.
           05  FILLER                     PIC  X(10).
           05  DET-INDCR-MOEDA-S          PIC  9(01).
           05  DET-INDCR-SITUACAO-S       PIC  9(01).
           05  DET-COD-OCORR-S            PIC S9(03).
           05  DET-SALDO-ABERTO-S         PIC S9(15)V99
           05  DET-QTD-PARC-S             PIC  9(02).
           05  DET-QTD-PARC-PEND-S        PIC  9(02).
           05  FILLER                     PIC  X(05).
           05  DET-CTRL-VISANET-S         PIC  X(11).
           05  DET-SEQ-DETALHE-S          PIC S9(09).
      **
       01  CAD-TRAILER-S REDEFINES CAD-HEADER-S.
      **
           05  TRA-AGENCIA-S              PIC S9(05).
           05  TRA-DIG-AGEN-S             PIC  X(01).
           05  TRA-CONTA-CORR-S           PIC S9(09).
           05  TRA-DIG-CCORR-S            PIC  X(01).
           05  TRA-VLR-TOT-CRED-S         PIC S9(15)V99.
           05  TRA-VLR-TOT-DEB-S          PIC S9(15)V99.
           05  TRA-SOMA-VLRS-DC-S         PIC S9(15)V99.
           05  FILLER                     PIC  X(78).
           05  TRA-SEQ-TRAILER-S          PIC S9(09).
           05  FILLER                     PIC  X(06).
