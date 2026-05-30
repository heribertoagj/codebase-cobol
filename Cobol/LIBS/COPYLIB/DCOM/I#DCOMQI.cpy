      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQI                                      *
      * TAMANHO : 0338 BYTES                                           *
      * ARQUIVO : TITULO ANALISE DE CREDITO                            *
      * INCLUDE DA TABELA DCOMB0I3- - DB2PRD.TTITLO_ANLSE_CREDT        *
      ******************************************************************
       01  QI-I3-REGISTRO.
  1  3     05 QI-I3-DANO-OPER-DESC          PIC S9(4)V USAGE COMP-3.
  4  5     05 QI-I3-NSEQ-OPER-DESC          PIC S9(9)V USAGE COMP-3.
  9  3     05 QI-I3-NTITLO-DESC-COML        PIC S9(5)V USAGE COMP-3.
 12  7     05 QI-I3-CPROT-REMSS-TITLO       PIC S9(12)V USAGE COMP-3.
 19 10     05 QI-I3-DRECEB-REMSS-TITLO      PIC X(10).
 29  6     05 QI-I3-NSEQ-PROT-TITLO         PIC S9(10)V USAGE COMP-3.
 35 26     05 QI-I3-HANLSE-TITLO            PIC X(26).
 61  2     05 QI-I3-CTPO-ANLSE-TITLO        PIC S9(3)V USAGE COMP-3.
 63  2     05 QI-I3-CTPO-IMPED-TITLO        PIC S9(3)V USAGE COMP-3.
 65 10     05 QI-I3-CNEGOC-COBR             PIC S9(18)V USAGE COMP-3.
 75 10     05 QI-I3-CTITLO-COBR-BCO         PIC S9(18)V USAGE COMP-3.
 85  2     05 QI-I3-CRESUL-ANLSE-TITLO      PIC S9(2)V USAGE COMP-3.
 87  1     05 QI-I3-CRESUL-ANLSE-TITLO-NULL PIC X(01).
 88250     05 QI-I3-RREST-ANLSE-TITLO       PIC X(250).
338  1     05 QI-I3-RREST-ANLSE-TITLO-NULL  PIC X(01).
