      ******************************************************************
      * NOME BOOK : CTECWDES                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC1A1C                                           *
      * TAMANHO   : 11115                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWDES-HEADER.
              10 CTECWDES-COD-LAYOUT         PIC X(08) VALUE 'CTECWDES'.
              10 CTECWDES-TAM-LAYOUT         PIC 9(05) VALUE 00351.
           05 CTECWDES-REGISTRO.
              10 CTECWDES-BLOCO-SAIDA.
                 15 CTECWDES-S-CPARM-ENCRR-BLOQ        PIC 9(03).
                 15 CTECWDES-S-CMOTVO-SIT-CTA          PIC 9(03).
                 15 CTECWDES-S-RMOTVO-SIT-CTA          PIC X(60).
                 15 CTECWDES-S-CINDCD-TPO-JUCIC        PIC X(01).
                 15 CTECWDES-S-RMOTVO-SIT-CTA          PIC X(60).
                 15 CTECWDES-S-CSIT-PARM-ENCRR         PIC X(01).
                 15 CTECWDES-S-ISIT-PARM-ENCRR         PIC X(20).
                 15 CTECWDES-S-CUSUAR-INCL             PIC X(09).
                 15 CTECWDES-S-IUSUAR-INCL             PIC X(60).
                 15 CTECWDES-S-CUSUAR-MANUT            PIC X(09).
                 15 CTECWDES-S-IUSUAR-MANUT            PIC X(60).
                 15 CTECWDES-S-HINCL-REG               PIC X(26).
                 15 CTECWDES-S-HMANUT-REG              PIC X(26).
