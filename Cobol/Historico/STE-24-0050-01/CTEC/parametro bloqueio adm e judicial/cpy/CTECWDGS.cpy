      ******************************************************************
      * NOME BOOK : CTECWDGS                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC1AGL                                           *
      * TAMANHO   : 18465                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWDGS-HEADER.
              10 CTECWDGS-COD-LAYOUT         PIC X(08) VALUE 'CTECWDGS'.
              10 CTECWDGS-TAM-LAYOUT         PIC 9(05) VALUE 18465.
           05 CTECWDGS-REGISTRO.
              10 CTECWDGS-BLOCO-SAIDA.
                 15 CTECWDGS-S-NUM-REG       PIC 9(02).
                 15 CTECWDGS-S-LISTA         OCCURS 0 
                                             TO 50 TIMES
                                             DEPENDING 
                                             ON CTECWDGS-S-NUM-REG.
                    20 CTECWDGS-S-CPARM-ENCRR-BLOQ        PIC 9(03). 
                    20 CTECWDGS-S-CMOTVO-SIT-CTA          PIC 9(03).
                    20 CTECWDGS-S-RMOTVO-SIT-CTA          PIC X(60).
                    15 CTECWDGS-S-CINDCD-TPO-JUCIC        PIC X(01).
                    15 CTECWDGS-S-RMOTVO-SIT-CTA          PIC X(60).
                    15 CTECWDGS-S-CTPO-BLOQ-OPER          PIC 9(01).
                    15 CTECWDGS-S-RTPO-BLOQ-OPER          PIC X(30).
                    15 CTECWDGS-S-CSIT-PARM-BLOQ          PIC X(01).
                    15 CTECWDGS-S-ISIT-PARM-BLOQ          PIC X(20).
                    20 CTECWDGS-S-CUSUAR-INCL             PIC X(09).
                    20 CTECWDGS-S-RUSUAR-INCL             PIC X(60).
                    20 CTECWDGS-S-CUSUAR-MANUT            PIC X(09).
                    20 CTECWDGS-S-RUSUAR-MANUT            PIC X(60).
                    20 CTECWDGS-S-HINCL-REG               PIC X(26).
                    20 CTECWDGS-S-HMANUT-REG              PIC X(26).
