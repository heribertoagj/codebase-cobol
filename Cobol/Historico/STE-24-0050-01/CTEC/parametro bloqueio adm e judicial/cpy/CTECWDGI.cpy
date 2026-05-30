      ******************************************************************
      * NOME BOOK : CTECWDGI - BOOK DE ENCTRADA                        *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC3AGL                                           *
      * TAMANHO   : 16953 BYTES                                        *
      ******************************************************************
           05 CTECWDGI-HEADER.
              10 CTECWDGI-COD-LAYOUT        PIC X(008) VALUE 'CTECWDGI'.
              10 CTECWDGI-TAM-LAYOUT        PIC 9(005) VALUE 18503.
           05 CTECWDGI-REGISTRO.
              10 CTECWDGI-BLOCO-PAGINACAO.
                 15 CTECWDGI-IND-PAGINACAO   PIC  X(001).
                    88 CTECWDGI-P-INICIAL    VALUE 'I'.
                    88 CTECWDGI-P-PRIMEIRA   VALUE 'P'.
                    88 CTECWDGI-P-SEGUINTE   VALUE 'S'.
                    88 CTECWDGI-P-ANTERIOR   VALUE 'A'.
                    88 CTECWDGI-P-ULTIMA     VALUE 'U'.
                 15 CTECWDGI-PRI-CHAVE.
                    20 CTECWDGI-PRI-CPARM-BLOQ   PIC 9(03).
                 15 CTECWDGI-ULT-CHAVE.
                    20 CTECWDGI-ULT-CPARM-BLOQ   PIC 9(03).
              10 CTECWDGI-BLOCO-ENTRADA.
                 15 CTECWDGI-E-CPARM-ENCRR-BLOQ  PIC 9(03).
                 15 CTECWDGI-E-QTD-REG           PIC 9(02).
              10 CTECWDGI-BLOCO-SAIDA.
                 15 CTECWDGI-S-NUM-REG       PIC 9(02).
                 15 CTECWDGI-S-LISTA         OCCURS 0 
                                             TO 50 TIMES
                                             DEPENDING 
                                             ON CTECWDGI-S-NUM-REG.
                    20 CTECWDGI-S-CPARM-ENCRR-BLOQ        PIC 9(03). 
                    20 CTECWDGI-S-CMOTVO-SIT-CTA          PIC 9(03).
                    20 CTECWDGI-S-RMOTVO-SIT-CTA          PIC X(255).
                    15 CTECWDGI-S-CINDCD-TPO-JUDIC        PIC X(01).
                    15 CTECWDGI-S-RINDCD-TPO-JUDIC        PIC X(60).
                    15 CTECWDGI-S-CTPO-BLOQ-OPER          PIC 9(01).
                    15 CTECWDGI-S-RTPO-BLOQ-OPER          PIC X(30).
                    15 CTECWDGI-S-CSIT-PARM-ENCRR         PIC X(01).
                    15 CTECWDGI-S-ISIT-PARM-ENCRR         PIC X(20).
                    20 CTECWDGI-S-CUSUAR-INCL             PIC X(09).
                    20 CTECWDGI-S-IUSUAR-INCL             PIC X(60).
                    20 CTECWDGI-S-CUSUAR-MANUT            PIC X(09).
                    20 CTECWDGI-S-IUSUAR-MANUT            PIC X(60).
                    20 CTECWDGI-S-HINCL-REG               PIC X(26).
                    20 CTECWDGI-S-HMANUT-REG              PIC X(26).
