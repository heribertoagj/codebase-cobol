      ******************************************************************
      * NOME BOOK : CTECWDEI - BOOK DE ENCTRADA                        *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC3A1C                                           *
      * TAMANHO   : 11124 BYTES                                        *
      ******************************************************************
           05 CTECWDEI-HEADER.
              10 CTECWDEI-COD-LAYOUT        PIC X(008) VALUE 'CTECWDEI'.
              10 CTECWDEI-TAM-LAYOUT        PIC 9(005) VALUE 11124.
           05 CTECWDEI-REGISTRO.
              10 CTECWDEI-BLOCO-ENTRADA.
                 15 CTECWDEI-E-CPARM-ENCRR-BLOQ        PIC 9(03).
              10 CTECWDEI-BLOCO-SAIDA.
                 15 CTECWDEI-S-CPARM-ENCRR-BLOQ        PIC 9(03).
                 15 CTECWDEI-S-CMOTVO-SIT-CTA          PIC 9(03).
                 15 CTECWDEI-S-RMOTVO-SIT-CTA          PIC X(60).
                 15 CTECWDEI-S-CINDCD-TPO-JUCIC        PIC X(01).
                 15 CTECWDEI-S-RINDCD-TPO-JUCIC        PIC X(60).
                 15 CTECWDEI-S-CTPO-BLOQ-OPER          PIC 9(01).
                 15 CTECWDEI-S-RTPO-BLOQ-OPER          PIC X(30).
                 15 CTECWDEI-S-CSIT-PARM-ENCRR         PIC X(01).
                 15 CTECWDEI-S-RSIT-PARM-ENCRR         PIC X(20).
                 15 CTECWDEI-S-CUSUAR-INCL             PIC X(09).
                 15 CTECWDEI-S-IUSUAR-INCL             PIC X(60).
                 15 CTECWDEI-S-CUSUAR-MANUT            PIC X(09).
                 15 CTECWDEI-S-IUSUAR-MANUT            PIC X(60).
                 15 CTECWDEI-S-HINCL-REG               PIC X(26).
                 15 CTECWDEI-S-HMANUT-REG              PIC X(26).
