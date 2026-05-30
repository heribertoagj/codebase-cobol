      ******************************************************************
      * NOME BOOK : CTECWDDE - BOOK DE ENTRADA                         *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC1ALI                                           *
      * TAMANHO   : 00016 BYTES                                        *
      ******************************************************************
           05 CTECWDDE-HEADER.
              10 CTECWDDE-COD-LAYOUT        PIC X(008) VALUE 'CTECWDDE'.
              10 CTECWDDE-TAM-LAYOUT        PIC 9(005) VALUE 00021.
           05 CTECWDDE-REGISTRO.
              10 CTECWDDE-BLOCO-ENTRADA.
                 15 CTECWDDE-E-CPARM-ENCRR-BLOQ    PIC 9(03).
                 15 CTECWDDE-E-CMOTVO-SIT-CTA      PIC 9(03).
                 15 CTECWDDE-E-CTPO-BLOQ-OPER      PIC 9(01).
                 15 CTECWDDE-E-CINDCD-TPO-JUDIC    PIC X(01).
