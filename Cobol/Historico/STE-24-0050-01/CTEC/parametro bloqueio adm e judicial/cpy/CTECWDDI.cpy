      ******************************************************************
      * NOME BOOK : CTECWDDI - BOOK DE ENTRADA                         *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC3ALI                                           *
      * TAMANHO   : 00030 BYTES                                        *
      ******************************************************************
      * CAMPOS                                                         *
      * CTECWDDI-E-CMOTVO-SIT-CTA = CODIGO MOTVO ENCRR ADMINISTRATIVO  *
      * CTECWDDI-E-CTPO-BLOQ-OPER = TIPO DE BLOQUEIO QUE PODE SER:     *
      *                             1 - ADMINISTRATIVO                 *
      *                             2 - JUDICIAL                       *
      * CTECWDDI-E-CINDCD-TPO-JUDIC = COD MOTVO ENCRR JUDICIAL         *     
      ******************************************************************
           05 CTECWDDI-HEADER.
              10 CTECWDDI-COD-LAYOUT        PIC X(008) VALUE 'CTECWDDI'.
              10 CTECWDDI-TAM-LAYOUT        PIC 9(005) VALUE 00030.
           05 CTECWDDI-REGISTRO.
              10 CTECWDDI-BLOCO-ENTRADA.
                 15 CTECWDDI-E-CPARM-ENCRR-BLOQ    PIC 9(03).
                 15 CTECWDDI-E-CMOTVO-SIT-CTA      PIC 9(03).
                 15 CTECWDDI-E-CTPO-BLOQ-OPER      PIC 9(01).
                 15 CTECWDDI-E-CINDCD-TPO-JUDIC    PIC X(01).
              10 CTECWDDI-AUDITORIA. 
                 15 CTECWDDI-E-CUSUAR-INCL         PIC X(09).
