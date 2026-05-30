      ******************************************************************
      * NOME BOOK : I#DCOMU8 - AREA DO MODULO DCOM4610                 *
      * DESCRICAO : IDENTIFICAR CLIENTE MEI NO SISTEMA RBRF            *
      * TAMANHO   : 315 BYTES                                          *
      *----------------------------------------------------------------*
      * DCOMU8-IND-MEI: S ou N                                         *
      ******************************************************************
       01  DCOMU8-COMMAREA.
           05 DCOMU8-AREA-ENTRADA.
              10 DCOMU8-E-CCPF-CNPJ                 PIC  9(009).
              10 DCOMU8-E-CFLIAL-CPF-CNPJ           PIC  9(004).
              10 DCOMU8-E-CCTRL-CPF-CNPJ            PIC  9(002).

           05 DCOMU8-AREA-SAIDA.
              10 DCOMU8-S-COD-RETORNO               PIC  9(002).
              10 DCOMU8-S-LOC-RETORNO               PIC  9(004).
              10 DCOMU8-S-MSG-RETORNO               PIC  X(050).
              10 DCOMU8-S-ERRO-AREA                 PIC  X(107).
              10 DCOMU8-S-SQLCA-AREA                PIC  X(136).
              10 DCOMU8-S-IND-MEI                   PIC  X(001).
      ******************************************************************
