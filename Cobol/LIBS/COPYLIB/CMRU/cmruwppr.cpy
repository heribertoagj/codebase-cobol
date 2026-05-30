      *---------------------------------------------------------------*
      *                         B R Q                                 *
      *---------------------------------------------------------------*
      * INCLUDE.............: I#CMRUPP - PARA MODULO RCOR5005(COR0006)*
      * GERACAO.............: SETEMBRO/2021                           *
      *---------------------------------------------------------------*
      * OBJETIVO....: AREA DE COMUNICACAO COM SERVICO CMRU8835        *
      *---------------------------------------------------------------*

       01  CMRUPP-AREA.
           05 CMRUPP-CONTROLE.
              10 CMRUPP-COD-RETORNO    PIC 9(02).
              10 CMRUPP-COD-ERRO       PIC X(04).
              10 CMRUPP-MSG-ERRO       PIC X(79).
              10 CMRUPP-ERRO-AREA      PIC X(110).
              10 CMRUPP-SQLCA          PIC X(136).
           05 CMRUPP-ENTRADA.
              10 CMRUPP-E-CCONTR       PIC 9(09).
              10 CMRUPP-E-CREFT-RECOR  PIC X(11).
           05 CMRUPP-SAIDA.
              10 CMRUPP-S-TAM-GRUPO    PIC 9(05).
              10 CMRUPP-S-GRUPO-PROP   PIC X(10000).
