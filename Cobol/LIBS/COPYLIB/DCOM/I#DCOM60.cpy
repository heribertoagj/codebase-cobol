      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  60-ARGUMENTOS-ENTRADA.

       05  60-COLUNAS-DA-TABELA.
           10  60-CPRODT               PIC S9(3)V USAGE COMP-3.
           10  60-CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
           10  60-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
           10  60-CINDCD-PSSOA-TOMAD   PIC X(1).
           10  60-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
           10  60-CCTA-CONC-CTBIL      PIC S9(3)V USAGE COMP-3.
           10  60-HINIC-VGCIA-DESC     PIC X(26).
           10  60-HFIM-VGCIA-DESC      PIC X(26).
           10  60-CEMPR                PIC S9(5)V USAGE COMP-3.
           10  60-CGRP-CTBIL-ATUAL     PIC S9(3)V USAGE COMP-3.
           10  60-CSGRP-CTBIL          PIC S9(3)V USAGE COMP-3.
           10  60-CCTA-CTBIL           PIC S9(7)V USAGE COMP-3.
           10  60-HULT-ATULZ           PIC X(26).
           10  60-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
           10  60-CTERM                PIC X(8).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  60-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  60-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  60-RETORNO.
       05  60-COD-RETORNO                         PIC 9(04).
       05  60-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  60-PROXIMO-RESTART                     PIC 9(05).
       05  60-QTDE-RETORNADA                      PIC 9(03).
       05  60-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  60-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T60.
           10  60RT-CPRODT               PIC S9(3)V USAGE COMP-3.
           10  60RT-CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
           10  60RT-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
           10  60RT-CINDCD-PSSOA-TOMAD   PIC X(1).
           10  60RT-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
           10  60RT-CCTA-CONC-CTBIL      PIC S9(3)V USAGE COMP-3.
           10  60RT-HINIC-VGCIA-DESC     PIC X(26).
           10  60RT-HFIM-VGCIA-DESC      PIC X(26).
           10  60RT-CEMPR                PIC S9(5)V USAGE COMP-3.
           10  60RT-CGRP-CTBIL-ATUAL     PIC S9(3)V USAGE COMP-3.
           10  60RT-CSGRP-CTBIL          PIC S9(3)V USAGE COMP-3.
           10  60RT-CCTA-CTBIL           PIC S9(7)V USAGE COMP-3.
           10  60RT-HULT-ATULZ           PIC X(26).
           10  60RT-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
           10  60RT-CTERM                PIC X(8).
