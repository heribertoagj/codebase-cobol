      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  17-ARGUMENTOS-ENTRADA.

       05  17-COLUNAS-DA-TABELA.
           10  17-CCONVE-GRAL-DESC         PIC S9(9)V COMP-3.
           10  17-CVRSAO-CONVE-GRAL        PIC S9(3)V COMP-3.
           10  17-CTPO-AGPTO-ROTRO         PIC S9(3)V COMP-3.
           10  17-CINDCD-OBRIG-AGPTO       PIC X(1).
           10  17-CINDCD-SIT-AGPTO         PIC X(1).
           10  17-CINDCD-CONCL-ROTRO       PIC X(1).
           10  17-DVALDC-ROTRO-ELMTO       PIC X(10).
           10  17-HULT-ATULZ               PIC X(26).
           10  17-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  17-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  17-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  17-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  17-RETORNO.
       05  17-COD-RETORNO                         PIC 9(04).
       05  17-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  17-PROXIMO-RESTART                     PIC 9(05).
       05  17-QTDE-RETORNADA                      PIC 9(03).
       05  17-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  17-OCORRENCIAS    OCCURS 070 TIMES INDEXED BY IND-T17.
           10  17RT-CCONVE-GRAL-DESC       PIC S9(9)V COMP-3.
           10  17RT-CVRSAO-CONVE-GRAL      PIC S9(3)V COMP-3.
           10  17RT-CTPO-AGPTO-ROTRO       PIC S9(3)V COMP-3.
           10  17RT-CINDCD-OBRIG-AGPTO     PIC X(1).
           10  17RT-CINDCD-SIT-AGPTO       PIC X(1).
           10  17RT-CINDCD-CONCL-ROTRO     PIC X(1).
           10  17RT-DVALDC-ROTRO-ELMTO     PIC X(10).
           10  17RT-HULT-ATULZ             PIC X(26).
           10  17RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  17RT-CTERM                  PIC X(8).
