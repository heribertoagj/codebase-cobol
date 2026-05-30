      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  69-ARGUMENTOS-ENTRADA.

       05  69-COLUNAS-DA-TABELA.
           10  69-CPRODT                   PIC S9(3)V COMP-3.
           10  69-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  69-CTPO-AGPTO-ROTRO         PIC S9(3)V COMP-3.
           10  69-CINDCD-OBRIG-AGPTO       PIC X(1).
           10  69-CINDCD-SIT-AGPTO         PIC X(1).
           10  69-CINDCD-CONCL-ROTRO       PIC X(1).
           10  69-HULT-ATULZ               PIC X(26).
           10  69-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  69-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  69-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  69-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  69-RETORNO.
       05  69-COD-RETORNO                         PIC 9(04).
       05  69-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  69-PROXIMO-RESTART                     PIC 9(05).
       05  69-QTDE-RETORNADA                      PIC 9(03).
       05  69-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  69-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T69.
           10  69RT-CPRODT                 PIC S9(3)V COMP-3.
           10  69RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  69RT-CTPO-AGPTO-ROTRO       PIC S9(3)V COMP-3.
           10  69RT-CINDCD-OBRIG-AGPTO     PIC X(1).
           10  69RT-CINDCD-SIT-AGPTO       PIC X(1).
           10  69RT-CINDCD-CONCL-ROTRO     PIC X(1).
           10  69RT-HULT-ATULZ             PIC X(26).
           10  69RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  69RT-CTERM                  PIC X(8).
