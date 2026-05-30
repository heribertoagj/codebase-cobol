      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  13-ARGUMENTOS-ENTRADA.

       05  13-COLUNAS-DA-TABELA.
           10  13-CCONVE-CLI-DESC          PIC S9(9)V COMP-3.
           10  13-CVRSAO-CONVE-CLI         PIC S9(3)V COMP-3.
           10  13-CTPO-AGPTO-ROTRO         PIC S9(3)V COMP-3.
           10  13-CINDCD-OBRIG-AGPTO       PIC X(1).
           10  13-CINDCD-SIT-AGPTO         PIC X(1).
           10  13-CINDCD-CONCL-ROTRO       PIC X(1).
           10  13-DVALDC-ROTRO-ELMTO       PIC X(10).
           10  13-HULT-ATULZ               PIC X(26).
           10  13-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  13-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  13-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  13-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  13-RETORNO.
       05  13-COD-RETORNO                         PIC 9(04).
       05  13-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  13-PROXIMO-RESTART                     PIC 9(05).
       05  13-QTDE-RETORNADA                      PIC 9(03).
       05  13-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  13-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T13.
           10  13RT-CCONVE-CLI-DESC        PIC S9(9)V COMP-3.
           10  13RT-CVRSAO-CONVE-CLI       PIC S9(3)V COMP-3.
           10  13RT-CTPO-AGPTO-ROTRO       PIC S9(3)V COMP-3.
           10  13RT-CINDCD-OBRIG-AGPTO     PIC X(1).
           10  13RT-CINDCD-SIT-AGPTO       PIC X(1).
           10  13RT-CINDCD-CONCL-ROTRO     PIC X(1).
           10  13RT-DVALDC-ROTRO-ELMTO     PIC X(10).
           10  13RT-HULT-ATULZ             PIC X(26).
           10  13RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  13RT-CTERM                  PIC X(8).
